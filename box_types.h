/*
 * box_types.h
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#ifndef BOX_TYPES_H_
#define BOX_TYPES_H_

#include <stdint.h>
#include <math.h>

#if defined(_MSC_VER)
#define inline __inline
#endif

union box_t
{
	double   m_dval;
	uint64_t m_uval;
};

static const uint64_t BOX_TAG_MASK        = UINT64_C(0xFFFF) << 48;
static const uint64_t BOX_TAG_VAR         = UINT64_C(0xFFF7) << 48;
static const uint64_t BOX_TAG_COMPOUND    = UINT64_C(0xFFF6) << 48;
static const uint64_t BOX_TAG_INT32       = UINT64_C(0xFFF5) << 48;
static const uint64_t BOX_TAG_ATOM        = UINT64_C(0xFFF4) << 48;
static const uint64_t BOX_TAG_CHARS       = UINT64_C(0xFFF3) << 48;
static const uint64_t BOX_TAG_CODES       = UINT64_C(0xFFF2) << 48;
static const uint64_t BOX_TAG_OBJECT      = UINT64_C(0xFFF1) << 48;

static const uint64_t BOX_TAG_TYPE_MASK        = UINT64_C(0xFFFFE) << 44;
static const uint64_t BOX_TAG_COMPOUND_BUILTIN = UINT64_C(0xFFF68) << 44;
static const uint64_t BOX_TAG_ATOM_EMBED       = UINT64_C(0xFFF48) << 44;
static const uint64_t BOX_TAG_CHARS_EMBED      = UINT64_C(0xFFF38) << 44;
static const uint64_t BOX_TAG_CODES_EMBED      = UINT64_C(0xFFF28) << 44;

static inline void box_pointer(union box_t* b, void* ptr)
{
#if UINTPTR_MAX == UINT32_MAX
	b->m_uval |= (uintptr_t)ptr;
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	b->m_uval |= ((uintptr_t)ptr >> 3 ) & ~BOX_TAG_MASK;
#else
#error No idea what to do with addresses on your architecture!
#endif
}

static inline void* unbox_pointer(const union box_t* b)
{
#if UINTPTR_MAX == UINT32_MAX
	return (void*)(uintptr_t)(b->m_uval);
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	/* Sign extend to make an x86_64 canonical address */
	struct pun { uint64_t u45 : 45; } p;
	return (void*)((uintptr_t)(p.u45 = b->m_uval) << 3);
#else
#error No idea what to do with addresses on your architecture!
#endif
}

struct context_t;

int box_string(struct context_t* context, union box_t* b, const unsigned char* str, size_t len);
const unsigned char* unbox_string(struct context_t* context, const union box_t* b, size_t* len);

static inline union box_t box_double(double d)
{
	union box_t r;
	r.m_dval = d;
	if (isnan(d))
		r.m_uval = UINT64_C(0xFFF8) << 48;
	return r;
}

static inline double unbox_double(const union box_t* b)
{
	return b->m_dval;
}

static inline union box_t box_int32(int32_t n)
{
	union box_t r;
	r.m_uval = BOX_TAG_INT32 | (uint32_t)n;
	return r;
}

static inline int32_t unbox_int32(const union box_t* b)
{
	return (int32_t)(b->m_uval & UINT64_C(0xFFFFFFFF));
}

#endif /* BOX_TYPES_H_ */
