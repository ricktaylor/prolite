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

#define BOX_TAG_MASK        (UINT64_C(0xFFFF) << 48)
#define BOX_TAG_VAR         (UINT64_C(0xFFF7) << 48)
#define BOX_TAG_COMPOUND    (UINT64_C(0xFFF6) << 48)
#define BOX_TAG_INT32       (UINT64_C(0xFFF5) << 48)
#define BOX_TAG_ATOM        (UINT64_C(0xFFF4) << 48)
#define BOX_TAG_CHARS       (UINT64_C(0xFFF3) << 48)
#define BOX_TAG_CODES       (UINT64_C(0xFFF2) << 48)
#define BOX_TAG_OBJECT      (UINT64_C(0xFFF1) << 48)

#define BOX_TAG_COMPOUND_EMBED   (UINT64_C(0xFFF68) << 44)

#define BOX_COMPOUND_EMBED_1(a,c)              (BOX_TAG_COMPOUND_EMBED | ((uint64_t)1 << 44) | ((uint64_t)(a) << 40) | ((uint64_t)(c) << 32))
#define BOX_COMPOUND_EMBED_2(a,c1,c2)          (BOX_TAG_COMPOUND_EMBED | ((uint64_t)2 << 44) | ((uint64_t)(a) << 40) | ((uint64_t)(c1) << 32) | ((uint64_t)(c2) << 24))
#define BOX_COMPOUND_EMBED_3(a,c1,c2,c3)       (BOX_TAG_COMPOUND_EMBED | ((uint64_t)3 << 44) | ((uint64_t)(a) << 40) | ((uint64_t)(c1) << 32) | ((uint64_t)(c2) << 24) | ((uint64_t)(c3) << 16))
#define BOX_COMPOUND_EMBED_4(a,c1,c2,c3,c4)    (BOX_TAG_COMPOUND_EMBED | ((uint64_t)4 << 44) | ((uint64_t)(a) << 40) | ((uint64_t)(c1) << 32) | ((uint64_t)(c2) << 24) | ((uint64_t)(c3) << 16) | ((uint64_t)(c4) << 8))
#define BOX_COMPOUND_EMBED_5(a,c1,c2,c3,c4,c5) (BOX_TAG_COMPOUND_EMBED | ((uint64_t)5 << 44) | ((uint64_t)(a) << 40) | ((uint64_t)(c1) << 32) | ((uint64_t)(c2) << 24) | ((uint64_t)(c3) << 16) | ((uint64_t)(c4) << 8) | (uint64_t)(c5))

#define MAX_ARITY ((UINT64_C(1) << 47) - 1)

#define BOX_TAG_ATOM_EMBED       (UINT64_C(0xFFF48) << 44)
#define BOX_TAG_ATOM_BUILTIN     (UINT64_C(0xFFF44) << 44)
#define BOX_TAG_CHARS_EMBED      (UINT64_C(0xFFF38) << 44)
#define BOX_TAG_CHARS_BUILTIN    (UINT64_C(0xFFF34) << 44)
#define BOX_TAG_CODES_EMBED      (UINT64_C(0xFFF28) << 44)
#define BOX_TAG_CODES_BUILTIN    (UINT64_C(0xFFF24) << 44)

#define BOX_ATOM_EMBED_1(c)              (BOX_TAG_ATOM_EMBED | ((uint64_t)1 << 40) | ((uint64_t)(c) << 32))
#define BOX_ATOM_EMBED_2(c1,c2)          (BOX_TAG_ATOM_EMBED | ((uint64_t)2 << 40) | ((uint64_t)(c1) << 32) | ((uint64_t)(c2) << 24))
#define BOX_ATOM_EMBED_3(c1,c2,c3)       (BOX_TAG_ATOM_EMBED | ((uint64_t)3 << 40) | ((uint64_t)(c1) << 32) | ((uint64_t)(c2) << 24) | ((uint64_t)(c3) << 16))
#define BOX_ATOM_EMBED_4(c1,c2,c3,c4)    (BOX_TAG_ATOM_EMBED | ((uint64_t)4 << 40) | ((uint64_t)(c1) << 32) | ((uint64_t)(c2) << 24) | ((uint64_t)(c3) << 16) | ((uint64_t)(c4) << 8))
#define BOX_ATOM_EMBED_5(c1,c2,c3,c4,c5) (BOX_TAG_ATOM_EMBED | ((uint64_t)5 << 40) | ((uint64_t)(c1) << 32) | ((uint64_t)(c2) << 24) | ((uint64_t)(c3) << 16) | ((uint64_t)(c4) << 8) | (uint64_t)(c5))

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

struct context_t;

int box_string(struct context_t* context, union box_t* b, const unsigned char* str, size_t len);

const unsigned char* unbox_string(struct context_t* context, const union box_t* b, size_t* len);

const unsigned char* unbox_compound(struct context_t* context, const union box_t* b, uint64_t* arity, size_t* flen);

/* Macro magic to declare the builtin string constants */
#define DECLARE_BUILTIN_STRING(name) BUILTIN_ATOM_##name,
enum builtin_atoms_t
{
#include "builtin_strings.h"
};

#define BUILTIN_ATOM(name) (BOX_TAG_ATOM_BUILTIN | (uint32_t)BUILTIN_ATOM_##name)

#endif /* BOX_TYPES_H_ */
