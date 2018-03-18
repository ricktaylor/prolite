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

#define BOX_TYPE_EMBED(type,flags,count,a,b,c,d,e)  ((type) | (((flags) & UINT64_C(0x1F)) << 43) | (((count) & UINT64_C(7)) << 40) | ((uint64_t)(a) << 32) | ((uint64_t)(b) << 24) | ((uint64_t)(c) << 16) | ((uint64_t)(d) << 8) | (uint64_t)(e))

#define BOX_TAG_TYPE_EMBED(type)  ((type) | (UINT64_C(0x10) << 43))

#define BOX_COMPOUND_EMBED_1(a,c)              BOX_TYPE_EMBED(BOX_TAG_COMPOUND,0x10 | a,1,c,0,0,0,0)
#define BOX_COMPOUND_EMBED_2(a,c1,c2)          BOX_TYPE_EMBED(BOX_TAG_COMPOUND,0x10 | a,2,c1,c2,0,0,0)
#define BOX_COMPOUND_EMBED_3(a,c1,c2,c3)       BOX_TYPE_EMBED(BOX_TAG_COMPOUND,0x10 | a,3,c1,c2,c3,0,0)
#define BOX_COMPOUND_EMBED_4(a,c1,c2,c3,c4)    BOX_TYPE_EMBED(BOX_TAG_COMPOUND,0x10 | a,4,c1,c2,c3,c4,0)
#define BOX_COMPOUND_EMBED_5(a,c1,c2,c3,c4,c5) BOX_TYPE_EMBED(BOX_TAG_COMPOUND,0x10 | a,5,c1,c2,c3,c4,c5)

#define MAX_ARITY ((UINT64_C(1) << 47) - 1)

#define BOX_TAG_COMPOUND_EMBED   BOX_TAG_TYPE_EMBED(BOX_TAG_COMPOUND)
#define BOX_TAG_ATOM_EMBED       BOX_TAG_TYPE_EMBED(BOX_TAG_ATOM)
#define BOX_TAG_ATOM_BUILTIN     (UINT64_C(0xFFF44) << 44)

#define BOX_ATOM_EMBED_1(c)              BOX_TYPE_EMBED(BOX_TAG_ATOM,0x10,1,c,0,0,0,0)
#define BOX_ATOM_EMBED_2(c1,c2)          BOX_TYPE_EMBED(BOX_TAG_ATOM,0x10,2,c1,c2,0,0,0)
#define BOX_ATOM_EMBED_3(c1,c2,c3)       BOX_TYPE_EMBED(BOX_TAG_ATOM,0x10,3,c1,c2,c3,0,0)
#define BOX_ATOM_EMBED_4(c1,c2,c3,c4)    BOX_TYPE_EMBED(BOX_TAG_ATOM,0x10,4,c1,c2,c3,c4,0)
#define BOX_ATOM_EMBED_5(c1,c2,c3,c4,c5) BOX_TYPE_EMBED(BOX_TAG_ATOM,0x10,5,c1,c2,c3,c4,c5)

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

uint32_t embed_string_code(const union box_t* b);

const unsigned char* unbox_compound(struct context_t* context, const union box_t* b, uint64_t* arity, size_t* flen);
uint64_t compound_arity(const union box_t* b);

/* Macro magic to declare the builtin string constants */
#define DECLARE_BUILTIN_STRING(name) BUILTIN_ATOM_##name,
enum builtin_atoms_t
{
#include "builtin_strings.h"
};

#define BUILTIN_ATOM(name) (BOX_TAG_ATOM_BUILTIN | (uint32_t)BUILTIN_ATOM_##name)

#endif /* BOX_TYPES_H_ */
