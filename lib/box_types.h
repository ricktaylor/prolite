/*
 * box_types.h
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#ifndef BOX_TYPES_H_
#define BOX_TYPES_H_

// Lifted straight out of duktape/duk_dblunion.h
/*
 *  Union to access IEEE double memory representation, indexes for double
 *  memory representation, and some macros for double manipulation.
 *
 *  Use a union for bit manipulation to
 *  minimize aliasing issues in practice.  The C99 standard does not
 *  guarantee that this should work, but it's a very widely supported
 *  practice for low level manipulation.
 *
 *  IEEE double format summary:
 *
 *    seeeeeee eeeeffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff
 *       A        B        C        D        E        F        G        H
 *
 *    s       sign bit
 *    eee...  exponent field
 *    fff...  fraction
 *
 *  See http://en.wikipedia.org/wiki/Double_precision_floating-point_format.
 *
 *  NaNs are represented as exponent 0x7ff and mantissa != 0.  The NaN is a
 *  signaling NaN when the highest bit of the mantissa is zero, and a quiet
 *  NaN when the highest bit is set.
 *
 *  At least three memory layouts are relevant here:
 *
 *    A B C D E F G H    Big endian (e.g. 68k)           DUK_USE_DOUBLE_BE
 *    H G F E D C B A    Little endian (e.g. x86)        DUK_USE_DOUBLE_LE
 *    D C B A H G F E    Mixed/cross endian (e.g. ARM)   DUK_USE_DOUBLE_ME
 *
 *  ARM is a special case: ARM double values are in mixed/cross endian
 *  format while ARM uint64_t values are in standard little endian
 *  format (H G F E D C B A).  When a double is read as a uint64_t
 *  from memory, the register will contain the (logical) value
 *  E F G H A B C D.  This requires some special handling below.
 *
 *
 *  Some processors may alter NaN values in a floating point load+store.
 *  For instance, on X86 a FLD + FSTP may convert a signaling NaN to a
 *  quiet one.  This is catastrophic when NaN space is used in packed
 *  duk_tval values.  See: misc/clang_aliasing.c.
 */

#include <stdint.h>
#include <math.h>

#if defined(_MSC_VER)
#define inline __inline
#endif

union box_t
{
	double   m_dval;
	uint64_t m_u64val;
};

enum tag_type_t
{
	prolite_double = 0,
	prolite_var = 2,
	prolite_compound = 3,
	prolite_int32 = 4,
	prolite_atom = 5,
	prolite_chars = 6,
	prolite_charcodes = 7,

	prolite_mask = 0xF
};

#if defined(__aarch64__) || defined(__arm__)
#define BOX_EXP_16(v)    ((UINT64_C(0xFFFF) & (v)) << 24)
#define UNBOX_EXP_16(v)  ((v) & (UINT64_C(0xFFFF) << 24) >> 24)
#define BOX_MANT_48(v)   (((v) & (UINT64_C(0xFFFF) << 32) >> 32) | (((v) & UINT64_C(0xFFFFFFFF)) << 32))
#define UNBOX_MANT_48(v) (((v) & (UINT64_C(0xFFFF) << 32) | (((v) & (UINT64_C(0xFFFFFFFF) << 32)) >> 32))
#else
#define BOX_EXP_16(v)    ((UINT64_C(0xFFFF) & (v)) << 48)
#define UNBOX_EXP_16(v)  ((v) & (UINT64_C(0xFFFF) << 48) >> 48)
#define BOX_MANT_48(v)   ((v) & ~(UINT64_C(0xFFFF) << 48))
#define UNBOX_MANT_48(v) BOX_MANT_48(v)
#endif

#define BOX_TYPE(type)   BOX_EXP_16(UINT64_C(0x7FF0) | (((type) & 8) << 11) | ((type) & 7))
#define UNBOX_TYPE(v)    ((UNBOX_EXP_16(v) & UINT64_C(0x8000) >> 12) | (UNBOX_EXP_16(v) & UINT64_C(0x7)))

#define BOX_HI48(u16)    BOX_MANT_48((UINT64_C(0xFFFF) & (u16)) << 32)
#define UNBOX_HI48(v)    ((UNBOX_MANT_48(v) & (UINT64_C(0xFFFF) << 16)) >> 32)

#define BOX_U32(u32)     BOX_MANT_48(UINT64_C(0xFFFFFFFF) & (u32))
#define UNBOX_U32(v)     (UNBOX_MANT_48(v) & UINT64_C(0xFFFFFFFF))

#define BOX_TYPE_EMBED(type,flags,count,a,b,c,d,e)  (BOX_TYPE(type) | BOX_MANT_48((UINT64_C(1) << 47) | (((flags) & UINT64_C(0xF)) << 43) | (((count) & UINT64_C(7)) << 40) | ((uint64_t)(a) << 32) | ((uint64_t)(b) << 24) | ((uint64_t)(c) << 16) | ((uint64_t)(d) << 8) | (uint64_t)(e)))
#define UNBOX_IS_TYPE_EMBED(v,type)                 (UNBOX_TYPE(v) == (type) && (UNBOX_MANT_48(v) & (UINT64_C(1) << 47)))
#define UNBOX_EMBED_FLAGS(v)                        ((UNBOX_MANT_48(v) >> 43) & 0xF)

#define BOX_COMPOUND_EMBED_1(a,c)              BOX_TYPE_EMBED(prolite_compound,a,1,c,0,0,0,0)
#define BOX_COMPOUND_EMBED_2(a,c1,c2)          BOX_TYPE_EMBED(prolite_compound,a,2,c1,c2,0,0,0)
#define BOX_COMPOUND_EMBED_3(a,c1,c2,c3)       BOX_TYPE_EMBED(prolite_compound,a,3,c1,c2,c3,0,0)
#define BOX_COMPOUND_EMBED_4(a,c1,c2,c3,c4)    BOX_TYPE_EMBED(prolite_compound,a,4,c1,c2,c3,c4,0)
#define BOX_COMPOUND_EMBED_5(a,c1,c2,c3,c4,c5) BOX_TYPE_EMBED(prolite_compound,a,5,c1,c2,c3,c4,c5)

#define MAX_EMBED_ARITY 0xF
#define MAX_ARITY       ((UINT64_C(1) << 47) - 1)

#define BOX_ATOM_EMBED_1(c)              BOX_TYPE_EMBED(prolite_atom,0,1,c,0,0,0,0)
#define BOX_ATOM_EMBED_2(c1,c2)          BOX_TYPE_EMBED(prolite_atom,0,2,c1,c2,0,0,0)
#define BOX_ATOM_EMBED_3(c1,c2,c3)       BOX_TYPE_EMBED(prolite_atom,0,3,c1,c2,c3,0,0)
#define BOX_ATOM_EMBED_4(c1,c2,c3,c4)    BOX_TYPE_EMBED(prolite_atom,0,4,c1,c2,c3,c4,0)
#define BOX_ATOM_EMBED_5(c1,c2,c3,c4,c5) BOX_TYPE_EMBED(prolite_atom,0,5,c1,c2,c3,c4,c5)

//#define BOX_TAG_COMPOUND_EMBED   BOX_TAG_TYPE_EMBED(BOX_TAG_COMPOUND)
//#define BOX_TAG_ATOM_EMBED       BOX_TAG_TYPE_EMBED(BOX_TAG_ATOM)
//#define BOX_TAG_ATOM_BUILTIN     BOX_TYPE_U32(prolite_atom,0x4000,0)

static inline void box_pointer(union box_t* b, void* ptr)
{
#if UINTPTR_MAX == UINT32_MAX
	b->m_u64val |= BOX_U32((uintptr_t)ptr);
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	b->m_u64val |= BOX_MANT_48(((uintptr_t)ptr >> 3) & UINT64_C(0x1FFFFFFFFFFF));
#else
#error No idea what to do with addresses on your architecture!
#endif
}

static inline void* unbox_pointer(const union box_t* b)
{
#if UINTPTR_MAX == UINT32_MAX
	return (void*)(uintptr_t)UNOX_U32(b->m_u64val);
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	/* Sign extend to make an x86_64 canonical address */
	struct pun { uint64_t u45 : 45; } p;
	return (void*)((uintptr_t)(p.u45 = UNBOX_MANT_48(b->m_u64val) << 3));
#else
#error No idea what to do with addresses on your architecture!
#endif
}

static inline union box_t box_double(double d)
{
	union box_t r;
	if (isnan(d))
		r.m_u64val = BOX_EXP_16(0x7FF8);
	else
		r.m_dval = d;
	return r;
}

static inline double unbox_double(const union box_t* b)
{
	return b->m_dval;
}

static inline union box_t box_int32(int32_t n)
{
	union box_t r;
	r.m_u64val = BOX_TYPE(prolite_int32) | BOX_U32((uint32_t)n);
	return r;
}

static inline int32_t unbox_int32(const union box_t* b)
{
	return (int32_t)UNBOX_U32(b->m_u64val);
}

struct context_t;

int box_string(enum tag_type_t type, struct context_t* context, union box_t* b, const unsigned char* str, size_t len);
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

#define BUILTIN_ATOM(name) (BOX_TYPE(prolite_atom) | BOX_HI48(0x4000) | BOX_U32(BUILTIN_ATOM_##name))

#endif /* BOX_TYPES_H_ */
