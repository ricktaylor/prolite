/*
 * term_types.h
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#ifndef PACKED_TYPES_H
#define PACKED_TYPES_H

// Explanation lifted straight out of duktape/duk_dblunion.h
/*
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
 *    A B C D E F G H    Big endian (e.g. 68k)
 *    H G F E D C B A    Little endian (e.g. x86)
 *    D C B A H G F E    Mixed/cross endian (e.g. ARM)
 *
 *  ARM is a special case: ARM double values are in mixed/cross endian
 *  format while ARM uint64_t values are in standard little endian
 *  format (H G F E D C B A).  When a double is read as a uint64_t
 *  from memory, the register will contain the (logical) value
 *  E F G H A B C D.  This requires some special handling below.
 * *
 *  Some processors may alter NaN values in a floating point load+store.
 *  For instance, on X86 a FLD + FSTP may convert a signaling NaN to a
 *  quiet one.  This is catastrophic when NaN space is used in term
 *  values.  See: misc/clang_aliasing.c.
 */

#include <stdint.h>

typedef enum prolite_type
{
	prolite_double = 0,
	prolite_var = 1,
	prolite_int32 = 2,
	prolite_atom = 3,
	prolite_compound = 4,	
	prolite_chars = 5,
	prolite_charcodes = 6,
	// unused = 7,
} prolite_type_t;

static const uint16_t prolite_debug_info = 0x8000;

#if defined(__aarch64__) || defined(__arm__)
#define PACK_EXP_16(v)    ((UINT64_C(0xFFFF) & (v)) << 24)
#define UNPACK_EXP_16(v)  ((uint16_t)((v) & (UINT64_C(0xFFFF) << 24) >> 24))
#define PACK_MANT_48(v)   (((v) & (UINT64_C(0xFFFF) << 32) >> 32) | (((v) & UINT64_C(0xFFFFFFFF)) << 32))
#define UNPACK_MANT_48(v) (((v) & (UINT64_C(0xFFFF) << 32) | (((v) & (UINT64_C(0xFFFFFFFF) << 32)) >> 32))
#else
#define PACK_EXP_16(v)    ((UINT64_C(0xFFFF) & (uint64_t)(v)) << 48)
#define UNPACK_EXP_16(v)  ((uint16_t)((uint64_t)(v) >> 48))
#define PACK_MANT_48(v)   ((uint64_t)(v) & ~(UINT64_C(0xFFFF) << 48))
#define UNPACK_MANT_48(v) PACK_MANT_48(v)
#endif

#define PACK_TYPE(type)   PACK_EXP_16(0x7FF0 | (type))
#define UNPACK_TYPE(v)    (UNPACK_EXP_16(v) & 0x8007)

#define PACK_TYPE_EMBED(type,flags,count,a,b,c,d,e)  (PACK_TYPE(type) | PACK_MANT_48(((UINT64_C(0x8000) | (((flags) & 7) << 11) | (((count) & 7) << 8) | (uint8_t)(a)) << 32) | (((uint32_t)(b) & 0xFF) << 24) | (((uint32_t)(c) & 0xFF) << 16) | (((uint32_t)(d) & 0xFF) << 8) | (uint8_t)(e)))

#define PACK_ATOM_EMBED_1(c)              PACK_TYPE_EMBED(prolite_atom,0,1,c,0,0,0,0)
#define PACK_ATOM_EMBED_2(c1,c2)          PACK_TYPE_EMBED(prolite_atom,0,2,c1,c2,0,0,0)
#define PACK_ATOM_EMBED_3(c1,c2,c3)       PACK_TYPE_EMBED(prolite_atom,0,3,c1,c2,c3,0,0)
#define PACK_ATOM_EMBED_4(c1,c2,c3,c4)    PACK_TYPE_EMBED(prolite_atom,0,4,c1,c2,c3,c4,0)
#define PACK_ATOM_EMBED_5(c1,c2,c3,c4,c5) PACK_TYPE_EMBED(prolite_atom,0,5,c1,c2,c3,c4,c5)

#define PACK_ATOM_BUILTIN(name)  (PACK_TYPE(prolite_atom) | PACK_MANT_48((UINT64_C(0x4000) << 32) | (uint32_t)(BUILTIN_ATOM_##name)))

#define MAX_ATOM_LEN      ((UINT64_C(1) << 47) - 1)

#define PACK_COMPOUND_EMBED_1(a,c)              PACK_TYPE_EMBED(prolite_compound,a,1,c,0,0,0,0)
#define PACK_COMPOUND_EMBED_2(a,c1,c2)          PACK_TYPE_EMBED(prolite_compound,a,2,c1,c2,0,0,0)
#define PACK_COMPOUND_EMBED_3(a,c1,c2,c3)       PACK_TYPE_EMBED(prolite_compound,a,3,c1,c2,c3,0,0)
#define PACK_COMPOUND_EMBED_4(a,c1,c2,c3,c4)    PACK_TYPE_EMBED(prolite_compound,a,4,c1,c2,c3,c4,0)
#define PACK_COMPOUND_EMBED_5(a,c1,c2,c3,c4,c5) PACK_TYPE_EMBED(prolite_compound,a,5,c1,c2,c3,c4,c5)

#define MAX_ARITY_EMBED   0x7
#define MAX_ARITY_BUILTIN 0x3FFF
#define MAX_ARITY         ((UINT64_C(1) << 47) - 1)

#define PACK_COMPOUND_BUILTIN(f,a)  (PACK_TYPE(prolite_compound) | PACK_MANT_48(((UINT64_C(0x4000) | ((uint16_t)(a) & MAX_ARITY_BUILTIN)) << 32) | (uint32_t)(BUILTIN_ATOM_##f)))

#define MAX_VAR_INDEX     ((UINT64_C(1) << 47) - 1)

typedef union term
{
	double   m_dval;
	uint64_t m_u64val;
} term_t;

static inline prolite_type_t get_term_type(const term_t* t)
{
	return UNPACK_TYPE(t->m_u64val) & 0x7;
}

#endif /* PACKED_TYPES_H */
