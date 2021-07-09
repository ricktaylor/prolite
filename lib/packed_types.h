/*
 * packed_types.h
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
 *  quiet one.  This is catastrophic when NaN space is used in packed
 *  values.  See: misc/clang_aliasing.c.
 */

enum tag_type_t
{
	prolite_double = 0,
	prolite_int32 = 1,
	prolite_atom = 2,
	prolite_compound = 3,
	prolite_var = 4,
	prolite_chars = 5,
	prolite_charcodes = 6,

	// unused = 7,
	// unused = 0x8001,
	// unused = 0x8002,
	// unused = 0x8003,
	// unused = 0x8004,
	// unused = 0x8005,
	// unused = 0x8006,
	PROLITE_DEBUG_INFO = 0x8007
};

#if defined(__aarch64__) || defined(__arm__)
#define PACK_EXP_16(v)    ((UINT64_C(0xFFFF) & (v)) << 24)
#define UNPACK_EXP_16(v)  ((uint16_t)((v) & (UINT64_C(0xFFFF) << 24) >> 24))
#define PACK_MANT_48(v)   (((v) & (UINT64_C(0xFFFF) << 32) >> 32) | (((v) & UINT64_C(0xFFFFFFFF)) << 32))
#define UNPACK_MANT_48(v) (((v) & (UINT64_C(0xFFFF) << 32) | (((v) & (UINT64_C(0xFFFFFFFF) << 32)) >> 32))
#else
#define PACK_EXP_16(v)    ((UINT64_C(0xFFFF) & (v)) << 48)
#define UNPACK_EXP_16(v)  ((uint16_t)((uint64_t)(v) >> 48))
#define PACK_MANT_48(v)   ((v) & ~(UINT64_C(0xFFFF) << 48))
#define UNPACK_MANT_48(v) PACK_MANT_48(v)
#endif

#define PACK_TYPE(type)   PACK_EXP_16(0x7FF0 | ((type) & 0x8007))
#define UNPACK_TYPE(v)    (UNPACK_EXP_16(v) & 0x8007)

#define PACK_HI16(u16)    PACK_MANT_48((UINT64_C(0xFFFF) & (u16)) << 32)
#define UNPACK_HI16(v)    ((uint16_t)(UNPACK_MANT_48(v) >> 32))

#define PACK_LOW32(u32)   PACK_MANT_48(UINT64_C(0xFFFFFFFF) & (u32))
#define UNPACK_LOW32(v)   ((uint32_t)(UNPACK_MANT_48(v) & UINT64_C(0xFFFFFFFF)))

#define PACK_TYPE_EMBED(type,flags,count,a,b,c,d,e)  (PACK_TYPE(type) | PACK_HI16(0x8000 | (((flags) & 0xF) << 11) | (((count) & 7) << 8) | (a)) | PACK_LOW32(((b) << 24) | ((c) << 16) | ((d) << 8) | (e)))
#define UNPACK_IS_TYPE_EMBED(v,type)                 (UNPACK_TYPE(v) == (type) && (UNPACK_HI16(v) & 0x8000))

#define PACK_ATOM_EMBED_1(c)              PACK_TYPE_EMBED(prolite_atom,0,1,c,0,0,0,0)
#define PACK_ATOM_EMBED_2(c1,c2)          PACK_TYPE_EMBED(prolite_atom,0,2,c1,c2,0,0,0)
#define PACK_ATOM_EMBED_3(c1,c2,c3)       PACK_TYPE_EMBED(prolite_atom,0,3,c1,c2,c3,0,0)
#define PACK_ATOM_EMBED_4(c1,c2,c3,c4)    PACK_TYPE_EMBED(prolite_atom,0,4,c1,c2,c3,c4,0)
#define PACK_ATOM_EMBED_5(c1,c2,c3,c4,c5) PACK_TYPE_EMBED(prolite_atom,0,5,c1,c2,c3,c4,c5)

#define PACK_COMPOUND_EMBED_1(a,c)              PACK_TYPE_EMBED(prolite_compound,a,1,c,0,0,0,0)
#define PACK_COMPOUND_EMBED_2(a,c1,c2)          PACK_TYPE_EMBED(prolite_compound,a,2,c1,c2,0,0,0)
#define PACK_COMPOUND_EMBED_3(a,c1,c2,c3)       PACK_TYPE_EMBED(prolite_compound,a,3,c1,c2,c3,0,0)
#define PACK_COMPOUND_EMBED_4(a,c1,c2,c3,c4)    PACK_TYPE_EMBED(prolite_compound,a,4,c1,c2,c3,c4,0)
#define PACK_COMPOUND_EMBED_5(a,c1,c2,c3,c4,c5) PACK_TYPE_EMBED(prolite_compound,a,5,c1,c2,c3,c4,c5)

#define MAX_ARITY_EMBED   0xF
#define MAX_ARITY_BUILTIN 0x3FFF
#define MAX_ARITY         ((UINT64_C(1) << 47) - 1)

#define PACK_ATOM_BUILTIN(name)        (PACK_TYPE(prolite_atom) | PACK_HI16(0x4000) | PACK_LOW32(BUILTIN_ATOM_##name))
#define PACK_COMPOUND_BUILTIN(f,a)     (PACK_TYPE(prolite_compound) | PACK_HI16(0x4000 | (a)) | PACK_LOW32(BUILTIN_ATOM_##f))

#define UNPACK_IS_TYPE_BUILTIN(v,type) (UNPACK_TYPE(v) == (type) && (UNPACK_HI16(v) & 0xC000) == 0x4000)

union packed_t
{
	double   m_dval;
	uint64_t m_u64val;
};

#endif /* PACKED_TYPES_H */
