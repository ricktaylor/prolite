/*
 * box_types.h
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#ifndef BOX_TYPES_H_
#define BOX_TYPES_H_

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

#if defined(__aarch64__) || defined(__arm__)
#define BOX_EXP_16(v)    ((UINT64_C(0xFFFF) & (v)) << 24)
#define UNBOX_EXP_16(v)  ((uint16_t)((v) & (UINT64_C(0xFFFF) << 24) >> 24))
#define BOX_MANT_48(v)   (((v) & (UINT64_C(0xFFFF) << 32) >> 32) | (((v) & UINT64_C(0xFFFFFFFF)) << 32))
#define UNBOX_MANT_48(v) (((v) & (UINT64_C(0xFFFF) << 32) | (((v) & (UINT64_C(0xFFFFFFFF) << 32)) >> 32))
#else
#define BOX_EXP_16(v)    ((UINT64_C(0xFFFF) & (v)) << 48)
#define UNBOX_EXP_16(v)  ((uint16_t)((uint64_t)(v) >> 48))
#define BOX_MANT_48(v)   ((v) & ~(UINT64_C(0xFFFF) << 48))
#define UNBOX_MANT_48(v) BOX_MANT_48(v)
#endif

#define BOX_TYPE(type)   BOX_EXP_16(0x7FF0 | ((type) & 0x8007))
#define UNBOX_TYPE(v)    (UNBOX_EXP_16(v) & 0x8007)

#define BOX_HI16(u16)    BOX_MANT_48((UINT64_C(0xFFFF) & (u16)) << 32)
#define UNBOX_HI16(v)    ((uint16_t)(UNBOX_MANT_48(v) >> 32))

#define BOX_LOW32(u32)   BOX_MANT_48(UINT64_C(0xFFFFFFFF) & (u32))
#define UNBOX_LOW32(v)   ((uint32_t)(UNBOX_MANT_48(v) & UINT64_C(0xFFFFFFFF)))

#define BOX_TYPE_EMBED(type,flags,count,a,b,c,d,e)  (BOX_TYPE(type) | BOX_HI16(0x8000 | (((flags) & 0xF) << 11) | (((count) & 7) << 8) | (a)) | BOX_LOW32(((b) << 24) | ((c) << 16) | ((d) << 8) | (e)))
#define UNBOX_IS_TYPE_EMBED(v,type)                 (UNBOX_TYPE(v) == (type) && (UNBOX_HI16(v) & 0x8000))

#define BOX_ATOM_EMBED_1(c)              BOX_TYPE_EMBED(prolite_atom,0,1,c,0,0,0,0)
#define BOX_ATOM_EMBED_2(c1,c2)          BOX_TYPE_EMBED(prolite_atom,0,2,c1,c2,0,0,0)
#define BOX_ATOM_EMBED_3(c1,c2,c3)       BOX_TYPE_EMBED(prolite_atom,0,3,c1,c2,c3,0,0)
#define BOX_ATOM_EMBED_4(c1,c2,c3,c4)    BOX_TYPE_EMBED(prolite_atom,0,4,c1,c2,c3,c4,0)
#define BOX_ATOM_EMBED_5(c1,c2,c3,c4,c5) BOX_TYPE_EMBED(prolite_atom,0,5,c1,c2,c3,c4,c5)

#define BOX_COMPOUND_EMBED_1(a,c)              BOX_TYPE_EMBED(prolite_compound,a,1,c,0,0,0,0)
#define BOX_COMPOUND_EMBED_2(a,c1,c2)          BOX_TYPE_EMBED(prolite_compound,a,2,c1,c2,0,0,0)
#define BOX_COMPOUND_EMBED_3(a,c1,c2,c3)       BOX_TYPE_EMBED(prolite_compound,a,3,c1,c2,c3,0,0)
#define BOX_COMPOUND_EMBED_4(a,c1,c2,c3,c4)    BOX_TYPE_EMBED(prolite_compound,a,4,c1,c2,c3,c4,0)
#define BOX_COMPOUND_EMBED_5(a,c1,c2,c3,c4,c5) BOX_TYPE_EMBED(prolite_compound,a,5,c1,c2,c3,c4,c5)

#define MAX_ARITY_EMBED   0xF
#define MAX_ARITY_BUILTIN 0x3FFF
#define MAX_ARITY         ((UINT64_C(1) << 47) - 1)

#define BOX_ATOM_BUILTIN(name)        (BOX_TYPE(prolite_atom) | BOX_HI16(0x4000) | BOX_LOW32(BUILTIN_ATOM_##name))
#define BOX_COMPOUND_BUILTIN(f,a)     (BOX_TYPE(prolite_compound) | BOX_HI16(0x4000 | (a)) | BOX_LOW32(BUILTIN_ATOM_##f))

#define UNBOX_IS_TYPE_BUILTIN(v,type) (UNBOX_TYPE(v) == (type) && (UNBOX_HI16(v) & 0xC000) == 0x4000)

uint32_t is_builtin_string(const unsigned char* str, size_t len);
struct string_ptr_t* box_stack_string(struct stack_t** stack, struct string_ptr_t** strings, const unsigned char* str, size_t len);
uint64_t box_pointer(void* ptr);

#endif /* BOX_TYPES_H_ */
