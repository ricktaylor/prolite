/*
 * types.h
 *
 *  Created on: 27 May 2017
 *      Author: rick
 */

#ifndef TYPES_H_
#define TYPES_H_

#include <stddef.h>

typedef enum prolite_type
{
	prolite_double = 0,
	prolite_var = 1,
	prolite_integer = 2,
	prolite_atom = 3,
	prolite_compound = 4,	
	prolite_chars = 5,
	prolite_charcodes = 6,
	prolite_userdata = 7

} prolite_type_t;

static const uint16_t prolite_debug_info = 0x8000;

#include "packed_types.h"

typedef union term
{
	double      m_dval;
	uint64_t    m_u64val;
	const void* m_pval;
} term_t;

/* Macro magic to declare the builtin string constants */
#define DECLARE_BUILTIN_STRING(name) BUILTIN_ATOM_##name,
typedef enum builtin_atom_id
{
#include "builtin_strings.h"
	MAX_BUILTIN_ATOM
} builtin_atom_id_t;

typedef struct string
{
	size_t               m_len;
	const unsigned char* m_str;
	unsigned char        m_data[5]; //< May be used for embedded strings
} string_t;

typedef struct debug_info
{
	int TODO;
} debug_info_t;

const debug_info_t* get_debug_info(const term_t* t);

term_t* push_string(term_t* stack, prolite_type_t type, const unsigned char* str, size_t len, int external, const debug_info_t* debug_info);
term_t* push_predicate(term_t* stack, uint64_t arity, const unsigned char* functor, size_t functor_len, int external, const debug_info_t* debug_info);
term_t* push_debug_info(term_t* stack, const debug_info_t* debug_info);

static inline term_t* push_integer(term_t* stack, int64_t i, const debug_info_t* debug_info)
{
	prolite_type_t type = prolite_integer;
	if (debug_info)
	{
		stack = push_debug_info(stack,debug_info);
		type |= prolite_debug_info;
	}
	(--stack)->m_u64val = PACK_TYPE(type) | PACK_MANT_48(i);
	return stack;
}

static inline term_t* push_double(term_t* stack, double d)
{
	(--stack)->m_dval = d;
	return stack;
}

static inline term_t* push_var(term_t* stack, size_t idx, const debug_info_t* debug_info)
{
	prolite_type_t type = prolite_var;
	if (debug_info)
	{
		stack = push_debug_info(stack,debug_info);
		type |= prolite_debug_info;
	}
	(--stack)->m_u64val = PACK_TYPE(type) | PACK_MANT_48(idx);
	return stack;
}

static inline size_t get_var_index(const term_t* v)
{
	return UNPACK_MANT_48(v->m_u64val);
}

static inline int64_t get_integer(const term_t* i)
{
	struct sign_extend
	{
		int64_t i64 : 48;
	};

	struct sign_extend se = { .i64 = UNPACK_MANT_48(i->m_u64val) };
	return se.i64;
}

static inline prolite_type_t get_term_type(const term_t* t)
{
	uint16_t exp = UNPACK_EXP_16(t->m_u64val);
	if ((exp & 0x7FF0) != 0x7FF0)
		return prolite_double;

	return (prolite_type_t)(exp & 0x7);
}

static inline unsigned int get_term_subtype(const term_t* t)
{
	return ((UNPACK_MANT_48(t->m_u64val) >> 32) >> 14);
}

const term_t* get_first_arg(const term_t* compound, size_t* arity);
const term_t* get_next_arg(const term_t* t);
void get_predicate(const term_t* t, string_t* str, size_t* arity, const debug_info_t** debug_info);
void get_string(const term_t* t, string_t* str, const debug_info_t** debug_info);

int predicate_compare(const term_t* c1, const term_t* c2);
int term_compare(const term_t* t1, const term_t* t2);

#endif /* TYPES_H_ */
