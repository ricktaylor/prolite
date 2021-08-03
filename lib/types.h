/*
 * types.h
 *
 *  Created on: 27 May 2017
 *      Author: rick
 */

#ifndef TYPES_H_
#define TYPES_H_

#include "heap.h"

typedef enum prolite_type
{
	prolite_double = 0,
	prolite_var = 1,
	prolite_int32 = 2,
	prolite_atom = 3,
	prolite_compound = 4,	
	prolite_chars = 5,
	prolite_charcodes = 6,
	prolite_userdata = 7

} prolite_type_t;

#include "packed_types.h"

typedef union term
{
	double   m_dval;
	uint64_t m_u64val;
} term_t;

/* Macro magic to declare the builtin string constants */
#define DECLARE_BUILTIN_STRING(name) BUILTIN_ATOM_##name,
typedef enum builtin_atom_id
{
#include "builtin_strings.h"
	BUILTIN_ATOM_
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

typedef enum operator_specifier
{
	eFX,
	eFY,
	eXFX,
	eXFY,
	eYFX,
	eXF,
	eYF
} operator_specifier_t;

typedef struct operator
{
	struct operator*     m_prev;
	operator_specifier_t m_specifier;
	unsigned int         m_precedence;
} operator_t;

typedef struct module
{
	struct module_flags
	{
		unsigned char_conversion : 1;
		unsigned double_quotes : 2;
		unsigned back_quotes : 2;
		unsigned debug : 1;
		unsigned unknown : 2;
		unsigned colon_sets_calling_context : 1;
	} m_flags;

	operator_t* m_operators;

} module_t;

typedef struct context
{
	heap_t*   m_heap;
	uint64_t* m_stack;
	module_t* m_module;
} context_t;

uint64_t* push_string(uint64_t* stack, prolite_type_t type, const unsigned char* str, size_t len, int external);
uint64_t* push_predicate(uint64_t* stack, uint64_t arity, const unsigned char* functor, size_t functor_len, int external);

static inline uint64_t* push_integer(uint64_t* stack, int32_t v)
{
	*(--stack) = PACK_TYPE(prolite_int32) | PACK_MANT_48(v);
	return stack;
}

static inline uint64_t* push_double(uint64_t* stack, double v)
{
	*(--stack) = v;
	return stack;
}

static inline uint64_t* push_var(uint64_t* stack, uint64_t idx)
{
	*(--stack) = PACK_TYPE(prolite_var) | PACK_MANT_48(idx);
	return stack;
}

static inline uint64_t get_var_index(const term_t* v)
{
	return UNPACK_MANT_48(v->m_u64val);
}

static inline int32_t get_integer(const term_t* v)
{
	return UNPACK_MANT_48(v->m_u64val);
}

static inline prolite_type_t get_term_type(const term_t* t)
{
	return UNPACK_TYPE(t->m_u64val) & 0x7;
}

static inline unsigned int get_term_subtype(const term_t* t)
{
	return ((UNPACK_MANT_48(t->m_u64val) >> 32) >> 14);
}

static inline int has_debug_info(const term_t* t)
{
	return !!(UNPACK_TYPE(t->m_u64val) & prolite_debug_info);
}

const term_t* get_first_arg(const term_t* compound, uint64_t* arity, const debug_info_t** debug_info);
const term_t* get_next_arg(const term_t* p, const debug_info_t** debug_info);
string_t get_predicate(const term_t* b, uint64_t* arity, const debug_info_t** debug_info);
string_t get_string(const term_t* b, const debug_info_t** debug_info);

int predicate_compare(const term_t* c1, const term_t* c2);
int term_compare(const term_t* t1, const term_t* t2);

#endif /* TYPES_H_ */
