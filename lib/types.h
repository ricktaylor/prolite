/*
 * types.h
 *
 *  Created on: 27 May 2017
 *      Author: rick
 */

#ifndef TYPES_H_
#define TYPES_H_

#include "heap.h"
#include "packed_types.h"

#include <assert.h>

/* Macro magic to declare the builtin string constants */
#define DECLARE_BUILTIN_STRING(name) ,BUILTIN_ATOM_##name
typedef enum builtin_atom_id
{
	BUILTIN_ATOM_ = 0
#include "builtin_strings"
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







struct predicate;

struct clause
{
	struct predicate*   m_pred;
	struct clause*      m_next;
	struct clause*      m_prev;
	size_t                m_var_count;
	const packed_t* m_head;
	size_t                m_entry_point;
};

typedef enum op_spec
{
	eFX,
	eFY,
	eXFX,
	eXFY,
	eYFX,
	eXF,
	eYF
} op_spec_t;

typedef struct operator
{
	struct operator* m_prev;
	op_spec_t        m_specifier;
	unsigned int     m_precedence;
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

	operator_t*        m_operators;

} module_t;

typedef struct context
{
	heap_t*   m_heap;
	packed_t* m_stack;
	module_t* m_module;
} context_t;

packed_t* push_string(packed_t* stack, prolite_type_t type, const unsigned char* str, size_t len);
packed_t* push_compound(packed_t* stack, uint64_t arity, const unsigned char* functor, size_t functor_len);

static inline packed_t* push_integer(packed_t* stack, int32_t v)
{
	(--stack)->m_u64val = PACK_TYPE(prolite_int32) | PACK_MANT_48(v);
	return stack;
}

static inline packed_t* push_double(packed_t* stack, double v)
{
	(--stack)->m_dval = v;
	return stack;
}

const packed_t* get_first_arg(const packed_t* compound, uint64_t* arity, debug_info_t* debug_info);
const packed_t* get_next_arg(const packed_t* p, debug_info_t* debug_info);
string_t get_compound(const packed_t** b, uint64_t* arity, debug_info_t* debug_info);
string_t get_string(const packed_t** b, debug_info_t* debug_info);

#endif /* TYPES_H_ */
