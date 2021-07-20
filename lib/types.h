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
#define DECLARE_BUILTIN_STRING(name) BUILTIN_ATOM_##name,
enum builtin_atoms_t
{
#include "builtin_strings"
};

struct string_t
{
	size_t               m_len;
	const unsigned char* m_str;
	unsigned char        m_data[5]; //< May be used for embedded strings
};

struct debug_info_t
{
	int TODO;
};







struct predicate_t;

struct clause_t
{
	struct predicate_t*   m_pred;
	struct clause_t*      m_next;
	struct clause_t*      m_prev;
	size_t                m_var_count;
	const union packed_t* m_head;
	size_t                m_entry_point;
};

struct module_t;

struct predicate_t
{
	struct predicate_flags_t
	{
		unsigned dynamic : 1;
		unsigned multifile : 1;
		unsigned discontiguous : 1;
		unsigned public : 1;

	} m_flags;

	struct module_t*      m_module;
	const union packed_t* m_indicator;
	struct clause_t*      m_first_clause;
	struct clause_t*      m_last_clause;
	struct clause_t*      m_free_clause;
};

struct predicate_table_t
{
	// TODO; This can be a much faster data structure
	//struct stack_t*     m_stack;

	size_t              m_count;
	struct predicate_t* m_predicates[];
};

enum eOpSpec
{
	eFX,
	eFY,
	eXFX,
	eXFY,
	eYFX,
	eXF,
	eYF
};

struct operator_t
{
	struct operator_t* m_prev;
	enum eOpSpec       m_specifier;
	unsigned int       m_precedence;
};

struct module_t
{
	struct module_flags_t
	{
		unsigned char_conversion : 1;
		unsigned double_quotes : 2;
		unsigned back_quotes : 2;
		unsigned debug : 1;
		unsigned unknown : 2;
		unsigned colon_sets_calling_context : 1;
	} m_flags;

	struct operator_t*        m_operators;
	struct predicate_table_t* m_predicates;
};

struct context_t
{
	struct heap_t*        m_heap;
	union packed_t*       m_stack;

	struct module_t*      m_module;
};

union packed_t* push_string(union packed_t* stack, enum tag_type_t type, const unsigned char* str, size_t len);
union packed_t* push_compound(union packed_t* stack, uint64_t arity, const unsigned char* functor, size_t functor_len);

static inline union packed_t* push_integer(union packed_t* stack, int32_t v)
{
	(--stack)->m_u64val = PACK_TYPE(prolite_int32) | PACK_MANT_48(v);
	return stack;
}

static inline union packed_t* push_double(union packed_t* stack, double v)
{
	(--stack)->m_dval = v;
	return stack;
}

const union packed_t* get_first_arg(const union packed_t* compound, uint64_t* arity, struct debug_info_t* debug_info);
const union packed_t* get_next_arg(const union packed_t* p, struct debug_info_t* debug_info);
struct string_t get_compound(const union packed_t** b, uint64_t* arity, struct debug_info_t* debug_info);
struct string_t get_string(const union packed_t** b, struct debug_info_t* debug_info);

#endif /* TYPES_H_ */
