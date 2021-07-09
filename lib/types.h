/*
 * types.h
 *
 *  Created on: 27 May 2017
 *      Author: rick
 */

#ifndef TYPES_H_
#define TYPES_H_

#include "stack.h"
#include "packed_types.h"

#include <assert.h>

/* Macro magic to declare the builtin string constants */
#define DECLARE_BUILTIN_STRING(name) BUILTIN_ATOM_##name,
enum builtin_atoms_t
{
#include "builtin_strings"
};

struct substs_t
{
	size_t                m_count;
	const union packed_t* m_values[];
};

struct string_ptr_t
{
	struct string_ptr_t* m_prev;
	size_t               m_len;
	unsigned char        m_str[];
};

struct predicate_t;

struct clause_t
{
	struct predicate_t*   m_pred;
	struct clause_t*      m_next;
	struct clause_t*      m_prev;
	struct stack_t*       m_stack;
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
	struct stack_t*       m_stack;
	const union packed_t* m_indicator;
	struct string_ptr_t*  m_strings;
	struct clause_t*      m_first_clause;
	struct clause_t*      m_last_clause;
	struct clause_t*      m_free_clause;
};

struct predicate_table_t
{
	// TODO; This can be a much faster data structure
	struct stack_t*     m_stack;

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

	struct stack_t*           m_stack;
	struct operator_t*        m_operators;
	struct predicate_table_t* m_predicates;
};

struct context_t
{
	struct stack_t*        m_scratch_stack;
	struct stack_t*        m_call_stack;
	const struct stack_t*  m_instr_stack;
	struct substs_t*       m_substs;
	struct string_ptr_t*   m_strings;
	struct module_t*       m_module;
};

struct line_info_t
{
	size_t         m_start_line;
	size_t         m_start_col;
	size_t         m_end_line;
	size_t         m_end_col;
};

static inline const union packed_t* first_arg(const union packed_t* v)
{
	assert(UNPACK_TYPE(v->m_u64val) == prolite_compound);

	// Skip functor atom
	if ((UNPACK_HI16(v->m_u64val) & 0xC000) == 0)
		++v;

	++v;

	if (UNPACK_TYPE(v->m_u64val) == PROLITE_DEBUG_INFO)
	{
		// TODO: Skip Debug info
		++v;
	}

	return v;
}

static inline const union packed_t* next_arg(const union packed_t* v)
{
	if (UNPACK_TYPE(v->m_u64val) == prolite_compound)
	{
		uint64_t arity = UNPACK_MANT_48(v->m_u64val);
		unsigned int hi16 = (arity >> 32);
		if (hi16 & 0x8000)
			arity = (hi16 & (MAX_ARITY_EMBED << 11)) >> 11;
		else if ((hi16 & 0xC000) == 0x4000)
			arity = (hi16 & MAX_ARITY_BUILTIN);

		v = first_arg(v);
		while (arity--)
			v = next_arg(v);
	}
	else
	{
		++v;

		if (UNPACK_TYPE(v->m_u64val) == PROLITE_DEBUG_INFO)
		{
			// TODO: Skip Debug info
			++v;
		}
	}

	return v;
}

static inline int64_t var_index(const union packed_t* v)
{
	// Sign extend
	struct pun { int64_t u48 : 48; } p;
	return (p.u48 = UNPACK_MANT_48(v->m_u64val));
}

static inline const union packed_t* deref_term(const struct substs_t* substs, const union packed_t* v)
{
	const union packed_t* r = v;
	do
	{
		const union packed_t* t = NULL;

		if (UNPACK_TYPE(r->m_u64val) == prolite_var)
		{
			int64_t var_idx = var_index(r);
			if (var_idx >= 0)
			{
				assert(substs && var_idx < substs->m_count);

				t = substs->m_values[var_idx];
			}
			else
			{
				assert(substs && -var_idx < substs->m_count);

				t = *(substs->m_values + substs->m_count + var_idx);
			}
		}

		if (!t)
			break;

		r = t;
	}
	while (r != v);

	return r;
}

static inline int append_packed_t(struct stack_t** stack, union packed_t const** v, union packed_t** term, size_t* term_size)
{
	*term = stack_realloc(stack,*term,*term_size * sizeof(union packed_t),((*term_size)+1) * sizeof(union packed_t));
	if (!*term)
		return -1;

	(*term)[(*term_size)++] = *((*v)++);
	return 0;
}

uint32_t is_builtin_string(const unsigned char* str, size_t len);
struct string_ptr_t* pack_stack_string(struct stack_t** stack, struct string_ptr_t** strings, const unsigned char* str, size_t len);
uint64_t pack_pointer(void* ptr);
void* unpack_pointer(uint64_t v);

#endif /* TYPES_H_ */
