/*
 * types.h
 *
 *  Created on: 27 May 2017
 *      Author: rick
 */

#ifndef TYPES_H_
#define TYPES_H_

#include "stack.h"
#include "box_types.h"

union box_t
{
	double   m_dval;
	uint64_t m_u64val;
};

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

/* Macro magic to declare the builtin string constants */
#define DECLARE_BUILTIN_STRING(name) BUILTIN_ATOM_##name,
enum builtin_atoms_t
{
#include "builtin_strings.h"
};

struct substs_t
{
	size_t             m_count;
	const union box_t* m_values[];
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
	struct predicate_t*  m_pred;
	struct clause_t*     m_next;
	struct clause_t*     m_prev;
	struct stack_t*      m_stack;
	struct substs_t*     m_substs;
	const union box_t*   m_head;
	const union box_t*   m_body;
	size_t               m_entry_point;
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

	struct module_t*     m_module;
	struct stack_t*      m_stack;
	const union box_t*   m_indicator;
	struct string_ptr_t* m_strings;
	struct clause_t*     m_first_clause;
	struct clause_t*     m_last_clause;
	struct clause_t*     m_free_clause;
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

const union box_t* first_arg(const union box_t* v);
const union box_t* next_arg(const union box_t* v);
const union box_t* deref_term(struct substs_t* substs, const union box_t* v);
union box_t* copy_term(struct substs_t* substs, struct stack_t** stack, struct string_ptr_t** strings, const union box_t* v);
int copy_term_append(struct substs_t* substs, struct stack_t** stack, struct string_ptr_t** strings, union box_t const** v, union box_t** new_term, size_t* term_size);

enum eSolveResult
{
	SOLVE_TRUE = 1,
	SOLVE_FAIL = 0,
	SOLVE_CUT = -1,
	SOLVE_HALT = -2,
	SOLVE_THROW = -3,
	SOLVE_NOMEM = -4,
	SOLVE_UNWIND = -5
};

typedef enum eSolveResult (*solve_fn_t)(struct context_t*,size_t);

static inline enum eSolveResult solve(struct context_t* context, size_t frame)
{
	return (**(const solve_fn_t*)stack_at(context->m_instr_stack,frame))(context,frame+1);
}

static inline enum eSolveResult redo(struct context_t* context, int unwind)
{
	typedef enum eSolveResult (*redo_fn_t)(struct context_t*,int);
	return (*(redo_fn_t)stack_pop_ptr(&context->m_call_stack))(context,unwind);
}

enum eSolveResult unify(struct substs_t* substs, const union box_t* a, const union box_t* b);

enum eSolveResult redo_true(struct context_t* context, int unwind);

enum eSolveResult throw_instantiation_error(struct context_t* context, const union box_t* culprit);
enum eSolveResult throw_type_error(struct context_t* context, uint64_t valid_type, const union box_t* culprit);
enum eSolveResult throw_representation_error(struct context_t* context, uint64_t flag, const union box_t* culprit);
enum eSolveResult throw_existence_error(struct context_t* context, uint64_t object_type, const union box_t* culprit);
enum eSolveResult throw_domain_error(struct context_t* context, uint64_t valid_domain, const union box_t* culprit);
enum eSolveResult throw_permission_error(struct context_t* context, uint64_t operation, uint64_t permission, const union box_t* culprit);
enum eSolveResult throw_evaluation_error(struct context_t* context, uint64_t error, const union box_t* culprit);

enum eCompileResult
{
	COMPILE_OK = 0,
	COMPILE_NOT_CALLABLE,
	COMPILE_ALWAYS_TRUE,
	COMPILE_ALWAYS_FAILS,
	COMPILE_NOMEM,
};

struct compile_context_t
{
	struct stack_t*  m_emit_stack;
	struct module_t* m_module;
	struct substs_t* m_substs;
};

enum eCompileResult compile(struct compile_context_t* context, const union box_t* goal);
enum eCompileResult compile_call(struct compile_context_t* context, const union box_t* goal);

struct line_info_t
{
	size_t         m_start_line;
	size_t         m_start_col;
	size_t         m_end_line;
	size_t         m_end_col;
};

enum eSolveResult emit_error(struct context_t* context, struct line_info_t* info, uint64_t error_functor, unsigned int arity, ...);

#endif /* TYPES_H_ */
