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

union box_t* next_value(union box_t* v);

struct var_info_t
{
	union box_t* m_value;
	union box_t  m_name;
};

struct term_t
{
	struct var_info_t* m_vars;
	union box_t*       m_value;
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
	struct procedure_table_t* m_procedures;
};

struct string_ptr_t
{
	struct string_ptr_t* m_prev;
	size_t               m_len;
	unsigned char        m_str[];
};

struct context_t
{
	struct stack_t* m_scratch_stack;
	struct stack_t* m_exec_stack;

	struct string_ptr_t* m_strings;
	struct module_t*     m_module;
};

struct clause_t
{
	struct term_t m_term;

	uint64_t* m_opcodes;
};

struct procedure_t
{
	struct procedure_flags_t
	{
		unsigned dynamic : 1;
		unsigned multifile : 1;
		unsigned discontiguous : 1;
		unsigned public : 1;
	} m_flags;

	uint64_t* m_opcodes;

	size_t m_clause_count;
	struct clause_t m_clauses[];
};

struct procedure_table_t
{
	int fast_hash_table;

	size_t m_procedure_count;
	struct procedure_t* m_procedures[];
};

enum eSolveResult
{
	SOLVE_TRUE = 0,
	SOLVE_FAIL,
	SOLVE_CUT,
	SOLVE_THROW,
	SOLVE_NOMEM
};

typedef enum eSolveResult(*solve_fn_t)(struct context_t*);

#endif /* TYPES_H_ */
