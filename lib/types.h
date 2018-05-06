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

#if defined(_MSC_VER)
#define inline __inline
#endif

union box_t
{
	double   m_dval;
	uint64_t m_u64val;
};

struct line_info_t
{
	size_t         m_start_line;
	size_t         m_start_col;
	size_t         m_end_line;
	size_t         m_end_col;
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

struct substs_t
{
	size_t             m_count;
	const union box_t* m_values[];
};

struct var_info_t
{
	uint64_t    m_use_count;
	union box_t m_name;
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
	struct stack_t*        m_scratch_stack;
	struct stack_t*        m_exec_stack;
	struct substs_t*       m_substs;

	struct string_ptr_t*   m_strings;
	struct module_t*       m_module;
};

struct clause_t
{
	//struct term_t m_term;

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
	// TODO!
	int fast_hash_table;

	size_t m_procedure_count;
	struct procedure_t* m_procedures[];
};

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

const union box_t* first_arg(const union box_t* v);
const union box_t* next_arg(const union box_t* v);

enum eSolveResult throw_instantiation_error(struct context_t* context, const union box_t* culprit);
enum eSolveResult throw_type_error(struct context_t* context, uint64_t valid_type, const union box_t* culprit);
enum eSolveResult throw_representation_error(struct context_t* context, uint64_t flag, const union box_t* culprit);
enum eSolveResult throw_existence_error(struct context_t* context, uint64_t object_type, const union box_t* culprit);
enum eSolveResult throw_domain_error(struct context_t* context, uint64_t valid_domain, const union box_t* culprit);
enum eSolveResult throw_permission_error(struct context_t* context, uint64_t operation, uint64_t permission, const union box_t* culprit);
enum eSolveResult throw_evaluation_error(struct context_t* context, uint64_t error, const union box_t* culprit);

#endif /* TYPES_H_ */
