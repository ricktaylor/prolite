#ifndef COMPILE_H_
#define COMPILE_H_

#include "context.h"
#include "predicates.h"

typedef enum prolite_type_flags
{
	type_flag_double = (1 << prolite_double),
	type_flag_var = (1 << prolite_var),
	type_flag_int32 = (1 << prolite_integer),
	type_flag_atom = (1 << prolite_atom),
	type_flag_compound = (1 << prolite_compound),
	type_flag_chars = (1 << prolite_chars),
	type_flag_charcodes = (1 << prolite_charcodes),
	type_flag_userdata = (1 << prolite_userdata)

} prolite_type_flags_t;

typedef enum optype
{
	OP_NOP = 0,
	OP_JMP,
	OP_GOSUB,
	OP_RET,
	OP_BUILTIN,
	OP_EXTERN,
	OP_SET_FLAGS,
	OP_CLEAR_FLAGS,
	OP_PUSH_CUT,
	OP_POP_CUT,
	OP_BRANCH,
	OP_BRANCH_NOT,
	OP_PUSH_CONST,
	OP_PUSH_NULL,
	OP_PUSH_TERM_REF
} optype_t;

size_t inc_ip(optype_t op);

struct op_arg
{
	optype_t m_op;
	uint32_t m_arg;
};

typedef union opcode
{
	struct op_arg m_opcode;
	term_t        m_term;
} opcode_t;

typedef struct compile_clause
{
	struct compile_clause* m_next;
	const term_t*          m_head;
	const term_t*          m_body;
	size_t                 m_var_count;

} compile_clause_t;

typedef struct compile_predicate
{
	predicate_base_t  m_base;
	_Bool             m_dynamic;
	compile_clause_t* m_clauses;
		
} compile_predicate_t;

static_assert(offsetof(compile_predicate_t,m_base) == 0,"structure members reorganised");

typedef void* (*link_fn_t)(void* context, void* param, const term_t* goal, const void* cont);

void compile_goal(context_t* context, link_fn_t link_fn, void* link_param, const term_t* goal, size_t var_count);

void* compile_predicate_call(void* context, const compile_predicate_t* pred, const term_t* goal, const void* cont);

#endif // COMPILE_H_
