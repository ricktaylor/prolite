#ifndef COMPILE_H_
#define COMPILE_H_

#include "context.h"
#include "predicates.h"

typedef enum prolite_type_flags
{
	type_flag_double = 0,
	type_flag_integer = 1,
	type_flag_var = (2 << prolite_var),
	type_flag_atom = (2 << prolite_atom),
	type_flag_compound = (2 << prolite_compound),
	type_flag_chars = (2 << prolite_chars),
	type_flag_charcodes = (2 << prolite_charcodes),
	//type_flag_userdata1 = (2 << prolite_userdata1),
	//type_flag_userdata2 = (2 << prolite_userdata2)

} prolite_type_flags_t;

typedef enum optype
{
	OP_NOP = 0,
	OP_CONTINUE,
	OP_JMP,
	OP_BRANCH,
	OP_BUILTIN,
	OP_EXTERN,
	OP_RET,
	OP_CUT,
	OP_PUSH_CUT,
	OP_POP_CUT,
	OP_PUSH_CONST,
	OP_PUSH_NULL,
	OP_PUSH_TERM_REF,
	OP_POP,

	// Expression ops...
	OP_ALLOC_REGS,
	OP_FREE_REGS,
	OP_PUSH_REG,
	OP_SET_REG,
	OP_LOAD_REG,
	OP_MOV_REG,
	OP_ADD_REG,
	OP_SUB_REG,

} optype_t;

size_t inc_ip(optype_t op);

typedef struct op_arg
{
	optype_t m_op : 8;
	uint64_t m_arg : 48;
} op_arg_t;

static const uint64_t c_op_arg_max = (UINT64_C(1) << 49) - 1;

static_assert(sizeof(op_arg_t) <= sizeof(term_t),"unpacked opcodes");

typedef union opcode
{
	op_arg_t m_opcode;
	term_t   m_term;
} opcode_t;

typedef struct cfg_block
{
	size_t    m_count;
	uint64_t  m_hash;
	opcode_t* m_ops;
} cfg_block_t;

typedef struct cfg
{
	cfg_block_t* m_entry_point;
	cfg_block_t* m_tail;
} cfg_t;

struct continuation;
struct compile_context;

typedef cfg_t* (*shim_fn_t)(struct compile_context* context, const struct continuation* goal);

typedef struct continuation
{
	const term_t*              m_term;
	shim_fn_t                  m_shim;
	const struct continuation* m_next;
} continuation_t;

typedef struct compile_clause
{
	struct compile_clause* m_next;
	const term_t*          m_head;
	const term_t*          m_body;
	size_t                 m_var_count;
	size_t                 m_param_count;

} compile_clause_t;

typedef struct compile_predicate
{
	predicate_base_t  m_base;
	_Bool             m_dynamic;
	compile_clause_t* m_clauses;

} compile_predicate_t;

static_assert(offsetof(compile_predicate_t,m_base) == 0,"structure members reorganised");

typedef struct compile_recursion
{
	struct compile_recursion*  m_prev;
	const compile_predicate_t* m_pred;
	const substitutions_t*     m_substs;
	cfg_t*                     m_cfg;
} compile_recursion_t;

struct compile_context;

typedef cfg_t* (*link_fn_t)(struct compile_context* context, void* param, const term_t* goal, const continuation_t* cont);

typedef struct compile_context
{
	prolite_allocator_t* m_allocator;
	substitutions_t*     m_substs;
	exec_flags_t         m_flags;
	jmp_buf              m_jmp;
	link_fn_t            m_link_fn;
	void*                m_link_param;
	compile_recursion_t* m_recursion;
} compile_context_t;

const term_t* compile_deref_var(compile_context_t* context, const term_t* goal);

cfg_t* new_cfg(compile_context_t* context);
opcode_t* append_opcodes(compile_context_t* context, cfg_block_t* blk, size_t count);
cfg_t* goto_next(compile_context_t* context, cfg_t* c, cfg_t* next);
cfg_t* add_branch(compile_context_t* context, cfg_t* c, exec_flags_t flags, cfg_t* next);
void append_ret(compile_context_t* context, cfg_block_t* c);

cfg_t* compile_unify_terms(compile_context_t* context, const term_t* t1, const term_t* t2, const continuation_t* next);
cfg_t* compile_builtin(compile_context_t* context, builtin_fn_t fn, const term_t* arg, const continuation_t* next);
cfg_t* compile_subgoal(compile_context_t* context, const continuation_t* goal);
cfg_t* compile_predicate_call(compile_context_t* context, const compile_predicate_t* pred, const term_t* goal, const continuation_t* next);

void compile_goal(context_t* context, link_fn_t link_fn, void* link_param, const term_t* goal, size_t var_count, const continuation_t* next);

#endif // COMPILE_H_
