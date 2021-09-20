#ifndef COMPILE_H_
#define COMPILE_H_

#include "context.h"

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
	OP_END,
	OP_SUCCEEDS,
	OP_JMP,
	OP_GOSUB,
	OP_RET,
	OP_BUILTIN,
	OP_SET_FLAGS,
	OP_CLEAR_FLAGS,
	OP_PUSH_CUT,
	OP_POP_CUT,
	OP_BRANCH,
	OP_BRANCH_NOT,
	OP_PUSH_TERM_REF,
	OP_SET_VAR,
	OP_CLEAR_VAR,
	OP_TYPE_TEST
} optype_t;

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

typedef struct cfg_block
{
	size_t    m_count;
	opcode_t* m_ops;
} cfg_block_t;

typedef struct continuation
{
	cfg_block_t* m_entry_point;
	cfg_block_t* m_tail;
	uint8_t      m_always_flags;
	unsigned     m_subroutine : 1;
} continuation_t;

typedef struct compile_context
{
	heap_t*          m_heap;
	substitutions_t* m_substs;
	jmp_buf          m_jmp;
} compile_context_t;

const term_t* deref_var(compile_context_t* context, const term_t* goal);

static inline continuation_t* compile_true(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return cont;
}
continuation_t* compile_false(compile_context_t* context, continuation_t* cont, const term_t* goal);
continuation_t* compile_builtin(compile_context_t* context, continuation_t* cont, builtin_fn_t fn, size_t arity, const term_t* g1);
continuation_t* compile_type_test(compile_context_t* context, continuation_t* cont, prolite_type_flags_t types, int negate, const term_t* goal);

typedef struct cfg_block_info
{
	intptr_t           m_offset;
	const cfg_block_t* m_blk;
} cfg_block_info_t;

typedef struct cfg_vec
{
	size_t            m_count;
	size_t            m_total;
	cfg_block_info_t* m_blks;
} cfg_vec_t;

size_t inc_ip(optype_t op);

#if ENABLE_TESTS
void dumpCFG(const cfg_vec_t* blks, const char* filename);
void dumpTrace(const opcode_t* code, size_t count, const char* filename);
#endif

#endif // COMPILE_H_
