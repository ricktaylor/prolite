#ifndef COMPILE_H_
#define COMPILE_H_

#include "types.h"

#include <setjmp.h>

typedef enum optype
{
	OP_NOP = 0,
	OP_END,
	OP_SUCCEEDS,
	OP_DATA,
	OP_JMP,
	OP_GOSUB,
	OP_RET,
	OP_BUILTIN,
	OP_THROW,
	OP_SET_FLAGS,
	OP_CLEAR_FLAGS,
	OP_PUSH_CUT,
	OP_POP_CUT,
	OP_BRANCH,
	OP_PUSH_TERM,
	OP_UNIFY_VAR,
	OP_CLEAR_VAR
} optype_t;

typedef union opcode
{
	optype_t    m_opcode;
	double      m_dval;
	uint64_t    m_u64val;
	const void* m_pval;
} opcode_t;

typedef struct cfg_block
{
	size_t    m_count;  //< in sizeof(m_ops[0])
	opcode_t* m_ops;
} cfg_block_t;

typedef struct continuation
{
	const cfg_block_t* m_entry_point;
	cfg_block_t*       m_tail;
	uint8_t            m_always_flags;
	unsigned           m_subroutine : 1;
} continuation_t;

typedef struct substitutions
{
	size_t        m_count;  //< in sizeof(m_ops[0])
	const term_t* m_vals[];
} substitutions_t;

typedef struct compile_context
{
	heap_t*          m_heap;
	substitutions_t* m_substs;
	jmp_buf          m_jmp;
} compile_context_t;

typedef const char* (*builtin_fn_t)(void);

#define DECLARE_BUILTIN_FUNCTION(f,n) \
static inline const char* builtin_##f(void) { return #f; }

#define DECLARE_BUILTIN_HYBRID(f,n) \
static inline const char* builtin_##f(void) { return #f; } \
continuation_t* compile_##f(compile_context_t* context, continuation_t* cont, const term_t* goal);

#include "builtin_functions.h"

const term_t* deref_var(compile_context_t* context, const term_t* goal);
continuation_t* compile_builtin(compile_context_t* context, continuation_t* cont, builtin_fn_t fn, uint64_t arity, const term_t* g1);

static inline continuation_t* compile_true(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return cont;
}
continuation_t* compile_false(compile_context_t* context, continuation_t* cont, const term_t* goal);

#endif // COMPILE_H_
