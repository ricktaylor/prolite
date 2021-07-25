
#include "stream.h"

#include <string.h>
#include <stdarg.h>
#include <setjmp.h>

// TEMP
#include <stdio.h>


enum Flags
{
	FLAG_FAIL = 1,
	FLAG_CUT = 2,
	FLAG_THROW = 4,
	FLAG_HALT = 8
};

enum OpCodes
{
	OP_NOP = 0,
	OP_END,
	OP_TRUE,
	OP_JMP,
	OP_CALL,
	OP_RET,
	OP_BUILTIN,
	OP_THROW,
	OP_SET_FLAGS,
	OP_CLEAR_FLAGS,
	OP_PUSH_CUT,
	OP_POP_CUT,
	OP_BRANCH
};

typedef union opcode
{
	enum OpCodes m_opcode;
	double       m_dval;
	uint64_t     m_u64val;
	const void*  m_pval;
} opcode_t;

typedef struct cfg_block
{
	size_t    m_len;  //< in sizeof(m_ops[0])
	opcode_t* m_ops;
} cfg_block_t;

typedef struct continuation
{
	const cfg_block_t* m_entry_point;
	cfg_block_t*       m_tail;
	uint8_t                   m_always_flags;
	unsigned                  m_call_site : 1;
} continuation_t;

typedef struct compile_context
{
	heap_t* m_heap;
	jmp_buf        m_jmp;
} compile_context_t;

static cfg_block_t* new_cfg_block(compile_context_t* context)
{
	cfg_block_t* b = heap_malloc(&context->m_heap,sizeof(cfg_block_t));
	if (!b)
		longjmp(context->m_jmp,1);

	b->m_len = 0;
	b->m_ops = NULL;
	return b;
}

static opcode_t* append_opcodes(compile_context_t* context, cfg_block_t* blk, size_t count)
{
	opcode_t* ret = blk->m_ops;
	if (count)
	{
		blk->m_ops = heap_realloc(&context->m_heap,blk->m_ops,blk->m_len * sizeof(opcode_t),(blk->m_len + count) * sizeof(opcode_t));
		if (!blk->m_ops)
			longjmp(context->m_jmp,1);

		ret = blk->m_ops + blk->m_len;
		blk->m_len += count;
	}
	return ret;
}

static continuation_t* new_continuation(compile_context_t* context)
{
	continuation_t* c = heap_malloc(&context->m_heap,sizeof(continuation_t));
	if (!c)
		longjmp(context->m_jmp,1);

	c->m_call_site = 0;
	c->m_always_flags = 0;
	c->m_tail = new_cfg_block(context);
	c->m_entry_point = c->m_tail;

	return c;
}

typedef const char* builtin_fn_t;

#define DECLARE_BUILTIN_FUNCTION(f,n) \
builtin_fn_t builtin_##f = #f;

#define DECLARE_BUILTIN_HYBRID(f,n) \
builtin_fn_t builtin_##f = #f;

#include "builtin_functions.h"

static continuation_t* set_flags(compile_context_t* context, continuation_t* c, uint8_t flags)
{
	if (c->m_tail->m_len >= 2 &&
		c->m_tail->m_ops[c->m_tail->m_len-2].m_opcode == OP_CLEAR_FLAGS &&
		(c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val & flags))
	{
		c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val &= ~flags;
		if (c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val == 0)
		{
			c->m_tail->m_ops[c->m_tail->m_len-2].m_opcode = OP_NOP;
			c->m_tail->m_ops[c->m_tail->m_len-1].m_opcode = OP_NOP;
		}
	}
	else if (c->m_tail->m_len >= 2 &&
		c->m_tail->m_ops[c->m_tail->m_len-2].m_opcode == OP_SET_FLAGS)
	{
		c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val |= flags;
	}
	else
	{
		opcode_t* ops = append_opcodes(context,c->m_tail,2);
		(ops++)->m_opcode = OP_SET_FLAGS;
		ops->m_u64val = flags;
	}

	c->m_always_flags |= flags;
	
	return c;
}

static continuation_t* clear_flags(compile_context_t* context, continuation_t* c, uint8_t flags)
{
	if (c->m_tail->m_len >= 2 &&
		c->m_tail->m_ops[c->m_tail->m_len-2].m_opcode == OP_SET_FLAGS &&
		(c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val & flags))
	{
		c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val &= ~flags;
		if (c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val == 0)
		{
			c->m_tail->m_ops[c->m_tail->m_len-2].m_opcode = OP_NOP;
			c->m_tail->m_ops[c->m_tail->m_len-1].m_opcode = OP_NOP;
		}
	}
	else if (c->m_tail->m_len >= 2 &&
		c->m_tail->m_ops[c->m_tail->m_len-2].m_opcode == OP_CLEAR_FLAGS)
	{
		c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val |= flags;
	}
	else
	{
		opcode_t* ops = append_opcodes(context,c->m_tail,2);
		(ops++)->m_opcode = OP_CLEAR_FLAGS;
		ops->m_u64val = flags;
	}

	c->m_always_flags &= ~flags;

	return c;
}

static continuation_t* goto_next(compile_context_t* context, continuation_t* c, continuation_t* next)
{
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	if (next->m_call_site)
	{
		while (next->m_entry_point->m_len == 3 &&
			next->m_entry_point->m_ops[0].m_opcode == OP_CALL &&
			next->m_entry_point->m_ops[2].m_opcode == OP_RET)
		{
			next->m_entry_point = next->m_entry_point->m_ops[1].m_pval;
		}

		(ops++)->m_opcode = OP_CALL;
		ops->m_pval = next->m_entry_point;
	}
	else
	{
		while (next->m_entry_point->m_len == 2 &&
			next->m_entry_point->m_ops[0].m_opcode == OP_JMP)
		{
			next->m_entry_point = next->m_entry_point->m_ops[1].m_pval;
		}

		(ops++)->m_opcode = OP_JMP;
		ops->m_pval = next->m_entry_point;
		c->m_tail = next->m_tail;
	}
	return c;
}

static continuation_t* make_call_site(compile_context_t* context, continuation_t* c)
{
	if (!c->m_call_site)
	{
		opcode_t* ops = append_opcodes(context,c->m_tail,1);
		ops->m_opcode = OP_RET;
		c->m_call_site = 1;
	}
	return c;
}

static continuation_t* convert_to_call(compile_context_t* context, continuation_t* c)
{
	c = make_call_site(context,c);

	continuation_t* c1 = new_continuation(context);
	c1 = goto_next(context,c1,c);
	c1->m_always_flags = c->m_always_flags;
	return c1;
}

static continuation_t* wrap_cut(compile_context_t* context, continuation_t* cont)
{
	cont->m_always_flags &= ~FLAG_CUT;

	if (cont->m_always_flags & (FLAG_THROW | FLAG_HALT))
		return cont;

	// Short-circuit just a cut
	if (cont->m_entry_point == cont->m_tail &&
		cont->m_entry_point->m_len == 2 &&
		cont->m_entry_point->m_ops[0].m_opcode == OP_SET_FLAGS)
	{
		cont->m_entry_point->m_ops[1].m_u64val &= ~FLAG_CUT;
		if (cont->m_entry_point->m_ops[1].m_u64val == 0)
		{
			cont->m_entry_point->m_ops[0].m_opcode = OP_NOP;
			cont->m_entry_point->m_ops[1].m_opcode = OP_NOP;
		}		
		return cont;
	}

	continuation_t* c = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,1);
	ops->m_opcode = OP_PUSH_CUT;
	c = goto_next(context,c,cont);

	ops = append_opcodes(context,c->m_tail,1);
	ops->m_opcode = OP_POP_CUT;
	c->m_always_flags = cont->m_always_flags;

	return c;
}

static continuation_t* compile_goal(compile_context_t* context, continuation_t* cont, const term_t* goal);

static continuation_t* compile_true(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return cont;
}

static continuation_t* compile_false(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	if (cont->m_always_flags & FLAG_FAIL)
		return cont;

	return set_flags(context,new_continuation(context),FLAG_FAIL);
}

static continuation_t* compile_cut(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return set_flags(context,cont,FLAG_CUT);
}

static continuation_t* compile_and(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	const term_t* g2 = get_next_arg(g1,NULL);

	return compile_goal(context,compile_goal(context,cont,g2),g1);
}

static continuation_t* compile_if_then_else(compile_context_t* context, continuation_t* c_if, continuation_t* c_then, continuation_t* c_else)
{
	if (!(c_if->m_always_flags & (FLAG_THROW | FLAG_HALT)))
	{
		// Check the inner flags before the wrap
		int always_true = (c_if->m_always_flags & FLAG_CUT);
		c_if = wrap_cut(context,c_if);

		if (c_if->m_always_flags & FLAG_FAIL)
		{
			if (c_else)
			{
				c_if = clear_flags(context,c_if,FLAG_FAIL);
				if (c_if->m_entry_point->m_len == 2 &&
					c_if->m_entry_point->m_ops[0].m_opcode == OP_NOP &&
					c_if->m_entry_point->m_ops[1].m_opcode == OP_NOP)
				{
					c_if = c_else;
				}
				else
				{
					c_if = goto_next(context,c_if,c_else);
					c_if->m_always_flags = c_else->m_always_flags;
				}
			}
		}
		else
		{
			c_if->m_always_flags = c_then->m_always_flags;

			if (always_true)
			{
				if (c_if->m_entry_point->m_len == 2 &&
					c_if->m_entry_point->m_ops[0].m_opcode == OP_NOP &&
					c_if->m_entry_point->m_ops[1].m_opcode == OP_NOP)
				{
					c_if = c_then;
				}
				else
					c_if = goto_next(context,c_if,c_then);
			}
			else
			{
				continuation_t* c_end = new_continuation(context);

				if (c_if->m_entry_point->m_len != 2 ||
					c_if->m_entry_point->m_ops[0].m_opcode != OP_NOP ||
					c_if->m_entry_point->m_ops[1].m_opcode != OP_NOP)
				{					
					opcode_t* ops = append_opcodes(context,c_if->m_tail,3);
					(ops++)->m_opcode = OP_BRANCH;

					if (c_else)
						(ops++)->m_u64val = FLAG_THROW | FLAG_HALT;
					else
						(ops++)->m_u64val = FLAG_THROW | FLAG_HALT | FLAG_FAIL;
					ops->m_pval = c_end->m_entry_point;
				}

				if (c_else)
				{
					continuation_t* c1 = clear_flags(context,new_continuation(context),FLAG_FAIL);
					c1 = goto_next(context,c1,c_else);
					c1 = goto_next(context,c1,c_end);

					opcode_t* ops = append_opcodes(context,c_if->m_tail,3);
					(ops++)->m_opcode = OP_BRANCH;
					(ops++)->m_u64val = FLAG_FAIL;
					ops->m_pval = c1->m_entry_point;

					c_if = goto_next(context,c_if,c_then);
					c_if = goto_next(context,c_if,c_end);

					c_if->m_always_flags &= c_else->m_always_flags;
				}			
			}
		}		
	}

	return c_if;
}

static continuation_t* compile_or(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	const term_t* g2 = get_next_arg(g1,NULL);

	if (g1->m_u64val == PACK_COMPOUND_EMBED_2(2,'-','>'))
	{
		const term_t* g_if = get_first_arg(g1,NULL,NULL);
		
		continuation_t* c_if = set_flags(context,new_continuation(context),FLAG_CUT);
		c_if = compile_goal(context,c_if,g_if);

		continuation_t* c_then = NULL;
		continuation_t* c_else = NULL;
		if (c_if->m_always_flags & FLAG_FAIL)
			c_else = compile_goal(context,cont,g2);
		else if (c_if->m_always_flags == FLAG_CUT)
			c_then = compile_goal(context,cont,get_next_arg(g_if,NULL));
		else
		{
			c_then = compile_goal(context,cont,get_next_arg(g_if,NULL));
			c_else = compile_goal(context,cont,g2);
		}

		return compile_if_then_else(context,c_if,c_then,c_else);
	}		

	continuation_t* c = compile_goal(context,convert_to_call(context,cont),g1);
	if (c->m_always_flags & FLAG_FAIL)
		return compile_goal(context,cont,g2);

	if (!(c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
	{
		c = clear_flags(context,c,FLAG_FAIL);

		continuation_t* c2 = compile_goal(context,convert_to_call(context,cont),g2);
		continuation_t* c_end = new_continuation(context);	

		opcode_t* ops = append_opcodes(context,c->m_tail,3);
		(ops++)->m_opcode = OP_BRANCH;
		(ops++)->m_u64val = FLAG_CUT | FLAG_THROW | FLAG_HALT;
		ops->m_pval = c_end->m_entry_point;

		c2 = goto_next(context,c2,c_end);
		c = goto_next(context,c,c2);

		c->m_always_flags = c2->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT);
	}

	return c;
}

static continuation_t* compile_if_then(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g_if = get_first_arg(goal,NULL,NULL);
			
	continuation_t* c_if = set_flags(context,new_continuation(context),FLAG_CUT);
	c_if = compile_goal(context,c_if,g_if);

	continuation_t* c_then = compile_goal(context,cont,get_next_arg(g_if,NULL));
	
	return compile_if_then_else(context,c_if,c_then,NULL);
}

static continuation_t* compile_builtin(compile_context_t* context, continuation_t* cont, builtin_fn_t fn)
{
	// Convert cont to a call site
	cont = make_call_site(context,cont);

	while (cont->m_entry_point->m_len == 3 &&
		cont->m_entry_point->m_ops[0].m_opcode == OP_CALL &&
		cont->m_entry_point->m_ops[2].m_opcode == OP_RET)
	{
		cont->m_entry_point = cont->m_entry_point->m_ops[1].m_pval;
	}

	continuation_t* c = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode = OP_BUILTIN;
	(ops++)->m_pval = fn;
	ops->m_pval = cont->m_entry_point;

	c->m_always_flags = cont->m_always_flags;

	return c;
}

continuation_t* compile_user_defined(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return compile_builtin(context,cont,"user_defined");
}

static continuation_t* compile_throw_call(compile_context_t* context, builtin_fn_t builtin)
{
	continuation_t* c = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode = OP_THROW;
	ops->m_pval = builtin;
	c->m_always_flags = FLAG_THROW;
	return c;
}

static continuation_t* compile_throw(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return compile_throw_call(context,builtin_throw);
}

static continuation_t* compile_halt(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	continuation_t* c = compile_throw_call(context,builtin_halt);
	
	if (get_term_type(goal) == prolite_atom)
		c->m_always_flags = FLAG_HALT;

	return c;
}

static int compile_is_callable(const term_t* goal)
{
	switch (get_term_type(goal))
	{
	case prolite_var:
		return -1;

	case prolite_atom:
		return 1;

	case prolite_compound:
		{
			uint64_t arity;
			for (const term_t* p = get_first_arg(goal,&arity,NULL); arity--; p = get_next_arg(p,NULL))
			{
				int r = compile_is_callable(p);
				if (r != 1)
					return r;
			}
		}
		return 1;

	default:
		return 0;
	}
}

static continuation_t* compile_call_inner(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	switch (compile_is_callable(goal))
	{
	case 1:
		return compile_goal(context,cont,goal);

	case 0:
		return compile_throw_call(context,builtin_call);

	default:
		return compile_builtin(context,cont,builtin_call);
	}	
}

static continuation_t* compile_call(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	continuation_t* c = compile_call_inner(context,cont,g1);
	if (!(c->m_always_flags & (FLAG_THROW | FLAG_HALT)))
		c = wrap_cut(context,c);
	
	return c;
}

static continuation_t* compile_catch(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	const term_t* g2 = get_next_arg(g1,NULL);
	const term_t* g3 = get_next_arg(g2,NULL);

	continuation_t* c = compile_call_inner(context,convert_to_call(context,cont),g1);
	if (!(c->m_always_flags & FLAG_HALT))
	{
		continuation_t* c_end = new_continuation(context);
		
		continuation_t* c_resume;
		if (c->m_always_flags & FLAG_THROW)
			c_resume = compile_call_inner(context,cont,g3);
		else
		{
			c = wrap_cut(context,c);
			c_resume = compile_call_inner(context,convert_to_call(context,cont),g3);
		}

		if (!(c_resume->m_always_flags & (FLAG_THROW | FLAG_HALT)))
			c_resume = wrap_cut(context,c_resume);
		
		continuation_t* c_catch = compile_builtin(context,c_resume,builtin_catch);
		c_catch = goto_next(context,c_catch,c_end);

		if (c->m_always_flags & FLAG_THROW)
			c_end = c_catch;
		else
		{
			opcode_t* ops = append_opcodes(context,c->m_tail,3);
			(ops++)->m_opcode = OP_BRANCH;
			(ops++)->m_u64val = FLAG_THROW;
			ops->m_pval = c_catch->m_entry_point;
		}
		c = goto_next(context,c,c_end);
	}
	return c;
}

static continuation_t* compile_once(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);

	continuation_t* c = set_flags(context,new_continuation(context),FLAG_CUT);
	c = compile_call_inner(context,c,g1);

	return compile_if_then_else(context,c,cont,NULL);
}

static continuation_t* compile_repeat(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	if (cont->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT))
		return cont;

	continuation_t* c_end = new_continuation(context);

	if (cont->m_call_site)
		cont = goto_next(context,new_continuation(context),cont);

	opcode_t* ops = append_opcodes(context,cont->m_tail,7);
	(ops++)->m_opcode = OP_BRANCH;
	(ops++)->m_u64val = FLAG_CUT | FLAG_THROW | FLAG_HALT;
	(ops++)->m_pval = c_end->m_entry_point;
	(ops++)->m_opcode = OP_CLEAR_FLAGS;
	(ops++)->m_u64val = FLAG_FAIL;
	(ops++)->m_opcode = OP_JMP;
	ops->m_pval = cont->m_entry_point;

	cont->m_tail = c_end->m_tail;

	return cont;
}

static continuation_t* compile_not_proveable(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);

	continuation_t* c = set_flags(context,new_continuation(context),FLAG_CUT);
	c = compile_call_inner(context,c,g1);

	return compile_if_then_else(context,c,compile_false(context,cont,NULL),cont);
}

static continuation_t* compile_var(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	if (get_term_type(g1) == prolite_var)
		return compile_builtin(context,cont,builtin_var);

	return compile_false(context,cont,goal);
}

static continuation_t* compile_atom(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,builtin_atom);

	case prolite_atom:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

static continuation_t* compile_integer(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,builtin_integer);

	case prolite_int32:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

static continuation_t* compile_float(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,builtin_float);

	case prolite_double:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

static continuation_t* compile_atomic(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,builtin_atomic);

	case prolite_compound:
		return compile_false(context,cont,goal);

	default:
		return compile_true(context,cont,goal);
	}
}

static continuation_t* compile_compound(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,builtin_compound);

	case prolite_compound:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

static continuation_t* compile_nonvar(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	if (get_term_type(g1) == prolite_var)
		return compile_builtin(context,cont,builtin_nonvar);

	return compile_true(context,cont,goal);
}

static continuation_t* compile_number(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,builtin_number);

	case prolite_int32:
	case prolite_double:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

static continuation_t* compile_callable(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	switch (compile_is_callable(g1))
	{
	case 1:
		return compile_true(context,cont,g1);

	case 0:
		return compile_false(context,cont,g1);

	default:
		return compile_builtin(context,cont,builtin_callable);
	}
}

static int compile_is_ground(const term_t* goal)
{
	switch (get_term_type(goal))
	{
	case prolite_var:
		return -1;

	case prolite_atom:
		return 1;

	case prolite_compound:
		{
			uint64_t arity;
			for (const term_t* p = get_first_arg(goal,&arity,NULL); arity--; p = get_next_arg(p,NULL))
			{
				int r = compile_is_ground(p);
				if (r != 1)
					return r;
			}
		}
		return 1;

	default:
		return 0;
	}
}

static continuation_t* compile_ground(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	switch (compile_is_ground(g1))
	{
	case 1:
		return compile_true(context,cont,g1);

	case 0:
		return compile_false(context,cont,g1);

	default:
		return compile_builtin(context,cont,builtin_ground);
	}
}

static continuation_t* compile_goal(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	int debug = 0;

	continuation_t* c;
	switch (goal->m_u64val)
	{
#define DECLARE_BUILTIN_INTRINSIC(f,n) \
	case (n): c = compile_##f(context,cont,goal); break;

#undef DECLARE_BUILTIN_HYBRID
#define DECLARE_BUILTIN_HYBRID(f,n) \
	case (n): c = compile_##f(context,cont,goal); break;

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,n) \
	case (n): c = compile_builtin(context,cont,builtin_##f); break;

#include "builtin_functions.h"

	default:
		switch (get_term_type(goal))
		{
		case prolite_compound:
			if ((goal->m_u64val & PACK_COMPOUND_EMBED_4(0,'c','a','l','l')) == PACK_COMPOUND_EMBED_4(0,'c','a','l','l') ||
				(goal->m_u64val & PACK_COMPOUND_BUILTIN(call,0)) == PACK_COMPOUND_BUILTIN(call,0) ||
				goal[1].m_u64val == PACK_ATOM_EMBED_4('c','a','l','l'))
			{
				c = compile_builtin(context,cont,builtin_call);
			}
			else
				c = compile_user_defined(context,cont,goal);
			break;

		case prolite_atom:
			c = compile_user_defined(context,cont,goal);
			break;

		case prolite_var:
			c = compile_builtin(context,cont,builtin_call);
			break;

		default:
			// We know this throws...
			c = compile_throw_call(context,builtin_call);
			break;
		}
	}

	if (debug)
	{
		// TODO: Emit tracepoints
	}

	return c;
}

static void dumpCFG(const cfg_block_t* s, FILE* f);

static void compile_term(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	continuation_t* c = compile_goal(context,cont,goal);

	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode = OP_SET_FLAGS;
	(ops++)->m_u64val = c->m_always_flags;
	ops->m_opcode = OP_END;

	// TODO: Link!!

	dumpCFG(c->m_entry_point,stdout);
}

void compile(context_t* context, stream_t* s)
{
	// Read a term and prepare it for execution
	enum eParseStatus result = read_term(context,s);
	if (result != PARSE_OK)
		fprintf(stderr,"Parser failure\n");
	else
	{
		// Pop varinfo
		const term_t* sp = context->m_stack;

		size_t varcount = (sp++)->m_u64val;
		while (varcount--)
		{
			get_string(&sp,NULL);

			sp++;
		}

		size_t heap_start = heap_top(context->m_heap);
		compile_context_t cc = {0};
		cc.m_heap = context->m_heap;

		if (!setjmp(cc.m_jmp))
		{
			continuation_t* cont = new_continuation(&cc);
			opcode_t* ops = append_opcodes(&cc,cont->m_tail,1);
			ops->m_opcode = OP_TRUE;

			compile_term(&cc,cont,sp);
		}
		else
		{
			/* Out of memory! */
			heap_reset(&context->m_heap,heap_start);
		}

		/* Bulk free all CFG blocks */
		heap_reset(&context->m_heap,heap_start);
	}

	/* We have just made heavy use of the heap */
	heap_compact(context->m_heap);
}








static void fmtFlags(uint64_t v, char* buf)
{
	char* s = buf;

	if (v & FLAG_FAIL)
		*buf++ = 'F';

	if (v & FLAG_CUT)
		*buf++ = 'C';

	if (v & FLAG_THROW)
		*buf++ = 'T';

	if (v & FLAG_HALT)
		*buf++ = 'H';

	if (buf == s)
		*buf++ = '0';

	*buf = '\0';
}

static void dumpCFGBlock(const cfg_block_t* blk, FILE* f)
{
	char buf[5] = {0};

	fprintf(f,"\tnode [shape=record];\n");
	fprintf(f,"\tN%p [label=\"{",blk);

	if (!blk->m_len)
	{
		fprintf(f,"<f0> WTF?!?!");
	}

	for (size_t i=0;i < blk->m_len; ++i)
	{
		if (i)
			fprintf(f,"|");

		fprintf(f,"<f%zu> ",i);

		switch (blk->m_ops[i].m_opcode)
		{
		case OP_NOP:
			fprintf(f,"(NOP)");
			break;

		case OP_TRUE:
			fprintf(f,"Success!");
			break;

		case OP_END:
			fprintf(f,"End");
			break;

		case OP_JMP:
			fprintf(f,"Jmp");
			++i;
			break;

		case OP_CALL:
			fprintf(f,"Call");
			++i;
			break;

		case OP_RET:
			fprintf(f,"Ret");
			break;

		case OP_BUILTIN:
			fprintf(f,"Builtin\\ %s|<f%zu_t> ...Call",(const char*)blk->m_ops[i+1].m_pval,i);
			i+=2;
			break;

		case OP_THROW:
			fprintf(f,"Throwing\\ %s",(const char*)blk->m_ops[i+1].m_pval);
			++i;
			break;

		case OP_SET_FLAGS:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"Set\\ Flags\\ %s",buf);
			++i;
			break;

		case OP_CLEAR_FLAGS:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"Clear\\ Flags\\ %s",buf);
			++i;
			break;

		case OP_PUSH_CUT:
			fprintf(f,"Push\\ Cut");
			break;

		case OP_POP_CUT:
			fprintf(f,"Pop\\ Cut");
			break;

		case OP_BRANCH:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"Branch|<f%zu> ...%s",i+1,buf);
			i += 2;
			break;

		default:
			fprintf(f,"WTF? %zu",(size_t)blk->m_ops[i].m_opcode);
			break;
		}
	}

	fprintf(f,"}\"];\n");

	for (size_t i=0;i < blk->m_len; ++i)
	{
		switch (blk->m_ops[i].m_opcode)
		{
		case OP_BRANCH:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [label=\"%s\"];\n",blk,i+1,blk->m_ops[i+2].m_pval,buf);
			i += 2;
			break;

		case OP_CALL:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [dir=both];\n",blk,i,blk->m_ops[i+1].m_pval);
			++i;
			break;

		case OP_JMP:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0>;\n",blk,i,blk->m_ops[i+1].m_pval);
			++i;
			break;

		case OP_BUILTIN:
			fprintf(f,"\tN%p:<f%zu_t> -> N%p:<f0> [dir=both];\n",blk,i,blk->m_ops[i+2].m_pval);
			i+=2;
			break;

		case OP_THROW:
		case OP_SET_FLAGS:
		case OP_CLEAR_FLAGS:
			++i;
			break;

		default:
			break;
		}
	}
}

typedef struct cfg_vec
{
	size_t len;
	const cfg_block_t** blks;
} cfg_vec_t;

static int addCFG(cfg_vec_t* blks, const cfg_block_t* blk)
{
	for (size_t i=0; i < blks->len; ++i)
	{
		if (blk == blks->blks[i])
			return 0;
	}

	blks->blks = realloc(blks->blks,(blks->len + 1) * sizeof(void*));
	blks->blks[blks->len++] = blk;

	return 1;
}

static void walkCFG(cfg_vec_t* blks, const cfg_block_t* blk)
{
	if (addCFG(blks,blk))
	{
		for (size_t i=0;i < blk->m_len; ++i)
		{
			switch (blk->m_ops[i].m_opcode)
			{
			case OP_JMP:
			case OP_CALL:
				walkCFG(blks,blk->m_ops[i+1].m_pval);
				++i;
				break;

			case OP_BRANCH:
			case OP_BUILTIN:
				walkCFG(blks,blk->m_ops[i+2].m_pval);
				i+=2;
				break;

			case OP_THROW:
			case OP_SET_FLAGS:
			case OP_CLEAR_FLAGS:
				++i;
				break;

			default:
				break;
			}
		}
	}
}

static void dumpCFG(const cfg_block_t* b, FILE* f)
{
	if (f)
	{
		fprintf(f,"digraph cfg {\n");

		if (b)
		{
			fprintf(f,"\tstart [shape=point];\n");

			cfg_vec_t blks = {0};
			walkCFG(&blks,b);

			for (size_t i=0; i < blks.len; ++i)
				dumpCFGBlock(blks.blks[i],f);

			fprintf(f,"\tstart -> N%p:<f0>;\n",b);
		}

		fprintf(f,"}");
	}
}
