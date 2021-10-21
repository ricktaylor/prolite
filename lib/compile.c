#include "compile.h"
#include "builtins.h"

#include <string.h>
#include <stdarg.h>

typedef struct cfg_block
{
	size_t    m_count;
	opcode_t* m_ops;
} cfg_block_t;

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

typedef struct cfg
{
	cfg_block_t* m_entry_point;
	cfg_block_t* m_tail;
	uint8_t      m_always_flags;
} cfg_t;

typedef struct compile_context
{
	term_t*          m_stack;
	heap_t*          m_heap;
	substitutions_t* m_substs;
	jmp_buf          m_jmp;
	link_fn_t        m_link_fn;
	void*            m_link_param;
} compile_context_t;

struct continuation;

typedef cfg_t* (*shim_fn_t)(compile_context_t* context, void* param, const struct continuation* next);

typedef struct continuation
{
	const term_t*              m_term;
	shim_fn_t                  m_shim;
	const struct continuation* m_next;
} continuation_t;

typedef struct cse_cfg
{
	size_t m_refcount;
	cfg_t* m_cfg;
} cse_cfg_t;

static cfg_block_t* new_cfg_block(compile_context_t* context)
{
	cfg_block_t* b = heap_malloc(context->m_heap,sizeof(cfg_block_t));
	if (!b)
		longjmp(context->m_jmp,1);

	b->m_count = 0;
	b->m_ops = NULL;
	return b;
}

static opcode_t* append_opcodes(compile_context_t* context, cfg_block_t* blk, size_t count)
{
	opcode_t* ret = blk->m_ops;
	if (count)
	{
		blk->m_ops = heap_realloc(context->m_heap,blk->m_ops,blk->m_count * sizeof(opcode_t),(blk->m_count + count) * sizeof(opcode_t));
		if (!blk->m_ops)
			longjmp(context->m_jmp,1);

		ret = blk->m_ops + blk->m_count;
		blk->m_count += count;
	}
	return ret;
}

static cfg_t* new_cfg(compile_context_t* context)
{
	cfg_t* c = heap_malloc(context->m_heap,sizeof(cfg_t));
	if (!c)
		longjmp(context->m_jmp,1);

	c->m_always_flags = 0;
	c->m_tail = new_cfg_block(context);
	c->m_entry_point = c->m_tail;

	return c;
}

static cfg_t* set_flags(compile_context_t* context, cfg_t* c, exec_flags_t flags)
{
	if (c->m_tail->m_count &&
		c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_op == OP_CLEAR_FLAGS &&
		(c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_arg & flags))
	{
		c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_arg &= ~flags;
		if (c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_arg == 0)
		{
			c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_op = OP_NOP;
		}
	}
	else if (c->m_tail->m_count &&
		c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_op == OP_SET_FLAGS)
	{
		c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_arg |= flags;
	}
	else
	{
		opcode_t* ops = append_opcodes(context,c->m_tail,1);
		ops->m_opcode = (struct op_arg){ .m_op = OP_SET_FLAGS, .m_arg = flags};
	}

	c->m_always_flags |= flags;

	return c;
}

static cfg_t* clear_flags(compile_context_t* context, cfg_t* c, exec_flags_t flags)
{
	if (c->m_tail->m_count &&
		c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_op == OP_SET_FLAGS &&
		(c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_arg & flags))
	{
		c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_arg &= ~flags;
		if (c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_arg == 0)
		{
			c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_op = OP_NOP;
		}
	}
	else if (c->m_tail->m_count &&
		c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_op == OP_CLEAR_FLAGS)
	{
		c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode.m_arg |= flags;
	}
	else
	{
		opcode_t* ops = append_opcodes(context,c->m_tail,1);
		ops->m_opcode = (struct op_arg){ .m_op = OP_CLEAR_FLAGS, .m_arg = flags};
	}

	c->m_always_flags &= ~flags;

	return c;
}

static cfg_t* goto_next(compile_context_t* context, cfg_t* c, cfg_t* next)
{
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode.m_op = OP_JMP;
	ops->m_term.m_pval = next->m_entry_point;
	c->m_tail = next->m_tail;
	
	return c;
}

static cfg_t* add_branch(compile_context_t* context, cfg_t* c, optype_t branch, exec_flags_t flags, cfg_t* next)
{
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	ops->m_opcode.m_op = branch;
	(ops++)->m_opcode.m_arg = flags;
	(ops++)->m_term.m_pval = next->m_entry_point;

	return c;
}

static const term_t* deref_var(compile_context_t* context, const term_t* goal)
{
	if (get_term_type(goal) == prolite_var)
	{
		size_t idx = get_var_index(goal);
		assert(context->m_substs && idx < context->m_substs->m_count);
		const term_t* g = context->m_substs->m_vals[idx];
		if (g)
			goal = deref_var(context,g);
	}
	return goal;
}

static cfg_t* compile_subgoal(compile_context_t* context, const continuation_t* goal);

static cfg_t* compile_true(compile_context_t* context, const continuation_t* goal)
{
	return compile_subgoal(context,goal->m_next);
}

static cfg_t* compile_false(compile_context_t* context, const continuation_t* goal)
{
	return NULL;
}

static cfg_t* compile_cut(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = compile_subgoal(context,goal->m_next);
	if (c)
	{
		if (c->m_always_flags & (FLAG_HALT | FLAG_THROW))
			return c;
	}
	else
		c = new_cfg(context);

	return set_flags(context,c,FLAG_CUT);
}

static cfg_t* compile_and(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);

	return compile_subgoal(context,&(continuation_t){
		.m_term = g1,
		.m_next = &(continuation_t){
			.m_term = g2,
			.m_next = goal->m_next
		}
	});
}

static cfg_t* compile_once_shim(compile_context_t* context, void* param, const continuation_t* next)
{
	cfg_t* c = compile_subgoal(context,next);
	if (c)
	{
		*(uint8_t*)param = c->m_always_flags;

		if (!(c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
		{
			opcode_t* ops = append_opcodes(context,c->m_tail,2);
			(ops++)->m_opcode.m_op = OP_PUSH_CUT;
			ops->m_opcode = (struct op_arg){ .m_op = OP_SET_FLAGS, .m_arg = FLAG_CUT};

			c->m_always_flags |= FLAG_CUT;
		}
	}

	return c;
}

static cfg_t* compile_once_inner(compile_context_t* context, const continuation_t* goal)
{
	uint8_t inner_flags = 0;
	cfg_t* c = compile_subgoal(context,&(continuation_t){
		.m_term = goal->m_term,
		.m_next = &(continuation_t){
			.m_shim = &compile_once_shim,
			.m_term = (const term_t*)&inner_flags,
			.m_next = goal->m_next
		}
	});

	if (c)
	{
		c->m_always_flags = (c->m_always_flags & ~FLAG_CUT) | inner_flags;

		if (!(c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
		{
			opcode_t* ops = append_opcodes(context,c->m_tail,1);
			(ops++)->m_opcode.m_op = OP_POP_CUT;
		}
	}
	return c;
}

static cfg_t* compile_once(compile_context_t* context, const continuation_t* goal)
{
	return compile_once_inner(context,&(continuation_t){
		.m_term = get_first_arg(goal->m_term,NULL),
		.m_next = goal->m_next
	});
}

static cfg_t* compile_if_then(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g_if = get_first_arg(goal->m_term,NULL);
	const term_t* g_then = get_next_arg(g_if);

	return compile_once_inner(context,&(continuation_t){
		.m_term = g_if,
		.m_next = &(continuation_t){
			.m_term = g_then,
			.m_next = goal->m_next
		}
	});
}

static int cfg_compare(const cfg_t* c1, const cfg_t* c2)
{
	// TODO!!

	return 0;
}

typedef struct cse_info
{
	size_t     m_count;
	cse_cfg_t* m_cfgs;
} cse_info_t;

static cfg_t* compile_cse(compile_context_t* context, void* param, const continuation_t* goal)
{
	cse_info_t* cse = param;
	cfg_t* c = compile_subgoal(context,goal);
	if (c && c->m_entry_point->m_count >= 2)
	{
		for (size_t i = 0; i < cse->m_count; ++i)
		{
			if (cfg_compare(cse->m_cfgs[i].m_cfg,c))
			{
				++cse->m_cfgs[i].m_refcount;
				return cse->m_cfgs[i].m_cfg;
			}
		}

		cse_cfg_t* new_cfgs = heap_realloc(context->m_heap,cse->m_cfgs,cse->m_count * sizeof(cse_cfg_t),(cse->m_count + 1) * sizeof(cse_cfg_t));
		if (!new_cfgs)
			longjmp(context->m_jmp,1);

		cse->m_cfgs = new_cfgs;
		cse->m_cfgs[cse->m_count++] = (cse_cfg_t){ .m_cfg = c, .m_refcount = 1 };
	}

	return c;
}

static void complete_cse(compile_context_t* context, cse_info_t* cse)
{
	for (size_t i = 0; i < cse->m_count; ++i)
	{
		if (cse->m_cfgs[i].m_refcount > 1)
		{
			// TODO - replace with gosubs!
		}
	}

	heap_free(context->m_heap,cse->m_cfgs,cse->m_count * sizeof(cse_cfg_t));
}

static cfg_t* compile_or(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);

	cse_info_t cse = {0};
	const continuation_t* next = &(continuation_t){
		.m_shim = &compile_cse,
		.m_term = (const term_t*)&cse,
		.m_next = goal->m_next
	};

	cfg_t* c;
	if (MASK_DEBUG_INFO(g1->m_u64val) == PACK_COMPOUND_EMBED_2(2,'-','>'))
	{
		const term_t* g_if = get_first_arg(g1,NULL);
		const term_t* g_then = get_next_arg(g_if);

		c = compile_once_inner(context,&(continuation_t){
			.m_term = g_if,
			.m_next = &(continuation_t){
				.m_term = g_then,
				.m_next = next
			}
		});
	}
	else
	{
		c = compile_subgoal(context,&(continuation_t){
			.m_term = g1,
			.m_next = next
		});
	}

	if (!c)
		c = compile_subgoal(context,&(continuation_t){ .m_term = g2, .m_next = goal->m_next });
	else if (!(c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
	{
		cfg_t* c2 = compile_subgoal(context,&(continuation_t){
			.m_term = g2,
			.m_next = next
		});

		if (c2)
		{
			cfg_t* c_end = new_cfg(context);
			goto_next(context,c2,c_end);
			
			add_branch(context,c,OP_BRANCH,FLAG_CUT | FLAG_THROW | FLAG_HALT,c_end);
			goto_next(context,c,c2);

			c->m_always_flags = c2->m_always_flags;

			complete_cse(context,&cse);
		}
	}

	return c;
}

static cfg_t* compile_builtin(compile_context_t* context, builtin_fn_t fn, size_t arity, const term_t* arg, const continuation_t* next)
{
	cfg_t* c = new_cfg(context);

	if (arity)
	{
		opcode_t* ops = append_opcodes(context,c->m_tail,2 * arity);
		for (size_t i = 1; i <= arity; ++i)
		{
			ops[(arity - i)*2].m_opcode.m_op = OP_PUSH_TERM_REF;
			ops[(arity - i)*2 + 1].m_term.m_pval = deref_var(context,arg);
			arg = get_next_arg(arg);
		}
	}

	cfg_t* cont = compile_subgoal(context,next);
	if (!cont)
		cont = new_cfg(context);

	opcode_t* ops = append_opcodes(context,cont->m_tail,1);
	ops->m_opcode.m_op = OP_RET;

	ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode.m_op = OP_BUILTIN;
	(ops++)->m_term.m_pval = fn;
	ops->m_term.m_pval = cont->m_entry_point;

	c->m_always_flags = cont->m_always_flags;

	return c;
}

static cfg_t* compile_throw(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = compile_builtin(context,&prolite_builtin_throw,1,get_first_arg(goal->m_term,NULL),NULL);
	return set_flags(context,c,FLAG_THROW);
}

static cfg_t* compile_halt(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c;
	if (get_term_type(goal->m_term) == prolite_atom)
		c = new_cfg(context);
	else
		c = compile_builtin(context,&prolite_builtin_halt,1,get_first_arg(goal->m_term,NULL),NULL);

	return set_flags(context,c,FLAG_HALT);
}

static int compile_is_callable(compile_context_t* context, const term_t* goal)
{
	switch (get_term_type(goal))
	{
	case prolite_atom:
		return 1;

	case prolite_compound:
		switch (MASK_DEBUG_INFO(goal->m_u64val))
		{
		case PACK_COMPOUND_EMBED_1(2,','):
		case PACK_COMPOUND_EMBED_1(2,';'):
		case PACK_COMPOUND_EMBED_2(2,'-','>'):
			{
				size_t arity;
				for (const term_t* p = get_first_arg(goal,&arity); arity--; p = get_next_arg(p))
				{
					if (!compile_is_callable(context,deref_var(context,p)))
						return 0;
				}
			}
			break;

		default:
			break;
		}
		return 1;

	default:
		return 0;
	}
}

static cfg_t* compile_call_shim(compile_context_t* context, void* param, const continuation_t* next)
{
	cfg_t* c = compile_subgoal(context,next);
	if (c)
	{
		*(uint8_t*)param = c->m_always_flags;

		if (!(c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
		{
			opcode_t* ops = append_opcodes(context,c->m_tail,1);
			ops->m_opcode.m_op = OP_PUSH_CUT;
		}
	}

	return c;
}

static cfg_t* compile_call_inner(compile_context_t* context, const term_t* goal, const continuation_t* next)
{
	uint8_t inner_flags = 0;
	continuation_t cont = {
		.m_term = deref_var(context,goal),
		.m_next = &(continuation_t){
			.m_shim = &compile_call_shim,
			.m_term = (const term_t*)&inner_flags,
			.m_next = next
		}
	};

	cfg_t* c;
	if (compile_is_callable(context,cont.m_term))
		c = compile_subgoal(context,&cont);
	else
		c = compile_builtin(context,&prolite_builtin_call,1,cont.m_term,cont.m_next);

	if (c)
	{
		c->m_always_flags = (c->m_always_flags & ~FLAG_CUT) | inner_flags;

		if (!(c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
		{
			opcode_t* ops = append_opcodes(context,c->m_tail,1);
			ops->m_opcode.m_op = OP_POP_CUT;
		}
	}

	return c;
}

static cfg_t* compile_call(compile_context_t* context, const continuation_t* goal)
{
	return compile_call_inner(context,get_first_arg(goal->m_term,NULL),goal->m_next);
}

static cfg_t* compile_callN(compile_context_t* context, const continuation_t* goal)
{
	uint8_t inner_flags = 0;
	cfg_t* c = compile_builtin(context,&prolite_builtin_callN,1,goal->m_term,&(continuation_t){
		.m_shim = &compile_call_shim,
		.m_term = (const term_t*)&inner_flags,
		.m_next = goal->m_next
	});

	if (c)
	{
		c->m_always_flags = (c->m_always_flags & ~FLAG_CUT) | inner_flags;

		if (!(c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
		{
			opcode_t* ops = append_opcodes(context,c->m_tail,1);
			ops->m_opcode.m_op = OP_POP_CUT;
		}
	}

	return c;
}

static cfg_t* compile_resume(compile_context_t* context, void* param, const continuation_t* goal)
{
	cfg_t* c = new_cfg(context);
	clear_flags(context,c,FLAG_THROW);

	cfg_t* cont = compile_call_inner(context,param,goal->m_next);
	if (cont)
	{
		goto_next(context,c,cont);

		c->m_always_flags = cont->m_always_flags;
	}

	return c;
}

static cfg_t* compile_catch(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);
	const term_t* g3 = get_next_arg(g2);

	cfg_t* c = compile_call_inner(context,g1,goal->m_next);
	if (c && !(c->m_always_flags & FLAG_HALT))
	{
		cfg_t* c_catch = compile_builtin(context,&prolite_builtin_catch,1,g2,&(continuation_t){ .m_term = g3, .m_shim = &compile_resume, .m_next = goal->m_next });

		if (c->m_always_flags & FLAG_THROW)
			goto_next(context,c,c_catch);
		else
		{
			cfg_t* c_end = new_cfg(context);
			goto_next(context,c_catch,c_end);

			add_branch(context,c,OP_BRANCH,FLAG_THROW,c_catch);
			goto_next(context,c,c_end);
		}

		c->m_always_flags = c_catch->m_always_flags;
	}
	return c;
}

static cfg_t* compile_repeat(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = compile_subgoal(context,goal->m_next);
	if (!c)
	{
		// TODO: Warning - endless loop!
		c = new_cfg(context);
	}

	if (!(c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
		add_branch(context,c,OP_BRANCH_NOT,FLAG_CUT | FLAG_THROW | FLAG_HALT,c);

	return c;
}

static int compile_occurs_check(compile_context_t* context, const term_t* t1, const term_t* t2)
{
	switch (get_term_type(t2))
	{
	case prolite_var:
		if (t1->m_u64val == t2->m_u64val)
			return 1;
		break;
			
	case prolite_charcodes:
	case prolite_chars:
		assert(0);
		break;

	case prolite_compound:
		{
			size_t arity;
			t2 = get_first_arg(t2,&arity);

			for (size_t i = 0; i < arity; ++i)
			{
				if (compile_occurs_check(context,t1,deref_var(context,t2)))
					return 1;
				
				t2 = get_next_arg(t2);
			}
			
		}

	default:
		break;
	}

	return 0;
}

static cfg_t* compile_unify_shim(compile_context_t* context, void* param, const continuation_t* next)
{
	const term_t** goals = param;
	
	size_t idx = get_var_index(goals[0]);
	assert(context->m_substs && idx < context->m_substs->m_count);

	cfg_t* c = compile_subgoal(context,next);
	if (c)
	{
		cfg_t* c1 = new_cfg(context);
		opcode_t* ops = append_opcodes(context,c1->m_tail,4);
		(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
		(ops++)->m_term.m_pval = goals[0];
		(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
		ops->m_term.m_pval = goals[1];

		c1->m_always_flags = c->m_always_flags;
		
		c = goto_next(context,c1,c);
	}

	return c;
}

typedef struct unify_info
{
	size_t m_var_count;
	int    m_with_occurs_check;
} unify_info_t;

static const continuation_t* compile_unify_var(compile_context_t* context, const term_t* t1, const term_t* t2, unify_info_t* ui, const continuation_t* next)
{
	size_t idx = get_var_index(t1);
	assert(context->m_substs && idx < context->m_substs->m_count && context->m_substs->m_vals[idx] == NULL);

	context->m_substs->m_vals[idx] = t2;

	++ui->m_var_count;

	context->m_stack -= bytes_to_cells(sizeof(continuation_t),sizeof(term_t));
	continuation_t* c = (continuation_t*)context->m_stack;

	(--context->m_stack)->m_pval = t2;
	(--context->m_stack)->m_pval = t1;

	c->m_next = next;
	c->m_shim = &compile_unify_shim;
	c->m_term = context->m_stack;

	return c;
}

static const continuation_t* compile_unify_term(compile_context_t* context, const term_t* t1, const term_t* t2, unify_info_t* ui, const continuation_t* next)
{
	prolite_type_t type1 = get_term_type(t1);
	if (type1 == prolite_var)
	{
		if (t1->m_u64val == t2->m_u64val)
			return next;

		return compile_unify_var(context,t1,t2,ui,next);
	}

	prolite_type_t type2 = get_term_type(t2);
	switch (type2)
	{
	case prolite_var:
		return compile_unify_term(context,t2,t1,ui,next);

	case prolite_charcodes:
	case prolite_chars:
		assert(0);
		break;

	case prolite_compound:
		if (predicate_compare(t1,t2))
		{
			size_t arity;
			t1 = get_first_arg(t1,&arity);
			t2 = get_first_arg(t2,NULL);

			for (size_t i = 0; i < arity; ++i)
			{
				next = compile_unify_term(context,deref_var(context,t1),deref_var(context,t2),ui,next);
				if (!next)
					break;

				t1 = get_next_arg(t1);
				t2 = get_next_arg(t2);
			}
			return next;
		}
		break;

	default:
		if (term_compare(t1,t2))
			return next;
		break;
	}

	return NULL;
}

static cfg_t* compile_unify_end_shim(compile_context_t* context, void* param, const continuation_t* next)
{
	unify_info_t* ui = param;
	
	cfg_t* cont = compile_subgoal(context,next);
	if (!cont || ui->m_var_count == 0)
		return cont;
	
	opcode_t* ops = append_opcodes(context,cont->m_tail,1);
	ops->m_opcode.m_op = OP_RET;

	cfg_t* c = new_cfg(context);
	ops = append_opcodes(context,c->m_tail,7);
	(ops++)->m_opcode.m_op = OP_PUSH_CONST;
	(ops++)->m_term.m_u64val = ui->m_var_count;
	(ops++)->m_opcode.m_op = OP_PUSH_CONST;
	(ops++)->m_term.m_u64val = ui->m_with_occurs_check;
	(ops++)->m_opcode.m_op = OP_BUILTIN;
	(ops++)->m_term.m_pval = &prolite_builtin_unify;
	ops->m_term.m_pval = cont->m_entry_point;

	c->m_always_flags = cont->m_always_flags;

	return c;
}

static cfg_t* compile_unify_inner(compile_context_t* context, const term_t* t1, const term_t* t2, cfg_t* (*fn)(compile_context_t*,const continuation_t*), const continuation_t* next, int with_occurs_check)
{
	t1 = deref_var(context,t1);
	t2 = deref_var(context,t2);

	if (compile_occurs_check(context,t1,t2))
		return NULL;

	term_t* sp = context->m_stack;
	
	substitutions_t* prev_substs = context->m_substs;
	if (context->m_substs)
	{
		context->m_stack -= bytes_to_cells(sizeof(substitutions_t) + (sizeof(term_t) * context->m_substs->m_count),sizeof(term_t));
		substitutions_t* substs = (substitutions_t*)context->m_stack;
		
		substs->m_count = context->m_substs->m_count;
		memcpy(substs->m_vals,context->m_substs->m_vals,context->m_substs->m_count * sizeof(term_t*));

		context->m_substs = substs;
	}

	context->m_stack -= bytes_to_cells(sizeof(continuation_t),sizeof(term_t));
	continuation_t* cont = (continuation_t*)context->m_stack;
	
	unify_info_t ui = { .m_with_occurs_check = with_occurs_check };
	*cont = (continuation_t){
		.m_shim = &compile_unify_end_shim,
		.m_term = (const term_t*)&ui,
		.m_next = next
	};
	
	cfg_t* c = NULL;
	const continuation_t* cont2 = compile_unify_term(context,deref_var(context,t1),deref_var(context,t2),&ui,cont);
	if (cont2)
	{
		if (cont2 == cont)
			c = (*fn)(context,next);
		else
			c = (*fn)(context,cont2);
	}
		
	context->m_substs = prev_substs;

	context->m_stack = sp;

	return c;
}

static cfg_t* compile_unify(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);

	return compile_unify_inner(context,g1,g2,&compile_subgoal,goal->m_next,0);
}

static cfg_t* compile_unify_with_occurs_check(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);

	return compile_unify_inner(context,g1,g2,&compile_subgoal,goal->m_next,1);
}

static cfg_t* compile_not_unifiable(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = compile_subgoal(context,goal->m_next);

	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);

	cfg_t* c2 = compile_unify_inner(context,g1,g2,&compile_once_inner,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l') } },0);
	if (!c)
		c = c2;
	else if (c2)
	{
		c2->m_always_flags = c->m_always_flags;

		c = goto_next(context,c2,c);
	}
	
	return c;
}

static const continuation_t* compile_unify_head_term(compile_context_t* context, const term_t* t1, const term_t* t2, substitutions_t* next_substs, unify_info_t* ui, const continuation_t* next)
{
	prolite_type_t type2 = get_term_type(t2);
	if (type2 == prolite_var)
	{
		// Equivalent of defer_var for next_substs;
		size_t idx = get_var_index(t2);
		assert(next_substs && idx < next_substs->m_count);

		if (next_substs->m_vals[idx] != NULL)
			return compile_unify_term(context,t1,deref_var(context,next_substs->m_vals[idx]),ui,next);

		next_substs->m_vals[idx] = t1;
		return next;
	}

	prolite_type_t type1 = get_term_type(t1);
	if (type1 == prolite_var)
		return compile_unify_var(context,t1,t2,ui,next);
	
	switch (type2)
	{
	case prolite_charcodes:
	case prolite_chars:
		assert(0);
		break;

	case prolite_compound:
		if (predicate_compare(t1,t2))
		{
			size_t arity;
			t1 = get_first_arg(t1,&arity);
			t2 = get_first_arg(t2,NULL);

			for (size_t i = 0; i < arity; ++i)
			{
				next = compile_unify_head_term(context,deref_var(context,t1),t2,next_substs,ui,next);
				if (!next)
					break;

				t1 = get_next_arg(t1);
				t2 = get_next_arg(t2);
			}
			return next;
		}
		break;

	default:
		if (term_compare(t1,t2))
			return next;
		break;
	}

	return NULL;
}

static cfg_t* compile_head_end_shim(compile_context_t* context, void* param, const continuation_t* next)
{
	cfg_t* c = compile_subgoal(context,next);
	if (c)
	{
		substitutions_t* next_substs = param;
		if (next_substs && next_substs->m_count)
		{
			cfg_t* c1 = new_cfg(context);
			for (size_t i = next_substs->m_count; i--;)
			{
				if (next_substs->m_vals[i])
				{
					opcode_t* ops = append_opcodes(context,c1->m_tail,2);
					(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
					ops->m_term.m_pval = next_substs->m_vals[i];
				}
				else
				{
					opcode_t* ops = append_opcodes(context,c1->m_tail,1);
					ops->m_opcode.m_op = OP_PUSH_NULL;
					ops->m_opcode.m_arg = 1;

					while (ops->m_opcode.m_arg < 0xFFFFFFFF && i > 0 && !next_substs->m_vals[i-1])
					{
						++ops->m_opcode.m_arg;
						--i;
					}
				}
			}

			c1->m_always_flags = c->m_always_flags;

			c = goto_next(context,c1,c);
		}
	}
	return c;
}

static cfg_t* compile_head(compile_context_t* context, const term_t* goal, const compile_clause_t* clause, const continuation_t* next)
{
	term_t* sp = context->m_stack;

	substitutions_t* prev_substs = context->m_substs;
	if (context->m_substs)
	{
		context->m_stack -= bytes_to_cells(sizeof(substitutions_t) + (sizeof(term_t) * context->m_substs->m_count),sizeof(term_t));
		substitutions_t* substs = (substitutions_t*)context->m_stack;
		
		substs->m_count = context->m_substs->m_count;
		memcpy(substs->m_vals,context->m_substs->m_vals,context->m_substs->m_count * sizeof(term_t*));

		context->m_substs = substs;
	}

	substitutions_t* next_substs = NULL;
	if (clause->m_var_count)
	{
		context->m_stack -= bytes_to_cells(sizeof(substitutions_t) + (sizeof(term_t) * clause->m_var_count),sizeof(term_t));
		next_substs = (substitutions_t*)context->m_stack;
		
		next_substs->m_count = clause->m_var_count;
		memset(next_substs->m_vals,0,clause->m_var_count * sizeof(term_t*));
	}

	context->m_stack -= bytes_to_cells(sizeof(continuation_t),sizeof(term_t));
	continuation_t* cont = (continuation_t*)context->m_stack;

	unify_info_t ui = {0};
	*cont = (continuation_t){
		.m_shim = &compile_unify_end_shim,
		.m_term = (const term_t*)&ui,
		.m_next = &(continuation_t){
			.m_shim = &compile_head_end_shim,
			.m_term = (const term_t*)next_substs,
			.m_next = next
		}
	};

	cfg_t* c = NULL;
	const continuation_t* cont2 = compile_unify_head_term(context,goal,clause->m_head,next_substs,&ui,cont);
	if (cont2)
	{
		if (cont2 == cont)
			c = compile_subgoal(context,cont->m_next);
		else
			c = compile_subgoal(context,cont2);
	}

	context->m_substs = prev_substs;

	context->m_stack = sp;

	return c;
}

static cfg_t* compile_dynamic(compile_context_t* context, void* clause, const continuation_t* next)
{
	return compile_builtin(context,&prolite_builtin_user_defined,1,((compile_clause_t*)clause)->m_head,next);
}

static cfg_t* compile_extern(compile_context_t* context, void* clause, const continuation_t* next)
{
	cfg_t* cont = compile_subgoal(context,next);
	cfg_t* c_end = new_cfg(context);
	if (!cont)
		cont = c_end;
	else
		goto_next(context,cont,c_end);
	
	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,5);
	(ops++)->m_opcode.m_op = OP_EXTERN;
	(ops++)->m_term.m_pval = clause;
	(ops++)->m_term.m_pval = cont->m_tail;
	(ops++)->m_opcode.m_op = OP_JMP;
	ops->m_term.m_pval = cont->m_entry_point;
	c->m_tail = cont->m_tail;
	
	c->m_always_flags = cont->m_always_flags;

	return c;
}

void* compile_predicate_call(void* vc, const compile_predicate_t* pred, const term_t* goal, const void* vnext)
{
	if (!pred)
		return NULL;

	compile_context_t* context = vc;
	
	prolite_type_t type = get_term_type(pred->m_base.m_functor);

	cse_info_t cse = {0};
	const continuation_t* next = &(continuation_t){
		.m_shim = &compile_cse,
		.m_term = (const term_t*)&cse,
		.m_next = vnext
	};
	if (pred->m_clauses && !pred->m_clauses->m_next)
		next = vnext;

	cfg_t* c_end = NULL;
	cfg_t* c = NULL;
	for (const compile_clause_t* clause = pred->m_clauses; clause; clause = clause->m_next)
	{
		const continuation_t* inner_next = &(continuation_t){
			.m_shim = pred->m_dynamic ? &compile_dynamic : &compile_extern,
			.m_term = (const term_t*)clause,
			.m_next = next
		};
		if (!clause->m_body)
			inner_next = next;

		cfg_t* c1;
		if (type == prolite_atom)
			c1 = compile_subgoal(context,inner_next);
		else
			c1 = compile_head(context,goal,clause,inner_next);
		
		if (!c)
			c = c1;
		else if (c1)
		{
			if (!c_end)
				c_end = new_cfg(context);

			if (!(c1->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
				add_branch(context,c1,OP_BRANCH,FLAG_CUT | FLAG_THROW | FLAG_HALT,c_end);
			else
				c->m_always_flags = c1->m_always_flags;
			
			goto_next(context,c,c1);
		}

		if (c && (c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
			break;
	}

	if (c && c_end)
		goto_next(context,c,c_end);

	return c;
}

static cfg_t* compile_user_defined(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = (*context->m_link_fn)(context,context->m_link_param,goal->m_term,goal->m_next);
	if (!c)
		c = compile_builtin(context,&prolite_builtin_user_defined,1,goal->m_term,goal->m_next);

	return c;
}

static cfg_t* compile_not_proveable(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = compile_subgoal(context,goal->m_next);

	cfg_t* c1 = compile_once_inner(context,&(continuation_t){
		.m_term = get_first_arg(goal->m_term,NULL),
		.m_next = &(continuation_t){
			.m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l') }
		}
	});

	if (!c)
		c = c1;
	else if (c1)
	{
		if (c1->m_always_flags & (FLAG_THROW | FLAG_HALT))
			c = c1;
		else
		{
			cfg_t* c_end = new_cfg(context);
			goto_next(context,c,c_end);

			c1->m_always_flags = c->m_always_flags;

			c = add_branch(context,c1,OP_BRANCH_NOT,FLAG_THROW | FLAG_HALT,c);
			goto_next(context,c,c_end);
		}
	}

	return c;
}

static cfg_t* compile_callable(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (compile_is_callable(context,g1))
	{
	case 1:
		return compile_subgoal(context,goal->m_next);

	case 0:
		return NULL;

	default:
		return compile_builtin(context,&prolite_builtin_callable,1,g1,goal->m_next);
	}
}

static cfg_t* compile_type_test(compile_context_t* context, prolite_type_flags_t types, int negate, const term_t* t, const continuation_t* next)
{
	cfg_t* cont = compile_subgoal(context,next);
	if (!cont)
		return NULL;

	opcode_t* ops = append_opcodes(context,cont->m_tail,1);
	ops->m_opcode.m_op = OP_RET;

	cfg_t* c = new_cfg(context);
	ops = append_opcodes(context,c->m_tail,7);
	(ops++)->m_opcode.m_op = OP_PUSH_CONST;
	(ops++)->m_term.m_u64val = types;
	(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
	(ops++)->m_term.m_pval = t;
	(ops++)->m_opcode.m_op = OP_BUILTIN;
	(ops++)->m_term.m_pval = &prolite_builtin_type_test;
	ops->m_term.m_pval = cont->m_entry_point;

	c->m_always_flags = cont->m_always_flags;

	return c;
}

static cfg_t* compile_var(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal->m_term,NULL));

	if (get_term_type(g1) != prolite_var)
		return NULL;

	return compile_type_test(context,type_flag_var,0,g1,goal->m_next);
}

static cfg_t* compile_atom(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_atom,0,g1,goal->m_next);

	case prolite_atom:
		return compile_subgoal(context,goal->m_next);

	default:
		return NULL;
	}
}

static cfg_t* compile_integer(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_int32,0,g1,goal->m_next);

	case prolite_integer:
		return compile_subgoal(context,goal->m_next);

	default:
		return NULL;
	}
}

static cfg_t* compile_float(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_double,0,g1,goal->m_next);

	case prolite_double:
		return compile_subgoal(context,goal->m_next);

	default:
		return NULL;
	}
}

static cfg_t* compile_atomic(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_var | type_flag_chars | type_flag_charcodes | type_flag_compound,1,g1,goal->m_next);

	case prolite_chars:
	case prolite_charcodes:
	case prolite_compound:
		return NULL;

	default:
		return compile_subgoal(context,goal->m_next);
	}
}

static cfg_t* compile_compound(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_chars | type_flag_charcodes | type_flag_compound,0,g1,goal->m_next);

	case prolite_chars:
	case prolite_charcodes:
	case prolite_compound:
		return compile_subgoal(context,goal->m_next);

	default:
		return NULL;
	}
}

static cfg_t* compile_nonvar(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal->m_term,NULL));
	if (get_term_type(g1) == prolite_var)
		return compile_type_test(context,type_flag_var,1,g1,goal->m_next);

	return compile_subgoal(context,goal->m_next);
}

static cfg_t* compile_number(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_int32 | type_flag_double,0,g1,goal->m_next);

	case prolite_integer:
	case prolite_double:
		return compile_subgoal(context,goal->m_next);

	default:
		return NULL;
	}
}

static int compile_is_ground(compile_context_t* context, const term_t* goal)
{
	goal = deref_var(context,goal);
	prolite_type_t t = get_term_type(goal);
	if (t == prolite_var)
		return 0;

	if (t == prolite_compound)
	{
		size_t arity;
		for (const term_t* p = get_first_arg(goal,&arity); arity--; p = get_next_arg(p))
		{
			if (!compile_is_ground(context,p))
				return 0;
		}
	}
	return 1;
}

static cfg_t* compile_ground(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal->m_term,NULL));

	if (compile_is_ground(context,g1))
		return compile_subgoal(context,goal->m_next);

	return compile_builtin(context,&prolite_builtin_ground,1,g1,goal->m_next);
}

static cfg_t* compile_subterm(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = NULL;
	
	const debug_info_t* debug_info = get_debug_info(goal->m_term);

	switch (MASK_DEBUG_INFO(goal->m_term->m_u64val))
	{
#define DECLARE_BUILTIN_INTRINSIC(f,p) \
	case (p): c = compile_##f(context,goal); break;

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,p,a) \
	case (p): c = compile_builtin(context,&prolite_builtin_##f,a,a ? get_first_arg(goal->m_term,NULL) : NULL,goal->m_next); break;

#include "builtin_functions.h"

	default:
		switch (get_term_type(goal->m_term))
		{
		case prolite_compound:
			if ((MASK_DEBUG_INFO(goal->m_term->m_u64val) & PACK_COMPOUND_EMBED_MASK) == PACK_COMPOUND_EMBED_4(0,'c','a','l','l') ||
				MASK_DEBUG_INFO(goal->m_term[1].m_u64val) == PACK_ATOM_EMBED_4('c','a','l','l'))
			{
				c = compile_callN(context,goal);
			}
			else
				c = compile_user_defined(context,goal);
			break;

		case prolite_atom:
			c = compile_user_defined(context,goal);
			break;

		default:
			c = compile_call_inner(context,goal->m_term,goal->m_next);
			break;
		}
	}

	if (debug_info)
	{
		// TODO: Emit tracepoints
	}
	
	return c;
}

static cfg_t* compile_subgoal(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = NULL;
	if (!goal)
		c = new_cfg(context);
	else if (goal->m_shim)
		c = (*goal->m_shim)(context,(void*)goal->m_term,goal->m_next);
	else if (goal->m_term)
		c = compile_subterm(context,goal);
	return c;
}

size_t inc_ip(optype_t op)
{
	size_t ip = 1;
	switch (op)
	{
	case OP_JMP:
	case OP_GOSUB:
	case OP_PUSH_CONST:
	case OP_PUSH_TERM_REF:
	case OP_BRANCH:
	case OP_BRANCH_NOT:
		++ip;
		break;

	case OP_BUILTIN:
	case OP_EXTERN:
		ip += 2;
		break;

	default:
		break;
	}

	return ip;
}

static void move_cfg(cfg_vec_t* blks, const cfg_block_t* blk, const cfg_block_t* next)
{
	// Try to place into some kind of sort order...
	size_t index = -1;
	size_t next_index = -1;
	for (size_t j = 0; j < blks->m_count; ++j)
	{
		if (blks->m_blks[j].m_blk == next)
			next_index = j;

		if (blks->m_blks[j].m_blk == blk)
			index = j;

		if (next_index != -1 && index != -1)
			break;
	}

	if (next_index < index)
	{
		cfg_block_info_t t = blks->m_blks[index];
		memmove(&blks->m_blks[next_index+1],&blks->m_blks[next_index],(index - next_index) * sizeof(cfg_block_info_t));
		blks->m_blks[next_index] = t;
	}
}

static void walk_cfgs(compile_context_t* context, cfg_vec_t* blks, const cfg_block_t* blk)
{
	for (size_t i=0; i < blks->m_count; ++i)
	{
		if (blk == blks->m_blks[i].m_blk)
			return;
	}

	blks->m_blks = heap_realloc(context->m_heap,blks->m_blks,blks->m_count * sizeof(cfg_block_info_t),(blks->m_count + 1) * sizeof(cfg_block_info_t));
	if (!blks->m_blks)
		longjmp(context->m_jmp,1);

	blks->m_blks[blks->m_count++] = (cfg_block_info_t){ .m_blk = blk, .m_offset = 0 };
	blks->m_total += blk->m_count;

	for (size_t i = 0; i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		const cfg_block_t** next = NULL;
		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_NOP:
			--blks->m_total;
			break;

		case OP_JMP:
		case OP_BRANCH:
		case OP_BRANCH_NOT:
			next = (const cfg_block_t**)&blk->m_ops[i+1].m_term.m_pval;
			break;

		case OP_EXTERN:
			next = (const cfg_block_t**)&blk->m_ops[i+2].m_term.m_pval;
			break;

		default:
			break;
		}

		if (next)
		{
			// Rewrite JMP -> JMP
			while ((*next)->m_count == 2 &&
				(*next)->m_ops[0].m_opcode.m_op == OP_JMP)
			{
				*next = (const cfg_block_t*)(*next)->m_ops[1].m_term.m_pval;
			}

			walk_cfgs(context,blks,*next);
			move_cfg(blks,blk,*next);
		}
	}

	for (size_t i = 0; i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_GOSUB:
			{
				const cfg_block_t* next = blk->m_ops[i+1].m_term.m_pval;
				walk_cfgs(context,blks,next);
				move_cfg(blks,blk,next);
			}
			break;

		case OP_BUILTIN:
			{
				const cfg_block_t* next = blk->m_ops[i+2].m_term.m_pval;
				walk_cfgs(context,blks,next);
				move_cfg(blks,blk,next);
			}
			break;

		default:
			break;

		}
	}
}

static size_t emit_ops(opcode_t* code, const cfg_vec_t* blks)
{
	opcode_t* start = code;
	for (size_t j = 0; j < blks->m_count; ++j)
	{
		const cfg_block_t* blk = blks->m_blks[j].m_blk;
		blks->m_blks[j].m_offset = (code - start);

		for (size_t i = 0; i < blk->m_count; )
		{
			size_t len = inc_ip(blk->m_ops[i].m_opcode.m_op);

			switch (blk->m_ops[i].m_opcode.m_op)
			{
			case OP_NOP:
				break;

			/*case OP_RET:
				if (blk->m_count == 1)
				{
					// See if there is a replacement we can reuse
					int found = 0;
					for (const opcode_t* c = start; c < code; c += inc_ip(code->m_opcode.m_op))
					{
						if (c->m_opcode.m_op == OP_RET)
						{
							found = 1;
							blks->m_blks[j].m_offset = (c - start);
							break;
						}
					}

					if (!found)
						*code++ = blk->m_ops[i];
				}
				else
					*code++ = blk->m_ops[i];
				break;*/

			case OP_JMP:
				if (j == blks->m_count-1 || blk->m_ops[i+1].m_term.m_pval != blks->m_blks[j+1].m_blk)
				{
					memcpy(code,blk->m_ops + i,len * sizeof(*code));
					code += len;
				}
				break;

			/*case OP_BRANCH:
				{
					*code++ = blk->m_ops[i];

					// Reduce BRANCH to BRANCH
					const cfg_block_t* next = blk->m_ops[i+1].m_term.m_pval;
					while (next->m_count >= 2 && next->m_ops[0].m_opcode.m_op == OP_BRANCH &&
						(next->m_ops[0].m_opcode.m_arg & blk->m_ops[i].m_opcode.m_arg) == blk->m_ops[i].m_opcode.m_arg)
					{
						next = next->m_ops[1].m_term.m_pval;
					}

					(code++)->m_term.m_pval = next;
				}
				break;*/

			default:
				memcpy(code,blk->m_ops + i,len * sizeof(*code));
				code += len;
				break;
			}

			i += len;
		}
	}

	opcode_t* end = code;
	for (code = start; code < end; code += inc_ip(code->m_opcode.m_op))
	{
		switch (code->m_opcode.m_op)
		{
		case OP_JMP:
		case OP_BRANCH:
		case OP_BRANCH_NOT:
		case OP_GOSUB:
			for (size_t j = 0; j < blks->m_count; ++j)
			{
				if (blks->m_blks[j].m_blk == code[1].m_term.m_pval)
				{
					code[1].m_term.m_u64val = blks->m_blks[j].m_offset - (code + 1 - start);
					break;
				}
			}
			break;

		case OP_BUILTIN:
		case OP_EXTERN:
			for (size_t j = 0; j < blks->m_count; ++j)
			{
				if (blks->m_blks[j].m_blk == code[2].m_term.m_pval)
				{
					code[2].m_term.m_u64val = blks->m_blks[j].m_offset - (code + 2 - start);
					break;
				}
			}
			break;

		default:
			break;
		}
	}

	return (end - start);
}

#if ENABLE_TESTS

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#undef DECLARE_BUILTIN_INTRINSIC
#define DECLARE_BUILTIN_INTRINSIC(f,p)

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,p,a) \
	{ &prolite_builtin_##f, #f },

static const char* builtinName(const builtin_fn_t fn)
{
	static const struct builtin_names
	{
		builtin_fn_t fn;
		const char* name;
	} bns[] =
	{
		#include "builtin_functions.h"

		{ &prolite_builtin_call, "call" },
		{ &prolite_builtin_callN, "call/N" },
		{ &prolite_builtin_catch, "catch" },
		{ &prolite_builtin_throw, "throw", },
		{ &prolite_builtin_halt, "halt" },
		{ &prolite_builtin_user_defined, "user_defined" },
		{ &prolite_builtin_callable, "callable" },
		{ &prolite_builtin_unify, "unify" },
		{ &prolite_builtin_ground, "ground" },
		{ &prolite_builtin_type_test, "type_test" },
		{ &prolite_builtin_term_compare, "term_compare" }
	};

	for (size_t i=0; i < sizeof(bns)/sizeof(bns[0]); ++i)
	{
		if (fn == bns[i].fn)
			return bns[i].name;
	}

	return "unknown";
}

static void fmtFlags(exec_flags_t v, char* buf)
{
	char* s = buf;

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

#include "write_term.h"

#include <inttypes.h>

struct std_output
{
	prolite_stream_t m_base;
	FILE* m_file;
};

static int std_output_stream_write(struct prolite_stream* s, const void* src, size_t len, prolite_stream_error_t* err)
{
	struct std_output* stream = (struct std_output*)s;

	const char* str = src;
	while (len)
	{
		const char* c = memchr(str,' ',len);
		if (c)
		{
			if (c > str && !fwrite(str,c - str,1,stream->m_file))
				return feof(stream->m_file) ? 0 : -1;

			if (!fwrite("\\ ",2,1,stream->m_file))
				return feof(stream->m_file) ? 0 : -1;
			
			len -= (c - str) + 1;
			str = c + 1;
		}
		else
		{
			if (!fwrite(str,len,1,stream->m_file))
				return feof(stream->m_file) ? 0 : -1;

			len = 0;
		}
	}

	return 1;
}

static void dumpPI(const term_t* t, FILE* f)
{
	size_t arity;
	string_t s;
	get_predicate(t,&s,&arity,NULL);
	fprintf(f,"%.*s/%zu",(int)s.m_len,s.m_str,arity);
}

static void dumpTerm(context_t* context, const term_t* t, FILE* f)
{
	struct std_output stream =
	{
		.m_base.m_fn_write = &std_output_stream_write,
		.m_file = f
	};

	if (get_term_type(t) != prolite_var)
		fprintf(f,"'");

	write_term(context,&stream.m_base,t,NULL,NULL);
	
	if (get_term_type(t) != prolite_var)
		fprintf(f,"'");
}

static void dumpCFGBlock(context_t* context, const cfg_block_t* blk, FILE* f)
{
	char buf[10] = {0};

	fprintf(f,"\tnode [shape=record];\n");
	fprintf(f,"\tN%p [label=\"{",blk);

	if (!blk->m_count)
	{
		fprintf(f,"<f0> WTF?!?!");
	}

	for (size_t i=0;i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		if (i)
			fprintf(f,"|");
		fprintf(f,"<f%zu> ",i);

		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_NOP:
			fprintf(f,"(NOP)");
			break;

		case OP_JMP:
			fprintf(f,"Jmp");
			break;

		case OP_GOSUB:
			fprintf(f,"Gosub");
			break;

		case OP_RET:
			fprintf(f,"Ret");
			break;

		case OP_BUILTIN:
			fprintf(f,"Builtin\\ %s\\ Gosub",builtinName(blk->m_ops[i+1].m_term.m_pval));
			break;

		case OP_EXTERN:
			fprintf(f,"if !Extern\\ ");
			dumpPI(((compile_clause_t*)blk->m_ops[i+1].m_term.m_pval)->m_head,f);
			fprintf(f,"\\ then\\ Goto");
			break;

		case OP_SET_FLAGS:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"Set\\ Flags\\ %s",buf);
			break;

		case OP_CLEAR_FLAGS:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"Clear\\ Flags\\ %s",buf);
			break;

		case OP_PUSH_CUT:
			fprintf(f,"Push\\ Cut");
			break;

		case OP_POP_CUT:
			fprintf(f,"Pop\\ Cut");
			break;

		case OP_BRANCH:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"Branch \\%s",buf);
			break;

		case OP_BRANCH_NOT:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"Branch !\\%s",buf);
			break;

		case OP_PUSH_CONST:
			fprintf(f,"Push\\ Const\\ %"PRIu64,blk->m_ops[i+1].m_term.m_u64val);
			break;

		case OP_PUSH_NULL:
			fprintf(f,"Push\\ NULL * %u",blk->m_ops[i].m_opcode.m_arg);
			break;

		case OP_PUSH_TERM_REF:
			fprintf(f,"Push\\ ");
			dumpTerm(context,blk->m_ops[i+1].m_term.m_pval,f);
			break;
		
		default:
			fprintf(f,"WTF? %zu",(size_t)blk->m_ops[i].m_term.m_u64val);
			break;
		}
	}

	fprintf(f,"}\"];\n");

	for (size_t i=0;i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_BRANCH:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [label=\"%s\"];\n",blk,i,blk->m_ops[i+1].m_term.m_pval,buf);
			break;

		case OP_BRANCH_NOT:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [label=\"!%s\"];\n",blk,i,blk->m_ops[i+1].m_term.m_pval,buf);
			break;

		case OP_GOSUB:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [dir=both];\n",blk,i,blk->m_ops[i+1].m_term.m_pval);
			break;

		case OP_JMP:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0>;\n",blk,i,blk->m_ops[i+1].m_term.m_pval);
			break;

		case OP_BUILTIN:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [dir=both];\n",blk,i,blk->m_ops[i+2].m_term.m_pval);
			break;

		case OP_EXTERN:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0>;\n",blk,i,blk->m_ops[i+2].m_term.m_pval);
			break;

		default:
			break;
		}
	}
}

void dumpCFG(context_t* context, const cfg_vec_t* blks, const char* filename)
{
	FILE* f = fopen(filename,"w");

	fprintf(f,"digraph cfg {\n\tstart [shape=point];\n");

	for (size_t i=0; i < blks->m_count; ++i)
		dumpCFGBlock(context,blks->m_blks[i].m_blk,f);

	fprintf(f,"\tstart -> N%p:<f0>;\n}",blks->m_blks[0].m_blk);

	fclose(f);
}

void dumpTrace(context_t* context, const opcode_t* code, size_t count, const char* filename)
{
	FILE* f = fopen(filename,"w");

	char buf[10] = {0};

	int spaces = 0;
	for (size_t c = count; c; c /= 10)
		++spaces;

	const opcode_t* start = code;
	for (const opcode_t* end = code + count; code < end; code += inc_ip(code->m_opcode.m_op))
	{
		fprintf(f,"%*zu: ",spaces,code - start);

		switch (code->m_opcode.m_op)
		{
		case OP_NOP:
			fprintf(f,"NOP;\n");
			break;

		case OP_JMP:
			fprintf(f,"goto %+d (%zu);\n",(int)code[1].m_term.m_u64val,(size_t)((code + 1 - start) + (int64_t)code[1].m_term.m_u64val));
			break;

		case OP_GOSUB:
			fprintf(f,"gosub %+d (%zu);\n",(int)code[1].m_term.m_u64val,(size_t)((code + 1 - start) + (int64_t)code[1].m_term.m_u64val));
			break;

		case OP_RET:
			fprintf(f,"return;\n");
			break;

		case OP_BUILTIN:
			fprintf(f,"builtin(%s) gosub %+d (%zu);\n",builtinName(code[1].m_term.m_pval),(int)code[2].m_term.m_u64val,(size_t)((code + 2 - start) + (int64_t)code[2].m_term.m_u64val));
			break;

		case OP_EXTERN:
			fprintf(f,"if !extern(");
			dumpPI(((compile_clause_t*)code[1].m_term.m_pval)->m_head,f);
			fprintf(f,") goto %+d (%zu);\n",(int)code[2].m_term.m_u64val,(size_t)((code + 2 - start) + (int64_t)code[2].m_term.m_u64val));
			break;

		case OP_SET_FLAGS:
			fmtFlags(code->m_opcode.m_arg,buf);
			fprintf(f,"flags |= %s;\n",buf);
			break;

		case OP_CLEAR_FLAGS:
			fmtFlags(code->m_opcode.m_arg,buf);
			fprintf(f,"flags &= ~(%s);\n",buf);
			break;

		case OP_PUSH_CUT:
			fprintf(f,"push cut;\n");
			break;

		case OP_POP_CUT:
			fprintf(f,"pop cut;\n");
			break;

		case OP_BRANCH:
			fmtFlags(code->m_opcode.m_arg,buf);
			fprintf(f,"if (flags & %s) goto %+d (%zu);\n",buf,(int)code[1].m_term.m_u64val,(size_t)((code + 1 - start) + (int64_t)code[1].m_term.m_u64val));
			break;

		case OP_BRANCH_NOT:
			fmtFlags(code->m_opcode.m_arg,buf);
			fprintf(f,"if (!(flags & %s)) goto %+d (%zu);\n",buf,(int)code[1].m_term.m_u64val,(size_t)((code + 1 - start) + (int64_t)code[1].m_term.m_u64val));
			break;

		case OP_PUSH_CONST:
			fprintf(f,"push cont %" PRIu64 ";\n",code[1].m_term.m_u64val);
			break;

		case OP_PUSH_NULL:
			fprintf(f,"push NULL * %u",code->m_opcode.m_arg);
			break;

		case OP_PUSH_TERM_REF:
			fprintf(f,"push ");
			dumpTerm(context,code[1].m_term.m_pval,f);
			fprintf(f,";\n");
			break;

		default:
			fprintf(f,"WTF? %zu;\n",(size_t)code->m_term.m_u64val);
			break;
		}
	}

	fclose(f);
}
#endif // ENABLE_TESTS

void compile_goal(context_t* context, link_fn_t link_fn, void* link_param, const term_t* goal, size_t var_count)
{
	size_t heap_start = heap_top(&context->m_heap);
	compile_context_t cc =
	{
		.m_stack = context->m_stack,
		.m_heap = &context->m_heap,
		.m_link_fn = link_fn,
		.m_link_param = link_param
	};
	if (!setjmp(cc.m_jmp))
	{
		if (var_count)
		{
			cc.m_substs = heap_malloc(&context->m_heap,sizeof(substitutions_t) + (sizeof(term_t) * var_count));
			if (!cc.m_substs)
				longjmp(cc.m_jmp,1);

			cc.m_substs->m_count = var_count;
			memset(cc.m_substs->m_vals,0,var_count * sizeof(term_t*));
		}

		cfg_t* c = compile_subgoal(&cc,&(continuation_t){ .m_term = goal });
		if (!c)
			c = new_cfg(&cc);
		opcode_t* ops = append_opcodes(&cc,c->m_tail,1);
		ops->m_opcode.m_op = OP_RET;

		cfg_vec_t blks = {0};
		walk_cfgs(&cc,&blks,c->m_entry_point);

#if ENABLE_TESTS
		dumpCFG(context,&blks,"./cfg.dot");
#endif

		if (blks.m_total)
		{
			opcode_t* code = heap_malloc(&context->m_heap,blks.m_total * sizeof(opcode_t));
			if (!code)
				longjmp(cc.m_jmp,1);

			size_t new_size = emit_ops(code,&blks);

			// Resize the memory block for sanity
			opcode_t* new_code = heap_realloc(&context->m_heap,code,blks.m_total * sizeof(opcode_t),new_size * sizeof(opcode_t));
			if (!new_code)
				longjmp(cc.m_jmp,1);

			code = new_code;
			blks.m_total = new_size;

#if ENABLE_TESTS
			dumpTrace(context,code,blks.m_total,"./pcode.txt");
#endif

			// Put pcode on the stack... JIT later...
			context->m_stack -= bytes_to_cells(blks.m_total * sizeof(opcode_t),sizeof(term_t));
			memcpy(context->m_stack,code,blks.m_total * sizeof(opcode_t));
		}
		(--context->m_stack)->m_u64val = blks.m_total;
	}

	/* Bulk free all heap allocs */
	heap_reset(&context->m_heap,heap_start);
}
