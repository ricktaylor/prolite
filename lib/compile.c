
#include "stream.h"
#include "compile.h"
#include "builtins.h"

#include <string.h>
#include <stdarg.h>
#include <assert.h>

// TEMP

void builtin_call(context_t* context) {  }
void builtin_callN(context_t* context) {  }
void builtin_catch(context_t* context) {  }
void builtin_throw(context_t* context) {  }
void builtin_halt(context_t* context) {  }
void builtin_occurs_check(context_t* context) {  }
void builtin_callable(context_t* context) {  }
void builtin_ground(context_t* context) {  }
void builtin_term_compare(context_t* context) {  }

// END TEMP

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

static continuation_t* new_continuation(compile_context_t* context)
{
	continuation_t* c = heap_malloc(context->m_heap,sizeof(continuation_t));
	if (!c)
		longjmp(context->m_jmp,1);

	c->m_subroutine = 0;
	c->m_always_flags = 0;
	c->m_tail = new_cfg_block(context);
	c->m_entry_point = c->m_tail;

	return c;
}

static substitutions_t* copy_substitutions(compile_context_t* context, const substitutions_t* s)
{
	if (!s)
		return NULL;

	substitutions_t* s2 = heap_malloc(context->m_heap,sizeof(substitutions_t) + (sizeof(term_t) * s->m_count));
	if (!s2)
		longjmp(context->m_jmp,1);

	s2->m_count = s->m_count;
	memcpy(s2->m_vals,s->m_vals,s->m_count * sizeof(term_t*));
	return s2;
}

static continuation_t* set_flags(compile_context_t* context, continuation_t* c, exec_flags_t flags)
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

static continuation_t* clear_flags(compile_context_t* context, continuation_t* c, exec_flags_t flags)
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

static continuation_t* goto_next(compile_context_t* context, continuation_t* c, continuation_t* next)
{
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	if (next->m_subroutine)
	{
		while (next->m_entry_point->m_count == 3 &&
			next->m_entry_point->m_ops[0].m_opcode.m_op == OP_GOSUB &&
			next->m_entry_point->m_ops[2].m_opcode.m_op == OP_RET)
		{
			next->m_entry_point = (void*)next->m_entry_point->m_ops[1].m_term.m_pval;
		}

		(ops++)->m_opcode.m_op = OP_GOSUB;
		ops->m_term.m_pval = next->m_entry_point;
	}
	else
	{
		while (next->m_entry_point->m_count == 2 &&
			next->m_entry_point->m_ops[0].m_opcode.m_op == OP_JMP)
		{
			next->m_entry_point = (void*)next->m_entry_point->m_ops[1].m_term.m_pval;
		}

		(ops++)->m_opcode.m_op = OP_JMP;
		ops->m_term.m_pval = next->m_entry_point;
		c->m_tail = next->m_tail;
	}
	return c;
}

static continuation_t* add_branch(compile_context_t* context, continuation_t* c, optype_t branch, exec_flags_t flags, continuation_t* next)
{
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	ops->m_opcode.m_op = branch;
	(ops++)->m_opcode.m_arg = flags;
	(ops++)->m_term.m_pval = next->m_entry_point;
	
	return c;
}

static continuation_t* make_subroutine(compile_context_t* context, continuation_t* c)
{
	if (!c->m_subroutine)
	{
		opcode_t* ops = append_opcodes(context,c->m_tail,1);
		ops->m_opcode.m_op = OP_RET;
		c->m_subroutine = 1;
	}
	return c;
}

static continuation_t* convert_to_gosub(compile_context_t* context, continuation_t* c)
{
	c = make_subroutine(context,c);

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

	// Short-circuit just a flag set (used by some builtins)
	if (cont->m_entry_point == cont->m_tail &&
		cont->m_entry_point->m_count == 1 &&
		cont->m_entry_point->m_ops[0].m_opcode.m_op == OP_SET_FLAGS)
	{
		cont->m_entry_point->m_ops[0].m_opcode.m_arg &= ~FLAG_CUT;
		if (cont->m_entry_point->m_ops[0].m_opcode.m_arg == 0)
			cont->m_entry_point->m_ops[0].m_opcode.m_op = OP_NOP;

		return cont;
	}

	continuation_t* c = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,1);
	ops->m_opcode.m_op = OP_PUSH_CUT;
	c = goto_next(context,c,cont);

	ops = append_opcodes(context,c->m_tail,1);
	ops->m_opcode.m_op = OP_POP_CUT;
	c->m_always_flags = cont->m_always_flags;

	return c;
}

const term_t* deref_var(compile_context_t* context, const term_t* goal)
{
	if (get_term_type(goal) == prolite_var)
	{
		assert(context->m_substs && get_var_index(goal) < context->m_substs->m_count);
		const term_t* g = context->m_substs->m_vals[get_var_index(goal)];
		if (g)
			goal = deref_var(context,g);
	}
	return goal;
}

static void pre_substitute_goal_inner(compile_context_t* context, const term_t* g1, const term_t* g2)
{
	prolite_type_t t1 = get_term_type(g1);
	if (t1 == prolite_var)
	{
		if (g1->m_u64val != g2->m_u64val)
		{
			assert(context->m_substs && get_var_index(g1) < context->m_substs->m_count);
			context->m_substs->m_vals[get_var_index(g1)] = g2;
		}
	}
	else
	{
		prolite_type_t t2 = get_term_type(g2);
		if (t2 == prolite_var)
		{
			assert(context->m_substs && get_var_index(g2) < context->m_substs->m_count);
			context->m_substs->m_vals[get_var_index(g2)] = g1;
		}
		else if (t1 == t2 &&
				t1 == prolite_compound &&
				predicate_compare(g1,g2))
		{
			size_t arity;
			const term_t* p1 = get_first_arg(g1,&arity,NULL);
			const term_t* p2 = get_first_arg(g2,NULL,NULL);
			while (arity--)
			{
				pre_substitute_goal_inner(context,deref_var(context,p1),deref_var(context,p2));

				p1 = get_next_arg(p1,NULL);
				p2 = get_next_arg(p2,NULL);
			}
		}
	}
}

static void pre_substitute_goal(compile_context_t* context, const term_t* goal)
{
	if (goal->m_u64val == PACK_COMPOUND_EMBED_1(2,'=') ||
		goal->m_u64val == PACK_COMPOUND_BUILTIN(unify_with_occurs_check,2))
	{
		const term_t* g1 = get_first_arg(goal,NULL,NULL);
		const term_t* g2 = get_next_arg(g1,NULL);

		pre_substitute_goal_inner(context,deref_var(context,g1),deref_var(context,g2));
	}
}

static continuation_t* compile_goal(compile_context_t* context, continuation_t* cont, const term_t* goal);

continuation_t* compile_false(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	if (cont->m_always_flags & FLAG_FAIL)
		return cont;

	return set_flags(context,new_continuation(context),FLAG_FAIL);
}

static continuation_t* compile_cut(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	continuation_t* c = set_flags(context,new_continuation(context),FLAG_CUT);
	return goto_next(context,c,cont);
}

static continuation_t* compile_and(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	const term_t* g2 = get_next_arg(g1,NULL);

	substitutions_t* s_orig = context->m_substs;
	context->m_substs = copy_substitutions(context,context->m_substs);

	pre_substitute_goal(context,g1);
	continuation_t* c = compile_goal(context,cont,g2);

	context->m_substs = s_orig;
	return compile_goal(context,c,g1);
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
				if (c_if->m_entry_point->m_count == 1 &&
					c_if->m_entry_point->m_ops[0].m_opcode.m_op == OP_NOP)
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
				if (c_if->m_entry_point->m_count == 1 &&
					c_if->m_entry_point->m_ops[0].m_opcode.m_op == OP_NOP)
				{
					c_if = c_then;
				}
				else
					c_if = goto_next(context,c_if,c_then);
			}
			else
			{
				continuation_t* c_end = new_continuation(context);
				c_then = goto_next(context,c_then,c_end);

				if (c_if->m_entry_point->m_count != 1 ||
					c_if->m_entry_point->m_ops[0].m_opcode.m_op != OP_NOP)
				{
					if (c_else)
						add_branch(context,c_if,OP_BRANCH,FLAG_THROW | FLAG_HALT,c_end);
					else
						add_branch(context,c_if,OP_BRANCH,FLAG_THROW | FLAG_HALT | FLAG_FAIL,c_end);
				}

				if (c_else)
				{
					c_else = goto_next(context,c_else,c_end);

					add_branch(context,c_if,OP_BRANCH_NOT,FLAG_FAIL,c_then);
					clear_flags(context,c_if,FLAG_FAIL);

					c_if = goto_next(context,c_if,c_else);
					c_if->m_always_flags &= c_else->m_always_flags;
				}
				else
					c_if = goto_next(context,c_if,c_then);
			}
		}
	}

	return c_if;
}

static continuation_t* compile_or(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	const term_t* g2 = get_next_arg(g1,NULL);

	substitutions_t* s_orig = context->m_substs;
	context->m_substs = copy_substitutions(context,context->m_substs);

	if (g1->m_u64val == PACK_COMPOUND_EMBED_2(2,'-','>'))
	{
		const term_t* g_if = get_first_arg(g1,NULL,NULL);

		continuation_t* c_if = compile_goal(context,set_flags(context,new_continuation(context),FLAG_CUT),g_if);

		continuation_t* c_then = NULL;
		continuation_t* c_else = NULL;
		if (c_if->m_always_flags & FLAG_FAIL)
		{
			context->m_substs = s_orig;
			c_else = compile_goal(context,cont,g2);
		}
		else
		{
			c_then = compile_goal(context,cont,get_next_arg(g_if,NULL));
			
			if (!(c_if->m_always_flags & FLAG_CUT))
			{
				context->m_substs = s_orig;
				c_else = compile_goal(context,cont,g2);
			}
		}

		return compile_if_then_else(context,c_if,c_then,c_else);
	}

	continuation_t* c = compile_goal(context,convert_to_gosub(context,cont),g1);
	if (c->m_always_flags & FLAG_FAIL)
	{
		context->m_substs = s_orig;
		c = compile_goal(context,cont,g2);
	}
	else if (!(c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
	{
		continuation_t* c_end = new_continuation(context);

		add_branch(context,c,OP_BRANCH,FLAG_CUT | FLAG_THROW | FLAG_HALT,c_end);
		c = clear_flags(context,c,FLAG_FAIL);

		context->m_substs = s_orig;
		continuation_t* c2 = compile_goal(context,convert_to_gosub(context,cont),g2);
		c2 = goto_next(context,c2,c_end);
		c = goto_next(context,c,c2);

		c->m_always_flags = c2->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT);
	}

	return c;
}

static continuation_t* compile_if_then(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g_if = get_first_arg(goal,NULL,NULL);
	const term_t* g_then = get_next_arg(g_if,NULL);

	continuation_t* c_if = set_flags(context,new_continuation(context),FLAG_CUT);
	c_if = compile_goal(context,c_if,g_if);

	continuation_t* c_then = compile_goal(context,cont,g_then);
	return compile_if_then_else(context,c_if,c_then,NULL);
}

continuation_t* compile_builtin(compile_context_t* context, continuation_t* cont, builtin_fn_t fn, size_t arity, const term_t* goal)
{
	continuation_t* c = new_continuation(context);
	if (arity > 1)
	{
		// Reverse the arguments
		const term_t** rev = heap_malloc(context->m_heap,arity * sizeof(term_t*));
		if (!rev)
			longjmp(context->m_jmp,1);

		for (size_t i = 0; i < arity; ++i)
		{
			rev[i] = goal;
			goal = get_next_arg(goal,NULL);
		}

		for (size_t i = arity; i--;)
		{
			opcode_t* ops = append_opcodes(context,c->m_tail,2);
			(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
			ops->m_term.m_pval = deref_var(context,rev[i]);
		}
	}
	else if (arity)
	{
		opcode_t* ops = append_opcodes(context,c->m_tail,2);
		(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
		ops->m_term.m_pval = deref_var(context,goal);
	}

	continuation_t* c1 = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c1->m_tail,3);
	(ops++)->m_opcode.m_op = OP_PUSH_CUT;
	(ops++)->m_opcode.m_op = OP_BUILTIN;
	ops->m_term.m_pval = fn;

	continuation_t* c_end = new_continuation(context);
	ops = append_opcodes(context,c_end->m_tail,1);
	ops->m_opcode.m_op = OP_POP_CUT;

	add_branch(context,c1,OP_BRANCH,FLAG_FAIL | FLAG_CUT | FLAG_THROW | FLAG_HALT,c_end);
	ops = append_opcodes(context,c1->m_tail,1);
	ops->m_opcode.m_op = OP_POP_CUT;
	c1 = goto_next(context,c1,cont);

	continuation_t* c2 = new_continuation(context);
	goto_next(context,c_end,c2);
	c_end = c2;
	
	if (cont->m_always_flags & (FLAG_FAIL | FLAG_CUT | FLAG_THROW | FLAG_HALT))
	{
		c1 = goto_next(context,c1,c_end);
	}
	else
	{
		add_branch(context,c1,OP_BRANCH,FLAG_FAIL | FLAG_CUT | FLAG_THROW | FLAG_HALT,c_end);
			
		ops = append_opcodes(context,c1->m_tail,2);
		(ops++)->m_opcode.m_op = OP_JMP;
		ops->m_term.m_pval = c1->m_entry_point;
	}
	
	c = goto_next(context,c,c1);
	c->m_always_flags = cont->m_always_flags;
	c->m_tail = c_end->m_tail;

	return c;
}

static continuation_t* compile_user_defined(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return compile_builtin(context,cont,&builtin_user_defined,1,goal);
}

static continuation_t* compile_throw(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	goal = deref_var(context,get_first_arg(goal,NULL,NULL));

	cont = set_flags(context,new_continuation(context),FLAG_THROW);

	return compile_builtin(context,cont,&builtin_throw,1,goal);
}

static continuation_t* compile_halt(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	cont = set_flags(context,new_continuation(context),FLAG_HALT);

	if (get_term_type(goal) != prolite_atom)
	{
		goal = deref_var(context,get_first_arg(goal,NULL,NULL));

		cont = compile_builtin(context,cont,&builtin_halt,1,goal);
	}

	return cont;
}

static int compile_is_callable(compile_context_t* context, const term_t* goal)
{
	switch (get_term_type(goal))
	{
	case prolite_atom:
		return 1;

	case prolite_compound:
		switch (goal->m_u64val)
		{
		case PACK_COMPOUND_EMBED_1(2,','):
		case PACK_COMPOUND_EMBED_1(2,';'):
		case PACK_COMPOUND_EMBED_2(2,'-','>'):
			{
				size_t arity;
				for (const term_t* p = get_first_arg(goal,&arity,NULL); arity--; p = get_next_arg(p,NULL))
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

static continuation_t* compile_call_inner(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	goal = deref_var(context,goal);

	if (compile_is_callable(context,goal))
		return compile_goal(context,cont,goal);

	return compile_builtin(context,cont,&builtin_call,1,goal);
}

static continuation_t* compile_call(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return wrap_cut(context,compile_call_inner(context,cont,get_first_arg(goal,NULL,NULL)));
}

static continuation_t* compile_callN(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return wrap_cut(context,compile_builtin(context,cont,&builtin_callN,1,goal));
}

static continuation_t* compile_catch(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	const term_t* g2 = get_next_arg(g1,NULL);
	const term_t* g3 = get_next_arg(g2,NULL);

	continuation_t* c = wrap_cut(context,compile_call_inner(context,convert_to_gosub(context,cont),g1));
	if (!(c->m_always_flags & FLAG_HALT))
	{
		continuation_t* c_end = new_continuation(context);

		continuation_t* c_resume;
		if (c->m_always_flags & FLAG_THROW)
			c_resume = wrap_cut(context,compile_call_inner(context,cont,g3));
		else
			c_resume = wrap_cut(context,compile_call_inner(context,convert_to_gosub(context,cont),g3));

		continuation_t* c_catch = compile_builtin(context,c_resume,&builtin_catch,1,g2);
		c_catch = goto_next(context,c_catch,c_end);

		if (c->m_always_flags & FLAG_THROW)
			c_end = c_catch;
		else
			add_branch(context,c,OP_BRANCH,FLAG_THROW,c_catch);

		c = goto_next(context,c,c_end);
	}
	return c;
}

static continuation_t* compile_once(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	continuation_t* c = set_flags(context,new_continuation(context),FLAG_CUT);
	c = compile_call_inner(context,c,get_first_arg(goal,NULL,NULL));

	return compile_if_then_else(context,c,cont,NULL);
}

static continuation_t* compile_repeat(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	if (cont->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT))
		return cont;

	continuation_t* c_end = new_continuation(context);

	if (cont->m_subroutine)
		cont = goto_next(context,new_continuation(context),cont);

	add_branch(context,cont,OP_BRANCH,FLAG_CUT | FLAG_THROW | FLAG_HALT,c_end);
	clear_flags(context,cont,FLAG_FAIL);

	opcode_t* ops = append_opcodes(context,cont->m_tail,2);
	(ops++)->m_opcode.m_op = OP_JMP;
	ops->m_term.m_pval = cont->m_entry_point;
	
	cont->m_tail = c_end->m_tail;

	return cont;
}

static continuation_t* compile_unify_var(compile_context_t* context, continuation_t* cont, const term_t* g1, const term_t* g2, int with_occurs_check)
{
	size_t idx = get_var_index(g1);
	assert(context->m_substs && idx < context->m_substs->m_count);

	cont = make_subroutine(context,cont);

	continuation_t* c_end = new_continuation(context);

	continuation_t* c_set = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c_set->m_tail,3);
	(ops++)->m_opcode.m_op = OP_SET_VAR;
	(ops++)->m_term.m_u64val = idx;
	ops->m_term.m_pval = g2;
	c_set = goto_next(context,c_set,cont);
	ops = append_opcodes(context,c_set->m_tail,2);
	(ops++)->m_opcode.m_op = OP_CLEAR_VAR;
	ops->m_term.m_u64val = idx;

	if (with_occurs_check)
		c_set = compile_builtin(context,c_set,&builtin_occurs_check,0,g1);

	c_set = goto_next(context,c_set,c_end);

	continuation_t* c = new_continuation(context);
	ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode = (struct op_arg){ .m_op = OP_TYPE_TEST, .m_arg = type_flag_var | (1 << get_term_type(g2)) };
	ops->m_term.m_u64val = idx;
	add_branch(context,c,OP_BRANCH,FLAG_FAIL,c_end);

	if (with_occurs_check)
	{
		ops = append_opcodes(context,c->m_tail,4);
		(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
		(ops++)->m_term.m_pval = g2;
		(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
		ops->m_term.m_pval = g1;
	}

	ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode = (struct op_arg){ .m_op = OP_TYPE_TEST, .m_arg = type_flag_var };
	ops->m_term.m_u64val = idx;
	add_branch(context,c,OP_BRANCH_NOT,FLAG_FAIL,c_set);

	if (!with_occurs_check)
	{
		ops = append_opcodes(context,c->m_tail,4);
		(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
		(ops++)->m_term.m_pval = g2;
		(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
		ops->m_term.m_pval = g1;
	}

	continuation_t* c1 = compile_builtin(context,cont,&builtin_term_compare,0,g1);
	c = goto_next(context,c,c1);
	return goto_next(context,c,c_end);
}

static continuation_t* compile_unify_inner(compile_context_t* context, continuation_t* cont, const term_t* g1, const term_t* g2, int with_occurs_check)
{
	prolite_type_t t1 = get_term_type(g1);
	if (t1 == prolite_var)
	{
		if (g1->m_u64val == g2->m_u64val)
			return cont;

		return compile_unify_var(context,cont,g1,g2,with_occurs_check);
	}

	prolite_type_t t2 = get_term_type(g2);
	if (t2 == prolite_var)
		return compile_unify_inner(context,cont,g2,g1,with_occurs_check);

	if (t1 == t2)
	{
		if (t1 == prolite_compound && predicate_compare(g1,g2))
		{
			size_t arity;
			const term_t* p1 = get_first_arg(g1,&arity,NULL);
			const term_t* p2 = get_first_arg(g2,NULL,NULL);

			const term_t** rev = heap_malloc(context->m_heap,arity * 2 * sizeof(term_t*));
			if (!rev)
				longjmp(context->m_jmp,1);

			for (size_t i = 0; i < arity; ++i)
			{
				rev[i*2] = deref_var(context,p1);
				rev[i*2+1] = deref_var(context,p2);

				p1 = get_next_arg(p1,NULL);
				p2 = get_next_arg(p2,NULL);
			}

			while (arity--)
			{
				cont = compile_unify_inner(context,cont,rev[arity*2],rev[arity*2+1],with_occurs_check);
				if (cont->m_always_flags & FLAG_FAIL)
					break;
			}
			return cont;
		}
		else if (term_compare(g1,g2))
			return compile_true(context,cont,NULL);
	}

	return compile_false(context,cont,NULL);
}

static continuation_t* compile_unify(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	const term_t* g2 = get_next_arg(g1,NULL);

	return compile_unify_inner(context,cont,deref_var(context,g1),deref_var(context,g2),0);
}

static continuation_t* compile_unify_with_occurs_check(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	const term_t* g2 = get_next_arg(g1,NULL);

	return compile_unify_inner(context,cont,deref_var(context,g1),deref_var(context,g2),1);
}

static continuation_t* compile_not_unifiable(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	const term_t* g2 = get_next_arg(g1,NULL);

	continuation_t* c = set_flags(context,new_continuation(context),FLAG_CUT);
	c = compile_unify_inner(context,c,deref_var(context,g1),deref_var(context,g2),0);

	return compile_if_then_else(context,c,compile_false(context,cont,NULL),cont);
}

static continuation_t* compile_not_proveable(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	continuation_t* c = set_flags(context,new_continuation(context),FLAG_CUT);
	c = compile_call_inner(context,c,get_first_arg(goal,NULL,NULL));

	return compile_if_then_else(context,c,compile_false(context,cont,NULL),cont);
}

static continuation_t* compile_callable(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (compile_is_callable(context,g1))
	{
	case 1:
		return compile_true(context,cont,g1);

	case 0:
		return compile_false(context,cont,g1);

	default:
		return compile_builtin(context,cont,&builtin_callable,1,g1);
	}
}

continuation_t* compile_type_test(compile_context_t* context, continuation_t* cont, prolite_type_flags_t types, int negate, const term_t* goal)
{
	size_t idx = get_var_index(goal);
	assert(context->m_substs && idx < context->m_substs->m_count);

	continuation_t* c_end = new_continuation(context);

	continuation_t* c = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode = (struct op_arg){ .m_op = OP_TYPE_TEST, .m_arg = types };
	ops->m_term.m_u64val = idx;

	add_branch(context,c,negate ? OP_BRANCH : OP_BRANCH_NOT,FLAG_FAIL,cont);

	cont = goto_next(context,cont,c_end);
	return goto_next(context,c,c_end);
}

static continuation_t* compile_var(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));

	if (get_term_type(g1) == prolite_var)
		return compile_type_test(context,cont,type_flag_var,0,g1);
	else
		return compile_false(context,cont,g1);
}

static continuation_t* compile_atom(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,cont,type_flag_atom,0,g1);

	case prolite_atom:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

static continuation_t* compile_integer(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,cont,type_flag_int32,0,g1);

	case prolite_int32:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

static continuation_t* compile_float(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,cont,type_flag_double,0,g1);

	case prolite_double:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

static continuation_t* compile_atomic(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,cont,type_flag_var | type_flag_chars | type_flag_charcodes | type_flag_compound,1,g1);

	case prolite_chars:
	case prolite_charcodes:
	case prolite_compound:
		return compile_false(context,cont,goal);

	default:
		return compile_true(context,cont,goal);
	}
}

static continuation_t* compile_compound(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,cont,type_flag_chars | type_flag_charcodes | type_flag_compound,0,g1);

	case prolite_chars:
	case prolite_charcodes:
	case prolite_compound:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

static continuation_t* compile_nonvar(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	if (get_term_type(g1) == prolite_var)
		return compile_type_test(context,cont,type_flag_var,1,g1);

	return compile_true(context,cont,goal);
}

static continuation_t* compile_number(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,cont,type_flag_int32 | type_flag_double,0,g1);

	case prolite_int32:
	case prolite_double:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
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
		for (const term_t* p = get_first_arg(goal,&arity,NULL); arity--; p = get_next_arg(p,NULL))
		{
			if (!compile_is_ground(context,p))
				return 0;
		}
	}
	return 1;
}

static continuation_t* compile_ground(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));

	if (compile_is_ground(context,g1))
		return compile_true(context,cont,g1);

	return compile_builtin(context,cont,&builtin_ground,1,g1);
}

static continuation_t* compile_builtin_fn(compile_context_t* context, continuation_t* cont, builtin_fn_t fn, const term_t* goal)
{
	size_t arity;
	goal = get_first_arg(goal,&arity,NULL);
	return compile_builtin(context,cont,fn,arity,goal);
}

static continuation_t* compile_goal(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	int debug = 0;

	continuation_t* c;
	switch (goal->m_u64val)
	{
#define DECLARE_BUILTIN_INTRINSIC(f,n) \
	case (n): c = compile_##f(context,cont,goal); break;

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,n) \
	case (n): c = compile_builtin_fn(context,cont,&builtin_##f,goal); break;

#include "builtin_functions.h"

	default:
		switch (get_term_type(goal))
		{
		case prolite_compound:
			if ((goal->m_u64val & PACK_COMPOUND_EMBED_4(0,'c','a','l','l')) == PACK_COMPOUND_EMBED_4(0,'c','a','l','l') ||
				(goal->m_u64val & PACK_COMPOUND_BUILTIN(call,0)) == PACK_COMPOUND_BUILTIN(call,0) ||
				goal[1].m_u64val == PACK_ATOM_EMBED_4('c','a','l','l'))
			{
				c = wrap_cut(context,compile_callN(context,cont,goal));
			}
			else
				c = compile_user_defined(context,cont,goal);
			break;

		case prolite_atom:
			c = compile_user_defined(context,cont,goal);
			break;

		default:
			c = wrap_cut(context,compile_call_inner(context,cont,goal));
			break;
		}
	}

	if (debug)
	{
		// TODO: Emit tracepoints
	}

	return c;
}

size_t inc_ip(optype_t op)
{
	size_t ip = 1;
	switch (op)
	{
	case OP_JMP:
	case OP_GOSUB:
	case OP_CLEAR_VAR:
	case OP_TYPE_TEST:
	case OP_PUSH_TERM_REF:
	case OP_BRANCH:
	case OP_BRANCH_NOT:
	case OP_BUILTIN:
		++ip;
		break;
	
    case OP_SET_VAR:
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
		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_NOP:
			--blks->m_total;
			break;

		case OP_JMP:
			{
				// Reduce JMP to JMP
				const cfg_block_t* next = blk->m_ops[i+1].m_term.m_pval;
				while (next->m_count == 2 && next->m_ops[0].m_opcode.m_op == OP_JMP)
					next = next->m_ops[1].m_term.m_pval;

				walk_cfgs(context,blks,next);
				move_cfg(blks,blk,next);
			}
			break;

		default:
			break;
		}	
	}

	for (size_t i = 0; i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_GOSUB:
		case OP_BRANCH:
		case OP_BRANCH_NOT:
			{
				const cfg_block_t* next = blk->m_ops[i+1].m_term.m_pval;
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

			case OP_RET:
				if (blk->m_count == 1)
				{
					// See if there is a replacement we can reuse
					for (const opcode_t* c = start; c < code; c += inc_ip(code->m_opcode.m_op))
					{
						if (c->m_opcode.m_op == OP_RET)
						{
							blks->m_blks[j].m_offset = (c - start);
							break;
						}
					}
				}
				else
					*code++ = blk->m_ops[i];
				break;

			case OP_JMP:
				if (j == blks->m_count-1 || blk->m_ops[i+1].m_term.m_pval != blks->m_blks[j+1].m_blk)
				{
					memcpy(code,blk->m_ops + i,len * sizeof(*code));
					code += len;
				}
				break;

			case OP_BRANCH:
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
				break;

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
		
		default:
			break;
		}
	}

	return (end - start);
}

void compile(context_t* context, stream_t* s)
{
	// Read a term and prepare it for execution
	parse_status_t result = read_term(context,s);
	if (result == PARSE_OK)
	{
		size_t heap_start = heap_top(context->m_heap);
		compile_context_t cc = {0};
		cc.m_heap = context->m_heap;
		if (!setjmp(cc.m_jmp))
		{
			// Pop varinfo
			size_t varcount = 0;
			{
				const term_t* sp = context->m_stack;
				varcount = (sp++)->m_u64val;
				for (size_t i = 0; i < varcount; ++i)
					sp = get_next_arg(sp,NULL) + 1;

				context->m_stack = (term_t*)sp;
			}

			if (varcount)
			{
				cc.m_substs = heap_malloc(cc.m_heap,sizeof(substitutions_t) + (sizeof(term_t) * varcount));
				if (!cc.m_substs)
					longjmp(cc.m_jmp,1);

				cc.m_substs->m_count = varcount;
				memset(cc.m_substs->m_vals,0,varcount * sizeof(term_t*));
			}

			continuation_t* c = new_continuation(&cc);
			opcode_t* ops = append_opcodes(&cc,c->m_tail,1);
			ops->m_opcode.m_op = OP_SUCCEEDS;

			c = compile_goal(&cc,c,context->m_stack);
			ops = append_opcodes(&cc,c->m_tail,1);
			ops->m_opcode.m_op = OP_END;
			
			cfg_vec_t blks = {0};
			walk_cfgs(&cc,&blks,c->m_entry_point);

			dumpCFG(&blks,"./cfg.dot");

			if (blks.m_total)
			{
				opcode_t* code = heap_malloc(cc.m_heap,blks.m_total * sizeof(opcode_t));
				if (!code)
					longjmp(cc.m_jmp,1);

				blks.m_total = emit_ops(code,&blks);

				dumpTrace(code,blks.m_total,"./pcode.txt");

				// Put pcode on the stack... JIT later...
				context->m_stack -= bytes_to_cells(blks.m_total * sizeof(opcode_t),sizeof(term_t));
				memcpy(context->m_stack,code,blks.m_total * sizeof(opcode_t));
			}			
			(--context->m_stack)->m_u64val = blks.m_total;
		}
		
		/* Bulk free all heap allocs */
		heap_reset(cc.m_heap,heap_start);
	}

	/* We have just made heavy use of the heap */
	heap_compact(context->m_heap);
}
