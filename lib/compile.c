
#include "compile.h"
#include "stream.h"

#include <string.h>
#include <stdarg.h>
#include <assert.h>

// TEMP
#include <stdio.h>

void dumpCFG(const cfg_block_t* s, FILE* f);

int builtin_call(context_t* context) { return 0; }
int builtin_callN(context_t* context) { return 0; }
int builtin_catch(context_t* context) { return 0; }
int builtin_throw(context_t* context) { return 0; }
int builtin_halt(context_t* context) { return 0; }
int builtin_user_defined(context_t* context) { return 0; }
int builtin_occurs_check(context_t* context) { return 0; }
int builtin_callable(context_t* context) { return 0; }
int builtin_ground(context_t* context) { return 0; }

// END TEMP

static cfg_block_t* new_cfg_block(compile_context_t* context)
{
	cfg_block_t* b = heap_malloc(&context->m_heap,sizeof(cfg_block_t));
	if (!b)
		longjmp(context->m_jmp,1);

	b->m_inputs = 0;
	b->m_count = 0;
	b->m_ops = NULL;
	return b;
}

static opcode_t* append_opcodes(compile_context_t* context, cfg_block_t* blk, size_t count)
{
	opcode_t* ret = blk->m_ops;
	if (count)
	{
		blk->m_ops = heap_realloc(&context->m_heap,blk->m_ops,blk->m_count * sizeof(opcode_t),(blk->m_count + count) * sizeof(opcode_t));
		if (!blk->m_ops)
			longjmp(context->m_jmp,1);

		ret = blk->m_ops + blk->m_count;
		blk->m_count += count;
	}
	return ret;
}

static continuation_t* new_continuation(compile_context_t* context)
{
	continuation_t* c = heap_malloc(&context->m_heap,sizeof(continuation_t));
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

	substitutions_t* s2 = heap_malloc(&context->m_heap,sizeof(substitutions_t) + (sizeof(term_t) * s->m_count));
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
			next->m_entry_point = (void*)next->m_entry_point->m_ops[1].m_pval;
		}

		(ops++)->m_opcode.m_op = OP_GOSUB;
		ops->m_pval = next->m_entry_point;
	}
	else
	{
		while (next->m_entry_point->m_count == 2 &&
			next->m_entry_point->m_ops[0].m_opcode.m_op == OP_JMP)
		{
			next->m_entry_point = (void*)next->m_entry_point->m_ops[1].m_pval;
		}

		(ops++)->m_opcode.m_op = OP_JMP;
		ops->m_pval = next->m_entry_point;
		++next->m_entry_point->m_inputs;
		c->m_tail = next->m_tail;
	}
	return c;
}

static continuation_t* add_branch(compile_context_t* context, continuation_t* c, optype_t branch, exec_flags_t flags, continuation_t* next)
{
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	ops->m_opcode.m_op = branch;
	(ops++)->m_opcode.m_arg = flags;
	(ops++)->m_pval = next->m_entry_point;
	++next->m_entry_point->m_inputs;

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
		cont->m_entry_point->m_ops[0].m_opcode.m_op &= ~FLAG_CUT;
		if (cont->m_entry_point->m_ops[0].m_opcode.m_op == 0)
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
			uint64_t arity;
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
	return set_flags(context,cont,FLAG_CUT);
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
		continuation_t* c_if = set_flags(context,new_continuation(context),FLAG_CUT);
		const term_t* g_if = get_first_arg(g1,NULL,NULL);

		c_if = compile_goal(context,c_if,g_if);

		continuation_t* c_then = NULL;
		continuation_t* c_else = NULL;
		if (c_if->m_always_flags & FLAG_FAIL)
		{
			context->m_substs = s_orig;
			c_else = compile_goal(context,cont,g2);
		}
		else
		{
			const term_t* g_then = get_next_arg(g_if,NULL);
			c_then = compile_goal(context,cont,g_then);

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
		c = clear_flags(context,c,FLAG_FAIL);

		context->m_substs = s_orig;
		continuation_t* c2 = compile_goal(context,convert_to_gosub(context,cont),g2);

		continuation_t* c_end = new_continuation(context);

		add_branch(context,c,OP_BRANCH,FLAG_CUT | FLAG_THROW | FLAG_HALT,c_end);
		
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

static continuation_t* compile_frame_start(compile_context_t* context)
{
	continuation_t* c = new_continuation(context);

	/* TODO:
	 * push ebp
	 * mov	ebp, esp
	 */

	return c;
}

static void compile_frame_end(compile_context_t* context, continuation_t* c)
{
	/* TODO:
	 * mov	esp, ebp
	 * pop	ebp
	 */
}

continuation_t* compile_builtin(compile_context_t* context, continuation_t* cont, builtin_fn_t fn, uint64_t arity, const term_t* goal)
{
	cont = make_subroutine(context,cont);

	while (cont->m_entry_point->m_count == 3 &&
		cont->m_entry_point->m_ops[0].m_opcode.m_op == OP_GOSUB &&
		cont->m_entry_point->m_ops[2].m_opcode.m_op == OP_RET)
	{
		cont->m_entry_point = (void*)cont->m_entry_point->m_ops[1].m_pval;
	}

	continuation_t* c = compile_frame_start(context);
	if (arity > 1)
	{
		// Reverse the arguments
		const term_t** rev = heap_malloc(&context->m_heap,arity * sizeof(term_t*));
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
			ops->m_pval = deref_var(context,rev[i]);
		}
	}
	else
	{
		opcode_t* ops = append_opcodes(context,c->m_tail,2);
		(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
		ops->m_pval = deref_var(context,goal);
	}

	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode.m_op = OP_BUILTIN;
	(ops++)->m_pval = fn;
	ops->m_pval = cont->m_entry_point;
	++cont->m_entry_point->m_inputs;

	compile_frame_end(context,c);

	c->m_always_flags = (cont->m_always_flags & (FLAG_FAIL | FLAG_THROW));

	return c;
}

continuation_t* compile_user_defined(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	// TODO - we can be more explicit here...

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
				uint64_t arity;
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
	ops->m_pval = cont->m_entry_point;
	++cont->m_entry_point->m_inputs;

	cont->m_tail = c_end->m_tail;

	return cont;
}

static continuation_t* compile_unify_var(compile_context_t* context, continuation_t* cont, const term_t* g1, const term_t* g2, int with_occurs_check)
{
	uint64_t idx = get_var_index(g1);
	assert(context->m_substs && idx < context->m_substs->m_count);

	cont = make_subroutine(context,cont);

	continuation_t* c_end = new_continuation(context);
	
	continuation_t* c_set = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c_set->m_tail,3);
	(ops++)->m_opcode.m_op = OP_SET_VAR;
	(ops++)->m_u64val = idx;
	ops->m_pval = g2;
	c_set = goto_next(context,c_set,cont);
	ops = append_opcodes(context,c_set->m_tail,2);
	(ops++)->m_opcode.m_op = OP_CLEAR_VAR;
	ops->m_u64val = idx;
	
	if (with_occurs_check)
	{
		c_set = compile_builtin(context,c_set,&builtin_occurs_check,1,g1);

		continuation_t* c1 = new_continuation(context);
		ops = append_opcodes(context,c1->m_tail,2);
		(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
		ops->m_pval = g2;
		c_set = goto_next(context,c1,c_set);
	}

	c_set = goto_next(context,c_set,c_end);

	continuation_t* c = new_continuation(context);
	ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode = (struct op_arg){ .m_op = OP_TYPE_TEST, .m_arg = type_flag_var | (1 << get_term_type(g2)) };
	ops->m_u64val = idx;
	add_branch(context,c,OP_BRANCH,FLAG_FAIL,c_end);

	ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode = (struct op_arg){ .m_op = OP_TYPE_TEST, .m_arg = type_flag_var };
	ops->m_u64val = idx;
	add_branch(context,c,OP_BRANCH_NOT,FLAG_FAIL,c_set);
	
	ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode.m_op = OP_TERM_CMP;
	(ops++)->m_pval = g1;
	ops->m_pval = g2;

	add_branch(context,c,OP_BRANCH,FLAG_FAIL,c_end);
	
	c = goto_next(context,c,cont);
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
			uint64_t arity;
			const term_t* p1 = get_first_arg(g1,&arity,NULL);
			const term_t* p2 = get_first_arg(g2,NULL,NULL);

			const term_t** rev = heap_malloc(&context->m_heap,arity * 2 * sizeof(term_t*));
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
	uint64_t idx = get_var_index(goal);
	assert(context->m_substs && idx < context->m_substs->m_count);

	continuation_t* c_end = new_continuation(context);
	
	continuation_t* c = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode = (struct op_arg){ .m_op = OP_TYPE_TEST, .m_arg = types };
	ops->m_u64val = idx;

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
		uint64_t arity;
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
	uint64_t arity;
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
				c = compile_callN(context,cont,goal);
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

static void compile_term(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	continuation_t* c = compile_goal(context,cont,goal);
	++c->m_entry_point->m_inputs;

	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	ops->m_opcode.m_op = OP_SET_FLAGS;
	(ops++)->m_opcode.m_arg = c->m_always_flags;
	ops->m_opcode.m_op = OP_END;

	// TODO: Link!!

	dumpCFG(c->m_entry_point,stdout);
}

void compile(context_t* context, stream_t* s)
{
	// Read a term and prepare it for execution
	parse_status_t result = read_term(context,s);
	if (result != PARSE_OK)
		fprintf(stderr,"Parser failure\n");
	else
	{
		size_t heap_start = heap_top(context->m_heap);
		compile_context_t cc = {0};
		cc.m_heap = context->m_heap;
		if (!setjmp(cc.m_jmp))
		{
			// Pop varinfo
			size_t varcount = 0;
			{
				const term_t* sp = (const term_t*)context->m_stack;
				varcount = (sp++)->m_u64val;
				for (size_t i = 0; i < varcount; ++i)
					sp = get_next_arg(sp,NULL) + 1;

				context->m_stack = (uint64_t*)sp;
			}

			if (varcount)
			{
				cc.m_substs = heap_malloc(&context->m_heap,sizeof(substitutions_t) + (sizeof(term_t) * varcount));
				if (!cc.m_substs)
					longjmp(cc.m_jmp,1);

				cc.m_substs->m_count = varcount;
				memset(cc.m_substs->m_vals,0,varcount * sizeof(term_t*));
			}

			continuation_t* cont = new_continuation(&cc);
			opcode_t* ops = append_opcodes(&cc,cont->m_tail,1);
			ops->m_opcode.m_op = OP_SUCCEEDS;

			compile_term(&cc,cont,(const term_t*)context->m_stack);
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
