
#include "compile.h"
#include "stream.h"

#include <string.h>
#include <stdarg.h>
#include <assert.h>

// TEMP
#include <stdio.h>

static const char* builtin_call(void) { return "call"; }
static const char* builtin_callN(void) { return "call_N"; }
static const char* builtin_catch(void) { return "catch"; }
static const char* builtin_throw(void) { return "throw"; }
static const char* builtin_halt(void) { return "halt"; }
static const char* builtin_user_defined(void) { return "user_defined"; }

void dumpCFG(const cfg_block_t* s, FILE* f);

// END TEMP

static cfg_block_t* new_cfg_block(compile_context_t* context)
{
	cfg_block_t* b = heap_malloc(&context->m_heap,sizeof(cfg_block_t));
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

	c->m_call_site = 0;
	c->m_always_flags = 0;
	c->m_tail = new_cfg_block(context);
	c->m_entry_point = c->m_tail;

	return c;
}

static substitutions_t* copy_substitutions(compile_context_t* context, const substitutions_t* s)
{
	substitutions_t* s2 = heap_malloc(&context->m_heap,sizeof(substitutions_t) + (sizeof(term_t) * s->m_count));
	if (!s2)
		longjmp(context->m_jmp,1);

	s2->m_count = s->m_count;
	memcpy(s2->m_vals,s->m_vals,s->m_count * sizeof(term_t*));
	return s2;
}

static continuation_t* set_flags(compile_context_t* context, continuation_t* c, uint8_t flags)
{
	if (c->m_tail->m_count >= 2 &&
		c->m_tail->m_ops[c->m_tail->m_count-2].m_opcode == OP_CLEAR_FLAGS &&
		(c->m_tail->m_ops[c->m_tail->m_count-1].m_u64val & flags))
	{
		c->m_tail->m_ops[c->m_tail->m_count-1].m_u64val &= ~flags;
		if (c->m_tail->m_ops[c->m_tail->m_count-1].m_u64val == 0)
		{
			c->m_tail->m_ops[c->m_tail->m_count-2].m_opcode = OP_NOP;
			c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode = OP_NOP;
		}
	}
	else if (c->m_tail->m_count >= 2 &&
		c->m_tail->m_ops[c->m_tail->m_count-2].m_opcode == OP_SET_FLAGS)
	{
		c->m_tail->m_ops[c->m_tail->m_count-1].m_u64val |= flags;
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
	if (c->m_tail->m_count >= 2 &&
		c->m_tail->m_ops[c->m_tail->m_count-2].m_opcode == OP_SET_FLAGS &&
		(c->m_tail->m_ops[c->m_tail->m_count-1].m_u64val & flags))
	{
		c->m_tail->m_ops[c->m_tail->m_count-1].m_u64val &= ~flags;
		if (c->m_tail->m_ops[c->m_tail->m_count-1].m_u64val == 0)
		{
			c->m_tail->m_ops[c->m_tail->m_count-2].m_opcode = OP_NOP;
			c->m_tail->m_ops[c->m_tail->m_count-1].m_opcode = OP_NOP;
		}
	}
	else if (c->m_tail->m_count >= 2 &&
		c->m_tail->m_ops[c->m_tail->m_count-2].m_opcode == OP_CLEAR_FLAGS)
	{
		c->m_tail->m_ops[c->m_tail->m_count-1].m_u64val |= flags;
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
		while (next->m_entry_point->m_count == 3 &&
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
		while (next->m_entry_point->m_count == 2 &&
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

	// Short-circuit just a flag set (used by some builtins)
	if (cont->m_entry_point == cont->m_tail &&
		cont->m_entry_point->m_count == 2 &&
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
				if (c_if->m_entry_point->m_count == 2 &&
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
				if (c_if->m_entry_point->m_count == 2 &&
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

				if (c_if->m_entry_point->m_count != 2 ||
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

	continuation_t* c = compile_goal(context,convert_to_call(context,cont),g1);
	if (c->m_always_flags & FLAG_FAIL)
	{
		context->m_substs = s_orig;
		c = compile_goal(context,cont,g2);
	}
	else if (!(c->m_always_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
	{
		c = clear_flags(context,c,FLAG_FAIL);

		context->m_substs = s_orig;
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
	const term_t* g_then = get_next_arg(g_if,NULL);
				
	continuation_t* c_if = set_flags(context,new_continuation(context),FLAG_CUT);
	c_if = compile_goal(context,c_if,g_if);

	continuation_t* c_then = compile_goal(context,cont,g_then);
	return compile_if_then_else(context,c_if,c_then,NULL);
}

continuation_t* compile_builtin(compile_context_t* context, continuation_t* cont, builtin_fn_t fn, uint64_t arity, const term_t* g1)
{
	// Convert cont to a call site
	cont = make_call_site(context,cont);

	while (cont->m_entry_point->m_count == 3 &&
		cont->m_entry_point->m_ops[0].m_opcode == OP_CALL &&
		cont->m_entry_point->m_ops[2].m_opcode == OP_RET)
	{
		cont->m_entry_point = cont->m_entry_point->m_ops[1].m_pval;
	}

	continuation_t* c = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3 + (2*arity));

	const term_t** rev = heap_malloc(&context->m_heap,arity * sizeof(term_t*));
	if (!rev)
		longjmp(context->m_jmp,1);
	
	for (size_t i = 0; i < arity; ++i)
	{
		rev[i] = deref_var(context,g1);
		g1 = get_next_arg(g1,NULL);
	}

	for (size_t i = arity; i--;)
	{
		(ops++)->m_opcode = OP_PUSH_TERM;
		(ops++)->m_pval = rev[i];
	}	
	(ops++)->m_opcode = OP_BUILTIN;
	(ops++)->m_pval = fn;
	(ops++)->m_pval = cont->m_entry_point;

	c->m_always_flags = cont->m_always_flags;

	return c;
}

continuation_t* compile_user_defined(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return compile_builtin(context,cont,&builtin_user_defined,1,goal);
}

static continuation_t* compile_throw_call(compile_context_t* context, builtin_fn_t builtin, const term_t* goal)
{
	continuation_t* c = new_continuation(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,4);
	(ops++)->m_opcode = OP_PUSH_TERM;
	(ops++)->m_pval = deref_var(context,goal);
	(ops++)->m_opcode = OP_THROW;
	ops->m_pval = builtin;
	c->m_always_flags = FLAG_THROW;
	return c;
}

static continuation_t* compile_throw(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	return compile_throw_call(context,&builtin_throw,get_first_arg(goal,NULL,NULL));
}

static continuation_t* compile_halt(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	continuation_t* c = compile_throw_call(context,&builtin_halt,get_first_arg(goal,NULL,NULL));
	
	if (get_term_type(goal) == prolite_atom)
		c->m_always_flags = FLAG_HALT;

	return c;
}

static int compile_is_callable(compile_context_t* context, const term_t* goal)
{
	goal = deref_var(context,goal);

	switch (get_term_type(goal))
	{
	case prolite_atom:
		return 1;

	case prolite_compound:
		{
			uint64_t arity;
			for (const term_t* p = get_first_arg(goal,&arity,NULL); arity--; p = get_next_arg(p,NULL))
			{
				int r = compile_is_callable(context,p);
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
	switch (compile_is_callable(context,goal))
	{
	case 1:
		return compile_goal(context,cont,goal);

	case 0:
		return compile_throw_call(context,&builtin_call,goal);

	default:
		return compile_builtin(context,cont,&builtin_call,1,goal);
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

static continuation_t* compile_callN(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	continuation_t* c = compile_builtin(context,cont,&builtin_callN,1,goal);
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
		
		continuation_t* c_catch = compile_builtin(context,c_resume,&builtin_catch,1,g2);
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

static continuation_t* compile_unify_inner(compile_context_t* context, continuation_t* cont, const term_t* g1, const term_t* g2)
{
	prolite_type_t t1 = get_term_type(g1);
	if (t1 == prolite_var)
	{
		if (g1->m_u64val != g2->m_u64val)
		{
			uint64_t idx = get_var_index(g1);

			assert(context->m_substs && idx < context->m_substs->m_count);

			continuation_t* c = new_continuation(context);
			opcode_t* ops = append_opcodes(context,c->m_tail,3);
			(ops++)->m_opcode = OP_UNIFY_VAR;
			(ops++)->m_u64val = idx;
			ops->m_pval = g2;

			c = goto_next(context,c,cont);
			ops = append_opcodes(context,c->m_tail,2);
			(ops++)->m_opcode = OP_CLEAR_VAR;
			(ops++)->m_u64val = idx;
			cont = c;

			context->m_substs->m_vals[idx] = g2;
		}
		return cont;
	}
	
	prolite_type_t t2 = get_term_type(g2);
	if (t2 == prolite_var)
		return compile_unify_inner(context,cont,g2,g1);

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

			continuation_t* c = cont;
			while (arity--)
			{
				c = compile_unify_inner(context,c,rev[arity*2],rev[arity*2+1]);
				if (c->m_always_flags & FLAG_FAIL)
					break;
			}					
			return c;
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

	return compile_unify_inner(context,cont,deref_var(context,g1),deref_var(context,g2));
}

static continuation_t* compile_not_unifiable(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	const term_t* g2 = get_next_arg(g1,NULL);

	continuation_t* c = set_flags(context,new_continuation(context),FLAG_CUT);
	c = compile_unify_inner(context,c,deref_var(context,g1),deref_var(context,g2));

	return compile_if_then_else(context,c,compile_false(context,cont,NULL),cont);
}

static continuation_t* compile_not_proveable(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);

	continuation_t* c = set_flags(context,new_continuation(context),FLAG_CUT);
	c = compile_call_inner(context,c,g1);

	return compile_if_then_else(context,c,compile_false(context,cont,NULL),cont);
}

continuation_t* compile_callable(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
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

#undef DECLARE_BUILTIN_HYBRID
#define DECLARE_BUILTIN_HYBRID(f,n) \
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

		case prolite_var:
			c = compile_call_inner(context,cont,deref_var(context,goal));
			if (!(c->m_always_flags & (FLAG_THROW | FLAG_HALT)))
				c = wrap_cut(context,c);
			break;

		default:
			// We know this throws...
			c = compile_throw_call(context,&builtin_call,goal);
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
	parse_status_t result = read_term(context,s);
	if (result != PARSE_OK)
		fprintf(stderr,"Parser failure\n");
	else
	{
		// Pop varinfo
		const term_t* sp = context->m_stack;

		size_t varcount = (sp++)->m_u64val;
		for (size_t i = 0; i < varcount; ++i)
			sp = get_next_arg(sp,NULL) + 1;

		size_t heap_start = heap_top(context->m_heap);
		compile_context_t cc = {0};
		cc.m_heap = context->m_heap;
		if (!setjmp(cc.m_jmp))
		{
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
