

#include "types.h"

#include <assert.h>

enum eSolveResult solve_goal(struct context_t* context, struct term_t* goal);

static int copy_term(struct context_t* context, struct term_t* src, struct term_t* dest, size_t* stack_pos)
{
	*stack_pos = stack_top(context->m_exec_stack);

	// TODO: Walk src, copying vars into scratch, copying body into exec

	// Copy vars into exec


}

static void clear_vars(struct var_info_t* vars)
{

}

static inline int stack_push_term(struct context_t* context, const struct term_t* t)
{
	if (stack_push_ptr(&context->m_exec_stack,t->m_vars) == -1)
		return -1;

	if (stack_push_ptr(&context->m_exec_stack,t->m_value) == -1)
	{
		stack_pop_ptr(&context->m_exec_stack);
		return -1;
	}

	return 0;
}

static inline void stack_pop_term(struct context_t* context, struct term_t* t)
{
	t->m_value = stack_pop_ptr(&context->m_exec_stack);
	t->m_vars = stack_pop_ptr(&context->m_exec_stack);
}

static enum eSolveResult redo_and(struct context_t* context)
{
	struct term_t second_goal;
	solve_fn_t* fn;
	enum eSolveResult result;

	stack_pop_term(context,&second_goal);

	/* Redo solve_goal(second_goal) */
	fn = stack_pop_ptr(&context->m_exec_stack);
	result = (*fn)(context);

	if (result == SOLVE_FAIL)
	{
redo:
		clear_vars(second_goal.m_vars);

		/* Redo solve_goal(first_goal) */
		fn = stack_pop_ptr(&context->m_exec_stack);
		result = (*fn)(context);
		if (result == SOLVE_TRUE)
		{
			result = solve_goal(context,&second_goal);
			if (result == SOLVE_FAIL)
				goto redo;
		}
	}

	if (result == SOLVE_TRUE)
	{
		if (stack_push_term(context,&second_goal) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_and) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static enum eSolveResult solve_and(struct context_t* context, struct term_t* orig_goal)
{
	enum eSolveResult result;
	struct term_t fresh_goal;
	size_t stack_base;

	if (copy_term(context,orig_goal,&fresh_goal,&stack_base) != 0)
		result = SOLVE_NOMEM;
	else
	{
		struct term_t first_goal;
		first_goal.m_vars = fresh_goal.m_vars;
		first_goal.m_value = fresh_goal.m_value + 1;

		result = solve_goal(context,&first_goal);
		if (result == SOLVE_TRUE)
		{
			struct term_t second_goal;
			second_goal.m_vars = fresh_goal.m_vars;
			second_goal.m_value = next_value(first_goal.m_value);
redo:
			result = solve_goal(context,&second_goal);
			if (result == SOLVE_TRUE)
			{
				if (stack_push_term(context,&second_goal) == -1 ||
					stack_push_ptr(&context->m_exec_stack,&redo_and) == -1)
				{
					result = SOLVE_NOMEM;
				}
			}
			else if (result == SOLVE_FAIL)
			{
				/* Redo solve_goal(first_goal) */
				solve_fn_t* fn = stack_pop_ptr(&context->m_exec_stack);

				clear_vars(fresh_goal.m_vars);

				result = (*fn)(context);
				if (result == SOLVE_TRUE)
					goto redo;
			}
		}

		if (result != SOLVE_TRUE && result != SOLVE_THROW && result != SOLVE_HALT)
			stack_reset(&context->m_exec_stack,stack_base);
	}

	return result;
}

static enum eSolveResult redo_or(struct context_t* context)
{
	struct term_t or_goal;
	solve_fn_t* fn;
	enum eSolveResult result;

	stack_pop_term(context,&or_goal);

	/* Redo solve_goal(first_goal) */
	fn = stack_pop_ptr(&context->m_exec_stack);
	result = (*fn)(context);
	if (result == SOLVE_TRUE)
	{
		if (stack_push_term(context,&or_goal) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_or) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}
	else if (result == SOLVE_FAIL)
	{
		struct term_t new_or_goal;
		size_t stack_base;

		if (copy_term(context,&or_goal,&new_or_goal,&stack_base) != 0)
			result = SOLVE_NOMEM;
		else
		{
			result = solve_goal(context,&new_or_goal);
			if (result != SOLVE_TRUE && result != SOLVE_THROW && result != SOLVE_HALT)
				stack_reset(&context->m_exec_stack,stack_base);
		}
	}

	return result;
}

static enum eSolveResult solve_or(struct context_t* context, struct term_t* orig_goal)
{
	enum eSolveResult result;
	struct term_t either_goal,or_goal;
	size_t stack_base;

	or_goal.m_vars = either_goal.m_vars = orig_goal->m_vars;
	either_goal.m_value = orig_goal->m_value + 1;
	or_goal.m_value = next_value(either_goal.m_value);

	if (copy_term(context,&either_goal,&either_goal,&stack_base) != 0)
		result = SOLVE_NOMEM;
	else
	{
		result = solve_goal(context,&either_goal);
		if (result == SOLVE_TRUE)
		{
			if (stack_push_term(context,&or_goal) == -1 ||
				stack_push_ptr(&context->m_exec_stack,&redo_or) == -1)
			{
				result = SOLVE_NOMEM;
			}
		}
		else
		{
			if (result != SOLVE_THROW && result != SOLVE_HALT)
				stack_reset(&context->m_exec_stack,stack_base);

			if (result == SOLVE_FAIL)
			{
				clear_vars(orig_goal->m_vars);

				if (copy_term(context,&or_goal,&or_goal,&stack_base) != 0)
					result = SOLVE_NOMEM;
				else
				{
					result = solve_goal(context,&or_goal);

					if (result != SOLVE_TRUE && result != SOLVE_THROW && result != SOLVE_HALT)
						stack_reset(&context->m_exec_stack,stack_base);
				}
			}
		}
	}

	return result;
}

static enum eSolveResult redo_cut(struct context_t* context)
{
	return SOLVE_CUT;
}

static enum eSolveResult solve_cut(struct context_t* context)
{
	if (stack_push_ptr(&context->m_exec_stack,&redo_cut) == -1)
		return SOLVE_NOMEM;

	return SOLVE_TRUE;
}

static enum eSolveResult redo_call(struct context_t* context)
{
	solve_fn_t* fn;
	enum eSolveResult result;

	/* Redo solve_goal() */
	fn = stack_pop_ptr(&context->m_exec_stack);
	result = (*fn)(context);

	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,&redo_call) == -1)
			result = SOLVE_NOMEM;
	}
	else if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	return result;
}

static enum eSolveResult solve_call(struct context_t* context, struct term_t* orig_goal)
{
	enum eSolveResult result;
	struct term_t fresh_goal;
	size_t stack_base;

	if (copy_term(context,orig_goal,&fresh_goal,&stack_base) != 0)
		result = SOLVE_NOMEM;
	else
	{
		result = solve_goal(context,&fresh_goal);
		if (result == SOLVE_TRUE)
		{
			if (stack_push_ptr(&context->m_exec_stack,&redo_call) == -1)
				result = SOLVE_NOMEM;
		}
		else if (result == SOLVE_CUT)
			result = SOLVE_FAIL;

		if (result != SOLVE_TRUE && result != SOLVE_THROW && result != SOLVE_HALT)
			stack_reset(&context->m_exec_stack,stack_base);
	}

	return result;
}

static enum eSolveResult redo_true(struct context_t* context)
{
	return SOLVE_FAIL;
}

static enum eSolveResult solve_true(struct context_t* context)
{
	if (stack_push_ptr(&context->m_exec_stack,&redo_true) == -1)
		return SOLVE_NOMEM;

	return SOLVE_TRUE;
}

static enum eSolveResult solve_if_then_else(struct context_t* context, struct term_t* orig_goal)
{
	assert(0);
}

static enum eSolveResult solve_if_then(struct context_t* context, struct term_t* orig_goal)
{
	assert(0);
}

static enum eSolveResult solve_catch(struct context_t* context, struct term_t* orig_goal)
{
	assert(0);
}

static enum eSolveResult solve_throw(struct context_t* context, struct term_t* orig_goal)
{
	assert(0);
}

static enum eSolveResult solve_halt(struct context_t* context, struct term_t* orig_goal)
{
	enum eSolveResult result = SOLVE_HALT;
	size_t stack_base = stack_top(context->m_exec_stack);

	if (stack_push_term(context,orig_goal) == -1 ||
		stack_push(&context->m_exec_stack,stack_base) == -1)
	{
		result = SOLVE_NOMEM;
	}

	return result;
}

enum eSolveResult solve_goal(struct context_t* context, struct term_t* goal)
{
	switch (goal->m_value->m_uval)
	{
	case BOX_COMPOUND_EMBED_1(2,','):
		return solve_and(context,goal);

	case BOX_COMPOUND_EMBED_1(2,';'):
		if ((goal->m_value->m_uval) == BOX_COMPOUND_EMBED_2(2,'-','>'))
			return solve_if_then_else(context,goal);
		return solve_or(context,goal);

	case BOX_COMPOUND_EMBED_2(2,'-','>'):
		return solve_if_then(context,goal);

	case BOX_ATOM_EMBED_1('!'):
		return solve_cut(context);

	case BOX_COMPOUND_EMBED_4(1,'c','a','l','l'):
		return solve_call(context,goal);

	case BOX_ATOM_EMBED_4('t','r','u','e'):
		return solve_true(context);

	case BOX_ATOM_EMBED_4('f','a','i','l'):
		return SOLVE_FAIL;

	case BOX_COMPOUND_EMBED_5(3,'c','a','t','c','h'):
		return solve_catch(context,goal);

	case BOX_COMPOUND_EMBED_5(1,'t','h','r','o','w'):
		return solve_throw(context,goal);

	case BOX_COMPOUND_EMBED_4(0,'h','a','l','t'):
	case BOX_COMPOUND_EMBED_4(1,'h','a','l','t'):
		return solve_halt(context,goal);

	default:
		break;
	}

	/* TODO: Check for builtins */

	/* Emit user defined */

	return SOLVE_FAIL;
}
