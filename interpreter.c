

#include "types.h"

static enum eSolveResult solve_goal(struct context_t* context, struct term_t* goal);

static int copy_term(struct context_t* context, struct term_t* src, struct term_t* dest)
{

}

static void clear_vars(struct var_info_t* vars)
{

}

static enum eSolveResult redo_and(struct context_t* context)
{
	struct term_t second_goal;
	solve_fn_t* fn;
	enum eSolveResult result;

	second_goal.m_value = stack_pop_ptr(&context->m_exec_stack);
	second_goal.m_vars = stack_pop_ptr(&context->m_exec_stack);

	/* Redo solve_goal(second_goal) */
	fn = stack_pop_ptr(&context->m_exec_stack);
	result = (*fn)(context);

	if (result == SOLVE_FAIL)
	{
redo:
		/* Redo solve_goal(first_goal) */
		fn = stack_pop_ptr(&context->m_exec_stack);
		
		clear_vars(second_goal.m_vars);
		
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
		if (stack_push_ptr(&context->m_exec_stack,second_goal.m_vars) == -1 ||
			stack_push_ptr(&context->m_exec_stack,second_goal.m_value) == -1 ||
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

	if (copy_term(context,orig_goal,&fresh_goal) != 0)
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
				if (stack_push_ptr(&context->m_exec_stack,second_goal.m_vars) == -1 ||
					stack_push_ptr(&context->m_exec_stack,second_goal.m_value) == -1 ||
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
	}

	return result;
}

static enum eSolveResult redo_or(struct context_t* context)
{
	struct term_t or_goal;
	solve_fn_t* fn;
	enum eSolveResult result;

	or_goal.m_value = stack_pop_ptr(&context->m_exec_stack);
	or_goal.m_vars = stack_pop_ptr(&context->m_exec_stack);

	/* Redo solve_goal(first_goal) */
	fn = stack_pop_ptr(&context->m_exec_stack);
	result = (*fn)(context);
	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,or_goal.m_vars) == -1 ||
			stack_push_ptr(&context->m_exec_stack,or_goal.m_value) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_or) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}
	else if (result == SOLVE_FAIL)
	{
		if (copy_term(context,&or_goal,&or_goal) != 0)
			result = SOLVE_NOMEM;
		else
			result = solve_goal(context,&or_goal);
	}

	return result;
}

static enum eSolveResult solve_or(struct context_t* context, struct term_t* orig_goal)
{
	enum eSolveResult result;
	struct term_t either_goal,or_goal;

	or_goal.m_vars = either_goal.m_vars = orig_goal->m_vars;
	either_goal.m_value = orig_goal->m_value + 1;
	or_goal.m_value = next_value(either_goal.m_value);

	if (copy_term(context,&either_goal,&either_goal) != 0)
		result = SOLVE_NOMEM;
	else
	{
		result = solve_goal(context,&either_goal);
		if (result == SOLVE_TRUE)
		{
			if (stack_push_ptr(&context->m_exec_stack,or_goal.m_vars) == -1 ||
				stack_push_ptr(&context->m_exec_stack,or_goal.m_value) == -1 ||
				stack_push_ptr(&context->m_exec_stack,&redo_or) == -1)
			{
				result = SOLVE_NOMEM;
			}
		}
		else if (result == SOLVE_FAIL)
		{
			if (copy_term(context,&or_goal,&or_goal) != 0)
				result = SOLVE_NOMEM;
			else
				result = solve_goal(context,&or_goal);
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
	if (copy_term(context,orig_goal,&fresh_goal) != 0)
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

static enum eSolveResult solve_goal(struct context_t* context, struct term_t* goal)
{
	switch (goal->m_value->m_uval & BOX_TAG_MASK)
	{
	case BOX_COMPOUND_EMBED_1(2,','):
		return solve_and(context,goal);

	case BOX_COMPOUND_EMBED_1(2,';'):
		if ((goal->m_value->m_uval & BOX_TAG_MASK) == BOX_COMPOUND_EMBED_2(2,'-','>'))
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

	default:
		break;
	}

	/* TODO: Check for builtins */

	/* Emit user defined */

	return SOLVE_FAIL;
}

static enum eSolveResult solve_start(struct context_t* context)
{
	struct term_t goal;
	goal.m_value = stack_pop_ptr(&context->m_exec_stack);
	goal.m_vars = stack_pop_ptr(&context->m_exec_stack);
	return solve_goal(context,&goal);
}

int interpreter_setup_stack(struct context_t* context, struct term_t* goal)
{
	if (stack_push_ptr(&context->m_exec_stack,goal->m_vars) == -1 ||
		stack_push_ptr(&context->m_exec_stack,goal->m_value) == -1 ||
		stack_push_ptr(&context->m_exec_stack,&solve_start) == -1)
	{
		return -1;
	}

	return 0;
}
