

#include "context.h"

#include <assert.h>

static enum eSolveResult solve_goal(struct context_t* context, struct term_t* goal);

static int copy_term(struct context_t* context, struct term_t* src, struct term_t* dest)
{
	if (!src->m_vars || src->m_vars->m_count == 0)
	{
		*dest = *src;
		return 0;
	}
	// TODO: Walk src, copying vars into scratch, copying body into exec

	// Copy vars into exec

	assert(0);

	return -1;
}

static void clear_vars(struct var_info_t* vars)
{
	if (vars && vars->m_count)
	{
		assert(0);
	}
}



static inline int stack_push_term(struct context_t* context, const struct term_t* t)
{
	if (stack_push_ptr(&context->m_exec_stack,t->m_vars) == -1 ||
		stack_push_ptr(&context->m_exec_stack,t->m_value) == -1)
	{
		return -1;
	}

	return 0;
}

static inline void stack_pop_term(struct context_t* context, struct term_t* t)
{
	t->m_value = stack_pop_ptr(&context->m_exec_stack);
	t->m_vars = stack_pop_ptr(&context->m_exec_stack);
}

enum eSolveResult redo_goal(struct context_t* context)
{
	solve_fn_t fn = stack_pop_ptr(&context->m_exec_stack);
	return (*fn)(context);
}

static enum eSolveResult redo_and(struct context_t* context)
{
	enum eSolveResult result;
	struct term_t second_goal;
	size_t stack_base = stack_pop(&context->m_exec_stack);
	stack_pop_term(context,&second_goal);

	result = redo_goal(context);
	if (result == SOLVE_FAIL)
	{
redo:
		context_reset(context,stack_base);

		clear_vars(second_goal.m_vars);

		result = redo_goal(context);
		if (result == SOLVE_TRUE)
		{
			stack_base = stack_top(context->m_exec_stack);

			result = solve_goal(context,&second_goal);
			if (result == SOLVE_FAIL)
				goto redo;
		}
	}

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_exec_stack,stack_base) == -1 ||
			stack_push_term(context,&second_goal) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_and) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static inline enum eSolveResult solve_and(struct context_t* context, struct term_t* orig_goal)
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
			size_t stack_base;
			struct term_t second_goal;
			second_goal.m_vars = fresh_goal.m_vars;
			second_goal.m_value = next_value(first_goal.m_value);
redo:
			stack_base = stack_top(context->m_exec_stack);
			result = solve_goal(context,&second_goal);
			if (result == SOLVE_TRUE)
			{
				if (stack_push(&context->m_exec_stack,stack_base) == -1 ||
					stack_push_term(context,&second_goal) == -1 ||
					stack_push_ptr(&context->m_exec_stack,&redo_and) == -1)
				{
					result = SOLVE_NOMEM;
				}
			}
			else if (result == SOLVE_FAIL)
			{
				context_reset(context,stack_base);

				clear_vars(fresh_goal.m_vars);

				result = redo_goal(context);
				if (result == SOLVE_TRUE)
					goto redo;
			}
		}
	}

	return result;
}

static enum eSolveResult redo_or(struct context_t* context)
{
	enum eSolveResult result;
	struct term_t or_goal;

	stack_pop_term(context,&or_goal);

	result = redo_goal(context);
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

		if (copy_term(context,&or_goal,&new_or_goal) != 0)
			result = SOLVE_NOMEM;
		else
			result = solve_goal(context,&new_or_goal);
	}

	return result;
}

static inline enum eSolveResult solve_or(struct context_t* context, struct term_t* orig_goal)
{
	enum eSolveResult result;
	struct term_t either_goal,or_goal;
	size_t stack_base = stack_top(context->m_exec_stack);

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
			if (stack_push_term(context,&or_goal) == -1 ||
				stack_push_ptr(&context->m_exec_stack,&redo_or) == -1)
			{
				result = SOLVE_NOMEM;
			}
		}
		else if (result == SOLVE_FAIL)
		{
			context_reset(context,stack_base);

			clear_vars(orig_goal->m_vars);

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

static inline enum eSolveResult solve_cut(struct context_t* context)
{
	if (stack_push_ptr(&context->m_exec_stack,&redo_cut) == -1)
		return SOLVE_NOMEM;

	return SOLVE_TRUE;
}

static enum eSolveResult redo_call(struct context_t* context)
{
	enum eSolveResult result = redo_goal(context);
	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,&redo_call) == -1)
			result = SOLVE_NOMEM;
	}
	else if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	return result;
}

enum eSolveResult solve_call(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result;
	struct term_t fresh_goal;

	if (copy_term(context,goal,&fresh_goal) != 0)
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

static inline enum eSolveResult solve_if_then_else(struct context_t* context, struct term_t* orig_goal)
{
	// TODO
	assert(0);
}

static inline enum eSolveResult solve_if_then(struct context_t* context, struct term_t* orig_goal)
{
	// TODO
	assert(0);
}

static enum eSolveResult solve_throw(struct context_t* context, struct term_t* ball)
{
	// TODO: Copy ball onto scratch stack...

	if (stack_push_term(context,ball) == -1)
	{
		return SOLVE_NOMEM;
	}

	return SOLVE_THROW;
}

static enum eSolveResult do_catch(struct context_t* context, enum eSolveResult result, struct term_t* catcher, struct term_t* recovery, size_t scratch_base)
{
	struct term_t ball = {0};
	if (result == SOLVE_THROW)
	{
		// TODO : Pop ball from scratch stack to exec_stack

		// Reset the exec stack hard

		stack_reset(&context->m_scratch_stack,scratch_base);
	}

	if (result == SOLVE_NOMEM)
	{
		// Try to regain some space, and the stack is trashed beyond stack_base


		// TODO: Try to push 'resource_error' here
		assert(0);
	}

	clear_vars(catcher->m_vars);

	// Unify ball and recovery
	assert(0);

	if (0)
		result = solve_throw(context,&ball);
	else
		result = solve_call(context,recovery);

	return result;
}

static enum eSolveResult redo_catch_goal(struct context_t* context)
{
	enum eSolveResult result;
	struct term_t catcher,recovery;
	size_t scratch_base;

	stack_pop_term(context,&recovery);
	stack_pop_term(context,&catcher);
	scratch_base = stack_pop(&context->m_exec_stack);

	result = redo_goal(context);
	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,&redo_catch_goal) == -1)
			result = SOLVE_NOMEM;
	}

	if (result == SOLVE_NOMEM || result == SOLVE_THROW)
		result = do_catch(context,result,&catcher,&recovery,scratch_base);

	return result;
}

static inline enum eSolveResult solve_catch(struct context_t* context, struct term_t* orig_goal)
{
	struct term_t goal,catcher,recovery;
	size_t scratch_base = stack_top(context->m_scratch_stack);

	goal.m_vars = catcher.m_vars = recovery.m_vars = orig_goal->m_vars;
	goal.m_value = next_value(orig_goal->m_value);
	catcher.m_value = next_value(goal.m_value);
	recovery.m_value = next_value(recovery.m_value);

	enum eSolveResult result = solve_call(context,&goal);
	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_exec_stack,scratch_base) == -1 ||
			stack_push_term(context,&catcher) == -1 ||
			stack_push_term(context,&recovery) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_catch_goal) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	if (result == SOLVE_NOMEM || result == SOLVE_THROW)
		result = do_catch(context,result,&catcher,&recovery,scratch_base);

	return result;
}

static enum eSolveResult solve_fail(struct context_t* context)
{
	return SOLVE_FAIL;
}

static inline enum eSolveResult solve_true(struct context_t* context)
{
	if (stack_push_ptr(&context->m_exec_stack,&solve_fail) == -1)
		return SOLVE_NOMEM;

	return SOLVE_TRUE;
}

static inline enum eSolveResult solve_not_proveable(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result;

	goal->m_value = next_value(goal->m_value);
	result = solve_call(context,goal);
	if (result == SOLVE_TRUE)
		result = SOLVE_FAIL;
	else if (result == SOLVE_FAIL)
	{
		result = SOLVE_TRUE;

		if (stack_push_ptr(&context->m_exec_stack,&solve_fail) == -1)
			result = SOLVE_NOMEM;
	}

	return result;
}

static inline enum eSolveResult solve_once(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result;
	size_t stack_base = stack_top(context->m_exec_stack);

	goal->m_value = next_value(goal->m_value);

	result = solve_call(context,goal);
	if (result == SOLVE_TRUE)
	{
		context_reset(context,stack_base);

		if (stack_push_ptr(&context->m_exec_stack,&solve_fail) == -1)
			result = SOLVE_NOMEM;
	}

	return result;
}

static enum eSolveResult solve_repeat(struct context_t* context)
{
	if (stack_push_ptr(&context->m_exec_stack,&solve_repeat) == -1)
		return SOLVE_NOMEM;

	return SOLVE_TRUE;
}

static enum eSolveResult solve_halt(struct context_t* context, struct term_t* goal)
{
	if (goal->m_value->m_u64val == BOX_COMPOUND_EMBED_4(1,'h','a','l','t'))
	{
		goal->m_value = next_value(goal->m_value);

		// Check goal->m_value

		if (stack_push(&context->m_exec_stack,goal->m_value->m_u64val) == -1)
			return SOLVE_NOMEM;

	}
	else
	{
		// Push 0 as the exit_code
		if (stack_push(&context->m_exec_stack,BOX_TYPE(prolite_int32)) == -1)
			return SOLVE_NOMEM;
	}

	return SOLVE_HALT;
}

static enum eSolveResult solve_goal(struct context_t* context, struct term_t* goal)
{
	// Optimize popular and special expressions first
	switch (goal->m_value->m_u64val)
	{
	case BOX_COMPOUND_EMBED_1(2,','):
		return solve_and(context,goal);

	case BOX_COMPOUND_EMBED_1(2,';'):
		if ((goal->m_value[1].m_u64val) == BOX_COMPOUND_EMBED_2(2,'-','>'))
			return solve_if_then_else(context,goal);
		return solve_or(context,goal);

	case BOX_ATOM_EMBED_4('t','r','u','e'):
		return solve_true(context);

	case BOX_ATOM_EMBED_4('f','a','i','l'):
		return SOLVE_FAIL;

	case BOX_ATOM_EMBED_1('!'):
		return solve_cut(context);

	case BOX_COMPOUND_EMBED_4(1,'c','a','l','l'):
		goal->m_value = next_value(goal->m_value);
		return solve_call(context,goal);
	}

	// Giant switch for builtins
	switch (goal->m_value->m_u64val)
	{
	case BOX_COMPOUND_EMBED_2(2,'-','>'):
		return solve_if_then(context,goal);

	case BOX_COMPOUND_EMBED_5(3,'c','a','t','c','h'):
		return solve_catch(context,goal);

	case BOX_COMPOUND_EMBED_5(1,'t','h','r','o','w'):
		goal->m_value = next_value(goal->m_value);
		return solve_throw(context,goal);

	case BOX_ATOM_EMBED_4('h','a','l','t'):
	case BOX_COMPOUND_EMBED_4(1,'h','a','l','t'):
		return solve_halt(context,goal);

	case BOX_COMPOUND_EMBED_2(1,'\\','+'):
		return solve_not_proveable(context,goal);

	case BOX_ATOM_EMBED_4('o','n','c','e'):
		return solve_once(context,goal);

	case BOX_ATOM_BUILTIN(repeat):
		return solve_repeat(context);

	default:
		break;
	}

	/* TODO: Check for builtins */

	/* Emit user defined */

	return SOLVE_FAIL;
}
