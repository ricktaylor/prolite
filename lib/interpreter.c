

#include "types.h"
#include "builtin_functions.h"

#include <assert.h>

static enum eSolveResult solve(struct context_t* context, struct term_t* goal);
enum eSolveResult redo(struct context_t* context);

union box_t* next_arg(union box_t* v);

union box_t* first_arg(union box_t* v)
{
	int extra_functor = 0;

	assert(UNBOX_TYPE(v->m_u64val) == prolite_compound);

	if ((UNBOX_HI16(v->m_u64val) & 0xC000) == 0)
		extra_functor = 1;

	// TODO: debug info
	//if (v->m_u64val < 0)

	++v;

	if (extra_functor)
	{
		// Skip functor atom
		v = next_arg(v);
	}
	return v;
}

union box_t* next_arg(union box_t* v)
{
	enum tag_type_t type = UNBOX_TYPE(v->m_u64val);
	if (type == prolite_compound)
	{
		uint64_t all48 = UNBOX_MANT_48(v->m_u64val);
		uint64_t arity;
		unsigned int hi16 = (all48 >> 32);
		if (hi16 & 0x8000)
			arity = (hi16 & (MAX_ARITY_EMBED << 11)) >> 11;
		else if ((hi16 & 0xC000) == 0x4000)
			arity = (hi16 & MAX_ARITY_BUILTIN);
		else
			arity = all48 & MAX_ARITY;

		v = first_arg(v);
		while (--arity)
			v = next_arg(v);
	}
	else
	{
		// TODO: debug info
		//if (v->m_u64val < 0)

		++v;
	}
	return v;
}

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


void context_reset(struct context_t* context, size_t pos)
{
	stack_reset(&context->m_exec_stack,pos);
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

static inline enum eSolveResult inline_solve_fail(struct context_t* context, struct term_t* goal)
{
	return SOLVE_FAIL;
}

static enum eSolveResult redo_true(struct context_t* context)
{
	enum eSolveResult result = SOLVE_FAIL;
	if (context->m_cont)
	{
		// This is so we can cleanup our stack by adding a halt/0 continuation after a true success
		struct continuation_t cont = *context->m_cont;
		context->m_cont = cont.m_next;

		result = solve(context,&cont.m_goal);
	}
	return result;
}

static inline enum eSolveResult inline_solve_true(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result = SOLVE_TRUE;
	if (context->m_cont)
	{
		struct continuation_t cont = *context->m_cont;
		context->m_cont = cont.m_next;

		result = solve(context,&cont.m_goal);
	}
	else if (stack_push_ptr(&context->m_exec_stack,&redo_true) == -1)
		result = SOLVE_NOMEM;

	return result;
}

static inline enum eSolveResult inline_solve_and(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result;
	struct term_t fresh_goal;

	if (copy_term(context,goal,&fresh_goal) != 0)
		result = SOLVE_NOMEM;
	else
	{
		struct continuation_t cont;

		fresh_goal.m_value = first_arg(fresh_goal.m_value);

		cont.m_goal.m_vars = fresh_goal.m_vars;
		cont.m_goal.m_value = next_arg(fresh_goal.m_value);
		cont.m_next = context->m_cont;
		context->m_cont = &cont;

		result = solve(context,&fresh_goal);
	}

	return result;
}

static enum eSolveResult redo_or(struct context_t* context)
{
	enum eSolveResult result;
	struct term_t or_goal;

	stack_pop_term(context,&or_goal);

	result = redo(context);
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
		if (copy_term(context,&or_goal,&or_goal) != 0)
			result = SOLVE_NOMEM;
		else
			result = solve(context,&or_goal);
	}

	return result;
}

static inline enum eSolveResult inline_solve_or(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result;
	struct term_t either_goal,or_goal;
	size_t stack_base = stack_top(context->m_exec_stack);

	or_goal.m_vars = either_goal.m_vars = goal->m_vars;
	either_goal.m_value = first_arg(goal->m_value);
	or_goal.m_value = next_arg(either_goal.m_value);

	if (copy_term(context,&either_goal,&either_goal) != 0)
		result = SOLVE_NOMEM;
	else
	{
		result = solve(context,&either_goal);
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

			clear_vars(goal->m_vars);

			if (copy_term(context,&or_goal,&or_goal) != 0)
				result = SOLVE_NOMEM;
			else
				result = solve(context,&or_goal);
		}
	}

	return result;
}

static enum eSolveResult redo_cut(struct context_t* context)
{
	enum eSolveResult result = SOLVE_CUT;

	// TODO: Cleanup here?? Fiddle with context->m_cont?

	if (context->m_cont)
	{
		// This is so we can cleanup our stack by adding a halt/0 continuation after a true success
		struct continuation_t cont = *context->m_cont;
		context->m_cont = cont.m_next;

		result = solve(context,&cont.m_goal);
	}

	return result;
}

static inline enum eSolveResult inline_solve_cut(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result = SOLVE_TRUE;
	if (context->m_cont)
	{
		struct continuation_t cont = *context->m_cont;
		context->m_cont = cont.m_next;

		result = solve(context,&cont.m_goal);
	}
	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,&redo_cut) == -1)
			result = SOLVE_NOMEM;
	}
	return result;
}

static enum eSolveResult redo_call(struct context_t* context)
{
	enum eSolveResult result = redo(context);
	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,&redo_call) == -1)
			result = SOLVE_NOMEM;
	}
	else if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	return result;
}

enum eSolveResult call(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result;
	struct term_t fresh_goal;
	size_t stack_base = stack_top(context->m_exec_stack);

	if (!context->m_module->m_flags.debug)
	{
		// Optimize call(call(_)) -> call(_)
		while (goal->m_value->m_u64val == BOX_COMPOUND_EMBED_4(1,'c','a','l','l'))
			goal->m_value = first_arg(goal->m_value);
	}

	if (copy_term(context,goal,&fresh_goal) != 0)
		result = SOLVE_NOMEM;
	else
	{
		result = solve(context,&fresh_goal);
		if (result == SOLVE_TRUE)
		{
			if (stack_push_ptr(&context->m_exec_stack,&redo_call) == -1)
				result = SOLVE_NOMEM;
		}
		else if (result == SOLVE_CUT)
			result = SOLVE_FAIL;
		else if (result == SOLVE_NOT_CALLABLE)
		{
			assert(0);
			// TODO: throw callable term fresh_goal
		}
	}

	return result;
}

static inline enum eSolveResult inline_solve_call(struct context_t* context, struct term_t* goal)
{
	goal->m_value = first_arg(goal->m_value);
	return call(context,goal);
}

static enum eSolveResult throw(struct context_t* context, union box_t* ball)
{
	// TODO: Copy ball onto scratch stack...

	if (stack_push(&context->m_scratch_stack,ball->m_u64val) == -1)
	{
		return SOLVE_NOMEM;
	}

	return SOLVE_THROW;
}

static inline enum eSolveResult inline_solve_throw(struct context_t* context, struct term_t* goal)
{
	return throw(context,first_arg(goal->m_value));
}

static enum eSolveResult redo_repeat(struct context_t* context)
{
	enum eSolveResult result = redo(context);
	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,&redo_repeat) == -1)
			result = SOLVE_NOMEM;
	}
	return result;
}

static inline enum eSolveResult inline_solve_repeat(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result = inline_solve_true(context,NULL);
	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,&redo_repeat) == -1)
			result = SOLVE_NOMEM;
	}
	return result;
}

static enum eSolveResult catch(struct context_t* context, enum eSolveResult result, struct term_t* catcher, struct term_t* recovery, size_t scratch_base)
{
	union box_t* ball = NULL;

	if (result == SOLVE_THROW)
	{
		// TODO : Pop ball from scratch stack to exec_stack
	}

	if (result == SOLVE_NOMEM)
	{
		// TODO: Try to push 'resource_error' here
	}

	clear_vars(catcher->m_vars);

	// Unify ball and recovery
	assert(0);

	if (0)
		result = throw(context,ball);
	else
		result = call(context,recovery);

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

	result = redo(context);
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
		result = catch(context,result,&catcher,&recovery,scratch_base);

	return result;
}

enum eSolveResult solve_catch(struct context_t* context, struct term_t* goal)
{
	struct term_t fresh_goal,catcher,recovery;
	size_t scratch_base = stack_top(context->m_scratch_stack);

	fresh_goal.m_vars = catcher.m_vars = recovery.m_vars = goal->m_vars;
	fresh_goal.m_value = first_arg(goal->m_value);
	catcher.m_value = next_arg(fresh_goal.m_value);
	recovery.m_value = next_arg(catcher.m_value);

	// TODO: Hold continuation...

	enum eSolveResult result = call(context,&fresh_goal);
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
		result = catch(context,result,&catcher,&recovery,scratch_base);

	return result;
}

static inline enum eSolveResult inline_solve_if_then_else(struct context_t* context, struct term_t* goal)
{
	// TODO
	assert(0);
}

static inline enum eSolveResult inline_solve_if_then(struct context_t* context, struct term_t* goal)
{
	// TODO
	assert(0);
}

enum eSolveResult solve_not_proveable(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result;
	size_t stack_base = stack_top(context->m_exec_stack);

	goal->m_value = first_arg(goal->m_value);

	// TODO: Hold continuation...

	result = call(context,goal);
	if (result == SOLVE_TRUE)
	{
		// TODO: Cleanup exec stack??

		result = SOLVE_FAIL;
	}
	else if (result == SOLVE_FAIL)
	{
		context_reset(context,stack_base);

		clear_vars(goal->m_vars);

		result = inline_solve_true(context,NULL);
	}

	return result;
}

static enum eSolveResult redo_once(struct context_t* context)
{
	// TODO: Clean up redo stack

	return SOLVE_FAIL;
}

static inline enum eSolveResult inline_solve_once(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result;
	size_t stack_base = stack_top(context->m_exec_stack);

	goal->m_value = first_arg(goal->m_value);

	// TODO: Hold continuation...

	result = call(context,goal);
	if (result == SOLVE_TRUE)
	{
		context_reset(context,stack_base);

		if (stack_push_ptr(&context->m_exec_stack,&redo_once) == -1)
			result = SOLVE_NOMEM;
	}

	return result;
}

static inline enum eSolveResult inline_solve_halt(struct context_t* context, struct term_t* goal)
{
	if (goal->m_value->m_u64val == BOX_COMPOUND_EMBED_4(1,'h','a','l','t'))
	{
		struct term_t fresh_goal;
		goal->m_value = first_arg(goal->m_value);
		if (copy_term(context,goal,&fresh_goal) != 0)
			return SOLVE_NOMEM;

		// TODO: Check fresh_goal.m_value

		if (stack_push(&context->m_scratch_stack,fresh_goal.m_value->m_u64val) == -1)
			return SOLVE_NOMEM;
	}
	else
	{
		// Push 0 as the exit_code
		if (stack_push(&context->m_scratch_stack,BOX_TYPE(prolite_int32)) == -1)
			return SOLVE_NOMEM;
	}

	return SOLVE_HALT;
}

static enum eSolveResult solve_user_defined(struct context_t* context, struct term_t* goal)
{
	assert(0);
	return SOLVE_FAIL;
}

static enum eSolveResult solve(struct context_t* context, struct term_t* goal)
{
	enum eSolveResult result;

	// TODO: tracepoint *call*

	switch (goal->m_value->m_u64val)
	{

#undef DECLARE_BUILTIN_CONTROL
#define DECLARE_BUILTIN_CONTROL(f,n) \
	case (n): result = inline_solve_##f(context,goal); break;

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,n) \
	case (n): result = solve_##f(context,goal); break;

#include "builtin_functions.h"

	default:
		switch (UNBOX_TYPE(goal->m_value->m_u64val))
		{
		case prolite_var:
			result = throw_instantiation_error(context,goal->m_value);
			break;

		case prolite_compound:
		case prolite_atom:
			result = solve_user_defined(context,goal);
			break;

		default:
			result = SOLVE_NOT_CALLABLE;
			break;
		}
		break;
	}

	// TODO: tracepoint *exit*/*throw*/*fail*

	return result;
}

enum eSolveResult redo(struct context_t* context)
{
	enum eSolveResult result;
	solve_fn_t fn = stack_pop_ptr(&context->m_exec_stack);

	// TODO: tracepoint *redo*

	result = (*fn)(context);

	// TODO: tracepoint *exit*/*throw*/*fail*

	return result;
}
