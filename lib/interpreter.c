

#include "types.h"

#include <assert.h>
#include <math.h>

enum eSolveResult solve(struct context_t* context, union box_t* goal);
enum eSolveResult redo(struct context_t* context, int unwind);

union box_t* next_arg(union box_t* v);

union box_t* first_arg(union box_t* v)
{
	int debug = (UNBOX_EXP_16(v->m_u64val) & 0x8000);

	assert(UNBOX_TYPE(v->m_u64val) == prolite_compound);

	// Skip functor atom
	if ((UNBOX_HI16(v->m_u64val) & 0xC000) == 0)
		++v;

	++v;

	if (debug)
	{
		// TODO: Debug info
		++v;
	}

	return v;
}

static inline uint64_t get_arity(uint64_t c)
{
	uint64_t all48 = UNBOX_MANT_48(c);
	unsigned int hi16 = (all48 >> 32);
	if (hi16 & 0x8000)
		return (hi16 & (MAX_ARITY_EMBED << 11)) >> 11;

	if ((hi16 & 0xC000) == 0x4000)
		return (hi16 & MAX_ARITY_BUILTIN);

	return all48 & MAX_ARITY;
}

union box_t* next_arg(union box_t* v)
{
	int debug = 0;
	switch (UNBOX_TYPE(v->m_u64val))
	{
	case prolite_compound:
		{
			uint64_t arity = get_arity(v->m_u64val);

			v = first_arg(v);
			while (arity--)
				v = next_arg(v);
		}
		return v;

	case prolite_int32:
	case prolite_atom:
	case prolite_var:
	case prolite_chars:
	case prolite_charcodes:
		debug = (UNBOX_EXP_16(v->m_u64val) & 0x8000);
		++v;
		break;

	default:
		// prolite_double
		++v;

		// TODO: Check v->m_u64val for some kind of debug_info magic marker?
		break;
	}

	if (debug)
	{
		// TODO: Debug info
		++v;
	}
	return v;
}

static inline enum eSolveResult inline_solve_fail(struct context_t* context, union box_t* goal)
{
	return SOLVE_FAIL;
}

static enum eSolveResult redo_true(struct context_t* context, int unwind)
{
	return unwind ? SOLVE_UNWIND : SOLVE_FAIL;
}

static inline enum eSolveResult inline_solve_true(struct context_t* context, union box_t* goal)
{
	return stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
}

static enum eSolveResult redo_and(struct context_t* context, int unwind)
{
	enum eSolveResult result;
	union box_t* cont_goal = stack_pop_ptr(&context->m_exec_stack);

	result = redo(context,unwind);
	if (result == SOLVE_FAIL)
	{
		result = redo(context,unwind);
		if (result == SOLVE_TRUE)
			result = solve(context,cont_goal);
	}
	else if (result != SOLVE_TRUE)
	{
		// Unwind the first goal on cut/halt/error etc
		redo(context,1);
	}

	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,cont_goal) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_and) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static inline enum eSolveResult inline_solve_and(struct context_t* context, union box_t* goal)
{
	enum eSolveResult result;

	goal = first_arg(goal);
	result = solve(context,goal);
	if (result == SOLVE_TRUE)
	{
		goal = next_arg(goal);

again:
		result = solve(context,goal);
		if (result == SOLVE_TRUE)
		{
			if (stack_push_ptr(&context->m_exec_stack,goal) == -1 ||
				stack_push_ptr(&context->m_exec_stack,&redo_and) == -1)
			{
				result = SOLVE_NOMEM;
			}
		}
		else if (result == SOLVE_FAIL)
		{
			result = redo(context,0);
			if (result == SOLVE_TRUE)
				goto again;
		}
	}

	return result;
}

static enum eSolveResult redo_or(struct context_t* context, int unwind)
{
	enum eSolveResult result;
	union box_t* or_goal = stack_pop_ptr(&context->m_exec_stack);

	result = redo(context,unwind);
	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,or_goal) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_or) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}
	else if (result == SOLVE_FAIL)
		result = solve(context,or_goal);

	return result;
}

static inline enum eSolveResult inline_solve_or(struct context_t* context, union box_t* goal)
{
	enum eSolveResult result;
	union box_t* or_goal;

	goal = first_arg(goal);
	or_goal = next_arg(goal);

	result = solve(context,goal);
	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,or_goal) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_or) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}
	else if (result == SOLVE_FAIL)
		result = solve(context,or_goal);

	return result;
}

static enum eSolveResult redo_cut(struct context_t* context, int unwind)
{
	return unwind ? SOLVE_UNWIND : SOLVE_CUT;
}

static inline enum eSolveResult inline_solve_cut(struct context_t* context, union box_t* goal)
{
	return stack_push_ptr(&context->m_exec_stack,&redo_cut) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
}

static enum eSolveResult redo_call(struct context_t* context, int unwind)
{
	size_t stack_base = stack_pop(&context->m_exec_stack);

	enum eSolveResult result = redo(context,unwind);
	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_exec_stack,stack_base) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_call) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}
	else if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result != SOLVE_TRUE)
		stack_reset(&context->m_exec_stack,stack_base);

	return result;
}

static enum eSolveResult clone_term(struct context_t* context, struct stack_t** stack, union box_t** v)
{
	enum eSolveResult result = SOLVE_TRUE;
	int debug = (UNBOX_EXP_16((*v)->m_u64val) & 0x8000);
	switch (UNBOX_TYPE((*v)->m_u64val))
	{
	case prolite_compound:
		{
			uint64_t arity = get_arity((*v)->m_u64val);

			if ((UNBOX_HI16((*v)->m_u64val) & 0xC000) == 0)
			{
				// Copy functor atom
				if (stack_push(stack,(*v)->m_u64val) == -1)
					return SOLVE_NOMEM;
				++(*v);
			}

			if (stack_push(stack,(*v)->m_u64val) == -1)
				return SOLVE_NOMEM;
			++(*v);

			if (debug)
			{
				// TODO: Debug info
				if (stack_push(stack,(*v)->m_u64val) == -1)
					return SOLVE_NOMEM;
				++(*v);
			}

			while (result == SOLVE_TRUE && arity--)
				result = clone_term(context,stack,v);
		}
		return result;

	case prolite_var:
		{
			uint64_t var_idx = UNBOX_MANT_48((*v)->m_u64val);

			assert(context->m_substs && var_idx < context->m_substs->m_count);

			if (context->m_substs->m_values[var_idx])
			{
				*v = context->m_substs->m_values[var_idx];
				return clone_term(context,stack,v);
			}

			if (stack_push(stack,(*v)->m_u64val) == -1)
				return SOLVE_NOMEM;
			++(*v);
		}
		break;

	case prolite_int32:
	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		if (stack_push(stack,(*v)->m_u64val) == -1)
			return SOLVE_NOMEM;
		++(*v);
		break;

	default:
		// prolite_double
		if (stack_push(stack,(*v)->m_u64val) == -1)
			return SOLVE_NOMEM;
		++(*v);

		// TODO: Check (*v)->m_u64val for some kind of debug_info magic marker?
		debug = 0;
		break;
	}

	if (debug)
	{
		// TODO: Debug info
		if (stack_push(stack,(*v)->m_u64val) == -1)
			return SOLVE_NOMEM;
		++(*v);
	}

	return result;
}

static enum eSolveResult term_to_goal_r(struct context_t* context, union box_t** v)
{
	enum eSolveResult result = SOLVE_TRUE;
	switch (UNBOX_TYPE((*v)->m_u64val))
	{
	case prolite_compound:
		{
			int debug = (UNBOX_EXP_16((*v)->m_u64val) & 0x8000);
			uint64_t arity = get_arity((*v)->m_u64val);
			int control = 0;

			if ((*v)->m_u64val == BOX_COMPOUND_EMBED_1(2,',') ||
				(*v)->m_u64val == BOX_COMPOUND_EMBED_1(2,';') ||
				(*v)->m_u64val == BOX_COMPOUND_EMBED_2(2,'-','>'))
			{
				control = 1;
			}

			if ((UNBOX_HI16((*v)->m_u64val) & 0xC000) == 0)
			{
				// Copy functor atom
				if (stack_push(&context->m_exec_stack,(*v)->m_u64val) == -1)
					return SOLVE_NOMEM;
				++(*v);
			}

			if (stack_push(&context->m_exec_stack,(*v)->m_u64val) == -1)
				return SOLVE_NOMEM;
			++(*v);

			if (debug)
			{
				// TODO: Debug info
				if (stack_push(&context->m_exec_stack,(*v)->m_u64val) == -1)
					return SOLVE_NOMEM;
				++(*v);
			}

			if (control)
			{
				while (result == SOLVE_TRUE && arity--)
					result = term_to_goal_r(context,v);
			}
			else
			{
				while (result == SOLVE_TRUE && arity--)
					result = clone_term(context,&context->m_exec_stack,v);
			}
		}
		return result;

	case prolite_var:
		{
			uint64_t var_idx = UNBOX_MANT_48((*v)->m_u64val);

			assert(context->m_substs && var_idx < context->m_substs->m_count);

			if (context->m_substs->m_values[var_idx])
			{
				*v = context->m_substs->m_values[var_idx];
				return clone_term(context,&context->m_exec_stack,v);
			}

			// Convert V -> call(V)
			if (stack_push(&context->m_exec_stack,BOX_COMPOUND_EMBED_4(1,'c','a','l','l')) == -1)
				return SOLVE_NOMEM;
		}
		return clone_term(context,&context->m_exec_stack,v);

	case prolite_atom:
		return clone_term(context,&context->m_exec_stack,v);

	default:
		return SOLVE_FAIL;
	}
}

enum eSolveResult term_to_goal(struct context_t* context, union box_t* src, union box_t** dest)
{
	size_t top = stack_top(context->m_exec_stack);
	enum eSolveResult result = term_to_goal_r(context,&src);
	if (result == SOLVE_TRUE)
		*dest = stack_at(context->m_exec_stack,top);
	else
		stack_reset(&context->m_exec_stack,top);

	return result;
}

static enum eSolveResult call(struct context_t* context, union box_t* goal)
{
	enum eSolveResult result;
	union box_t* fresh_goal;
	size_t stack_base = stack_top(context->m_exec_stack);

	result = term_to_goal(context,goal,&fresh_goal);
	if (result == SOLVE_FAIL)
		result = throw_type_error(context,BOX_ATOM_BUILTIN(callable),goal);
	else if (result == SOLVE_TRUE)
	{
		if (!context->m_module->m_flags.debug)
		{
			// Optimize call(call(_)) -> call(_)
			while (fresh_goal->m_u64val == BOX_COMPOUND_EMBED_4(1,'c','a','l','l'))
				fresh_goal = first_arg(fresh_goal);
		}

		result = solve(context,fresh_goal);
		if (result == SOLVE_TRUE)
		{
			if (stack_push(&context->m_exec_stack,stack_base) == -1 ||
				stack_push_ptr(&context->m_exec_stack,&redo_call) == -1)
			{
				result = SOLVE_NOMEM;
			}
		}
		else if (result == SOLVE_CUT)
			result = SOLVE_FAIL;

		if (result != SOLVE_TRUE)
			stack_reset(&context->m_exec_stack,stack_base);
	}
	return result;
}

static inline enum eSolveResult inline_solve_call(struct context_t* context, union box_t* goal)
{
	return call(context,first_arg(goal));
}

static inline enum eSolveResult inline_solve_throw(struct context_t* context, union box_t* goal)
{
	enum eSolveResult result;

	goal = first_arg(goal);

	if (UNBOX_TYPE(goal->m_u64val) == prolite_var)
		return throw_instantiation_error(context,goal);

	stack_reset(&context->m_scratch_stack,0);

	result = clone_term(context,&context->m_scratch_stack,&goal);
	if (result == SOLVE_TRUE)
		result = SOLVE_THROW;

	return result;
}

static enum eSolveResult redo_repeat(struct context_t* context, int unwind)
{
	if (unwind)
		return SOLVE_UNWIND;

	return stack_push_ptr(&context->m_exec_stack,&redo_repeat) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
}

static inline enum eSolveResult inline_solve_repeat(struct context_t* context, union box_t* goal)
{
	return stack_push_ptr(&context->m_exec_stack,&redo_repeat) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
}

static enum eSolveResult catch(struct context_t* context, enum eSolveResult result, union box_t* catcher, union box_t* recovery)
{
	if (result == SOLVE_NOMEM)
	{
		if (catcher->m_u64val != BOX_COMPOUND_BUILTIN(resource_error,1))
			return SOLVE_NOMEM;

		catcher = first_arg(catcher);
		if (UNBOX_TYPE(catcher->m_u64val) == prolite_var)
		{
			// TODO: Unify catcher with 'memory' here
			assert(0);
		}
		else if (catcher->m_u64val != BOX_ATOM_BUILTIN(memory))
			return SOLVE_NOMEM;
	}
	else
	{
		union box_t* ball = stack_at(context->m_scratch_stack,0);
		int unified = 0;

		// Unify ball and catcher
		assert(0);

		if (!unified)
			return SOLVE_THROW;
	}

	return call(context,recovery);
}

static enum eSolveResult redo_catch_goal(struct context_t* context, int unwind)
{
	enum eSolveResult result;

	union box_t* recovery = stack_pop_ptr(&context->m_exec_stack);
	union box_t* catcher = stack_pop_ptr(&context->m_exec_stack);

	result = redo(context,unwind);
	if (result == SOLVE_NOMEM || result == SOLVE_THROW)
		result = catch(context,result,catcher,recovery);
	else if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,catcher) == -1 ||
			stack_push_ptr(&context->m_exec_stack,recovery) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_catch_goal) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

enum eSolveResult solve_catch(struct context_t* context, union box_t* goal)
{
	union box_t *catcher, *recovery;

	goal = first_arg(goal);
	catcher = next_arg(goal);
	recovery = next_arg(catcher);

	enum eSolveResult result = call(context,goal);
	if (result == SOLVE_NOMEM || result == SOLVE_THROW)
		result = catch(context,result,catcher,recovery);
	else if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,catcher) == -1 ||
			stack_push_ptr(&context->m_exec_stack,recovery) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_catch_goal) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static inline enum eSolveResult inline_solve_if_then_else(struct context_t* context, union box_t* goal)
{
	// TODO
	assert(0);
}

static inline enum eSolveResult inline_solve_if_then(struct context_t* context, union box_t* goal)
{
	// TODO
	assert(0);
}

enum eSolveResult solve_not_proveable(struct context_t* context, union box_t* goal)
{
	enum eSolveResult result = call(context,first_arg(goal));
	if (result == SOLVE_TRUE)
	{
		result = redo(context,1);
		if (result == SOLVE_TRUE)
			result = SOLVE_FAIL;
	}
	else if (result == SOLVE_FAIL)
		result = SOLVE_TRUE;

	return result;
}

enum eSolveResult solve_once(struct context_t* context, union box_t* goal)
{
	enum eSolveResult result;

	goal = first_arg(goal);

	if (!context->m_module->m_flags.debug)
	{
		// Optimize once(once(_)) -> once(_)
		while (goal->m_u64val == BOX_COMPOUND_EMBED_4(1,'o','n','c','e'))
			goal = first_arg(goal);
	}

	result = call(context,goal);
	if (result == SOLVE_TRUE)
		result = redo(context,1);

	return result;
}

static inline enum eSolveResult inline_solve_halt(struct context_t* context, union box_t* goal)
{
	// halt/0
	return SOLVE_HALT;
}

enum eSolveResult solve_halt(struct context_t* context, union box_t* goal)
{
	// halt/1
	goal = first_arg(goal);

	if (UNBOX_TYPE(goal->m_u64val) == prolite_var)
		return throw_instantiation_error(context,goal);
	else if (UNBOX_TYPE(goal->m_u64val) != prolite_int32)
		return throw_type_error(context,BOX_ATOM_BUILTIN(integer),goal);

	return stack_push(&context->m_scratch_stack,goal->m_u64val) == -1 ? SOLVE_NOMEM : SOLVE_HALT;
}

static enum eSolveResult solve_user_defined(struct context_t* context, union box_t* goal)
{
	if (context->m_module->m_flags.unknown == 0)
		return throw_existence_error(context,BOX_ATOM_BUILTIN(procedure),goal);

	return SOLVE_FAIL;
}

#define DECLARE_BUILTIN_CONTROL(f,n)

#define DECLARE_BUILTIN_FUNCTION(f,n) \
	enum eSolveResult solve_##f(struct context_t* context, union box_t* goal);

#include "builtin_functions.h"

enum eSolveResult solve(struct context_t* context, union box_t* goal)
{
	enum eSolveResult result;

	// TODO: tracepoint *call*

	switch (goal->m_u64val)
	{

#undef DECLARE_BUILTIN_CONTROL
#define DECLARE_BUILTIN_CONTROL(f,n) \
	case (n): result = inline_solve_##f(context,goal); break;

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,n) \
	case (n): result = solve_##f(context,goal); break;

#include "builtin_functions.h"

	default:
		switch (UNBOX_TYPE(goal->m_u64val))
		{
		case prolite_var:
			result = throw_instantiation_error(context,goal);
			break;

		case prolite_compound:
		case prolite_atom:
			result = solve_user_defined(context,goal);
			break;

		default:
			result = throw_type_error(context,BOX_ATOM_BUILTIN(callable),goal);
			break;
		}
		break;
	}

	// TODO: tracepoint *exit*/*throw*/*fail*

	return result;
}

enum eSolveResult redo(struct context_t* context, int unwind)
{
	enum eSolveResult result;
	redo_fn_t fn = stack_pop_ptr(&context->m_exec_stack);

	// TODO: if (!unwind) tracepoint *redo*

	result = (*fn)(context,unwind);

	// TODO: if (!unwind) tracepoint *exit*/*throw*/*fail*

	return result;
}
