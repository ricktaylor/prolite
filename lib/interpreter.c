

#include "types.h"

#include <assert.h>
#include <math.h>

enum eSolveResult solve(struct context_t* context, const union box_t* goal);
enum eSolveResult redo(struct context_t* context, int unwind);

const union box_t* next_arg(const union box_t* v);

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

const union box_t* first_arg(const union box_t* v)
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

const union box_t* next_arg(const union box_t* v)
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

const union box_t* deref_arg(struct context_t* context, const union box_t* v)
{
	uint64_t var_idx;
	const union box_t* t = v;
	do
	{
		if (UNBOX_TYPE(t->m_u64val) != prolite_var)
			return t;

		var_idx = UNBOX_MANT_48(t->m_u64val);
		assert(context->m_substs && var_idx < context->m_substs->m_count);

		if (!context->m_substs->m_values[var_idx])
			return t;

		t = context->m_substs->m_values[var_idx];
	}
	while (t != v);
	return t;
}

static enum eSolveResult redo_true(struct context_t* context, int unwind)
{
	return unwind ? SOLVE_UNWIND : SOLVE_FAIL;
}

static enum eSolveResult redo_cut(struct context_t* context, int unwind)
{
	return unwind ? SOLVE_UNWIND : SOLVE_CUT;
}

static enum eSolveResult redo_repeat(struct context_t* context, int unwind)
{
	if (unwind)
		return SOLVE_UNWIND;

	return stack_push_ptr(&context->m_exec_stack,&redo_repeat) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
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

enum eSolveResult solve_and(struct context_t* context, const union box_t* goal)
{
	enum eSolveResult result;

	// TODO: Copy substs!!

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
			// TODO: Revert substs!!!

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

static enum eSolveResult solve_if_then_else(struct context_t* context, const union box_t* goal)
{
	// TODO
	assert(0);
}

enum eSolveResult solve_or(struct context_t* context, const union box_t* goal)
{
	enum eSolveResult result;
	const union box_t* either_goal = first_arg(goal);
	const union box_t* or_goal = next_arg(either_goal);

	either_goal = deref_arg(context,either_goal);
	if (either_goal->m_u64val == BOX_COMPOUND_EMBED_2(2,'-','>'))
		return solve_if_then_else(context,goal);

	// TODO: Copy substs!!

	result = solve(context,either_goal);
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

enum eSolveResult solve_if_then(struct context_t* context, const union box_t* goal)
{
	// TODO
	assert(0);
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

static int clone_term_part(struct stack_t** stack, union box_t const** v, union box_t** term, size_t* term_size)
{
	*term = stack_realloc(stack,*term,*term_size * sizeof(union box_t),((*term_size)+1) * sizeof(union box_t));
	if (!*term)
		return -1;

	(*term)[*term_size++] = *(*v++);
	return 0;
}

static enum eSolveResult clone_term(struct context_t* context, struct stack_t** stack, union box_t const** v, union box_t** new_term, size_t* term_size)
{
	int debug = (UNBOX_EXP_16((*v)->m_u64val) & 0x8000);
	switch (UNBOX_TYPE((*v)->m_u64val))
	{
	case prolite_compound:
		{
			uint64_t arity = get_arity((*v)->m_u64val);

			if ((UNBOX_HI16((*v)->m_u64val) & 0xC000) == 0)
			{
				// Copy functor atom
				if (clone_term_part(stack,v,new_term,term_size))
					return SOLVE_NOMEM;
			}

			if (clone_term_part(stack,v,new_term,term_size))
				return SOLVE_NOMEM;

			if (debug)
			{
				// TODO: Debug info
			}

			while (arity--)
			{
				enum eSolveResult result = clone_term(context,stack,v,new_term,term_size);
				if (result != SOLVE_TRUE)
					return result;
			}
		}
		break;

	case prolite_var:
		{
			const union box_t* r = deref_arg(context,*v++);
			return clone_term(context,stack,&r,new_term,term_size);
		}
		break;

	case prolite_int32:
	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		if (clone_term_part(stack,v,new_term,term_size))
			return SOLVE_NOMEM;
		break;

	default:
		// prolite_double
		if (clone_term_part(stack,v,new_term,term_size))
			return SOLVE_NOMEM;

		// TODO: Check (*v)->m_u64val for some kind of debug_info magic marker?
		debug = 0;
		break;
	}

	if (debug)
	{
		// TODO: Debug info
	}

	return SOLVE_TRUE;
}

static enum eSolveResult term_to_goal_r(struct context_t* context, union box_t const** v, union box_t** new_term, size_t* term_size)
{
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
				if (clone_term_part(&context->m_exec_stack,v,new_term,term_size))
					return SOLVE_NOMEM;
			}

			if (clone_term_part(&context->m_exec_stack,v,new_term,term_size))
				return SOLVE_NOMEM;

			if (debug)
			{
				// TODO: Debug info
			}

			if (control)
			{
				while (arity--)
				{
					enum eSolveResult result = term_to_goal_r(context,v,new_term,term_size);
					if (result != SOLVE_TRUE)
						return result;
				}
			}
			else
			{
				while (arity--)
				{
					enum eSolveResult result = clone_term(context,&context->m_exec_stack,v,new_term,term_size);
					if (result != SOLVE_TRUE)
						return result;
				}
			}
		}
		return SOLVE_TRUE;

	case prolite_var:
		{
			const union box_t* r = deref_arg(context,*v++);
			if (UNBOX_TYPE(r->m_u64val) == prolite_var)
			{
				// Convert V -> call(V)
				*new_term = stack_realloc(&context->m_exec_stack,*new_term,*term_size * sizeof(union box_t),((*term_size)+1) * sizeof(union box_t));
				if (!*new_term)
					return SOLVE_NOMEM;

				return clone_term(context,&context->m_exec_stack,&r,new_term,term_size);
			}

			return term_to_goal_r(context,&r,new_term,term_size);
		}

	case prolite_atom:
		return clone_term(context,&context->m_exec_stack,v,new_term,term_size);

	default:
		return SOLVE_FAIL;
	}
}

enum eSolveResult term_to_goal(struct context_t* context, const union box_t* src, union box_t** dest)
{
	size_t top = stack_top(context->m_exec_stack);
	size_t term_size = 0;
	return term_to_goal_r(context,&src,dest,&term_size);
}

static enum eSolveResult call(struct context_t* context, const union box_t* goal)
{
	enum eSolveResult result;
	union box_t* fresh_goal;
	size_t stack_base = stack_top(context->m_exec_stack);

	if (!context->m_module->m_flags.debug)
	{
		// Optimize call(call(_)) -> call(_)
		while (goal->m_u64val == BOX_COMPOUND_EMBED_4(1,'c','a','l','l'))
			goal = deref_arg(context,first_arg(goal));
	}

	result = term_to_goal(context,goal,&fresh_goal);
	if (result == SOLVE_FAIL)
		result = throw_type_error(context,BOX_ATOM_BUILTIN(callable),goal);
	else if (result == SOLVE_TRUE)
	{


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

enum eSolveResult solve_throw(struct context_t* context, const union box_t* goal)
{
	enum eSolveResult result;
	union box_t* new_term = NULL;
	size_t term_size = 0;

	goal = deref_arg(context,first_arg(goal));

	if (UNBOX_TYPE(goal->m_u64val) == prolite_var)
		return throw_instantiation_error(context,goal);

	stack_reset(&context->m_scratch_stack,0);

	result = clone_term(context,&context->m_scratch_stack,&goal,&new_term,&term_size);
	if (result == SOLVE_TRUE)
		result = SOLVE_THROW;

	return result;
}

static enum eSolveResult catch(struct context_t* context, enum eSolveResult result, const union box_t* catcher, const union box_t* recovery)
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

enum eSolveResult solve_catch(struct context_t* context, const union box_t* goal)
{
	const union box_t *catcher, *recovery;

	goal = first_arg(goal);
	catcher = next_arg(goal);
	recovery = next_arg(catcher);

	catcher = deref_arg(context,catcher);
	recovery = deref_arg(context,recovery);

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

enum eSolveResult solve_not_proveable(struct context_t* context, const union box_t* goal)
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

enum eSolveResult solve_once(struct context_t* context, const union box_t* goal)
{
	enum eSolveResult result;

	goal = deref_arg(context,first_arg(goal));

	if (!context->m_module->m_flags.debug)
	{
		// Optimize once(once(_)) -> once(_)
		while (goal->m_u64val == BOX_COMPOUND_EMBED_4(1,'o','n','c','e'))
			goal = deref_arg(context,first_arg(goal));
	}

	result = call(context,goal);
	if (result == SOLVE_TRUE)
		result = redo(context,1);

	return result;
}

static enum eSolveResult solve_halt(struct context_t* context, const union box_t* goal)
{
	// halt/1
	goal = deref_arg(context,first_arg(goal));

	if (UNBOX_TYPE(goal->m_u64val) == prolite_var)
		return throw_instantiation_error(context,goal);
	else if (UNBOX_TYPE(goal->m_u64val) != prolite_int32)
		return throw_type_error(context,BOX_ATOM_BUILTIN(integer),goal);

	return stack_push(&context->m_scratch_stack,goal->m_u64val) == -1 ? SOLVE_NOMEM : SOLVE_HALT;
}

static enum eSolveResult solve_user_defined(struct context_t* context, const union box_t* goal)
{
	if (context->m_module->m_flags.unknown == 0)
		return throw_existence_error(context,BOX_ATOM_BUILTIN(procedure),goal);

	return SOLVE_FAIL;
}

#define DECLARE_BUILTIN_CONTROL(f,n)

#define DECLARE_BUILTIN_FUNCTION(f,n) \
	enum eSolveResult solve_##f(struct context_t* context, const union box_t* goal);

#include "builtin_functions.h"

enum eSolveResult solve(struct context_t* context, const union box_t* goal)
{
	enum eSolveResult result;

	goal = deref_arg(context,goal);

	// TODO: tracepoint *call*

	switch (goal->m_u64val)
	{

#undef DECLARE_BUILTIN_CONTROL
#define DECLARE_BUILTIN_CONTROL(f,n)

	case BOX_ATOM_EMBED_4('t','r','u','e'):
		result = stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		break;

	case BOX_ATOM_EMBED_4('f','a','i','l'):
	case BOX_ATOM_EMBED_5('f','a','l','s','e'):
		result = SOLVE_FAIL;
		break;

	case BOX_ATOM_EMBED_1('!'):
		result = stack_push_ptr(&context->m_exec_stack,&redo_cut) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		break;

	case BOX_COMPOUND_EMBED_4(1,'c','a','l','l'):
		result = call(context,deref_arg(context,first_arg(goal)));
		break;

	case BOX_ATOM_BUILTIN(repeat):
		result = stack_push_ptr(&context->m_exec_stack,&redo_repeat) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		break;

	case BOX_ATOM_EMBED_4('h','a','l','t'):
		return SOLVE_HALT;

	case BOX_COMPOUND_EMBED_4(1,'h','a','l','t'):
		return solve_halt(context,goal);

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
			// TODO: call/N


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
