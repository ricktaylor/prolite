

#include "types.h"

#include <assert.h>
#include <string.h>
#include <math.h>

const union box_t* next_arg(const union box_t* v);

static uint64_t get_arity(uint64_t c)
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
	assert(UNBOX_TYPE(v->m_u64val) == prolite_compound);

	// Skip functor atom
	if ((UNBOX_HI16(v->m_u64val) & 0xC000) == 0)
		++v;

	++v;

	if (UNBOX_TYPE(v->m_u64val) == PROLITE_DEBUG_INFO)
	{
		// TODO: Debug info
		++v;
	}

	return v;
}

const union box_t* next_arg(const union box_t* v)
{
	if (UNBOX_TYPE(v->m_u64val) == prolite_compound)
	{
		uint64_t arity = get_arity(v->m_u64val);

		v = first_arg(v);
		while (arity--)
			v = next_arg(v);
	}
	else
	{
		++v;

		if (UNBOX_TYPE(v->m_u64val) == PROLITE_DEBUG_INFO)
		{
			// TODO: Debug info
			++v;
		}
	}

	return v;
}

static const union box_t* get_debug_info(const union box_t* v)
{
	const union box_t* d = NULL;

	if (UNBOX_TYPE(v->m_u64val) == prolite_compound)
	{
		// Skip functor atom
		if ((UNBOX_HI16(v->m_u64val) & 0xC000) == 0)
			++v;
	}

	++v;
	if (UNBOX_TYPE(v->m_u64val) == PROLITE_DEBUG_INFO)
	{
		// TODO: Debug info
		d = v;
	}

	return d;
}

const union box_t* deref_term(struct context_t* context, const union box_t* v)
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

static int clone_term_part(struct stack_t** stack, union box_t const** v, union box_t** term, size_t* term_size)
{
	*term = stack_realloc(stack,*term,*term_size * sizeof(union box_t),((*term_size)+1) * sizeof(union box_t));
	if (!*term)
		return -1;

	(*term)[(*term_size)++] = *((*v)++);

	if (UNBOX_TYPE((*v)->m_u64val) == PROLITE_DEBUG_INFO)
	{
		// TODO: Debug info

		*term = stack_realloc(stack,*term,*term_size * sizeof(union box_t),((*term_size)+1) * sizeof(union box_t));
		if (!*term)
			return -1;

		(*term)[(*term_size)++] = *((*v)++);
	}

	return 0;
}

static enum eSolveResult clone_term(struct context_t* context, struct stack_t** stack, union box_t const** v, union box_t** new_term, size_t* term_size)
{
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
			const union box_t* r = deref_term(context,*v);
			if (r == *v)
				return clone_term_part(stack,v,new_term,term_size);

			++(*v);
			return clone_term(context,stack,&r,new_term,term_size);
		}
		break;

	default:
		if (clone_term_part(stack,v,new_term,term_size))
			return SOLVE_NOMEM;
		break;
	}

	return SOLVE_TRUE;
}

static enum eSolveResult unify(struct context_t* context, const union box_t* a, const union box_t* b, int sto)
{
	a = deref_term(context,a);
	b = deref_term(context,b);
	if (a != b)
	{
		enum tag_type_t t_a = UNBOX_TYPE(a->m_u64val);
		enum tag_type_t t_b = UNBOX_TYPE(b->m_u64val);
		if (t_a == prolite_var)
		{
			uint64_t var_idx = UNBOX_MANT_48(a->m_u64val);
			assert(context->m_substs && var_idx < context->m_substs->m_count);

			assert(!sto);

			context->m_substs->m_values[var_idx] = b;
		}
		else if (t_b == prolite_var)
		{
			uint64_t var_idx = UNBOX_MANT_48(b->m_u64val);
			assert(context->m_substs && var_idx < context->m_substs->m_count);

			assert(!sto);

			context->m_substs->m_values[var_idx] = a;
		}
		else if (a->m_u64val != b->m_u64val)
		{
			// TODO: char_codes and compounds?
			return SOLVE_FAIL;
		}
		else if (t_a == prolite_compound)
		{
			uint64_t arity;
			uint64_t all48 = UNBOX_MANT_48(a->m_u64val);
			unsigned int hi16 = (all48 >> 32);

			if (hi16 & 0x8000)
				arity = (hi16 & (MAX_ARITY_EMBED << 11)) >> 11;
			else if ((hi16 & 0xC000) == 0x4000)
				arity = (hi16 & MAX_ARITY_BUILTIN);
			else
			{
				// Check embedded functor
				if ((a+1)->m_u64val != (b+1)->m_u64val)
					return SOLVE_FAIL;

				arity = all48 & MAX_ARITY;
			}

			a = first_arg(a);
			b = first_arg(b);
			while (arity--)
			{
				enum eSolveResult result = unify(context,a,b,sto);
				if (result != SOLVE_TRUE)
					return result;

				a = next_arg(a);
				b = next_arg(b);
			}
		}
	}

	return SOLVE_TRUE;
}

typedef enum eSolveResult (*solve_fn_t)(struct context_t* context, size_t frame);

#define INLINE_SOLVE(result,context,frame) \
	(result) = (**(solve_fn_t*)stack_at((context)->m_exec_stack,(frame)))((context),(frame)+1);

typedef enum eSolveResult (*redo_fn_t)(struct context_t*,int);

#define INLINE_REDO(result,context,unwind) \
	(result) = (*(redo_fn_t)stack_pop_ptr(&(context)->m_exec_stack))((context),(unwind));

enum eSolveResult redo(struct context_t* context, int unwind)
{
	enum eSolveResult result;
	INLINE_REDO(result,context,unwind);
	return result;
}

enum eCompileResult
{
	COMPILE_OK = 0,
	COMPILE_NOT_CALLABLE,
	COMPILE_ALWAYS_TRUE,
	COMPILE_ALWAYS_FAILS,
	COMPILE_NOMEM,
};

static enum eCompileResult compile(struct context_t* context, const union box_t* goal);

static enum eSolveResult redo_true(struct context_t* context, int unwind)
{
	return unwind ? SOLVE_UNWIND : SOLVE_FAIL;
}

static enum eSolveResult redo_cut(struct context_t* context, int unwind)
{
	return unwind ? SOLVE_UNWIND : SOLVE_CUT;
}

static enum eSolveResult solve_cut(struct context_t* context, size_t frame)
{
	return stack_push_ptr(&context->m_exec_stack,&redo_cut) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
}

static enum eCompileResult compile_cut(struct context_t* context, const union box_t* goal, int debug)
{
	return stack_push_ptr(&context->m_exec_stack,&solve_cut) == -1 ? COMPILE_NOMEM : COMPILE_OK;
}

static enum eSolveResult redo_repeat(struct context_t* context, int unwind)
{
	if (unwind)
		return SOLVE_UNWIND;

	return stack_push_ptr(&context->m_exec_stack,&redo_repeat) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
}

static enum eSolveResult solve_repeat(struct context_t* context, size_t frame)
{
	return stack_push_ptr(&context->m_exec_stack,&redo_repeat) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
}

static enum eCompileResult compile_repeat(struct context_t* context, const union box_t* goal, int debug)
{
	return stack_push_ptr(&context->m_exec_stack,&solve_repeat) == -1 ? COMPILE_NOMEM : COMPILE_OK;
}

static enum eSolveResult redo_and(struct context_t* context, int unwind)
{
	enum eSolveResult result;
	size_t cont_frame = stack_pop(&context->m_exec_stack);

	INLINE_REDO(result,context,unwind);
	if (result != SOLVE_TRUE)
	{
		INLINE_REDO(result,context,result != SOLVE_FAIL);
		if (result == SOLVE_TRUE)
		{
			if (cont_frame == -1)
				result = SOLVE_FAIL;
			else if (!cont_frame)
				result = stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
			else
				INLINE_SOLVE(result,context,cont_frame);
		}
	}

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_exec_stack,cont_frame) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_and) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static enum eSolveResult solve_and(struct context_t* context, size_t frame)
{
	enum eSolveResult result;

	INLINE_SOLVE(result,context,frame+1);
	if (result == SOLVE_TRUE)
	{
		size_t cont_frame = *(size_t*)stack_at(context->m_exec_stack,frame);
again:
		if (cont_frame == -1)
			result = SOLVE_FAIL;
		else if (!cont_frame)
			result = stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		else
			INLINE_SOLVE(result,context,frame+cont_frame);

		if (result == SOLVE_TRUE)
		{
			if (stack_push(&context->m_exec_stack,frame+cont_frame) == -1 ||
				stack_push_ptr(&context->m_exec_stack,&redo_and) == -1)
			{
				result = SOLVE_NOMEM;
			}
		}

		if (result != SOLVE_TRUE)
		{
			INLINE_REDO(result,context,result != SOLVE_FAIL);
			if (result == SOLVE_TRUE)
				goto again;
		}
	}

	return result;
}

static enum eCompileResult compile_and(struct context_t* context, const union box_t* goal, int debug)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_exec_stack);

	if (stack_push_ptr(&context->m_exec_stack,&solve_and) == -1 ||
		stack_push(&context->m_exec_stack,0) == -1)
	{
		return COMPILE_NOMEM;
	}

	goal = first_arg(goal);
	result = compile(context,goal);
	if (result == COMPILE_ALWAYS_TRUE)
	{
		stack_reset(&context->m_exec_stack,frame);
		result = compile(context,next_arg(goal));
	}
	else if (result == COMPILE_OK)
	{
		size_t* cont_frame = stack_at(context->m_exec_stack,frame+1);
		*cont_frame = stack_top(context->m_exec_stack) - frame;

		result = compile(context,next_arg(goal));
		if (result == COMPILE_ALWAYS_TRUE)
		{
			*cont_frame = 0;
			result = COMPILE_OK;
		}
		else if (result == COMPILE_ALWAYS_FAILS)
		{
			*cont_frame = -1;
			result = COMPILE_OK;
		}
	}

	return result;
}

static enum eSolveResult solve_if_then(struct context_t* context, size_t frame)
{
	enum eSolveResult result;

	INLINE_SOLVE(result,context,frame+1);
	if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result == SOLVE_TRUE)
	{
		// Discard the if - implicit cut
		INLINE_REDO(result,context,1);
		if (result == SOLVE_UNWIND)
		{
			size_t then_frame = *(size_t*)stack_at(context->m_exec_stack,frame);
			if (then_frame == -1)
				result = SOLVE_FAIL;
			else if (!then_frame)
				result = stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
			else
				INLINE_SOLVE(result,context,frame+then_frame);
		}
	}

	return result;
}

static enum eCompileResult compile_if_then(struct context_t* context, const union box_t* goal, int debug)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_exec_stack);

	if (stack_push_ptr(&context->m_exec_stack,&solve_if_then) == -1 ||
		stack_push(&context->m_exec_stack,0) == -1)
	{
		return COMPILE_NOMEM;
	}

	goal = first_arg(goal);
	result = compile(context,goal);
	if (result == COMPILE_ALWAYS_TRUE)
	{
		stack_reset(&context->m_exec_stack,frame);
		result = compile(context,next_arg(goal));
	}
	else if (result == COMPILE_OK)
	{
		size_t* then_frame = stack_at(context->m_exec_stack,frame+1);
		*then_frame = stack_top(context->m_exec_stack) - frame;

		result = compile(context,next_arg(goal));
		if (result == COMPILE_ALWAYS_TRUE)
		{
			*then_frame = 0;
			result = COMPILE_OK;
		}
		else if (result == COMPILE_ALWAYS_FAILS)
		{
			*then_frame = -1;
			result = COMPILE_OK;
		}
	}

	return result;
}

static enum eSolveResult solve_if_then_else(struct context_t* context, size_t frame)
{
	enum eSolveResult result;

	INLINE_SOLVE(result,context,frame+1);
	if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result == SOLVE_TRUE)
	{
		// Discard the if - implicit cut
		INLINE_REDO(result,context,1);
		if (result == SOLVE_UNWIND)
		{
			size_t then_frame = *(size_t*)stack_at(context->m_exec_stack,frame);
			if (then_frame == -1)
				result = SOLVE_FAIL;
			else if (!then_frame)
				result = stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
			else
				INLINE_SOLVE(result,context,frame+then_frame);
		}
	}
	else if (result == SOLVE_FAIL)
	{
		size_t else_frame = *(size_t*)stack_at(context->m_exec_stack,frame+1);
		if (else_frame == -1)
			result = SOLVE_FAIL;
		if (!else_frame)
			result = stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		else
			INLINE_SOLVE(result,context,frame+else_frame);
	}

	return result;
}

static enum eCompileResult compile_if_then_else(struct context_t* context, const union box_t* if_then_goal, const union box_t* else_goal, int debug)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_exec_stack);

	if (stack_push_ptr(&context->m_exec_stack,&solve_if_then_else) == -1 ||
		stack_push(&context->m_exec_stack,0) == -1 ||
		stack_push(&context->m_exec_stack,0) == -1)
	{
		return COMPILE_NOMEM;
	}

	if_then_goal = first_arg(if_then_goal);
	result = compile(context,if_then_goal);
	if (result == COMPILE_ALWAYS_TRUE)
	{
		// Check callable state of else
		result = compile(context,else_goal);
		if (result != COMPILE_NOT_CALLABLE && result != COMPILE_NOMEM)
		{
			stack_reset(&context->m_exec_stack,frame);
			result = compile(context,next_arg(if_then_goal));
		}
	}
	else if (result == COMPILE_ALWAYS_FAILS)
	{
		// Check callable state of if_then
		result = compile(context,next_arg(if_then_goal));
		if (result != COMPILE_NOT_CALLABLE && result != COMPILE_NOMEM)
		{
			stack_reset(&context->m_exec_stack,frame);
			result = compile(context,else_goal);
		}
	}
	else if (result == COMPILE_OK)
	{
		size_t* then_cont = stack_at(context->m_exec_stack,frame+1);
		*then_cont = stack_top(context->m_exec_stack) - frame;

		result = compile(context,next_arg(if_then_goal));
		if (result == COMPILE_ALWAYS_TRUE)
		{
			*then_cont = 0;
			result = COMPILE_OK;
		}
		else if (result == COMPILE_ALWAYS_FAILS)
		{
			*then_cont = -1;
			result = COMPILE_OK;
		}

		if (result == COMPILE_OK)
		{
			size_t* else_cont = stack_at(context->m_exec_stack,frame+2);
			*else_cont = stack_top(context->m_exec_stack) - frame;

			result = compile(context,else_goal);
			if (result == COMPILE_ALWAYS_TRUE)
			{
				*else_cont = 0;
				result = COMPILE_OK;
			}
			else if (result == COMPILE_ALWAYS_FAILS)
			{
				*else_cont = -1;
				result = COMPILE_OK;
			}
		}
	}

	return result;
}

static enum eSolveResult redo_or(struct context_t* context, int unwind)
{
	enum eSolveResult result;
	size_t else_frame = stack_pop(&context->m_exec_stack);

	INLINE_REDO(result,context,unwind);
	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_exec_stack,else_frame) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_or) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}
	else if (result == SOLVE_FAIL)
	{
		if (else_frame == -1)
			result = SOLVE_FAIL;
		else if (!else_frame)
			result = stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		else
			INLINE_SOLVE(result,context,else_frame);
	}

	return result;
}

static enum eSolveResult solve_or(struct context_t* context, size_t frame)
{
	enum eSolveResult result;
	size_t else_frame = *(size_t*)stack_at(context->m_exec_stack,frame);
	if (else_frame && else_frame != -1)
		else_frame += frame;

	INLINE_SOLVE(result,context,frame+1);
	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_exec_stack,else_frame) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_or) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}
	else if (result == SOLVE_FAIL)
	{
		if (else_frame == -1)
			result = SOLVE_FAIL;
		else if (!else_frame)
			result = stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		else
			INLINE_SOLVE(result,context,frame+else_frame);
	}

	return result;
}

static enum eCompileResult compile_or(struct context_t* context, const union box_t* goal, int debug)
{
	enum eCompileResult result;
	const union box_t* else_goal;
	size_t frame;

	goal = first_arg(goal);
	else_goal = deref_term(context,next_arg(goal));
	goal = deref_term(context,goal);

	if (goal->m_u64val == BOX_COMPOUND_EMBED_2(2,'-','>'))
		return compile_if_then_else(context,goal,else_goal,debug);

	frame = stack_top(context->m_exec_stack);
	if (stack_push_ptr(&context->m_exec_stack,&solve_or) == -1 ||
		stack_push(&context->m_exec_stack,0) == -1)
	{
		return COMPILE_NOMEM;
	}

	result = compile(context,goal);
	if (result == COMPILE_ALWAYS_FAILS)
	{
		stack_reset(&context->m_exec_stack,frame);
		result = compile(context,else_goal);
	}
	else if (result == COMPILE_ALWAYS_TRUE)
	{
		// Check callable state of else_goal
		result = compile(context,else_goal);
		if (result != COMPILE_NOT_CALLABLE && result != COMPILE_NOMEM)
		{
			stack_reset(&context->m_exec_stack,frame);
			result = COMPILE_ALWAYS_TRUE;
		}
	}
	else if (result == COMPILE_OK)
	{
		size_t* else_frame = stack_at(context->m_exec_stack,frame+1);
		*else_frame = stack_top(context->m_exec_stack) - frame;

		result = compile(context,else_goal);
		if (result == COMPILE_ALWAYS_TRUE)
		{
			*else_frame = 0;
			result = COMPILE_OK;
		}
		else if (result == COMPILE_ALWAYS_FAILS)
		{
			*else_frame = -1;
			result = COMPILE_OK;
		}
	}

	return result;
}

static enum eSolveResult redo_call(struct context_t* context, int unwind)
{
	enum eSolveResult result;

	INLINE_REDO(result,context,unwind);
	if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,&redo_call) == -1)
			result = SOLVE_NOMEM;
	}

	return result;
}

static enum eSolveResult solve_call(struct context_t* context, size_t frame)
{
	enum eSolveResult result;

	INLINE_SOLVE(result,context,frame);
	if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_exec_stack,&redo_call) == -1)
			result = SOLVE_NOMEM;
	}

	return result;
}

static enum eSolveResult redo_compile(struct context_t* context, int unwind)
{
	enum eSolveResult result;
	size_t stack_base = stack_pop(&context->m_exec_stack);

	INLINE_REDO(result,context,unwind);
	if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_exec_stack,stack_base) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_compile) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	if (result != SOLVE_TRUE)
		stack_reset(&context->m_exec_stack,stack_base);

	return result;
}

static enum eSolveResult solve_compile(struct context_t* context, size_t frame)
{
	enum eSolveResult result;
	size_t stack_base;
	const union box_t* goal = *(const union box_t**)stack_at(context->m_exec_stack,frame);

	goal = deref_term(context,goal);
	switch (UNBOX_TYPE(goal->m_u64val))
	{
	case prolite_compound:
	case prolite_atom:
		break;

	default:
		return throw_type_error(context,BOX_ATOM_BUILTIN(callable),goal);
	}

	stack_base = stack_top(context->m_exec_stack);
	switch (compile(context,goal))
	{
	case COMPILE_OK:
		INLINE_SOLVE(result,context,stack_base);
		if (result == SOLVE_CUT)
			result = SOLVE_FAIL;
		break;

	case COMPILE_NOT_CALLABLE:
		result = throw_type_error(context,BOX_ATOM_BUILTIN(callable),goal);
		break;

	case COMPILE_ALWAYS_TRUE:
		result = stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		break;

	case COMPILE_ALWAYS_FAILS:
		result = SOLVE_FAIL;
		break;

	case COMPILE_NOMEM:
		result = SOLVE_NOMEM;
		break;
	}

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_exec_stack,stack_base) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_compile) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	if (result != SOLVE_TRUE)
		stack_reset(&context->m_exec_stack,stack_base);

	return result;
}

static enum eCompileResult compile_call(struct context_t* context, const union box_t* goal, int debug)
{
	enum eCompileResult result;
	size_t frame;

	while (goal->m_u64val == BOX_COMPOUND_EMBED_4(1,'c','a','l','l'))
	{
		// Optimize call(call(_)) -> call(_)
		goal = deref_term(context,first_arg(goal));
		if (debug)
			break;
	}

	switch (UNBOX_TYPE(goal->m_u64val))
	{
	case prolite_compound:
	case prolite_atom:
		frame = stack_top(context->m_exec_stack);
		if (stack_push_ptr(&context->m_exec_stack,&solve_call) == -1)
			result = COMPILE_NOMEM;
		else
		{
			result = compile(context,goal);
			if (result == COMPILE_ALWAYS_TRUE || result == COMPILE_ALWAYS_FAILS)
				stack_reset(&context->m_exec_stack,frame);
		}
		break;

	case prolite_var:
		if (stack_push_ptr(&context->m_exec_stack,&solve_compile) == -1 ||
			stack_push_ptr(&context->m_exec_stack,goal) == -1)
		{
			result = COMPILE_NOMEM;
		}
		result = COMPILE_OK;
		break;

	default:
		result = COMPILE_NOT_CALLABLE;
		break;
	}

	return result;
}

static enum eSolveResult redo_unify(struct context_t* context, int unwind)
{
	struct substs_t* prev_substs = stack_pop_ptr(&context->m_exec_stack);
	if (prev_substs)
	{
		uint64_t stack_base = stack_pop(&context->m_exec_stack);
		stack_reset(&context->m_exec_stack,stack_base);
		context->m_substs = prev_substs;
	}

	return unwind ? SOLVE_UNWIND : SOLVE_FAIL;
}

static enum eSolveResult solve_unify(struct context_t* context, size_t frame)
{
	enum eSolveResult result;
	struct substs_t* prev_substs = context->m_substs;
	uint64_t stack_base = stack_top(context->m_exec_stack);
	const union box_t *arg1,*arg2;

	if (prev_substs)
	{
		context->m_substs = stack_malloc(&context->m_exec_stack,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
		if (!context->m_substs)
		{
			context->m_substs = prev_substs;
			return SOLVE_NOMEM;
		}
		memcpy(context->m_substs,prev_substs,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
	}

	arg1 = *(const union box_t**)stack_at(context->m_exec_stack,frame);
	arg2 = *(const union box_t**)stack_at(context->m_exec_stack,frame+1);

	result = unify(context,arg1,arg2,0);
	if (result == SOLVE_TRUE)
	{
		if ((prev_substs && stack_push(&context->m_exec_stack,stack_base) == -1) ||
			stack_push_ptr(&context->m_exec_stack,prev_substs) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_unify) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	if (result != SOLVE_TRUE && prev_substs)
	{
		stack_reset(&context->m_exec_stack,stack_base);
		context->m_substs = prev_substs;
	}

	return result;
}

static enum eCompileResult compile_unify(struct context_t* context, const union box_t* goal, int debug)
{
	const union box_t* arg1;
	size_t frame = stack_top(context->m_exec_stack);

	goal = first_arg(goal);
	arg1 = deref_term(context,next_arg(goal));
	goal = deref_term(context,goal);

	if (stack_push_ptr(&context->m_exec_stack,&solve_unify) == -1 ||
		stack_push_ptr(&context->m_exec_stack,goal) == -1 ||
		stack_push_ptr(&context->m_exec_stack,arg1) == -1)
	{
		return COMPILE_NOMEM;
	}

	return COMPILE_OK;
}

static enum eSolveResult solve_not_unifiable(struct context_t* context, size_t frame)
{
	enum eSolveResult result;
	struct substs_t* prev_substs = context->m_substs;
	uint64_t stack_base = stack_top(context->m_exec_stack);
	const union box_t *arg1,*arg2;

	if (prev_substs)
	{
		context->m_substs = stack_malloc(&context->m_exec_stack,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
		if (!context->m_substs)
		{
			context->m_substs = prev_substs;
			return SOLVE_NOMEM;
		}
		memcpy(context->m_substs,prev_substs,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
	}

	arg1 = *(const union box_t**)stack_at(context->m_exec_stack,frame);
	arg2 = *(const union box_t**)stack_at(context->m_exec_stack,frame+1);

	result = unify(context,arg1,arg2,0);
	if (result == SOLVE_TRUE)
		result = SOLVE_FAIL;
	else if (result == SOLVE_FAIL)
		result = SOLVE_TRUE;

	stack_reset(&context->m_exec_stack,stack_base);
	context->m_substs = prev_substs;

	return result;
}

static enum eCompileResult compile_not_unifiable(struct context_t* context, const union box_t* goal, int debug)
{
	const union box_t* arg1;
	size_t frame = stack_top(context->m_exec_stack);

	goal = first_arg(goal);
	arg1 = deref_term(context,next_arg(goal));
	goal = deref_term(context,goal);

	if (stack_push_ptr(&context->m_exec_stack,&solve_not_unifiable) == -1 ||
		stack_push_ptr(&context->m_exec_stack,goal) == -1 ||
		stack_push_ptr(&context->m_exec_stack,arg1) == -1)
	{
		return COMPILE_NOMEM;
	}

	return COMPILE_OK;
}

static enum eSolveResult solve_throw(struct context_t* context, size_t frame)
{
	enum eSolveResult result;
	union box_t* new_term = NULL;
	size_t term_size = 0;
	const union box_t* ball = *(const union box_t**)stack_at(context->m_exec_stack,frame);

	ball = deref_term(context,ball);

	if (UNBOX_TYPE(ball->m_u64val) == prolite_var)
		return throw_instantiation_error(context,ball);

	stack_reset(&context->m_scratch_stack,0);

	result = clone_term(context,&context->m_scratch_stack,&ball,&new_term,&term_size);
	if (result == SOLVE_TRUE)
		result = SOLVE_THROW;

	return result;
}

static enum eCompileResult compile_throw(struct context_t* context, const union box_t* goal, int debug)
{
	goal = deref_term(context,first_arg(goal));

	if (stack_push_ptr(&context->m_exec_stack,&solve_throw) == -1 ||
		stack_push_ptr(&context->m_exec_stack,goal) == -1)
	{
		return COMPILE_NOMEM;
	}

	return COMPILE_OK;
}

static enum eSolveResult catch(struct context_t* context, enum eSolveResult result, const union box_t* catcher, size_t recovery_frame)
{
	if (result == SOLVE_NOMEM)
	{
		union box_t* ball = NULL;

		stack_reset(&context->m_scratch_stack,0);

		ball = stack_malloc(&context->m_scratch_stack,2 * sizeof(union box_t));
		if (ball)
		{
			ball[0].m_u64val = BOX_COMPOUND_BUILTIN(resource_error,1);
			ball[1].m_u64val = BOX_ATOM_BUILTIN(memory);

			result = SOLVE_THROW;
		}
	}

	if (result == SOLVE_THROW)
	{
		// Unify catcher and ball
		struct substs_t* prev_substs = context->m_substs;
		uint64_t stack_base = stack_top(context->m_exec_stack);

		if (prev_substs)
		{
			context->m_substs = stack_malloc(&context->m_exec_stack,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
			if (!context->m_substs)
			{
				context->m_substs = prev_substs;
				result = SOLVE_NOMEM;
			}
			else
				memcpy(context->m_substs,prev_substs,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
		}

		if (result == SOLVE_THROW)
		{
			result = unify(context,catcher,stack_at(context->m_scratch_stack,0),0);
			if (result == SOLVE_FAIL)
				result = SOLVE_THROW;
			else if (result == SOLVE_TRUE)
			{
				if (recovery_frame == -1)
					result = SOLVE_FAIL;
				else if (!recovery_frame)
					result = stack_push_ptr(&context->m_exec_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
				else if (recovery_frame)
					INLINE_SOLVE(result,context,recovery_frame);

				if (result == SOLVE_TRUE)
				{
					if ((prev_substs && stack_push(&context->m_exec_stack,stack_base) == -1) ||
						stack_push_ptr(&context->m_exec_stack,prev_substs) == -1 ||
						stack_push_ptr(&context->m_exec_stack,&redo_unify) == -1)
					{
						result = SOLVE_NOMEM;
					}
				}
			}
		}

		if (result != SOLVE_TRUE && prev_substs)
		{
			stack_reset(&context->m_exec_stack,stack_base);
			context->m_substs = prev_substs;
		}
	}

	return result;
}

static enum eSolveResult redo_catch(struct context_t* context, int unwind)
{
	enum eSolveResult result;

	union box_t* catcher = stack_pop_ptr(&context->m_exec_stack);
	size_t recovery_frame = stack_pop(&context->m_exec_stack);

	INLINE_REDO(result,context,unwind);
	if (result == SOLVE_NOMEM || result == SOLVE_THROW)
		return catch(context,result,catcher,recovery_frame);

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_exec_stack,recovery_frame) == -1 ||
			stack_push_ptr(&context->m_exec_stack,catcher) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_catch) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static enum eSolveResult solve_catch(struct context_t* context, size_t frame)
{
	enum eSolveResult result;

	const union box_t* catcher = *(const union box_t**)stack_at(context->m_exec_stack,frame);
	size_t recovery_frame = *(size_t*)stack_at(context->m_exec_stack,frame+1);
	if (recovery_frame && recovery_frame != -1)
		recovery_frame += frame;

	INLINE_SOLVE(result,context,frame+2)
	if (result == SOLVE_NOMEM || result == SOLVE_THROW)
		return catch(context,result,catcher,recovery_frame);

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_exec_stack,recovery_frame) == -1 ||
			stack_push_ptr(&context->m_exec_stack,catcher) == -1 ||
			stack_push_ptr(&context->m_exec_stack,&redo_catch) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static enum eCompileResult compile_catch(struct context_t* context, const union box_t* goal, int debug)
{
	enum eCompileResult result;
	const union box_t *catcher, *recovery;
	size_t frame = stack_top(context->m_exec_stack);

	goal = first_arg(goal);
	catcher = next_arg(goal);
	recovery = next_arg(catcher);

	catcher = deref_term(context,catcher);
	recovery = deref_term(context,recovery);

	if (stack_push_ptr(&context->m_exec_stack,&solve_catch) == -1 ||
		stack_push_ptr(&context->m_exec_stack,catcher) == -1 ||
		stack_push(&context->m_exec_stack,0) == -1)
	{
		result = COMPILE_NOMEM;
	}

	result = compile_call(context,goal,debug);
	if (result == COMPILE_ALWAYS_TRUE || result == COMPILE_ALWAYS_FAILS)
		stack_reset(&context->m_exec_stack,frame);
	else if (result == COMPILE_OK)
	{
		size_t* recovery_frame = stack_at(context->m_exec_stack,frame+2);
		*recovery_frame = stack_top(context->m_exec_stack) - frame;

		result = compile_call(context,recovery,debug);
		if (result == COMPILE_ALWAYS_TRUE)
		{
			*recovery_frame = 0;
			result = COMPILE_OK;
		}
		else if (result == COMPILE_ALWAYS_TRUE)
		{
			*recovery_frame = -1;
			result = COMPILE_OK;
		}
	}

	return result;
}

static enum eSolveResult solve_not_proveable(struct context_t* context, size_t frame)
{
	enum eSolveResult result;

	INLINE_SOLVE(result,context,frame);
	if (result == SOLVE_TRUE)
	{
		INLINE_REDO(result,context,1);
		if (result == SOLVE_UNWIND)
			result = SOLVE_FAIL;
	}
	else if (result == SOLVE_FAIL)
		result = SOLVE_TRUE;

	return result;
}

static enum eCompileResult compile_not_proveable(struct context_t* context, const union box_t* goal, int debug)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_exec_stack);

	if (stack_push_ptr(&context->m_exec_stack,&solve_not_proveable) == -1)
		return COMPILE_NOMEM;

	goal = deref_term(context,first_arg(goal));

	result = compile_call(context,goal,debug);
	if (result == COMPILE_ALWAYS_TRUE)
	{
		stack_reset(&context->m_exec_stack,frame);
		result = COMPILE_ALWAYS_FAILS;
	}
	else if (result == COMPILE_ALWAYS_FAILS)
	{
		stack_reset(&context->m_exec_stack,frame);
		result = COMPILE_ALWAYS_TRUE;
	}

	return result;
}

static enum eSolveResult solve_once(struct context_t* context, size_t frame)
{
	enum eSolveResult result;

	INLINE_SOLVE(result,context,frame);
	if (result == SOLVE_TRUE)
	{
		INLINE_REDO(result,context,1);
		if (result == SOLVE_UNWIND)
			result = SOLVE_TRUE;
	}

	return result;
}

static enum eCompileResult compile_once(struct context_t* context, const union box_t* goal, int debug)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_exec_stack);

	if (stack_push_ptr(&context->m_exec_stack,&solve_once) == -1)
		return COMPILE_NOMEM;

	while (goal->m_u64val == BOX_COMPOUND_EMBED_4(1,'o','n','c','e'))
	{
		// Optimize once(once(_)) -> once(_)
		goal = deref_term(context,first_arg(goal));
		if (debug)
			break;
	}

	result = compile_call(context,goal,debug);
	if (result == COMPILE_ALWAYS_TRUE || result == COMPILE_ALWAYS_FAILS)
		stack_reset(&context->m_exec_stack,frame);

	return result;
}

static enum eSolveResult solve_halt(struct context_t* context, size_t frame)
{
	enum eSolveResult result = SOLVE_HALT;

	const union box_t* goal = *(const union box_t**)stack_at(context->m_exec_stack,frame);
	if (goal)
	{
		goal = deref_term(context,goal);

		// halt/1
		if (UNBOX_TYPE(goal->m_u64val) == prolite_var)
			result = throw_instantiation_error(context,goal);
		else if (UNBOX_TYPE(goal->m_u64val) != prolite_int32)
			result = throw_type_error(context,BOX_ATOM_BUILTIN(integer),goal);
		else
		{
			stack_reset(&context->m_scratch_stack,0);
			if (stack_push(&context->m_scratch_stack,goal->m_u64val) == -1)
				result = SOLVE_NOMEM;
		}
	}

	return result;
}

static enum eCompileResult compile_halt(struct context_t* context, const union box_t* goal, int debug)
{
	enum eCompileResult result;

	if (goal->m_u64val == BOX_COMPOUND_EMBED_4(1,'h','a','l','t'))
	{
		// halt/1
		goal = deref_term(context,first_arg(goal));
	}
	else
		goal = NULL;

	if (stack_push_ptr(&context->m_exec_stack,&solve_halt) == -1 ||
		stack_push_ptr(&context->m_exec_stack,goal) == -1)
	{
		return COMPILE_NOMEM;
	}

	return COMPILE_OK;
}

static enum eCompileResult compile_user_defined(struct context_t* context, const union box_t* goal)
{
	//if (context->m_module->m_flags.unknown == 0)
	//	return throw_existence_error(context,BOX_ATOM_BUILTIN(procedure),goal);

	assert(0);

	return COMPILE_OK;
}

#define DECLARE_BUILTIN_FUNCTION(f,n) \
enum eCompileResult solve_##f(struct context_t* context, size_t frame);

#include "builtin_functions.h"

static enum eCompileResult compile(struct context_t* context, const union box_t* goal)
{
	enum eCompileResult result = COMPILE_NOT_CALLABLE;
	int debug = 0;

	goal = deref_term(context,goal);
	if (get_debug_info(goal))
	{
		debug = 1;

		// TODO: Emit tracepoint
	}

	switch (goal->m_u64val)
	{
	case BOX_ATOM_EMBED_4('t','r','u','e'):
		result = COMPILE_ALWAYS_TRUE;
		break;

	case BOX_ATOM_EMBED_4('f','a','i','l'):
	case BOX_ATOM_EMBED_5('f','a','l','s','e'):
		result = COMPILE_ALWAYS_FAILS;
		break;

#define DECLARE_BUILTIN_STATIC(f,n) \
	case (n): result = compile_##f(context,goal,debug); break;

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,n) \
	case (n): result = (stack_push_ptr(&context->m_exec_stack,&solve_##f) == -1 || stack_push_ptr(&context->m_exec_stack,goal) == -1) ? COMPILE_NOMEM : COMPILE_OK; break; \
		break;

#include "builtin_functions.h"

	default:
		switch (UNBOX_TYPE(goal->m_u64val))
		{
		case prolite_var:
			result = compile_call(context,goal,debug);
			break;

		case prolite_compound:
			if ((goal->m_u64val & BOX_COMPOUND_EMBED_4(0,'c','a','l','l')) == BOX_COMPOUND_EMBED_4(0,'c','a','l','l') ||
				(goal->m_u64val & BOX_COMPOUND_BUILTIN(call,0)) == BOX_COMPOUND_BUILTIN(call,0))
			{
				// TODO: call/N
				assert(0);
			}
			result = compile_user_defined(context,goal);
			break;

		case prolite_atom:
			result = compile_user_defined(context,goal);
			break;

		default:
			break;
		}
		break;
	}

	if (debug)
	{
		// TODO: Emit tracepoint
	}

	return result;
}
