

#include "types.h"
#include "stream.h"

#include <assert.h>
#include <string.h>
#include <math.h>

#include <stdio.h>

const union box_t* next_arg(const union box_t* v);

inline const union box_t* first_arg(const union box_t* v)
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

inline const union box_t* next_arg(const union box_t* v)
{
	if (UNBOX_TYPE(v->m_u64val) == prolite_compound)
	{
		uint64_t arity = UNBOX_MANT_48(v->m_u64val);
		unsigned int hi16 = (arity >> 32);
		if (hi16 & 0x8000)
			arity = (hi16 & (MAX_ARITY_EMBED << 11)) >> 11;
		else if ((hi16 & 0xC000) == 0x4000)
			arity = (hi16 & MAX_ARITY_BUILTIN);

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

inline const union box_t* deref_term(struct substs_t* substs, const union box_t* v)
{
	uint64_t var_idx;
	const union box_t* t = v;
	do
	{
		if (UNBOX_TYPE(t->m_u64val) != prolite_var)
			return t;

		var_idx = UNBOX_MANT_48(t->m_u64val);
		assert(substs && var_idx < substs->m_count);

		if (!substs->m_values[var_idx])
			return t;

		t = substs->m_values[var_idx];
	}
	while (t != v);
	return t;
}

enum eSolveResult unify(struct context_t* context, const union box_t* a, const union box_t* b, int sto)
{
	a = deref_term(context->m_substs,a);
	b = deref_term(context->m_substs,b);
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

enum eSolveResult redo_true(struct context_t* context, int unwind)
{
	return unwind ? SOLVE_UNWIND : SOLVE_FAIL;
}

static enum eSolveResult redo_cut(struct context_t* context, int unwind)
{
	return unwind ? SOLVE_UNWIND : SOLVE_CUT;
}

static enum eSolveResult solve_cut(struct context_t* context, size_t frame)
{
	return stack_push_ptr(&context->m_call_stack,&redo_cut) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
}

static inline enum eCompileResult compile_cut(struct compile_context_t* context, const union box_t* goal)
{
	return stack_push_ptr(&context->m_emit_stack,&solve_cut) == -1 ? COMPILE_NOMEM : COMPILE_OK;
}

static enum eSolveResult redo_repeat(struct context_t* context, int unwind)
{
	enum eSolveResult result = redo(context,unwind);
	if (result == SOLVE_TRUE || result == SOLVE_FAIL)
		result = stack_push_ptr(&context->m_call_stack,&redo_repeat) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;

	return result;
}

static enum eSolveResult solve_repeat(struct context_t* context, size_t frame)
{
	return stack_push_ptr(&context->m_call_stack,&redo_repeat) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
}

static inline enum eCompileResult compile_repeat(struct compile_context_t* context, const union box_t* goal)
{
	return stack_push_ptr(&context->m_emit_stack,&solve_repeat) == -1 ? COMPILE_NOMEM : COMPILE_OK;
}

static enum eSolveResult redo_and(struct context_t* context, int unwind)
{
	size_t cont_frame = stack_pop(&context->m_call_stack);

	enum eSolveResult result = redo(context,unwind);
	if (result != SOLVE_TRUE)
	{
		result = redo(context,result != SOLVE_FAIL);
		if (result == SOLVE_TRUE)
		{
			if (cont_frame == -1)
				result = SOLVE_FAIL;
			else if (!cont_frame)
				result = stack_push_ptr(&context->m_call_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
			else
				result = solve(context,cont_frame);
		}
	}

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_call_stack,cont_frame) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_and) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static enum eSolveResult solve_and(struct context_t* context, size_t frame)
{
	enum eSolveResult result = solve(context,frame+1);
	if (result == SOLVE_TRUE)
	{
		size_t cont_frame = *(const size_t*)stack_at(context->m_instr_stack,frame);
		if (cont_frame && cont_frame != -1)
			cont_frame += (frame-1);
again:
		if (cont_frame == -1)
			result = SOLVE_FAIL;
		else if (!cont_frame)
			result = stack_push_ptr(&context->m_call_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		else
			result = solve(context,cont_frame);

		if (result == SOLVE_TRUE)
		{
			if (stack_push(&context->m_call_stack,cont_frame) == -1 ||
				stack_push_ptr(&context->m_call_stack,&redo_and) == -1)
			{
				result = SOLVE_NOMEM;
			}
		}

		if (result != SOLVE_TRUE)
		{
			result = redo(context,result != SOLVE_FAIL);
			if (result == SOLVE_TRUE)
				goto again;
		}
	}

	return result;
}

static enum eCompileResult compile_and(struct compile_context_t* context, const union box_t* goal)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_emit_stack);

	if (stack_push_ptr(&context->m_emit_stack,&solve_and) == -1 ||
		stack_push(&context->m_emit_stack,0) == -1)
	{
		return COMPILE_NOMEM;
	}

	//printf("%u: &solve_and\n",frame);
	//printf("%u: cont_frame = 0\n",frame+1);

	goal = first_arg(goal);
	result = compile(context,goal);
	if (result == COMPILE_ALWAYS_TRUE)
	{
		//printf("%u: RESET\n",frame);

		stack_reset(&context->m_emit_stack,frame);
		result = compile(context,next_arg(goal));
	}
	else if (result == COMPILE_ALWAYS_FAILS)
	{
		// Check callable state of cont_goal
		result = compile(context,next_arg(goal));
		if (result != COMPILE_NOT_CALLABLE && result != COMPILE_NOMEM)
		{
			stack_reset(&context->m_emit_stack,frame);
			result = COMPILE_ALWAYS_FAILS;
		}
	}
	else if (result == COMPILE_OK)
	{
		size_t* cont_frame = (size_t*)stack_at(context->m_emit_stack,frame+1);
		*cont_frame = stack_top(context->m_emit_stack) - frame;

		//printf("cont_frame (@%u) = %d\n",frame+1,(int)*cont_frame);

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
	enum eSolveResult result = solve(context,frame+1);
	if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result == SOLVE_TRUE)
	{
		// Discard the if - implicit cut
		result = redo(context,1);
		if (result == SOLVE_UNWIND)
		{
			size_t then_frame = *(const size_t*)stack_at(context->m_instr_stack,frame);
			if (then_frame == -1)
				result = SOLVE_FAIL;
			else if (!then_frame)
				result = stack_push_ptr(&context->m_call_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
			else
				result = solve(context,frame+then_frame);
		}
	}

	return result;
}

static enum eCompileResult compile_if_then(struct compile_context_t* context, const union box_t* goal)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_emit_stack);

	if (stack_push_ptr(&context->m_emit_stack,&solve_if_then) == -1 ||
		stack_push(&context->m_emit_stack,0) == -1)
	{
		return COMPILE_NOMEM;
	}

	goal = first_arg(goal);
	result = compile(context,goal);
	if (result == COMPILE_ALWAYS_TRUE)
	{
		stack_reset(&context->m_emit_stack,frame);
		result = compile(context,next_arg(goal));
	}
	else if (result == COMPILE_ALWAYS_FAILS)
	{
		// Check callable state of then_goal
		result = compile(context,next_arg(goal));
		if (result != COMPILE_NOT_CALLABLE && result != COMPILE_NOMEM)
		{
			stack_reset(&context->m_emit_stack,frame);
			result = COMPILE_ALWAYS_FAILS;
		}
	}
	else if (result == COMPILE_OK)
	{
		size_t* then_frame = (size_t*)stack_at(context->m_emit_stack,frame+1);
		*then_frame = stack_top(context->m_emit_stack) - frame;

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
	enum eSolveResult result = solve(context,frame+1);
	if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result == SOLVE_TRUE)
	{
		// Discard the if - implicit cut
		result = redo(context,1);
		if (result == SOLVE_UNWIND)
		{
			size_t then_frame = *(const size_t*)stack_at(context->m_instr_stack,frame);
			if (then_frame == -1)
				result = SOLVE_FAIL;
			else if (!then_frame)
				result = stack_push_ptr(&context->m_call_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
			else
				result = solve(context,frame+then_frame);
		}
	}
	else if (result == SOLVE_FAIL)
	{
		size_t else_frame = *(const size_t*)stack_at(context->m_instr_stack,frame+1);
		if (else_frame == -1)
			result = SOLVE_FAIL;
		if (!else_frame)
			result = stack_push_ptr(&context->m_call_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		else
			result = solve(context,frame+else_frame);
	}

	return result;
}

static enum eCompileResult compile_if_then_else(struct compile_context_t* context, const union box_t* if_then_goal, const union box_t* else_goal)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_emit_stack);

	if (stack_push_ptr(&context->m_emit_stack,&solve_if_then_else) == -1 ||
		stack_push(&context->m_emit_stack,0) == -1 ||
		stack_push(&context->m_emit_stack,0) == -1)
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
			stack_reset(&context->m_emit_stack,frame);
			result = compile(context,next_arg(if_then_goal));
		}
	}
	else if (result == COMPILE_ALWAYS_FAILS)
	{
		// Check callable state of if_then
		result = compile(context,next_arg(if_then_goal));
		if (result != COMPILE_NOT_CALLABLE && result != COMPILE_NOMEM)
		{
			stack_reset(&context->m_emit_stack,frame);
			result = compile(context,else_goal);
		}
	}
	else if (result == COMPILE_OK)
	{
		size_t* then_cont = (size_t*)stack_at(context->m_emit_stack,frame+1);
		*then_cont = stack_top(context->m_emit_stack) - frame;

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
			size_t* else_cont = (size_t*)stack_at(context->m_emit_stack,frame+2);
			*else_cont = stack_top(context->m_emit_stack) - frame;

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
	size_t else_frame = stack_pop(&context->m_call_stack);

	enum eSolveResult result = redo(context,unwind);
	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_call_stack,else_frame) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_or) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}
	else if (result == SOLVE_FAIL)
	{
		if (else_frame == -1)
			result = SOLVE_FAIL;
		else if (!else_frame)
			result = stack_push_ptr(&context->m_call_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		else
			result = solve(context,else_frame);
	}

	return result;
}

static enum eSolveResult solve_or(struct context_t* context, size_t frame)
{
	enum eSolveResult result;
	size_t else_frame = *(const size_t*)stack_at(context->m_instr_stack,frame);
	if (else_frame && else_frame != -1)
		else_frame += (frame-1);

	result = solve(context,frame+1);
	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_call_stack,else_frame) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_or) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}
	else if (result == SOLVE_FAIL)
	{
		if (else_frame == -1)
			result = SOLVE_FAIL;
		else if (!else_frame)
			result = stack_push_ptr(&context->m_call_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
		else
			result = solve(context,frame+else_frame);
	}

	return result;
}

static enum eCompileResult compile_or(struct compile_context_t* context, const union box_t* goal)
{
	enum eCompileResult result;
	const union box_t* else_goal;
	size_t frame;

	goal = first_arg(goal);
	else_goal = deref_term(context->m_substs,next_arg(goal));
	goal = deref_term(context->m_substs,goal);

	if (goal->m_u64val == BOX_COMPOUND_EMBED_2(2,'-','>'))
		return compile_if_then_else(context,goal,else_goal);

	frame = stack_top(context->m_emit_stack);
	if (stack_push_ptr(&context->m_emit_stack,&solve_or) == -1 ||
		stack_push(&context->m_emit_stack,0) == -1)
	{
		return COMPILE_NOMEM;
	}

	result = compile(context,goal);
	if (result == COMPILE_ALWAYS_FAILS)
	{
		stack_reset(&context->m_emit_stack,frame);
		result = compile(context,else_goal);
	}
	else if (result == COMPILE_ALWAYS_TRUE)
	{
		// Check callable state of else_goal
		result = compile(context,else_goal);
		if (result != COMPILE_NOT_CALLABLE && result != COMPILE_NOMEM)
		{
			stack_reset(&context->m_emit_stack,frame);
			result = COMPILE_ALWAYS_TRUE;
		}
	}
	else if (result == COMPILE_OK)
	{
		size_t* else_frame = (size_t*)stack_at(context->m_emit_stack,frame+1);
		*else_frame = stack_top(context->m_emit_stack) - frame;

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
	enum eSolveResult result = redo(context,unwind);
	if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_call_stack,&redo_call) == -1)
			result = SOLVE_NOMEM;
	}

	return result;
}

static enum eSolveResult solve_call(struct context_t* context, size_t frame)
{
	enum eSolveResult result = solve(context,frame);
	if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&context->m_call_stack,&redo_call) == -1)
			result = SOLVE_NOMEM;
	}

	return result;
}

static enum eSolveResult redo_compile(struct context_t* context, int unwind)
{
	size_t stack_base = stack_pop(&context->m_call_stack);

	enum eSolveResult result = redo(context,unwind);
	if (result == SOLVE_CUT)
		result = SOLVE_FAIL;

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_call_stack,stack_base) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_compile) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	if (result != SOLVE_TRUE)
		stack_reset(&context->m_call_stack,stack_base);

	return result;
}

static enum eSolveResult solve_compile(struct context_t* context, size_t frame)
{
	enum eSolveResult result;
	size_t stack_base;
	const union box_t* goal = deref_term(context->m_substs,*(const union box_t**)stack_at(context->m_instr_stack,frame));
	struct compile_context_t compile_context;

	switch (UNBOX_TYPE(goal->m_u64val))
	{
	case prolite_compound:
	case prolite_atom:
		break;

	default:
		return throw_type_error(context,BOX_ATOM_BUILTIN(callable),goal);
	}

	compile_context.m_emit_stack = context->m_call_stack;
	compile_context.m_substs = context->m_substs;
	compile_context.m_module = context->m_module;

	stack_base = stack_top(context->m_call_stack);
	switch (compile(&compile_context,goal))
	{
	case COMPILE_OK:
		result = solve(context,stack_base);
		if (result == SOLVE_CUT)
			result = SOLVE_FAIL;
		break;

	case COMPILE_NOT_CALLABLE:
		result = throw_type_error(context,BOX_ATOM_BUILTIN(callable),goal);
		break;

	case COMPILE_ALWAYS_TRUE:
		result = stack_push_ptr(&context->m_call_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
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
		if (stack_push(&context->m_call_stack,stack_base) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_compile) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	if (result != SOLVE_TRUE)
		stack_reset(&context->m_call_stack,stack_base);

	return result;
}

enum eCompileResult compile_call(struct compile_context_t* context, const union box_t* goal)
{
	enum eCompileResult result;
	size_t frame;

	do
	{
		goal = deref_term(context->m_substs,first_arg(goal));

	} // Optimize call(call(_)) -> call(_)
	while (!get_debug_info(goal) && goal->m_u64val == BOX_COMPOUND_EMBED_4(1,'c','a','l','l'));

	switch (UNBOX_TYPE(goal->m_u64val))
	{
	case prolite_compound:
	case prolite_atom:
		frame = stack_top(context->m_emit_stack);
		if (stack_push_ptr(&context->m_emit_stack,&solve_call) == -1)
			result = COMPILE_NOMEM;
		else
		{
			result = compile(context,goal);
			if (result == COMPILE_ALWAYS_TRUE || result == COMPILE_ALWAYS_FAILS)
				stack_reset(&context->m_emit_stack,frame);
		}
		break;

	default:
		if (stack_push_ptr(&context->m_emit_stack,&solve_compile) == -1 ||
			stack_push_ptr(&context->m_emit_stack,goal) == -1)
		{
			result = COMPILE_NOMEM;
		}
		result = COMPILE_OK;
		break;
	}

	return result;
}

static enum eSolveResult redo_unify(struct context_t* context, int unwind)
{
	struct substs_t* prev_substs = stack_pop_ptr(&context->m_call_stack);
	if (prev_substs)
	{
		size_t stack_base = stack_pop(&context->m_call_stack);
		stack_reset(&context->m_call_stack,stack_base);
		context->m_substs = prev_substs;
	}

	return unwind ? SOLVE_UNWIND : SOLVE_FAIL;
}

static enum eSolveResult solve_unify(struct context_t* context, size_t frame)
{
	enum eSolveResult result;
	struct substs_t* prev_substs = context->m_substs;
	size_t stack_base = stack_top(context->m_call_stack);
	const union box_t *goal = first_arg(*(const union box_t**)stack_at(context->m_instr_stack,frame));

	if (prev_substs)
	{
		context->m_substs = stack_malloc(&context->m_call_stack,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
		if (!context->m_substs)
		{
			context->m_substs = prev_substs;
			return SOLVE_NOMEM;
		}
		memcpy(context->m_substs,prev_substs,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
	}

	result = unify(context,goal,next_arg(goal),0);
	if (result == SOLVE_TRUE)
	{
		if ((prev_substs && stack_push(&context->m_call_stack,stack_base) == -1) ||
			stack_push_ptr(&context->m_call_stack,prev_substs) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_unify) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	if (result != SOLVE_TRUE && prev_substs)
	{
		stack_reset(&context->m_call_stack,stack_base);
		context->m_substs = prev_substs;
	}

	return result;
}

static enum eSolveResult solve_not_unifiable(struct context_t* context, size_t frame)
{
	enum eSolveResult result;
	struct substs_t* prev_substs = context->m_substs;
	size_t stack_base = stack_top(context->m_call_stack);
	const union box_t *goal = first_arg(*(const union box_t**)stack_at(context->m_instr_stack,frame));

	if (prev_substs)
	{
		context->m_substs = stack_malloc(&context->m_call_stack,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
		if (!context->m_substs)
		{
			context->m_substs = prev_substs;
			return SOLVE_NOMEM;
		}
		memcpy(context->m_substs,prev_substs,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
	}

	result = unify(context,goal,next_arg(goal),0);
	if (result == SOLVE_TRUE)
		result = SOLVE_FAIL;
	else if (result == SOLVE_FAIL)
		result = SOLVE_TRUE;

	stack_reset(&context->m_call_stack,stack_base);
	context->m_substs = prev_substs;

	return result;
}

static enum eSolveResult solve_not_proveable(struct context_t* context, size_t frame)
{
	enum eSolveResult result = solve(context,frame);
	if (result == SOLVE_TRUE)
	{
		result = redo(context,1);
		if (result == SOLVE_UNWIND)
			result = SOLVE_FAIL;
	}
	else if (result == SOLVE_FAIL)
		result = SOLVE_TRUE;

	return result;
}

static enum eCompileResult compile_not_proveable(struct compile_context_t* context, const union box_t* goal)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_emit_stack);

	if (stack_push_ptr(&context->m_emit_stack,&solve_not_proveable) == -1)
		return COMPILE_NOMEM;

	goal = deref_term(context->m_substs,first_arg(goal));

	result = compile_call(context,goal);
	if (result == COMPILE_ALWAYS_TRUE)
	{
		stack_reset(&context->m_emit_stack,frame);
		result = COMPILE_ALWAYS_FAILS;
	}
	else if (result == COMPILE_ALWAYS_FAILS)
	{
		stack_reset(&context->m_emit_stack,frame);
		result = COMPILE_ALWAYS_TRUE;
	}

	return result;
}

static enum eSolveResult solve_once(struct context_t* context, size_t frame)
{
	enum eSolveResult result = solve(context,frame);
	if (result == SOLVE_TRUE)
	{
		result = redo(context,1);
		if (result == SOLVE_UNWIND)
			result = SOLVE_TRUE;
	}

	return result;
}

static enum eCompileResult compile_once(struct compile_context_t* context, const union box_t* goal)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_emit_stack);

	if (stack_push_ptr(&context->m_emit_stack,&solve_once) == -1)
		return COMPILE_NOMEM;

	do
	{
		goal = deref_term(context->m_substs,first_arg(goal));

	}  // Optimize once(once(_)) -> once(_)
	while (!get_debug_info(goal) && goal->m_u64val == BOX_COMPOUND_EMBED_4(1,'o','n','c','e'));

	result = compile_call(context,goal);
	if (result == COMPILE_ALWAYS_TRUE || result == COMPILE_ALWAYS_FAILS)
		stack_reset(&context->m_emit_stack,frame);

	return result;
}

static enum eSolveResult solve_halt(struct context_t* context, size_t frame)
{
	enum eSolveResult result = SOLVE_HALT;

	const union box_t* goal = *(const union box_t**)stack_at(context->m_instr_stack,frame);
	if (goal->m_u64val == BOX_COMPOUND_EMBED_4(1,'h','a','l','t'))
	{
		goal = deref_term(context->m_substs,first_arg(goal));

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

static inline enum eCompileResult compile_builtin(struct compile_context_t* context, enum eSolveResult (*solve_fn)(struct context_t*,size_t), const union box_t* goal)
{
	if (stack_push_ptr(&context->m_emit_stack,solve_fn) == -1 ||
		stack_push_ptr(&context->m_emit_stack,goal) == -1)
	{
		return COMPILE_NOMEM;
	}

	return COMPILE_OK;
}

#define DECLARE_BUILTIN_FUNCTION(f,n) \
enum eSolveResult solve_##f(struct context_t* context, size_t frame);

#include "builtin_functions.h"

enum eCompileResult compile_catch(struct compile_context_t* context, const union box_t* goal);
enum eCompileResult compile_user_defined(struct compile_context_t* context, const union box_t* goal);

enum eCompileResult compile(struct compile_context_t* context, const union box_t* goal)
{
	enum eCompileResult result;
	size_t frame = stack_top(context->m_emit_stack);
	int debug = 0;

	goal = deref_term(context->m_substs,goal);
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
	case (n): result = compile_##f(context,goal); break;

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,n) \
	case (n): result = compile_builtin(context,&solve_##f,goal); break; \
		break;

#include "builtin_functions.h"

	default:
		switch (UNBOX_TYPE(goal->m_u64val))
		{
		case prolite_var:
			result = compile_call(context,goal);
			break;

		case prolite_compound:
			if ((goal->m_u64val & BOX_COMPOUND_EMBED_4(0,'c','a','l','l')) == BOX_COMPOUND_EMBED_4(0,'c','a','l','l') ||
				(goal->m_u64val & BOX_COMPOUND_BUILTIN(call,0)) == BOX_COMPOUND_BUILTIN(call,0))
			{
				// TODO: call/N
				assert(0);
			}
			else
				result = compile_user_defined(context,goal);
			break;

		case prolite_atom:
			result = compile_user_defined(context,goal);
			break;

		default:
			result = COMPILE_NOT_CALLABLE;
			break;
		}
		break;
	}

	if (debug)
	{
		if (result != COMPILE_OK)
		{
			stack_reset(&context->m_emit_stack,frame);

			if (result == COMPILE_ALWAYS_FAILS)
			{
				// TODO: Emit tracepoint
			}
			else if (result == COMPILE_ALWAYS_TRUE)
			{
				// TODO: Emit tracepoint
			}
		}
	}

	return result;
}

static enum eSolveResult redo_prepare(struct context_t* context, int unwind)
{
	size_t stack_base = stack_pop(&context->m_call_stack);

	enum eSolveResult result = redo(context,unwind);
	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_call_stack,stack_base) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_prepare) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	if (result != SOLVE_TRUE)
		stack_reset(&context->m_call_stack,stack_base);

	return result;
}

static enum eSolveResult redo_never(struct context_t* context, int unwind)
{
	return stack_push_ptr(&context->m_call_stack,&redo_never) == -1 ? SOLVE_NOMEM : (unwind ? SOLVE_UNWIND : SOLVE_FAIL);
}

enum eSolveResult context_init(struct context_t* context)
{
	return stack_push_ptr(&context->m_call_stack,&redo_never) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
}

enum eSolveResult context_reset(struct context_t* context)
{
	enum eSolveResult result = redo(context,1);
	if (result == SOLVE_UNWIND)
		result = SOLVE_TRUE;

	stack_reset(&context->m_scratch_stack,0);
	stack_compact(context->m_scratch_stack);

	stack_compact(context->m_call_stack);

	return result;
}

static enum eSolveResult start(struct context_t* context, int unwind)
{
	size_t frame = stack_pop(&context->m_call_stack);
	if (!unwind)
		context->m_instr_stack = context->m_call_stack;

	return unwind ? SOLVE_UNWIND : solve(context,frame);
}

static enum eSolveResult start_true(struct context_t* context, int unwind)
{
	enum eSolveResult result;

	if (unwind)
		result = SOLVE_UNWIND;
	else if (stack_push_ptr(&context->m_call_stack,&redo_true) == -1)
		result = SOLVE_NOMEM;
	else
		result = SOLVE_TRUE;

	return result;
}

static enum eSolveResult start_fail(struct context_t* context, int unwind)
{
	return unwind ? SOLVE_UNWIND : SOLVE_FAIL;
}

enum eSolveResult prepare_term(struct context_t* context, struct stream_t* s, union box_t** term, const char*** varnames);

enum eSolveResult context_prepare(struct context_t* context, struct stream_t* s, const char*** varnames)
{
	size_t term_base = stack_top(context->m_call_stack);
	union box_t* goal = NULL;
	enum eSolveResult result = prepare_term(context,s,&goal,varnames);
	if (result == SOLVE_TRUE)
	{
		size_t frame = stack_top(context->m_call_stack);
		struct compile_context_t compile_context;

		compile_context.m_emit_stack = context->m_call_stack;
		compile_context.m_substs = context->m_substs;
		compile_context.m_module = context->m_module;

		switch (compile(&compile_context,goal))
		{
		case COMPILE_OK:
			if (stack_push(&context->m_call_stack,frame) == -1 ||
				stack_push_ptr(&context->m_call_stack,&start) == -1)
			{
				result = SOLVE_NOMEM;
			}
			else
				result = SOLVE_TRUE;
			break;

		case COMPILE_NOT_CALLABLE:
			printf("type_error(callable(G)).\n");

			result = throw_type_error(context,BOX_ATOM_BUILTIN(callable),goal);
			break;

		case COMPILE_ALWAYS_TRUE:
			result = stack_push_ptr(&context->m_call_stack,&start_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
			break;

		case COMPILE_ALWAYS_FAILS:
			result = stack_push_ptr(&context->m_call_stack,&start_fail) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
			break;

		case COMPILE_NOMEM:
			result = SOLVE_NOMEM;
			break;
		}
	}

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_call_stack,term_base) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_prepare) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	if (result != SOLVE_TRUE)
	{
		// TODO: Reset strings etc...
		stack_reset(&context->m_call_stack,term_base);
	}

	return result;
}

enum eSolveResult context_solve(struct context_t* context)
{
	return redo(context,0);
}
