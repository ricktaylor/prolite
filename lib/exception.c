
#include "types.h"

#include <assert.h>
#include <string.h>
#include <stdarg.h>

static struct line_info_t* get_debug_info(const union box_t* v)
{
	struct line_info_t* info = NULL;

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
		info = NULL;
	}

	return info;
}

static enum eSolveResult emit_error_line_info(struct context_t* context, struct line_info_t* info, union box_t** term, size_t* term_size)
{
	*term = stack_realloc(&context->m_scratch_stack,*term,*term_size * sizeof(union box_t),((*term_size)+1) * sizeof(union box_t));
	if (!*term)
		return SOLVE_NOMEM;

	if (!info)
		(*term)[(*term_size)++].m_u64val = BOX_ATOM_EMBED_5('f','a','l','s','e');
	else
	{
		// TODO: !!
		(*term)[(*term_size)++].m_u64val = BOX_ATOM_EMBED_4('T','o','d','o');
	}

	return SOLVE_TRUE;
}

enum eSolveResult emit_error(struct context_t* context, struct line_info_t* info, uint64_t error_functor, unsigned int arity, ...)
{
	enum eSolveResult result = SOLVE_TRUE;
	va_list args;
	unsigned int a;
	union box_t** pball;
	size_t term_size = 2;
	struct string_ptr_t* strings = NULL;

	stack_reset(&context->m_scratch_stack,0);
	if (stack_push_ptr(&context->m_scratch_stack,NULL) == -1)
		return SOLVE_NOMEM;

	pball = (union box_t**)stack_at(context->m_scratch_stack,0);
	*pball = stack_malloc(&context->m_scratch_stack,term_size * sizeof(union box_t));
	if (!*pball)
		result = SOLVE_NOMEM;
	else
	{
		(*pball)[0].m_u64val = BOX_COMPOUND_EMBED_5(2,'e','r','r','o','r');
		(*pball)[1].m_u64val = error_functor;
	}

	va_start(args,arity);

	for (a = 0; result == SOLVE_TRUE && a < arity; ++a)
	{
		const union box_t* v = va_arg(args,union box_t*);
		if (copy_term_append(context->m_substs,&context->m_scratch_stack,&strings,&v,pball,&term_size) != 0)
			result = SOLVE_NOMEM;
	}

	va_end(args);

	if (result == SOLVE_TRUE)
		result = emit_error_line_info(context,info,pball,&term_size);

	if (result == SOLVE_TRUE)
		result = SOLVE_THROW;

	return result;
}

enum eSolveResult throw_instantiation_error(struct context_t* context, const union box_t* culprit)
{
	return emit_error(context,get_debug_info(culprit),BOX_ATOM_BUILTIN(instantiation_error),0);
}

enum eSolveResult throw_type_error(struct context_t* context, uint64_t valid_type, const union box_t* culprit)
{
	union box_t arg;
	arg.m_u64val = valid_type;
	return emit_error(context,get_debug_info(culprit),BOX_COMPOUND_BUILTIN(type_error,2),2,&arg,culprit);
}

enum eSolveResult throw_representation_error(struct context_t* context, uint64_t flag, const union box_t* culprit)
{
	union box_t arg;
	arg.m_u64val = flag;
	return emit_error(context,get_debug_info(culprit),BOX_COMPOUND_BUILTIN(representation_error,1),1,&arg);
}

enum eSolveResult throw_existence_error(struct context_t* context, uint64_t object_type, const union box_t* culprit)
{
	union box_t arg;
	arg.m_u64val = object_type;
	return emit_error(context,get_debug_info(culprit),BOX_COMPOUND_BUILTIN(existence_error,2),2,&arg,culprit);
}

enum eSolveResult throw_domain_error(struct context_t* context, uint64_t valid_domain, const union box_t* culprit)
{
	union box_t arg;
	arg.m_u64val = valid_domain;
	return emit_error(context,get_debug_info(culprit),BOX_COMPOUND_BUILTIN(domain_error,2),2,&arg,culprit);
}

enum eSolveResult throw_permission_error(struct context_t* context, uint64_t operation, uint64_t permission, const union box_t* culprit)
{
	union box_t args[2];
	args[0].m_u64val = operation;
	args[1].m_u64val = permission;
	return emit_error(context,get_debug_info(culprit),BOX_COMPOUND_BUILTIN(permission_error,3),3,&args[0],&args[1],culprit->m_u64val);
}

enum eSolveResult throw_evaluation_error(struct context_t* context, uint64_t error, const union box_t* culprit)
{
	union box_t arg;
	arg.m_u64val = error;
	return emit_error(context,get_debug_info(culprit),BOX_COMPOUND_BUILTIN(evaluation_error,1),1,&arg);
}

enum eSolveResult solve_throw(struct context_t* context, size_t frame)
{
	enum eSolveResult result = SOLVE_THROW;
	const union box_t* ball = *(const union box_t**)stack_at(context->m_instr_stack,frame);
	struct string_ptr_t* strings = NULL;
	union box_t** scratch_ball;

	ball = deref_term(context->m_substs,first_arg(ball));

	stack_reset(&context->m_scratch_stack,0);
	if (stack_push_ptr(&context->m_scratch_stack,NULL) == -1)
		return SOLVE_NOMEM;

	scratch_ball = (union box_t**)stack_at(context->m_scratch_stack,0);
	*scratch_ball = copy_term(context->m_substs,&context->m_scratch_stack,&strings,ball);
	if (!*scratch_ball)
		result = SOLVE_NOMEM;

	return result;
}

static enum eSolveResult redo_recovery(struct context_t* context, int unwind)
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

static enum eSolveResult catch(struct context_t* context, enum eSolveResult result, const union box_t* catcher, size_t recovery_frame)
{
	if (result == SOLVE_NOMEM)
	{
		union box_t arg;
		arg.m_u64val = BOX_ATOM_BUILTIN(memory);
		result = emit_error(context,NULL,BOX_COMPOUND_BUILTIN(resource_error,1),1,&arg);
		if (result == SOLVE_TRUE)
			result = SOLVE_THROW;
	}

	if (result == SOLVE_THROW)
	{
		// Unify catcher and ball
		struct substs_t* prev_substs = context->m_substs;
		size_t stack_base = stack_top(context->m_call_stack);

		if (prev_substs)
		{
			context->m_substs = stack_malloc(&context->m_call_stack,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
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
			const union box_t* ball = *(const union box_t**)stack_at(context->m_scratch_stack,0);

			result = unify(context->m_substs,catcher,ball);
			if (result == SOLVE_FAIL)
				result = SOLVE_THROW;
			else if (result == SOLVE_TRUE)
			{
				if (recovery_frame == -1)
					result = SOLVE_FAIL;
				else if (!recovery_frame)
					result = stack_push_ptr(&context->m_call_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
				else if (recovery_frame)
					result = solve(context,recovery_frame);

				if (result == SOLVE_TRUE)
				{
					if ((prev_substs && stack_push(&context->m_call_stack,stack_base) == -1) ||
						stack_push_ptr(&context->m_call_stack,prev_substs) == -1 ||
						stack_push_ptr(&context->m_call_stack,&redo_recovery) == -1)
					{
						result = SOLVE_NOMEM;
					}
				}
			}
		}

		if (result != SOLVE_TRUE && prev_substs)
		{
			stack_reset(&context->m_call_stack,stack_base);
			context->m_substs = prev_substs;
		}
	}

	return result;
}

static enum eSolveResult redo_catch(struct context_t* context, int unwind)
{
	union box_t* catcher = stack_pop_ptr(&context->m_call_stack);
	size_t recovery_frame = stack_pop(&context->m_call_stack);

	enum eSolveResult result = redo(context,unwind);
	if (result == SOLVE_NOMEM || result == SOLVE_THROW)
		return catch(context,result,catcher,recovery_frame);

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_call_stack,recovery_frame) == -1 ||
			stack_push_ptr(&context->m_call_stack,catcher) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_catch) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static enum eSolveResult solve_catch(struct context_t* context, size_t frame)
{
	enum eSolveResult result;

	const union box_t* catcher = deref_term(context->m_substs,*(const union box_t**)stack_at(context->m_instr_stack,frame));
	size_t recovery_frame = *(const size_t*)stack_at(context->m_instr_stack,frame+1);
	if (recovery_frame && recovery_frame != -1)
		recovery_frame += (frame-1);

	result = solve(context,frame+2);
	if (result == SOLVE_NOMEM || result == SOLVE_THROW)
		return catch(context,result,catcher,recovery_frame);

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_call_stack,recovery_frame) == -1 ||
			stack_push_ptr(&context->m_call_stack,catcher) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_catch) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

enum eCompileResult compile_catch(struct compile_context_t* context, const union box_t* goal)
{
	enum eCompileResult result;
	const union box_t *catcher, *recovery;
	size_t frame = stack_top(context->m_emit_stack);

	goal = first_arg(goal);
	catcher = next_arg(goal);
	recovery = next_arg(catcher);

	catcher = deref_term(context->m_substs,catcher);
	recovery = deref_term(context->m_substs,recovery);

	if (stack_push_ptr(&context->m_emit_stack,&solve_catch) == -1 ||
		stack_push_ptr(&context->m_emit_stack,catcher) == -1 ||
		stack_push(&context->m_emit_stack,0) == -1)
	{
		result = COMPILE_NOMEM;
	}

	result = compile_call(context,goal);
	if (result == COMPILE_ALWAYS_TRUE || result == COMPILE_ALWAYS_FAILS)
		stack_reset(&context->m_emit_stack,frame);
	else if (result == COMPILE_OK)
	{
		size_t* recovery_frame = (size_t*)stack_at(context->m_emit_stack,frame+2);
		*recovery_frame = stack_top(context->m_emit_stack) - frame;

		result = compile_call(context,recovery);
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

const char* unpack_exception(struct context_t* context)
{
	/* Unpack the exception onto the scratch stack */

	return "exception!";
}
