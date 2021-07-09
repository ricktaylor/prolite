
#include "types.h"

#include <assert.h>
#include <string.h>
#include <stdarg.h>

static struct line_info_t* get_debug_info(const union packed_t* v)
{
	struct line_info_t* info = NULL;

	if (UNPACK_TYPE(v->m_u64val) == prolite_compound)
	{
		// Skip functor atom
		if ((UNPACK_HI16(v->m_u64val) & 0xC000) == 0)
			++v;
	}

	++v;
	if (UNPACK_TYPE(v->m_u64val) == PROLITE_DEBUG_INFO)
	{
		// TODO: Debug info
		info = NULL;
	}

	return info;
}

static enum eSolveResult emit_error_line_info(struct context_t* context, struct line_info_t* info, union packed_t** term, size_t* term_size)
{
	*term = stack_realloc(&context->m_scratch_stack,*term,*term_size * sizeof(union packed_t),((*term_size)+1) * sizeof(union packed_t));
	if (!*term)
		return SOLVE_NOMEM;

	if (!info)
		(*term)[(*term_size)++].m_u64val = PACK_ATOM_EMBED_5('f','a','l','s','e');
	else
	{
		// TODO: !!
		(*term)[(*term_size)++].m_u64val = PACK_ATOM_EMBED_4('T','o','d','o');
	}

	return SOLVE_TRUE;
}

static inline int append_packed_t(struct stack_t** stack, union packed_t const** v, union packed_t** term, size_t* term_size)
{
	*term = stack_realloc(stack,*term,*term_size * sizeof(union packed_t),((*term_size)+1) * sizeof(union packed_t));
	if (!*term)
		return -1;

	(*term)[(*term_size)++] = *((*v)++);
	return 0;
}

static int copy_term(struct context_t* context, struct string_ptr_t** strings, union packed_t const** v, union packed_t** new_term, size_t* term_size)
{
	int r = 0;
	enum tag_type_t type = UNPACK_TYPE((*v)->m_u64val);
	switch (type)
	{
	case prolite_compound:
		{
			uint64_t arity = UNPACK_MANT_48((*v)->m_u64val);
			unsigned int hi16 = (arity >> 32);
			if (hi16 & 0x8000)
				arity = (hi16 & (MAX_ARITY_EMBED << 11)) >> 11;
			else if ((hi16 & 0xC000) == 0x4000)
				arity = (hi16 & MAX_ARITY_BUILTIN);
			else
			{
				// Copy functor atom
				if (append_packed_t(&context->m_scratch_stack,v,new_term,term_size))
					return -1;
			}

			if (append_packed_t(&context->m_scratch_stack,v,new_term,term_size))
				return -1;

			if (!r && UNPACK_TYPE((*v)->m_u64val) == PROLITE_DEBUG_INFO)
			{
				// TODO: Debug info
			}

			while (arity--)
			{
				if (copy_term(context,strings,v,new_term,term_size) != 0)
					return -1;
			}
		}
		break;

	case prolite_var:
		{
			const union packed_t* ptr = deref_term(context->m_substs,*v);
			if (ptr == *v)
			{
				// TODO: Make this a better error!
				assert(0);

				return -2;
			}

			return copy_term(context,strings,&ptr,new_term,term_size);
		}
		break;

	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		{
			unsigned int hi16 = UNPACK_HI16((*v)->m_u64val);
			if (hi16 & 0xC000)
				r = append_packed_t(&context->m_scratch_stack,v,new_term,term_size);
			else
			{
				union packed_t b[1];
				struct string_ptr_t* s = unpack_pointer((*v)->m_u64val);
				s = pack_stack_string(&context->m_scratch_stack,strings,s->m_str,s->m_len);
				if (!s)
					return -1;

				b[0].m_u64val = PACK_TYPE(type) | pack_pointer(s);
				r = append_packed_t(&context->m_scratch_stack,(const union packed_t**)&b,new_term,term_size);
			}
		}
		break;

	default:
		r = append_packed_t(&context->m_scratch_stack,v,new_term,term_size);
	}

	if (!r && UNPACK_TYPE((*v)->m_u64val) == PROLITE_DEBUG_INFO)
	{
		// TODO: Debug info
	}

	return r;
}

enum eSolveResult emit_error(struct context_t* context, struct line_info_t* info, uint64_t error_functor, unsigned int arity, ...)
{
	enum eSolveResult result = SOLVE_TRUE;
	va_list args;
	unsigned int a;
	union packed_t** pball;
	size_t term_size = 2;
	struct string_ptr_t* strings = NULL;

	stack_reset(&context->m_scratch_stack,0);
	if (stack_push_ptr(&context->m_scratch_stack,NULL) == -1)
		return SOLVE_NOMEM;

	pball = (union packed_t**)stack_at(context->m_scratch_stack,0);
	*pball = stack_malloc(&context->m_scratch_stack,term_size * sizeof(union packed_t));
	if (!*pball)
		result = SOLVE_NOMEM;
	else
	{
		(*pball)[0].m_u64val = PACK_COMPOUND_EMBED_5(2,'e','r','r','o','r');
		(*pball)[1].m_u64val = error_functor;
	}

	va_start(args,arity);

	for (a = 0; result == SOLVE_TRUE && a < arity; ++a)
	{
		const union packed_t* v = va_arg(args,union packed_t*);
		if (copy_term(context,&strings,&v,pball,&term_size) != 0)
			result = SOLVE_NOMEM;
	}

	va_end(args);

	if (result == SOLVE_TRUE)
		result = emit_error_line_info(context,info,pball,&term_size);

	if (result == SOLVE_TRUE)
		result = SOLVE_THROW;

	return result;
}

enum eSolveResult throw_instantiation_error(struct context_t* context, const union packed_t* culprit)
{
	return emit_error(context,get_debug_info(culprit),PACK_ATOM_BUILTIN(instantiation_error),0);
}

enum eSolveResult throw_type_error(struct context_t* context, uint64_t valid_type, const union packed_t* culprit)
{
	union packed_t arg;
	arg.m_u64val = valid_type;
	return emit_error(context,get_debug_info(culprit),PACK_COMPOUND_BUILTIN(type_error,2),2,&arg,culprit);
}

enum eSolveResult throw_representation_error(struct context_t* context, uint64_t flag, const union packed_t* culprit)
{
	union packed_t arg;
	arg.m_u64val = flag;
	return emit_error(context,get_debug_info(culprit),PACK_COMPOUND_BUILTIN(representation_error,1),1,&arg);
}

enum eSolveResult throw_existence_error(struct context_t* context, uint64_t object_type, const union packed_t* culprit)
{
	union packed_t arg;
	arg.m_u64val = object_type;
	return emit_error(context,get_debug_info(culprit),PACK_COMPOUND_BUILTIN(existence_error,2),2,&arg,culprit);
}

enum eSolveResult throw_domain_error(struct context_t* context, uint64_t valid_domain, const union packed_t* culprit)
{
	union packed_t arg;
	arg.m_u64val = valid_domain;
	return emit_error(context,get_debug_info(culprit),PACK_COMPOUND_BUILTIN(domain_error,2),2,&arg,culprit);
}

enum eSolveResult throw_permission_error(struct context_t* context, uint64_t operation, uint64_t permission, const union packed_t* culprit)
{
	union packed_t args[2];
	args[0].m_u64val = operation;
	args[1].m_u64val = permission;
	return emit_error(context,get_debug_info(culprit),PACK_COMPOUND_BUILTIN(permission_error,3),3,&args[0],&args[1],culprit->m_u64val);
}

enum eSolveResult throw_evaluation_error(struct context_t* context, uint64_t error, const union packed_t* culprit)
{
	union packed_t arg;
	arg.m_u64val = error;
	return emit_error(context,get_debug_info(culprit),PACK_COMPOUND_BUILTIN(evaluation_error,1),1,&arg);
}

enum eSolveResult solve_throw(struct context_t* context, size_t frame)
{
	enum eSolveResult result = SOLVE_THROW;

	const union packed_t* ball = *(const union packed_t**)stack_at(context->m_instr_stack,frame);
	ball = deref_term(context->m_substs,first_arg(ball));

	// TODO: Check for uninstantiated variables in ball!

	stack_reset(&context->m_scratch_stack,0);
	if (stack_push_ptr(&context->m_scratch_stack,NULL) == -1)
		result = SOLVE_NOMEM;
	else
	{
		struct string_ptr_t* strings = NULL;
		size_t term_size = 0;
		union packed_t** scratch_ball = (union packed_t**)stack_at(context->m_scratch_stack,0);

		if (copy_term(context,&strings,&ball,scratch_ball,&term_size) != 0)
			result = SOLVE_NOMEM;
	}

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

static enum eSolveResult catch(struct context_t* context, enum eSolveResult result, const union packed_t* catcher, size_t recovery_frame)
{
	if (result == SOLVE_NOMEM)
	{
		union packed_t arg;
		arg.m_u64val = PACK_ATOM_BUILTIN(memory);
		result = emit_error(context,NULL,PACK_COMPOUND_BUILTIN(resource_error,1),1,&arg);
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
			context->m_substs = stack_malloc(&context->m_call_stack,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union packed_t*)));
			if (!context->m_substs)
			{
				context->m_substs = prev_substs;
				result = SOLVE_NOMEM;
			}
			else
				memcpy(context->m_substs,prev_substs,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union packed_t*)));
		}

		if (result == SOLVE_THROW)
		{
			const union packed_t* ball = *(const union packed_t**)stack_at(context->m_scratch_stack,0);

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
	union packed_t* catcher = stack_pop_ptr(&context->m_call_stack);
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

	const union packed_t* catcher = deref_term(context->m_substs,*(const union packed_t**)stack_at(context->m_instr_stack,frame));
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

enum eCompileResult compile_catch(struct compile_context_t* context, const union packed_t* goal)
{
	enum eCompileResult result;
	const union packed_t *catcher, *recovery;
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
