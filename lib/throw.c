/*
 * throw.c
 *
 *  Created on: 16 May 2017
 *      Author: rick
 */

#include "throw.h"

// TODO: These are all wrong!

static int emit_compound(struct context_t* context, uint64_t functor, uint64_t arity)
{
	int r;
	if (arity <= MAX_EMBED_ARITY && UNBOX_IS_TYPE_EMBED(functor,prolite_atom))
	{
		// Convert embed atom to embed compound
		uint16_t hi16 = UNBOX_HI16(functor);
		hi16 |= (arity < 11);

		r = (stack_push(&context->m_scratch_stack,BOX_TYPE(prolite_compound) | BOX_HI16(hi16) | UNBOX_LOW32(functor)) == -1 ? -1 : 0);
	}
	else if (arity < MAX_BUILTIN_ARITY && UNBOX_IS_TYPE_BUILTIN(functor,prolite_atom))
	{
		// Convert builtin atom to builtin compound
		r = (stack_push(&context->m_scratch_stack,BOX_TYPE(prolite_compound) | BOX_HI16(0x4000 | arity) | UNBOX_LOW32(functor)) == -1 ? -1 : 0);
	}
	else
	{
		r = (stack_push(&context->m_scratch_stack,BOX_TYPE(prolite_compound) | BOX_MANT_48(arity)) == -1 ? -1 : 0);
		if (!r)
			r = (stack_push(&context->m_scratch_stack,functor) == -1 ? -1 : 0);
	}
	return r;
}

static int copy_to_scratch(struct context_t* context, const union box_t* b)
{
	/* TODO */
	return -1;
}

static int emit_info(struct context_t* context)
{
	/* TODO: line info */
	return (stack_push(&context->m_scratch_stack,BOX_ATOM_EMBED_4('i','n','f','o')) == -1 ? -1 : 0);
}

int throw_instantiation_error(struct context_t* context)
{
	int r = emit_compound(context,BOX_ATOM_EMBED_5('e','r','r','o','r'),2);
	if (!r)
		r = (stack_push(&context->m_scratch_stack,BOX_BUILTIN_ATOM(instantiation_error)) == -1 ? -1 : 0);
	if (!r)
		r = emit_info(context);
	return r;
}

int throw_type_error(struct context_t* context, uint64_t valid_type_atom, const union box_t* culprit)
{
	int r = emit_compound(context,BOX_ATOM_EMBED_5('e','r','r','o','r'),2);
	if (!r)
		r = emit_compound(context,BOX_BUILTIN_ATOM(type_error),2);
	if (!r)
		r = (stack_push(&context->m_scratch_stack,valid_type_atom) == -1 ? -1 : 0);
	if (!r)
		r = copy_to_scratch(context,culprit);
	if (!r)
		r = emit_info(context);
	return r;
}

int throw_domain_error(struct context_t* context, uint64_t valid_domain_atom, const union box_t* culprit)
{
	int r = emit_compound(context,BOX_ATOM_EMBED_5('e','r','r','o','r'),2);
	if (!r)
		r = emit_compound(context,BOX_BUILTIN_ATOM(domain_error),2);
	if (!r)
		r = (stack_push(&context->m_scratch_stack,valid_domain_atom) == -1 ? -1 : 0);
	if (!r)
		r = copy_to_scratch(context,culprit);
	if (!r)
		r = emit_info(context);
	return r;
}

int throw_existence_error(struct context_t* context, uint64_t object_atom, const union box_t* culprit)
{
	int r = emit_compound(context,BOX_ATOM_EMBED_5('e','r','r','o','r'),2);
	if (!r)
		r = emit_compound(context,BOX_BUILTIN_ATOM(existence_error),2);
	if (!r)
		r = (stack_push(&context->m_scratch_stack,object_atom) == -1 ? -1 : 0);
	if (!r)
		r = copy_to_scratch(context,culprit);
	if (!r)
		r = emit_info(context);
	return r;
}

int throw_permission_error(struct context_t* context, uint64_t operation_atom, uint64_t permission_atom, const union box_t* culprit)
{
	int r = emit_compound(context,BOX_ATOM_EMBED_5('e','r','r','o','r'),2);
	if (!r)
		r = emit_compound(context,BOX_BUILTIN_ATOM(permission_error),3);
	if (!r)
		r = (stack_push(&context->m_scratch_stack,operation_atom) == -1 ? -1 : 0);
	if (!r)
		r = (stack_push(&context->m_scratch_stack,permission_atom) == -1 ? -1 : 0);
	if (!r)
		r = copy_to_scratch(context,culprit);
	if (!r)
		r = emit_info(context);
	return r;
}

int throw_representation_error(struct context_t* context, uint64_t flag_atom)
{
	int r = emit_compound(context,BOX_ATOM_EMBED_5('e','r','r','o','r'),2);
	if (!r)
		r = emit_compound(context,BOX_BUILTIN_ATOM(representation_error),1);
	if (!r)
		r = (stack_push(&context->m_scratch_stack,flag_atom) == -1 ? -1 : 0);
	if (!r)
		r = emit_info(context);
	return r;
}

int throw_evaluation_error(struct context_t* context, uint64_t error_atom)
{
	int r = emit_compound(context,BOX_ATOM_EMBED_5('e','r','r','o','r'),2);
	if (!r)
		r = emit_compound(context,BOX_BUILTIN_ATOM(evaluation_error),1);
	if (!r)
		r = (stack_push(&context->m_scratch_stack,error_atom) == -1 ? -1 : 0);
	if (!r)
		r = emit_info(context);
	return r;
}

int throw_oom_error(struct context_t* context)
{
	int r = emit_compound(context,BOX_ATOM_EMBED_5('e','r','r','o','r'),2);
	if (!r)
		r = emit_compound(context,BOX_BUILTIN_ATOM(resource_error),1);
	if (!r)
		r = (stack_push(&context->m_scratch_stack,BOX_BUILTIN_ATOM(memory)) == -1 ? -1 : 0);
	if (!r)
		r = emit_info(context);
	return r;
}
