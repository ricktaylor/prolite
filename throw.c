/*
 * throw.c
 *
 *  Created on: 16 May 2017
 *      Author: rick
 */

#include "throw.h"

static int emit_compound(struct context_t* context, uint64_t functor, uint64_t arity)
{
	if (arity < 16 && (functor & BOX_TAG_ATOM_EMBED) == BOX_TAG_ATOM_EMBED)
	{
		uint64_t len = (functor & ((uint64_t)7 << 40)) << 4;
		uint64_t val = BOX_TAG_COMPOUND_EMBED | (functor & ~(UINT64_C(0xFFFFFF) << 40)) | len | (arity << 40);
		return (stack_push(&context->m_scratch_stack,val) == -1 ? -1: 0);
	}
	else
	{
		int r = (stack_push(&context->m_scratch_stack,BOX_TAG_COMPOUND | arity) == -1 ? -1 : 0);
		if (!r)
			r = (stack_push(&context->m_scratch_stack,functor) == -1 ? -1 : 0);
		return r;
	}
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
		r = (stack_push(&context->m_scratch_stack,BUILTIN_ATOM(instantiation_error)) == -1 ? -1 : 0);
	if (!r)
		r = emit_info(context);
	return r;
}

int throw_type_error(struct context_t* context, uint64_t valid_type_atom, const union box_t* culprit)
{
	int r = emit_compound(context,BOX_ATOM_EMBED_5('e','r','r','o','r'),2);
	if (!r)
		r = emit_compound(context,BUILTIN_ATOM(type_error),2);
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
		r = emit_compound(context,BUILTIN_ATOM(domain_error),2);
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
		r = emit_compound(context,BUILTIN_ATOM(existence_error),2);
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
		r = emit_compound(context,BUILTIN_ATOM(permission_error),3);
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
		r = emit_compound(context,BUILTIN_ATOM(representation_error),1);
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
		r = emit_compound(context,BUILTIN_ATOM(evaluation_error),1);
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
		r = emit_compound(context,BUILTIN_ATOM(resource_error),1);
	if (!r)
		r = (stack_push(&context->m_scratch_stack,BUILTIN_ATOM(memory)) == -1 ? -1 : 0);
	if (!r)
		r = emit_info(context);
	return r;
}
