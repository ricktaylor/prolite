
#include "throw.h"

static union box_t* next_value(union box_t* v)
{
	if ((v->m_uval & BOX_TAG_MASK) == BOX_TAG_COMPOUND)
	{
		uint64_t arity = compound_arity(v);
		while (arity--)
			v = next_value(v);
	}
	else
		++v;
	return v;
}

/* Assert a clause */
int assert_clause(struct context_t* context, struct term_t* term, int z)
{
	union box_t* head;
	union box_t* body;
	union box_t t;
	uint64_t stack_base = stack_top(context->m_exec_stack);

	if (term->m_value->m_uval == BOX_COMPOUND_EMBED_2(2,':','-'))
		head = term->m_value + 1;
	else
	{
		if (stack_push(&context->m_exec_stack,BOX_ATOM_EMBED_4('t','r','u','e')) == -1)
			return -1;
		head = term->m_value;
	}

	switch (head->m_uval & BOX_TAG_MASK)
	{
	case BOX_TAG_VAR:
		stack_reset(&context->m_exec_stack,stack_base);
		return throw_instantiation_error(context);

	case BOX_TAG_COMPOUND:
	case BOX_TAG_ATOM:
		break;

	default:
		stack_reset(&context->m_exec_stack,stack_base);
		return throw_type_error(context,BUILTIN_ATOM(callable),head);
	}

	/* TODO: Check for static procedure */

	body = next_value(head);

	switch (body->m_uval & BOX_TAG_MASK)
	{
	case BOX_TAG_VAR:
		/* Convert to call(V) */
		if (stack_push(&context->m_exec_stack,body->m_uval) == -1)
		{
			stack_reset(&context->m_exec_stack,stack_base);
			return -1;
		}
		t.m_uval = BOX_COMPOUND_EMBED_4(1,'c','a','l','l');
		body = &t;
		++body;
		break;

	case BOX_TAG_COMPOUND:
	case BOX_TAG_ATOM:
		break;

	default:
		stack_reset(&context->m_exec_stack,stack_base);
		return throw_type_error(context,BUILTIN_ATOM(callable),body);
	}

	/* TODO Make a clause!! */

	return 0;
}
