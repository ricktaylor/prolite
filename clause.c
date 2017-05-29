
#include "clause.h"
#include "throw.h"

static union box_t* next_value(union box_t* v)
{
	if ((v->m_uval & BOX_TAG_MASK) == BOX_TAG_COMPOUND)
	{
		uint64_t arity = compound_arity(v);
		if ((v->m_uval & BOX_TAG_COMPOUND_EMBED) != BOX_TAG_COMPOUND_EMBED)
			++v;

		while (arity--)
			v = next_value(v);
	}
	else
		++v;
	return v;
}

/* Returns -1 on instantiation error, 1 on callable error, 0 ok */
int check_callable_term(union box_t* v)
{
	switch (v->m_uval & BOX_TAG_MASK)
	{
	case BOX_TAG_VAR:
		return -1;

	case BOX_TAG_COMPOUND:
		if (v->m_uval == BOX_COMPOUND_EMBED_1(2,',') ||
				v->m_uval == BOX_COMPOUND_EMBED_1(2,';') ||
				v->m_uval == BOX_COMPOUND_EMBED_2(2,'-','>'))
		{
			int r = check_callable_term(v + 1);
			if (!r)
				r = check_callable_term(next_value(v + 1));
			return r;
		}
		return 0;

	case BOX_TAG_ATOM:
		return 0;

	default:
		return 1;
	}
}

/* Assert a clause */
int assert_clause(struct context_t* context, struct term_t* term, int z)
{
	union box_t* head = term->m_value;
	union box_t* body = NULL;
	union box_t t;
	uint64_t stack_base = stack_top(context->m_exec_stack);

	if (term->m_value->m_uval == BOX_COMPOUND_EMBED_2(2,':','-'))
	{
		head = term->m_value + 1;
		body = next_value(head);
	}

	switch (head->m_uval & BOX_TAG_MASK)
	{
	case BOX_TAG_VAR:
		return throw_instantiation_error(context);

	case BOX_TAG_COMPOUND:
	case BOX_TAG_ATOM:
		break;

	default:
		return throw_type_error(context,BUILTIN_ATOM(callable),head);
	}

	/* TODO: Check for static procedure */

	if (!body)
	{
		if (stack_push(&context->m_exec_stack,BOX_ATOM_EMBED_4('t','r','u','e')) == -1)
			return -1;

		body = next_value(head);
	}
	else if ((body->m_uval & BOX_TAG_MASK) == BOX_TAG_VAR)
	{
		/* Convert to call(V) */
		if (stack_push(&context->m_exec_stack,body->m_uval) == -1)
		{
			stack_reset(&context->m_exec_stack,stack_base);
			return -1;
		}
		t.m_uval = BOX_COMPOUND_EMBED_4(1,'c','a','l','l');
		body = &t;
		++body;
	}
	else
	{
		switch (check_callable_term(body))
		{
		case -1:
			stack_reset(&context->m_exec_stack,stack_base);
			return throw_instantiation_error(context);

		case 1:
			stack_reset(&context->m_exec_stack,stack_base);
			return throw_type_error(context,BUILTIN_ATOM(callable),body);
		}
	}

	/* TODO Make a clause(head,body)!! */

	return 0;
}
