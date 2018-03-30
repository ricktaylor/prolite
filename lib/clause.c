
#include "clause.h"
#include "throw.h"

union box_t* next_value(union box_t* v)
{
	enum tag_type_t type = UNBOX_TYPE(v->m_u64val);
	if (type == prolite_compound)
	{
		uint64_t all48 = UNBOX_MANT_48(v->m_u64val);
		uint64_t arity;
		unsigned int hi16 = (all48 >> 32);
		if (hi16 & 0x8000)
			arity = (hi16 & (MAX_ARITY_EMBED << 11)) >> 11;
		else if ((hi16 & 0xC000) == 0x4000)
			arity = (hi16 & MAX_ARITY_BUILTIN);
		else
		{
			arity = all48 & MAX_ARITY;
			++v;
		}

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
	switch (UNBOX_TYPE(v->m_u64val))
	{
	case prolite_var:
		return -1;

	case prolite_compound:
		if (v->m_u64val == BOX_COMPOUND_EMBED_1(2,',') ||
				v->m_u64val == BOX_COMPOUND_EMBED_1(2,';') ||
				v->m_u64val == BOX_COMPOUND_EMBED_2(2,'-','>'))
		{
			int r = check_callable_term(v + 1);
			if (!r)
				r = check_callable_term(next_value(v + 1));
			return r;
		}
		return 0;

	case prolite_atom:
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

	if (term->m_value->m_u64val == BOX_COMPOUND_EMBED_2(2,':','-'))
	{
		head = term->m_value + 1;
		body = next_value(head);
	}

	switch (UNBOX_TYPE(head->m_u64val))
	{
	case prolite_var:
		return throw_instantiation_error(context);

	case prolite_compound:
	case prolite_atom:
		break;

	default:
		return throw_type_error(context,BOX_ATOM_BUILTIN(callable),head);
	}

	/* TODO: Check for static procedure */

	if (!body)
	{
		if (stack_push(&context->m_exec_stack,BOX_ATOM_EMBED_4('t','r','u','e')) == -1)
			return -1;

		body = next_value(head);
	}
	else if (UNBOX_TYPE(body->m_u64val) == prolite_var)
	{
		/* Convert to call(V) */
		if (stack_push(&context->m_exec_stack,body->m_u64val) == -1)
		{
			stack_reset(&context->m_exec_stack,stack_base);
			return -1;
		}
		t.m_u64val = BOX_COMPOUND_EMBED_4(1,'c','a','l','l');
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
			return throw_type_error(context,BOX_ATOM_BUILTIN(callable),body);
		}
	}

	/* TODO Make a clause(head,body)!! */

	return 0;
}
