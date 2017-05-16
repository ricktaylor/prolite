/*
 * context.c
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#include "context.h"

#include <assert.h>

uint32_t convert_char(struct context_t* context, uint32_t in_char)
{
	return in_char;
}

int char_conversion_2(struct context_t* context, struct term_t* term)
{
	uint32_t in_char = -1;
	uint32_t out_char = -1;

	/* Check value[0] first, otherwise we don't know what value[1] is! */
	if ((term->m_value[0].m_uval & BOX_TAG_MASK) == BOX_TAG_VAR)
	{
		/* TODO: instantiation_error */
		return 1;
	}

	if ((term->m_value[0].m_uval & BOX_TAG_ATOM_EMBED) != BOX_TAG_ATOM_EMBED ||
			(in_char = embed_string_code(&term->m_value[0])) == -1)
	{
		/* TODO: representation_error(character) */
		return 1;
	}

	if ((term->m_value[1].m_uval & BOX_TAG_MASK) == BOX_TAG_VAR)
	{
		/* TODO: instantiation_error */
		return 1;
	}

	if ((term->m_value[1].m_uval & BOX_TAG_ATOM_EMBED) != BOX_TAG_ATOM_EMBED ||
			(out_char = embed_string_code(&term->m_value[1])) == -1)
	{
		/* TODO: representation_error(character) */
		return 1;
	}

	if (in_char == out_char)
	{
		/* TODO: Remove in_char from the char_conversion table */
	}
	else
	{
		/* TODO: Update the char_conversion table */
	}

	return 0;
}

/* Try to find a infix/suffix op, otherwise find prefix */
struct operator_t* lookup_op(struct context_t* context, const union box_t* b)
{
	return NULL;
}

/* Try to find a prefix op, otherwise find infix/suffix */
struct operator_t* lookup_prefix_op(struct context_t* context, const union box_t* b)
{
	return NULL;
}

static int add_op(struct context_t* context, unsigned int priority, enum eOpSpec op_spec, uint64_t atom)
{
	/* TODO */
	return -1;
}

static int remove_op(struct context_t* context, enum eOpSpec op_spec, uint64_t atom)
{
	/* TODO */
	return -1;
}

int op_3(struct context_t* context, struct term_t* term)
{
	/* Check value[0] first, otherwise we don't know what value[1 and 2] is! */
	int priority;
	enum eOpSpec op_spec;

	if ((term->m_value[0].m_uval & BOX_TAG_MASK) == BOX_TAG_VAR)
	{
		/* TODO: instantiation_error */
		return 1;
	}

	if ((term->m_value[0].m_uval & BOX_TAG_INT32) != BOX_TAG_INT32)
	{
		/* TODO: type_error(integer,value[0]) */
		return 1;
	}

	priority = unbox_int32(&term->m_value[0]);
	if (priority < 0 || priority > 1200)
	{
		/* TODO: domain_error(operator_priority,value[0]) */
		return -1;
	}

	if ((term->m_value[1].m_uval & BOX_TAG_MASK) == BOX_TAG_VAR)
	{
		/* TODO: instantiation_error */
		return 1;
	}

	if ((term->m_value[1].m_uval & BOX_TAG_ATOM_EMBED) != BOX_TAG_ATOM_EMBED)
	{
		/* TODO: type_error(atom,value[1]) */
		return 1;
	}

	switch (term->m_value[1].m_uval)
	{
	case BOX_ATOM_EMBED_2('f','x'):
		op_spec = eFX;
		break;
	case BOX_ATOM_EMBED_2('f','y'):
		op_spec = eFY;
		break;
	case BOX_ATOM_EMBED_3('x','f','x'):
		op_spec = eXFX;
		break;
	case BOX_ATOM_EMBED_3('x','f','y'):
		op_spec = eXFY;
		break;
	case BOX_ATOM_EMBED_3('y','f','x'):
		op_spec = eYFX;
		break;
	case BOX_ATOM_EMBED_2('x','f'):
		op_spec = eXF;
		break;
	case BOX_ATOM_EMBED_2('y','f'):
		op_spec = eYF;
		break;

	default:
		/* TODO: domain_error(operator_specifier,value[1]) */
		return 1;
	}

	if ((term->m_value[2].m_uval & BOX_TAG_MASK) == BOX_TAG_VAR ||
			term->m_value[2].m_uval == BOX_COMPOUND_EMBED_1(2,'|'))
	{
		/* TODO: instantiation_error */
		return 1;
	}

	if ((term->m_value[2].m_uval & BOX_TAG_MASK) == BOX_TAG_ATOM)
	{
		if (priority == 0)
			return remove_op(context,op_spec,term->m_value[2].m_uval);
		return add_op(context,priority,op_spec,term->m_value[2].m_uval);
	}
	else if (term->m_value[2].m_uval == BOX_COMPOUND_EMBED_1(2,'.'))
	{
		const union box_t* list = term->m_value + 2;
		do
		{
			/* List - enumerate */
			if ((list[1].m_uval & BOX_TAG_MASK) == BOX_TAG_VAR)
			{
				/* TODO: instantiation_error */
				return 1;
			}

			if ((list[1].m_uval & BOX_TAG_MASK) == BOX_TAG_ATOM)
			{
				int err;
				if (priority == 0)
					err = remove_op(context,op_spec,term->m_value[2].m_uval);
				else
					err = add_op(context,priority,op_spec,term->m_value[2].m_uval);
				if (err)
					return err;
			}
			else
			{
				/* TODO: type_error(atom,list[1]) */
				return 1;
			}

			if (list[2].m_uval == BOX_ATOM_EMBED_2('[',']'))
				return 0;

			list += 2;
		}
		while (list->m_uval == BOX_COMPOUND_EMBED_1(2,'.'));

		if ((list->m_uval & BOX_TAG_MASK) == BOX_TAG_VAR ||
				list->m_uval == BOX_COMPOUND_EMBED_1(2,'|'))
		{
			/* TODO: instantiation_error */
			return 1;
		}

		/* TODO: type_error(atom,list->m_uval) */
		return 1;
	}

	/* TODO: type_error(list,value[2]) */
	return 1;
}

/* 'Do' a directive */
int directive(struct context_t* context, struct term_t* term)
{
	if (term->m_value->m_uval == BOX_COMPOUND_EMBED_2(3,'o','p'))
	{
		++term->m_value;
		return op_3(context,term);
	}

	if (term->m_value->m_uval == (UINT64_C(0xFFF6) << 48 | 1))
	{
		switch (term->m_value[1].m_uval)
		{
		case BUILTIN_ATOM(dynamic):
		case BUILTIN_ATOM(multifile):
		case BUILTIN_ATOM(discontiguous):
		case BUILTIN_ATOM(initialization):
		case BUILTIN_ATOM(include):
		case BUILTIN_ATOM(ensure_loaded):
		default:
			break;
		}
	}
	else if (term->m_value->m_uval == (UINT64_C(0xFFF6) << 48 | 2))
	{
		switch (term->m_value[1].m_uval)
		{
		case BUILTIN_ATOM(char_conversion):
			term->m_value += 2;
			return char_conversion_2(context,term);

		case BUILTIN_ATOM(set_prolog_flag):
		default:
			break;
		}
	}

	/* TODO: push error */
	return 1;
}

/* Assert a clause */
int assert_clause(struct context_t* context, struct term_t* term, int z)
{

}
