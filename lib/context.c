/*
 * context.c
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#include "context.h"
#include "throw.h"

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
	enum tag_type_t type = UNBOX_TYPE(term->m_value[0].m_u64val);
	if (type == prolite_var)
		return throw_instantiation_error(context);

	if (type != prolite_atom ||
		(in_char = embed_string_code(&term->m_value[0])) == -1)
	{
		return throw_representation_error(context,BUILTIN_ATOM(character));
	}

	type = UNBOX_TYPE(term->m_value[1].m_u64val);
	if (type == prolite_var)
		return throw_instantiation_error(context);

	if (type != prolite_atom ||
		(out_char = embed_string_code(&term->m_value[1])) == -1)
	{
		return throw_representation_error(context,BUILTIN_ATOM(character));
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
	enum tag_type_t type = UNBOX_TYPE(term->m_value[0].m_u64val);

	if (type == prolite_var)
		return throw_instantiation_error(context);

	if (type != prolite_int32)
		return throw_type_error(context,BUILTIN_ATOM(integer),&term->m_value[0]);

	priority = unbox_int32(&term->m_value[0]);
	if (priority < 0 || priority > 1200)
		return throw_domain_error(context,BUILTIN_ATOM(operator_priority),&term->m_value[0]);

	type = UNBOX_TYPE(term->m_value[1].m_u64val);
	if (type == prolite_var)
		return throw_instantiation_error(context);

	if (type != prolite_atom)
		return throw_type_error(context,BOX_ATOM_EMBED_4('a','t','o','m'),&term->m_value[1]);

	switch (term->m_value[1].m_u64val)
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
		return throw_domain_error(context,BUILTIN_ATOM(operator_specifier),&term->m_value[1]);
	}

	type = UNBOX_TYPE(term->m_value[2].m_u64val);
	if (type == prolite_var ||
		term->m_value[2].m_u64val == BOX_COMPOUND_EMBED_1(2,'|'))
	{
		return throw_instantiation_error(context);
	}

	if (type == prolite_atom)
	{
		if (priority == 0)
			return remove_op(context,op_spec,term->m_value[2].m_u64val);
		return add_op(context,priority,op_spec,term->m_value[2].m_u64val);
	}
	else if (term->m_value[2].m_u64val == BOX_COMPOUND_EMBED_1(2,'.'))
	{
		const union box_t* list = term->m_value + 2;
		do
		{
			/* List - enumerate */
			type = UNBOX_TYPE(list[1].m_u64val);
			if (type == prolite_var)
				return throw_instantiation_error(context);

			if (type == prolite_atom)
			{
				int err;
				if (priority == 0)
					err = remove_op(context,op_spec,term->m_value[2].m_u64val);
				else
					err = add_op(context,priority,op_spec,term->m_value[2].m_u64val);
				if (err)
					return err;
			}
			else
				return throw_type_error(context,BOX_ATOM_EMBED_4('a','t','o','m'),&list[1]);

			if (list[2].m_u64val == BOX_ATOM_EMBED_2('[',']'))
				return 0;

			list += 2;
		}
		while (list->m_u64val == BOX_COMPOUND_EMBED_1(2,'.'));

		type = UNBOX_TYPE(list->m_u64val);
		if (type == prolite_var ||
			list->m_u64val == BOX_COMPOUND_EMBED_1(2,'|'))
		{
			return throw_instantiation_error(context);
		}

		return throw_type_error(context,BOX_ATOM_EMBED_4('a','t','o','m'),list);
	}

	return throw_type_error(context,BOX_ATOM_EMBED_4('l','i','s','t'),&term->m_value[2]);
}
