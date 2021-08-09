/*
 * context.c
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#include "types.h"

#include <assert.h>

enum eSolveResult solve_char_conversion(struct context_t* context, const union packed_t* goal)
{
	uint32_t in_char = -1;
	uint32_t out_char = -1;

	const union packed_t* arg;
	enum tag_type_t type;

	goal = first_arg(goal);
	arg = deref_term(context->m_substs,next_arg(goal));
	goal = deref_term(context->m_substs,arg);

	type = UNPACK_TYPE(goal->m_u64val);
	if (type == prolite_var)
		return throw_instantiation_error(context,NULL);

	if (!UNPACK_IS_TYPE_EMBED(goal->m_u64val,prolite_atom) ||
		(in_char = atom_to_code(goal)) == -1)
	{
		return throw_representation_error(context,PACK_ATOM_BUILTIN(character),goal);
	}

	type = UNPACK_TYPE(arg->m_u64val);
	if (type == prolite_var)
		return throw_instantiation_error(context,NULL);

	if (!UNPACK_IS_TYPE_EMBED(arg->m_u64val,prolite_atom) ||
		(out_char = atom_to_code(arg)) == -1)
	{
		return throw_representation_error(context,PACK_ATOM_BUILTIN(character),arg);
	}

	if (in_char == out_char)
	{
		/* TODO: Remove in_char from the char_conversion table */
	}
	else
	{
		/* TODO: Update the char_conversion table */
	}

	return SOLVE_TRUE;
}

enum eSolveResult solve_op(struct context_t* context, const union packed_t* goal)
{
	int priority;
	enum eOpSpec op_spec;

	goal = first_arg(goal);
	switch (UNPACK_TYPE(goal->m_u64val))
	{
	case prolite_var:
		return throw_instantiation_error(context,NULL);

	case prolite_int32:
		break;

	default:
		return throw_type_error(context,PACK_ATOM_BUILTIN(integer),goal);
	}

	priority = UNPACK_LOW32(goal->m_u64val);
	if (priority < 0 || priority > 1200)
		return throw_domain_error(context,PACK_ATOM_BUILTIN(operator_priority),goal);

	goal = next_arg(goal);
	switch (UNPACK_TYPE(goal->m_u64val))
	{
	case prolite_var:
		return throw_instantiation_error(context,NULL);

	case prolite_atom:
		break;

	default:
		return throw_type_error(context,PACK_ATOM_EMBED_4('a','t','o','m'),goal);
	}

	switch (goal->m_u64val)
	{
	case PACK_ATOM_EMBED_2('f','x'):
		op_spec = eFX;
		break;
	case PACK_ATOM_EMBED_2('f','y'):
		op_spec = eFY;
		break;
	case PACK_ATOM_EMBED_3('x','f','x'):
		op_spec = eXFX;
		break;
	case PACK_ATOM_EMBED_3('x','f','y'):
		op_spec = eXFY;
		break;
	case PACK_ATOM_EMBED_3('y','f','x'):
		op_spec = eYFX;
		break;
	case PACK_ATOM_EMBED_2('x','f'):
		op_spec = eXF;
		break;
	case PACK_ATOM_EMBED_2('y','f'):
		op_spec = eYF;
		break;
	default:
		return throw_domain_error(context,PACK_ATOM_BUILTIN(operator_specifier),goal);
	}

	goal = next_arg(goal);
	switch (UNPACK_TYPE(goal->m_u64val))
	{
	case prolite_var:
		return throw_instantiation_error(context,NULL);

	case prolite_atom:
		if (priority == 0)
			return remove_op(context,op_spec,goal);
		return add_op(context,priority,op_spec,goal);

	default:
		break;
	}

	if (goal->m_u64val == PACK_COMPOUND_EMBED_1(2,'|'))
		return throw_instantiation_error(context,NULL);

	if (goal->m_u64val != PACK_COMPOUND_EMBED_1(2,'.'))
		return throw_type_error(context,PACK_ATOM_EMBED_4('l','i','s','t'),goal);
	else
	{
		const union packed_t* list = first_arg(goal);
		do
		{
			/* List - enumerate */
			switch (UNPACK_TYPE(list->m_u64val))
			{
			case prolite_var:
				return throw_instantiation_error(context,NULL);

			case prolite_atom:
				{
					enum eSolveResult result;
					if (priority == 0)
						result = remove_op(context,op_spec,list);
					else
						result = add_op(context,priority,op_spec,list);
					if (result != SOLVE_TRUE)
						return result;
				}
				break;

			default:
				return throw_type_error(context,PACK_ATOM_EMBED_4('a','t','o','m'),list);
			}

			list = next_arg(list);
		}
		while (list->m_u64val == PACK_COMPOUND_EMBED_1(2,'.'));

		if (UNPACK_TYPE(list->m_u64val) == prolite_var ||
			list->m_u64val == PACK_COMPOUND_EMBED_1(2,'|'))
		{
			return throw_instantiation_error(context,NULL);
		}
		else if (list->m_u64val != PACK_ATOM_EMBED_2('[',']'))
			return throw_type_error(context,PACK_ATOM_EMBED_4('a','t','o','m'),list);
	}

	return 0;
}
