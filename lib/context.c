/*
 * context.c
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#include "types.h"

#include <assert.h>

uint32_t convert_char(struct context_t* context, uint32_t in_char)
{
	/* TODO */

	return in_char;
}

static uint32_t atom_to_code(const union box_t* b)
{
	uint16_t hi16 = UNBOX_HI16(b->m_u64val);
	uint32_t lo32 = UNBOX_LOW32(b->m_u64val);
	unsigned int len = (hi16 & 0x0700) >> 8;
	unsigned int count = 0;
	uint32_t val = 0;
	uint8_t c[5] =
	{
		(hi16 & 0xFF),
		(lo32 >> 24),
		(lo32 >> 16) & 0xFF,
		(lo32 >> 8) & 0xFF,
		lo32 & 0xFF
	};

	if (c[0] <= 0x7f)
		return len == 1 ? c[0] : -1;

	if (c[0] < 0xC2 || c[0] > 0xF4)
		return -1;

	if ((c[0] & 0xE0) == 0xC0)
	{
		count = 2;
		val = (c[0] & 0x1F);
	}
	else if ((c[0] & 0xF0) == 0xE0)
	{
		if ((c[0] == 0xE0 && c[1] >= 0x80 && c[1] <= 0x9F) ||
			(c[0] == 0xED && c[1] >= 0xA0 && c[1] <= 0xBF))
		{
			return -1;
		}

		count = 3;
		val = (c[0] & 0x0F);
	}
	else if ((c[0] & 0xF8) == 0xF0)
	{
		if ((c[0] == 0xF0 && c[1] >= 0x80 && c[1] <= 0x8F) ||
			(c[0] == 0xF4 && c[1] >= 0x90 && c[1] <= 0xBF))
		{
			return -1;
		}

		count = 4;
		val = (c[0] & 0x7);
	}
	else
		return -1;

	if (len != count)
		return -1;
	else
	{
		unsigned int i;
		for (i=1;i<count;++i)
		{
			if ((c[i] & 0xC0) != 0x80)
				return -1;

			val = (val << 6) | (c[i] & 0x3F);
		}
	}

	return val;
}

enum eSolveResult solve_char_conversion(struct context_t* context, const union box_t* goal)
{
	uint32_t in_char = -1;
	uint32_t out_char = -1;

	const union box_t* arg;
	enum tag_type_t type;

	goal = first_arg(goal);
	arg = deref_term(context->m_substs,next_arg(goal));
	goal = deref_term(context->m_substs,arg);

	type = UNBOX_TYPE(goal->m_u64val);
	if (type == prolite_var)
		return throw_instantiation_error(context,NULL);

	if (!UNBOX_IS_TYPE_EMBED(goal->m_u64val,prolite_atom) ||
		(in_char = atom_to_code(goal)) == -1)
	{
		return throw_representation_error(context,BOX_ATOM_BUILTIN(character),goal);
	}

	type = UNBOX_TYPE(arg->m_u64val);
	if (type == prolite_var)
		return throw_instantiation_error(context,NULL);

	if (!UNBOX_IS_TYPE_EMBED(arg->m_u64val,prolite_atom) ||
		(out_char = atom_to_code(arg)) == -1)
	{
		return throw_representation_error(context,BOX_ATOM_BUILTIN(character),arg);
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

static struct operator_t* find_op(struct context_t* context, const union box_t* b)
{
	static struct operator_t s_builtins[] =
	{
		/* 0 */ { NULL, eXFX, 1200 },
		/* 1 */ { NULL, eXFY, 1100 },
		/* 2 */ { NULL, eXFY, 1050 },
		/* 3 */ { NULL, eXFY, 1000 },
		/* 4 */ { NULL, eXFX, 700 },
		/* 5 */ { NULL, eYFX, 500 },
		/* 6 */ { NULL, eYFX, 400 },
		/* 7 */ { NULL, eXFX, 200 },
		/* 8 */ { NULL, eXFY, 200 },
	};

	if (context->m_module->m_operators)
	{
		// TODO: Use the module table
		assert(0);
	}

	switch (b->m_u64val)
	{
	case BOX_ATOM_EMBED_2(':','-'):
	case BOX_ATOM_EMBED_3('-','-','>'):
		return &s_builtins[0];

	case BOX_ATOM_EMBED_1(';'):
		return &s_builtins[1];

	case BOX_ATOM_EMBED_2('-','>'):
		return &s_builtins[2];

	case BOX_ATOM_EMBED_1(','):
		return &s_builtins[3];

	case BOX_ATOM_EMBED_1('='):
	case BOX_ATOM_EMBED_2('\\','='):
	case BOX_ATOM_EMBED_2('=','='):
	case BOX_ATOM_EMBED_3('\\','=','='):
	case BOX_ATOM_EMBED_2('@','<'):
	case BOX_ATOM_EMBED_3('@','=','<'):
	case BOX_ATOM_EMBED_2('@','>'):
	case BOX_ATOM_EMBED_3('@','>','='):
	case BOX_ATOM_EMBED_3('=','.','.'):
	case BOX_ATOM_EMBED_2('i','s'):
	case BOX_ATOM_EMBED_3('=',':','='):
	case BOX_ATOM_EMBED_3('=','\\','='):
	case BOX_ATOM_EMBED_1('<'):
	case BOX_ATOM_EMBED_2('=','<'):
	case BOX_ATOM_EMBED_1('>'):
	case BOX_ATOM_EMBED_2('>','='):
		return &s_builtins[4];

	case BOX_ATOM_EMBED_1('+'):
	case BOX_ATOM_EMBED_1('-'):
	case BOX_ATOM_EMBED_2('/','\\'):
	case BOX_ATOM_EMBED_2('\\','/'):
		return &s_builtins[5];

	case BOX_ATOM_EMBED_1('*'):
	case BOX_ATOM_EMBED_1('/'):
	case BOX_ATOM_EMBED_2('/','/'):
	case BOX_ATOM_EMBED_3('r','e','m'):
	case BOX_ATOM_EMBED_3('m','o','d'):
	case BOX_ATOM_EMBED_2('<','<'):
	case BOX_ATOM_EMBED_2('>','>'):
	case BOX_ATOM_EMBED_3('d','i','v'):
		return &s_builtins[6];

	case BOX_ATOM_EMBED_2('*','*'):
		return &s_builtins[7];

	case BOX_ATOM_EMBED_1('^'):
		return &s_builtins[8];

	default:
		return NULL;
	}
}

struct operator_t* find_prefix_op(struct context_t* context, const union box_t* b)
{
	static struct operator_t s_builtins[] =
	{
		/* 0 */ { NULL, eFX, 1200 },
		/* 1 */ { NULL, eFY, 900 },
		/* 2 */ { NULL, eFY, 200 },
	};

	if (context->m_module->m_operators)
	{
		// TODO: Use the module table
		assert(0);
	}

	switch (b->m_u64val)
	{
	case BOX_ATOM_EMBED_2(':','-'):
	case BOX_ATOM_EMBED_2('?','-'):
		return &s_builtins[0];

	case BOX_ATOM_EMBED_2('\\','+'):
		return &s_builtins[1];

	case BOX_ATOM_EMBED_1('-'):
	case BOX_ATOM_EMBED_1('\\'):
	case BOX_ATOM_EMBED_1('+'):
		return &s_builtins[2];

	default:
		return NULL;
	}
}

/* Try to find a infix/postfix op, otherwise find prefix */
struct operator_t* lookup_op(struct context_t* context, const union box_t* b)
{
	struct operator_t* op = find_op(context,b);
	if (!op)
		op = find_prefix_op(context,b);
	return op;
}

/* Try to find a prefix op, otherwise find infix/suffix */
struct operator_t* lookup_prefix_op(struct context_t* context, const union box_t* b)
{
	struct operator_t* op = find_prefix_op(context,b);
	if (!op)
		op = find_op(context,b);
	return op;
}

static enum eSolveResult add_op(struct context_t* context, unsigned int priority, enum eOpSpec op_spec, const union box_t* atom)
{
	/* TODO */
	assert(0);
	return SOLVE_FAIL;
}

static enum eSolveResult remove_op(struct context_t* context, enum eOpSpec op_spec, const union box_t* atom)
{
	/* TODO */
	assert(0);
	return SOLVE_FAIL;
}

enum eSolveResult solve_op(struct context_t* context, const union box_t* goal)
{
	int priority;
	enum eOpSpec op_spec;

	goal = first_arg(goal);
	switch (UNBOX_TYPE(goal->m_u64val))
	{
	case prolite_var:
		return throw_instantiation_error(context,NULL);

	case prolite_int32:
		break;

	default:
		return throw_type_error(context,BOX_ATOM_BUILTIN(integer),goal);
	}

	priority = UNBOX_LOW32(goal->m_u64val);
	if (priority < 0 || priority > 1200)
		return throw_domain_error(context,BOX_ATOM_BUILTIN(operator_priority),goal);

	goal = next_arg(goal);
	switch (UNBOX_TYPE(goal->m_u64val))
	{
	case prolite_var:
		return throw_instantiation_error(context,NULL);

	case prolite_atom:
		break;

	default:
		return throw_type_error(context,BOX_ATOM_EMBED_4('a','t','o','m'),goal);
	}

	switch (goal->m_u64val)
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
		return throw_domain_error(context,BOX_ATOM_BUILTIN(operator_specifier),goal);
	}

	goal = next_arg(goal);
	switch (UNBOX_TYPE(goal->m_u64val))
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

	if (goal->m_u64val == BOX_COMPOUND_EMBED_1(2,'|'))
		return throw_instantiation_error(context,NULL);

	if (goal->m_u64val != BOX_COMPOUND_EMBED_1(2,'.'))
		return throw_type_error(context,BOX_ATOM_EMBED_4('l','i','s','t'),goal);
	else
	{
		const union box_t* list = first_arg(goal);
		do
		{
			/* List - enumerate */
			switch (UNBOX_TYPE(list->m_u64val))
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
				return throw_type_error(context,BOX_ATOM_EMBED_4('a','t','o','m'),list);
			}

			list = next_arg(list);
		}
		while (list->m_u64val == BOX_COMPOUND_EMBED_1(2,'.'));

		if (UNBOX_TYPE(list->m_u64val) == prolite_var ||
			list->m_u64val == BOX_COMPOUND_EMBED_1(2,'|'))
		{
			return throw_instantiation_error(context,NULL);
		}
		else if (list->m_u64val != BOX_ATOM_EMBED_2('[',']'))
			return throw_type_error(context,BOX_ATOM_EMBED_4('a','t','o','m'),list);
	}

	return 0;
}

enum eSolveResult solve_set_prolog_flag(struct context_t* context, const union box_t* goal)
{
	assert(0);

	return SOLVE_FAIL;
}
