
#include "clause.h"

union instruction_t
{
	void*       m_ptr;
	union box_t m_value;
};

static int emit_call(struct context_t* context, struct term_t* goal)
{
	int r = emit_copy_term(context);
	if (!r)
	{
		r = emit_check_var(context);
	}

	if ((goal->m_value->m_uval & BOX_TAG_MASK) == BOX_TAG_VAR)
	{
		/* Emit a run-time goal eval function */
	}

	return r;
}

/* Emit the opcode for a goal on the scratch stack */
int emit_goal(struct context_t* context, struct term_t* goal)
{
	switch (goal->m_value->m_uval & BOX_TAG_MASK)
	{
	case BOX_COMPOUND_EMBED_1(2,','):
		goal->m_value++;
		return emit_and(context,goal);

	case BOX_COMPOUND_EMBED_1(2,';'):
		goal->m_value++;
		return emit_or(context,goal);

	case BOX_COMPOUND_EMBED_2(2,'-','>'):
		goal->m_value++;
		return emit_if(context,goal);

	case BOX_ATOM_EMBED_1('!'):
		return emit_cut(context);

	case BOX_COMPOUND_EMBED_4(1,'c','a','l','l'):
		goal->m_value++;
		return emit_call(context,goal);

	case BOX_ATOM_EMBED_4('t','r','u','e'):
		return emit_true(context);

	case BOX_ATOM_EMBED_4('f','a','i','l'):
		return emit_fail(context);

	case BOX_COMPOUND_EMBED_5(3,'c','a','t','c','h'):
		goal->m_value++;
		return emit_catch(context,goal);

	case BOX_COMPOUND_EMBED_5(1,'t','h','r','o','w'):
		goal->m_value++;
		return emit_throw(context,goal);

	default:
		break;
	}

	/* TODO: Check for builtins */

	/* Emit user defined */
	return emit_user_defined(context,goal);
}
