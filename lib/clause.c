
#include "types.h"

#include <assert.h>

/* Assert a clause */
enum eSolveResult assert_clause(struct context_t* context, const union box_t* clause, int z)
{
	const union box_t* head = clause;
	const union box_t* body = NULL;
	union box_t* goal = NULL;

	uint64_t stack_base = stack_top(context->m_call_stack);

	if (clause->m_u64val == BOX_COMPOUND_EMBED_2(2,':','-'))
	{
		head = first_arg(clause);
		body = next_arg(head);
	}

	switch (UNBOX_TYPE(head->m_u64val))
	{
	case prolite_var:
		return throw_instantiation_error(context,head);

	case prolite_compound:
	case prolite_atom:
		break;

	default:
		return throw_type_error(context,BOX_ATOM_BUILTIN(callable),head);
	}

	/* TODO: Check for static procedure */

	/*
	if (body)
	{
		enum eSolveResult result = term_to_goal(context,body,&goal);
		if (result == SOLVE_FAIL)
			result = throw_type_error(context,BOX_ATOM_BUILTIN(callable),body);
		if (result != SOLVE_TRUE)
			return result;

		switch (UNBOX_TYPE(goal->m_u64val))
		{
		case prolite_var:
			return throw_instantiation_error(context,goal);

		case prolite_compound:
		case prolite_atom:
			break;

		default:
			return throw_type_error(context,BOX_ATOM_BUILTIN(callable),goal);
		}
	}
	else
	{
		goal = stack_malloc(&context->m_exec_stack,sizeof(union box_t));
		if (!goal)
			return SOLVE_NOMEM;

		goal->m_u64val = BOX_ATOM_EMBED_4('t','r','u','e');
	}*/

	/* TODO Make a clause(head,goal)!! */
	assert(0);

	return SOLVE_TRUE;
}
