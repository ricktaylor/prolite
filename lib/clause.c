
#include "types.h"

enum eSolveResult term_to_goal(struct context_t* context, const union box_t* src, union box_t** dest);

/* Assert a clause */
enum eSolveResult assert_clause(struct context_t* context, const union box_t* clause, int z)
{
	const union box_t* head = clause;
	const union box_t* body = NULL;
	union box_t* goal = NULL;

	uint64_t stack_base = stack_top(context->m_exec_stack);

	if (clause->m_u64val == BOX_COMPOUND_EMBED_2(2,':','-'))
	{
		head = first_arg(clause);
		body = next_arg(head);
	}

	switch (UNBOX_TYPE(head->m_u64val))
	{
	case prolite_var:
		return throw_instantiation_error(context,NULL);

	case prolite_compound:
	case prolite_atom:
		break;

	default:
		return throw_type_error(context,BOX_ATOM_BUILTIN(callable),head);
	}

	/* TODO: Check for static procedure */

	if (body)
	{
		enum eSolveResult result = term_to_goal(context,body,&goal);
		if (result == SOLVE_FAIL)
			result = throw_type_error(context,BOX_ATOM_BUILTIN(callable),body);

		if (result != SOLVE_TRUE)
			return result;
	}
	else
	{
		uint64_t top = stack_top(context->m_exec_stack);
		if (stack_push(&context->m_exec_stack,BOX_ATOM_EMBED_4('t','r','u','e')) == -1)
			return SOLVE_NOMEM;

		goal = stack_at(context->m_exec_stack,top);
	}

	/* TODO Make a clause(head,goal)!! */

	return SOLVE_TRUE;
}
