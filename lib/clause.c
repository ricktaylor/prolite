
#include "types.h"

#include <string.h>
#include <assert.h>

static struct clause_t* new_clause(struct context_t* context)
{
	struct clause_t* clause = NULL;
	struct stack_t* s = stack_new(100,context->m_call_stack->m_fn_malloc,context->m_call_stack->m_fn_free);
	if (s)
	{
		clause = stack_malloc(&s,sizeof(struct clause_t));
		if (clause)
		{
			memset(clause,0,sizeof(struct clause_t));
			clause->m_stack = s;
		}
		else
			stack_delete(s);
	}

	return clause;
}

static void delete_clause(struct clause_t* clause)
{
	if (clause)
		stack_delete(clause->m_stack);
}

static enum eSolveResult compile_clause(struct clause_t* clause, struct context_t* context, const union box_t* goal)
{
	enum eSolveResult result;

	goal = copy_term(context,&clause->m_stack,goal);
	if (!goal)
		return SOLVE_NOMEM;

	if (goal->m_u64val != BOX_COMPOUND_EMBED_2(2,':','-'))
	{
		clause->m_head = goal;
		return SOLVE_TRUE;
	}

	clause->m_head = first_arg(goal);
	clause->m_body = next_arg(clause->m_head);

	if (!clause->m_head || !clause->m_body)
		return SOLVE_NOMEM;

	clause->m_entry_point = stack_top(clause->m_stack);

	switch (compile(context,&clause->m_stack,clause->m_body))
	{
	case COMPILE_OK:
		result = SOLVE_TRUE;
		break;

	case COMPILE_ALWAYS_TRUE:
		clause->m_entry_point = 0;
		result = SOLVE_TRUE;
		break;

	case COMPILE_ALWAYS_FAILS:
		clause->m_entry_point = -1;
		result = SOLVE_TRUE;
		break;

	case COMPILE_NOT_CALLABLE:
		result = throw_type_error(context,BOX_ATOM_BUILTIN(callable),clause->m_body);
		break;

	case COMPILE_NOMEM:
		result = SOLVE_NOMEM;
		break;
	}

	return result;
}

static struct predicate_t* find_predicate(struct context_t* context, const union box_t* head)
{
	struct predicate_t* pred = NULL;

	return pred;
}

static struct predicate_t* new_predicate(struct context_t* context, const union box_t* head, int dynamic)
{
	struct predicate_t* pred = NULL;

	return pred;
}

static void delete_predicate(struct predicate_t* predicate)
{
	if (predicate)
	{
		size_t i;
		for (i = 0; i < predicate->m_clause_count; ++i)
			delete_clause(predicate->m_clauses[i]);

		stack_delete(predicate->m_stack);
	}
}

static union box_t* predicate_indicator(struct context_t* context, const struct predicate_t* predicate)
{
	// TODO: Emit PI onto context->m_call_stack;

	assert(0);

	return NULL;
}

enum eSolveResult assert_clause(struct context_t* context, const union box_t* goal, int z, int dynamic)
{
	enum eSolveResult result;
	const union box_t* head = goal;
	struct predicate_t* pred;
	struct clause_t* clause;

	if (goal->m_u64val == BOX_COMPOUND_EMBED_2(2,':','-'))
		head = deref_term(context,first_arg(goal));

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

	pred = find_predicate(context,head);
	if (pred && dynamic && !pred->m_flags.dynamic)
	{
		union box_t* indicator = predicate_indicator(context,pred);
		if (!indicator)
			return SOLVE_NOMEM;
		return throw_permission_error(context,BOX_ATOM_BUILTIN(modify),BOX_ATOM_BUILTIN(static_procedure),indicator);
	}

	clause = new_clause(context);
	if (!clause)
		return SOLVE_NOMEM;

	result = compile_clause(clause,context,goal);
	if (result == SOLVE_TRUE)
	{
		int new_pred = 0;
		if (!pred)
		{
			pred = new_predicate(context,head,dynamic);
			if (!pred)
				result = SOLVE_NOMEM;
			else
				new_pred = 1;
		}

		if (result == SOLVE_TRUE)
		{
			// Add clause to predicate


		}

		if (new_pred)
		{
			if (result == SOLVE_TRUE)
			{
				// Add predicate to module
			}
			else
			{
				delete_predicate(pred);
			}
		}
	}

	if (result != SOLVE_TRUE)
		delete_clause(clause);

	return result;
}

enum eSolveResult solve_asserta(struct context_t* context, size_t frame)
{
	const union box_t* goal = *(const union box_t**)stack_at(context->m_instr_stack,frame);
	return assert_clause(context,deref_term(context,first_arg(goal)),0,1);
}

enum eSolveResult solve_assertz(struct context_t* context, size_t frame)
{
	const union box_t* goal = *(const union box_t**)stack_at(context->m_instr_stack,frame);
	return assert_clause(context,deref_term(context,first_arg(goal)),1,1);
}

static enum eSolveResult redo_compiled_clause(struct context_t* context, int unwind)
{
	size_t body_frame = stack_pop(&context->m_call_stack);

	enum eSolveResult result = redo(context,unwind);
	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_call_stack,body_frame) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_compiled_clause) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static enum eSolveResult solve_compiled_clause(struct context_t* context, size_t frame)
{
	enum eSolveResult result;
	size_t body_frame = *(const size_t*)stack_at(context->m_instr_stack,frame);
	if (body_frame && body_frame != -1)
		body_frame += (frame-1);

	if (body_frame == -1)
		result = SOLVE_FAIL;
	else if (!body_frame)
		result = stack_push_ptr(&context->m_call_stack,&redo_true) == -1 ? SOLVE_NOMEM : SOLVE_TRUE;
	else
		result = solve(context,body_frame);

	if (result == SOLVE_TRUE)
	{
		if (stack_push(&context->m_call_stack,body_frame) == -1 ||
			stack_push_ptr(&context->m_call_stack,&redo_compiled_clause) == -1)
		{
			result = SOLVE_NOMEM;
		}
	}

	return result;
}

static enum eSolveResult solve_user_defined(struct context_t* context, size_t frame)
{
	const struct predicate_t* pred = *(const struct predicate_t**)stack_at(context->m_instr_stack,frame);
	const union box_t* goal = deref_term(context,*(const union box_t**)stack_at(context->m_instr_stack,frame+1));

	if (!pred)
		pred = find_predicate(context,goal);

	if (pred)
	{
		/* Emit all the current clauses now, to give us a 'logical database' */
		size_t i;
		for (i = 0; i < pred->m_clause_count; ++i)
		{
			// TODO: Jmp to the instructions
		}
	}

	if (context->m_module->m_flags.unknown == 0)
		return throw_existence_error(context,BOX_ATOM_BUILTIN(procedure),goal);

	return SOLVE_FAIL;
}

enum eCompileResult compile_user_defined(struct context_t* context, struct stack_t** emit_stack, const union box_t* goal)
{
	const struct predicate_t* pred = find_predicate(context,goal);
	if (pred && !pred->m_flags.dynamic)
	{
		size_t i;
		for (i = 0; i < pred->m_clause_count; ++i)
		{
			// Push substs

			// Unify

			// Jmp to the instructions

			// Undo unify on fail
		}
	}

	if (stack_push_ptr(emit_stack,&solve_user_defined) == -1 ||
		stack_push_ptr(emit_stack,pred) == -1 ||
		stack_push_ptr(emit_stack,goal) == -1)
	{
		return COMPILE_NOMEM;
	}

	return COMPILE_OK;
}
