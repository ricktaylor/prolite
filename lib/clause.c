
#include "types.h"

#include <string.h>
#include <assert.h>

struct predicate_t* find_predicate(struct module_t* module, const union box_t* head);
struct predicate_t* new_predicate(struct module_t* module, const union box_t* head, int dynamic);
void delete_predicate(struct predicate_t* predicate);
int add_predicate(struct module_t* module, struct predicate_t* pred);

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

void delete_clause(struct clause_t* clause)
{
	if (clause)
		stack_delete(clause->m_stack);
}

static enum eSolveResult compile_clause(struct clause_t* clause, struct context_t* context, const union box_t* goal)
{
	enum eSolveResult result;
	struct compile_context_t compile_context;

	clause->m_head = copy_term(context->m_substs,&clause->m_stack,&clause->m_pred->m_strings,goal);
	if (!clause->m_head)
		return SOLVE_NOMEM;

	if (clause->m_head->m_u64val != BOX_COMPOUND_EMBED_2(2,':','-'))
		return SOLVE_TRUE;

	clause->m_head = first_arg(clause->m_head);
	clause->m_body = next_arg(clause->m_head);
	clause->m_entry_point = stack_top(clause->m_stack);

	compile_context.m_emit_stack = clause->m_stack;
	compile_context.m_substs = clause->m_substs;
	compile_context.m_module = context->m_module;

	switch (compile(&compile_context,clause->m_body))
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

enum eSolveResult assert_clause(struct context_t* context, const union box_t* goal, int z, int dynamic)
{
	enum eSolveResult result;
	const union box_t* head = goal;
	struct predicate_t* pred;
	int new_pred = 0;
	struct clause_t* clause;

	if (goal->m_u64val == BOX_COMPOUND_EMBED_2(2,':','-'))
		head = deref_term(context->m_substs,first_arg(goal));

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

	pred = find_predicate(context->m_module,head);
	if (pred && dynamic && !pred->m_flags.dynamic)
		return throw_permission_error(context,BOX_ATOM_BUILTIN(modify),BOX_ATOM_BUILTIN(static_procedure),pred->m_indicator);

	if (!pred)
	{
		pred = new_predicate(context->m_module,head,dynamic);
		if (!pred)
			return SOLVE_NOMEM;

		new_pred = 1;
	}

	clause = new_clause(context);
	if (!clause)
		result = SOLVE_NOMEM;
	else
		result = compile_clause(clause,context,goal);

	if (result == SOLVE_TRUE)
	{
		// Add clause to predicate
		if (!z)
		{
			clause->m_next = pred->m_first_clause;
			if (pred->m_first_clause)
				pred->m_first_clause->m_prev = clause;
			pred->m_first_clause = clause;
			if (!pred->m_last_clause)
				pred->m_last_clause = clause;
		}
		else
		{
			clause->m_prev = pred->m_last_clause;
			if (pred->m_last_clause)
				pred->m_last_clause->m_next = clause;
			pred->m_last_clause = clause;
			if (!pred->m_first_clause)
				pred->m_first_clause = clause;
		}

		if (new_pred && add_predicate(context->m_module,pred) != 0)
			result = SOLVE_NOMEM;
	}

	if (result != SOLVE_TRUE)
	{
		delete_clause(clause);
		if (new_pred)
			delete_predicate(pred);
	}

	return result;
}

enum eSolveResult solve_asserta(struct context_t* context, size_t frame)
{
	const union box_t* goal = *(const union box_t**)stack_at(context->m_instr_stack,frame);
	return assert_clause(context,deref_term(context->m_substs,first_arg(goal)),0,1);
}

enum eSolveResult solve_assertz(struct context_t* context, size_t frame)
{
	const union box_t* goal = *(const union box_t**)stack_at(context->m_instr_stack,frame);
	return assert_clause(context,deref_term(context->m_substs,first_arg(goal)),1,1);
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
	enum eSolveResult result = SOLVE_FAIL;
	struct predicate_t** pred = (struct predicate_t**)stack_at(context->m_instr_stack,frame);
	const union box_t* goal = deref_term(context->m_substs,*(const union box_t**)stack_at(context->m_instr_stack,frame+1));

	if (!*pred)
		*pred = find_predicate(context->m_module,goal);

	if (*pred && (*pred)->m_first_clause)
	{
		// Solve first_clause

		if (result == SOLVE_TRUE)
		{
			// Push a redo for all clauses onto the stack
		}
	}
	else if (context->m_module->m_flags.unknown == 0)
		result = throw_existence_error(context,BOX_ATOM_BUILTIN(procedure),goal);

	return result;
}

enum eCompileResult compile_user_defined(struct compile_context_t* context, const union box_t* goal)
{
	const struct predicate_t* pred = find_predicate(context->m_module,goal);
	if (stack_push_ptr(&context->m_emit_stack,&solve_user_defined) == -1 ||
		stack_push_ptr(&context->m_emit_stack,pred) == -1 ||
		stack_push_ptr(&context->m_emit_stack,goal) == -1)
	{
		return COMPILE_NOMEM;
	}

	return COMPILE_OK;
}
