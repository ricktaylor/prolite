
#include "types.h"

#include <string.h>
#include <assert.h>

struct predicate_t* find_predicate(struct module_t* module, const union box_t* head);
int add_predicate(struct module_t* module, struct predicate_t* pred);

void predicate_lock(struct predicate_t* predicate) { /* TODO */ }
void predicate_unlock(struct predicate_t* predicate) { /* TODO */ }

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

static struct predicate_t* new_predicate(struct module_t* module, const union box_t* head, int dynamic)
{
	struct predicate_t* pred = NULL;
	struct stack_t* s = stack_new(100,module->m_stack->m_fn_malloc,module->m_stack->m_fn_free);
	if (s)
	{
		int ok = 0;
		pred = stack_malloc(&s,sizeof(struct predicate_t));
		if (pred)
		{
			memset(pred,0,sizeof(struct predicate_t));
			pred->m_stack = s;
			pred->m_module = module;

			pred->m_indicator = make_pi(&pred->m_stack,&pred->m_strings,head);
			if (pred->m_indicator)
				ok = 1;
		}

		if (!ok)
		{
			stack_delete(s);
			pred = NULL;
		}
	}

	return pred;
}

void delete_predicate(struct predicate_t* predicate)
{
	if (predicate)
	{
		struct clause_t* c;
		for (c = predicate->m_first_clause; c != NULL; c = c->m_next)
			delete_clause(c);

		stack_delete(predicate->m_stack);
	}
}

static inline int append_box_t(struct stack_t** stack, union box_t const** v, union box_t** term, size_t* term_size)
{
	*term = stack_realloc(stack,*term,*term_size * sizeof(union box_t),((*term_size)+1) * sizeof(union box_t));
	if (!*term)
		return -1;

	(*term)[(*term_size)++] = *((*v)++);
	return 0;
}

static int copy_term(struct context_t* context, struct string_ptr_t** strings, union box_t const** v, union box_t** new_term, size_t* term_size)
{
	int r = 0;
	enum tag_type_t type = UNBOX_TYPE((*v)->m_u64val);
	switch (type)
	{
	case prolite_compound:
		{
			uint64_t arity = UNBOX_MANT_48((*v)->m_u64val);
			unsigned int hi16 = (arity >> 32);
			if (hi16 & 0x8000)
				arity = (hi16 & (MAX_ARITY_EMBED << 11)) >> 11;
			else if ((hi16 & 0xC000) == 0x4000)
				arity = (hi16 & MAX_ARITY_BUILTIN);
			else
			{
				// Copy functor atom
				if (append_box_t(&context->m_scratch_stack,v,new_term,term_size))
					return -1;
			}

			if (append_box_t(&context->m_scratch_stack,v,new_term,term_size))
				return -1;

			if (!r && UNBOX_TYPE((*v)->m_u64val) == PROLITE_DEBUG_INFO)
			{
				// TODO: Debug info
			}

			while (arity--)
			{
				if (copy_term(context,strings,v,new_term,term_size) != 0)
					return -1;
			}
		}
		break;

	case prolite_var:
		{
			const union box_t* ptr = deref_term(context->m_substs,*v);
			if (ptr == *v)
			{
				// TODO: Make this a better error!
				assert(0);

				return -2;
			}

			return copy_term(context,strings,&ptr,new_term,term_size);
		}
		break;

	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		{
			unsigned int hi16 = UNBOX_HI16((*v)->m_u64val);
			if (hi16 & 0xC000)
				r = append_box_t(&context->m_scratch_stack,v,new_term,term_size);
			else
			{
				union box_t b[1];
				struct string_ptr_t* s = unbox_pointer((*v)->m_u64val);
				s = box_stack_string(&context->m_scratch_stack,strings,s->m_str,s->m_len);
				if (!s)
					return -1;

				b[0].m_u64val = BOX_TYPE(type) | box_pointer(s);
				r = append_box_t(&context->m_scratch_stack,(const union box_t**)&b,new_term,term_size);
			}
		}
		break;

	default:
		r = append_box_t(&context->m_scratch_stack,v,new_term,term_size);
	}

	if (!r && UNBOX_TYPE((*v)->m_u64val) == PROLITE_DEBUG_INFO)
	{
		// TODO: Debug info
	}

	return r;
}

static const union box_t* copy_head(struct context_t* context, struct clause_t* clause, const union box_t* head, int64_t** var_rewrites, size_t* var_count)
{

}

static const union box_t* copy_body(struct context_t* context, struct clause_t* clause, const union box_t* head, int64_t** var_rewrites, size_t* var_count)
{

}

static enum eSolveResult compile_clause(struct clause_t* clause, struct predicate_t* pred, struct context_t* context, const union box_t* head, const union box_t* body)
{
	enum eSolveResult result = SOLVE_TRUE;
	struct compile_context_t compile_context;
	size_t scratch_base = stack_top(context->m_scratch_stack);
	size_t var_count = 0;
	int64_t* var_rewrites = NULL;

	if (context->m_substs->m_count)
	{
		var_rewrites = stack_malloc(&context->m_scratch_stack,sizeof(int64_t) * context->m_substs->m_count);
		if (!var_rewrites)
			return SOLVE_NOMEM;
	}

	clause->m_head = copy_head(context,clause,head,&var_rewrites,&var_count);
	if (!clause->m_head)
		return SOLVE_NOMEM;

	clause->m_var_count = var_count;

	if (body)
	{
		body = copy_body(context,clause,body,&var_rewrites,&var_count);
		if (!body)
			return SOLVE_NOMEM;

		clause->m_entry_point = stack_top(clause->m_stack);

		compile_context.m_emit_stack = clause->m_stack;
		compile_context.m_substs = context->m_substs;
		compile_context.m_module = context->m_module;

		switch (compile(&compile_context,body))
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
			result = throw_type_error(context,BOX_ATOM_BUILTIN(callable),body);
			break;

		case COMPILE_NOMEM:
			result = SOLVE_NOMEM;
			break;
		}
	}

	stack_reset(&context->m_scratch_stack,scratch_base);

	return result;
}

enum eSolveResult assert_clause(struct context_t* context, const union box_t* goal, int z, int dynamic)
{
	enum eSolveResult result;
	const union box_t* head = goal;
	const union box_t* body = NULL;
	struct predicate_t* pred;
	int new_pred = 0;
	struct clause_t* clause;

	if (goal->m_u64val == BOX_COMPOUND_EMBED_2(2,':','-'))
	{
		head = first_arg(goal);
		body = deref_term(context->m_substs,next_arg(head));
		head = deref_term(context->m_substs,head);
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

	pred = find_predicate(context->m_module,head);
	if (pred)
	{
		if (dynamic && !pred->m_flags.dynamic)
			return throw_permission_error(context,BOX_ATOM_BUILTIN(modify),BOX_ATOM_BUILTIN(static_procedure),pred->m_indicator);
	}
	else
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
	{
		if (!new_pred)
			predicate_lock(pred);

		result = compile_clause(clause,pred,context,head,body);
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

			if (add_predicate(context->m_module,pred) != 0)
				result = SOLVE_NOMEM;
		}

		if (!new_pred)
			predicate_unlock(pred);
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

/*static enum eSolveResult solve_static_predicate(struct context_t* context, size_t frame)
{
	enum eSolveResult result = SOLVE_TRUE;
	struct predicate_t* pred = *(struct predicate_t**)stack_at(context->m_instr_stack,frame);
	const union box_t* goal = deref_term(context->m_substs,*(const union box_t**)stack_at(context->m_instr_stack,frame+1));

	// TODO

	return result;
}

static enum eSolveResult jit_static_predicate(struct context_t* context, size_t frame)
{
	enum eSolveResult result = SOLVE_TRUE;
	struct predicate_t* pred = *(struct predicate_t**)stack_at(context->m_instr_stack,frame);
	//const union box_t* goal = deref_term(context->m_substs,*(const union box_t**)stack_at(context->m_instr_stack,frame+1));

	predicate_lock(pred);

	// TODO: Compile the predicate!!

	struct clause_t* c = pred->m_last_clause;
	do
	{
		// Push a JMP for c




		if (c == pred->m_first_clause)
			break;

		c = c->m_prev;
	}
	while (result == SOLVE_TRUE);

	predicate_unlock(pred);

	if (result == SOLVE_TRUE)
	{
		// Patch the call to point to the compiled static function
		solve_fn_t* fn = (solve_fn_t*)stack_at(context->m_instr_stack,frame-1);
		*fn = &solve_static_predicate;

		result = (*fn)(context,frame);
	}

	return result;
}*/

static enum eSolveResult solve_dynamic_clause(struct context_t* context, struct clause_t* clause, const union box_t* goal)
{
	enum eSolveResult result;

	// Grow substs to include head vars

	// Unify goal with head

	// Grow substs to include tail vars, and shift substs to head vars

	// Call clause body

	// Copy substs
	/*struct substs_t* prev_substs = context->m_substs;
	size_t stack_base = stack_top(context->m_call_stack);
	if (prev_substs)
	{
		context->m_substs = stack_malloc(&context->m_call_stack,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));
		if (!context->m_substs)
		{
			context->m_substs = prev_substs;
			return SOLVE_NOMEM;
		}
		else
		{
			size_t i;
			context->m_substs->m_count = prev_substs->m_count;
			for (i = 0;i < prev_substs->m_count; ++i)
			{
				int copy = 1;
				if (prev_substs->m_values[i])
				{
					context->m_substs->m_values[i] = deref_term(prev_substs,prev_substs->m_values[i]);
					if (UNBOX_TYPE(context->m_substs->m_values[i]->m_u64val) != prolite_var)
						copy = 0;
				}

				if (copy)
				{

				}
			}
		}
	}*/

	// Deref all substs

	// Allocate new vars for every unbound var

	// Allocate clause substs

	// Append new var ids

	// Unify

	size_t new_subst_count = clause->m_var_count;
	if (prev_substs)
		new_subst_count += prev_substs->m_count;

	context->m_substs = stack_malloc(&context->m_call_stack,sizeof(struct substs_t) + (new_subst_count * sizeof(union box_t*)));
	if (!context->m_substs)
	{
		context->m_substs = prev_substs;
		return SOLVE_NOMEM;
	}

	if (prev_substs)
		memcpy(context->m_substs,prev_substs,sizeof(struct substs_t) + (prev_substs->m_count * sizeof(union box_t*)));

	result = unify(context->m_substs,goal,clause->m_head);
	if (result == SOLVE_TRUE)
	{
		// Call clause->m_entrypoint
	}

	if (result != SOLVE_TRUE)
	{
		stack_reset(&context->m_call_stack,stack_base);
		context->m_substs = prev_substs;
	}

	return result;
}

static enum eSolveResult redo_dynamic_predicate(struct context_t* context, int unwind)
{
	assert(0);

	return SOLVE_FAIL;
}

static enum eSolveResult solve_dynamic_predicate(struct context_t* context, const union box_t* goal)
{
	enum eSolveResult result = SOLVE_FAIL;
	size_t clause_count = stack_pop(&context->m_call_stack);
	size_t clause_pos = stack_top(context->m_call_stack);

	while (clause_count)
	{
		struct clause_t* clause = *(struct clause_t**)stack_at(context->m_call_stack,clause_pos);

		result = solve_dynamic_clause(context,clause,goal);
		if (result == SOLVE_TRUE)
		{
			if (stack_push(&context->m_call_stack,clause_pos) == -1 ||
				stack_push(&context->m_call_stack,clause_count) == -1 ||
				stack_push_ptr(&context->m_call_stack,&redo_dynamic_predicate) == -1)
			{
				result = SOLVE_NOMEM;
			}
		}

		if (result == SOLVE_FAIL)
		{
			// Try the next clause
			--clause_pos;
			--clause_count;
		}
		else
			break;
	}

	return result;
}

static enum eSolveResult solve_user_defined(struct context_t* context, size_t frame)
{
	enum eSolveResult result = SOLVE_TRUE;
	struct predicate_t** pred = (struct predicate_t**)stack_at(context->m_instr_stack,frame);
	const union box_t* goal = deref_term(context->m_substs,*(const union box_t**)stack_at(context->m_instr_stack,frame+1));

	if (!*pred)
		*pred = find_predicate(context->m_module,goal);

	if (*pred && (*pred)->m_first_clause)
	{
		/*if (!(*pred)->m_flags.dynamic)
			result = jit_static_predicate(context,frame);
		else*/
		{
			size_t count = 0;

			predicate_lock(*pred);

			// Emit the list of current clauses now
			for (struct clause_t* c = (*pred)->m_last_clause;;c = c->m_prev)
			{
				// TODO:  We could pre-filter c->m_head against goal as an optimisation

				// if (filtered(c->m_head,goal)
				{
					if (stack_push_ptr(&context->m_call_stack,c) == -1)
					{
						result = SOLVE_NOMEM;
						break;
					}

					++count;
				}

				if (c == (*pred)->m_first_clause)
					break;
			}

			predicate_unlock(*pred);

			if (result == SOLVE_TRUE)
			{
				if (stack_push(&context->m_call_stack,count) == -1)
					result = SOLVE_NOMEM;
				else
					result = solve_dynamic_predicate(context,goal);
			}
		}
	}
	else if (context->m_module->m_flags.unknown == 0)
		result = throw_existence_error(context,BOX_ATOM_BUILTIN(procedure),goal);
	else
		result = SOLVE_FAIL;

	return result;
}

enum eCompileResult compile_user_defined(struct compile_context_t* context, const union box_t* goal)
{
	enum eCompileResult result = COMPILE_OK;
	struct predicate_t* pred = find_predicate(context->m_module,goal);

	// We can't compile the predicate now as it may not have been fully loaded yet
	if (stack_push_ptr(&context->m_emit_stack,&solve_user_defined) == -1 ||
		stack_push_ptr(&context->m_emit_stack,pred) == -1 ||
		stack_push_ptr(&context->m_emit_stack,goal) == -1)
	{
		result = COMPILE_NOMEM;
	}

	return result;
}
