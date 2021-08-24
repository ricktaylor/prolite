#include "predicates.h"

static predicate_t* find_predicate(const term_t* head)
{
	// TODO - We can do a fast B+Tree lookup on head->m_u64val
	
	return NULL;
}

static int assert_is_callable(context_t* context, const term_t* goal)
{
	switch (get_term_type(goal))
	{
	case prolite_atom:
	case prolite_var:
		return 1;

	case prolite_compound:
		switch (goal->m_u64val)
		{
		case PACK_COMPOUND_EMBED_1(2,','):
		case PACK_COMPOUND_EMBED_1(2,';'):
		case PACK_COMPOUND_EMBED_2(2,'-','>'):
			{
				size_t arity;
				for (const term_t* p = get_first_arg(goal,&arity); arity--; p = get_next_arg(p))
				{
					if (!assert_is_callable(context,deref_local_var(context,p)))
						return 0;
				}
			}
			break;

		default:
			break;
		}
		return 1;

	default:
		return 0;
	}
}

void term_to_clause(context_t* context, const term_t* t, clause_t* clause)
{
	const term_t* head = t;
	const term_t* body = NULL;

	if (head->m_u64val == PACK_COMPOUND_EMBED_2(2,':','-'))
	{
		head = get_first_arg(head,NULL);
		body = deref_local_var(context,get_next_arg(head));
	}
	head = deref_local_var(context,head);

	switch (head->m_u64val)
	{
#define DECLARE_BUILTIN_INTRINSIC(f,n) \
	case (n):

#define DECLARE_BUILTIN_FUNCTION(f,n) \
	case (n):

#include "builtin_functions.h"

		return throw_permission_error(context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),head);

	default:
		if (get_term_type(head) == prolite_var)
			return throw_instantiation_error(context,head);

		if (!assert_is_callable(context,head))
			return throw_type_error(context,PACK_ATOM_BUILTIN(callable),head);
		break;
	}	
	
	if (body && !assert_is_callable(context,body))
		return throw_type_error(context,PACK_ATOM_BUILTIN(callable),body);

	// Copy t to heap
	size_t top = heap_top(context->m_heap);
	clause->m_head = copy_term_to_heap(context,t,&clause->m_locals_count);
	if (!clause->m_head)
		return throw_out_of_memory_error(context,t);

	// TODO: Safely copy away head+body

	if (body)
	{
		clause->m_head = get_first_arg(clause->m_head,NULL);
		clause->m_body = get_next_arg(clause->m_head);
	}
			
	heap_reset(context->m_heap,top);
}

static void assert_clause(context_t* context, const term_t* t, int z)
{
	clause_t clause = {0};
	term_to_clause(context,t,&clause);
			
	predicate_t* pred = find_predicate(clause.m_head);

	
}

void builtin_assert(context_t* context)
{
	const term_t* goal = (context->m_stack--)->m_pval;
	const builtin_fn_t gosub = (context->m_stack--)->m_pval;

	int assertz = (goal->m_u64val == PACK_COMPOUND_BUILTIN(assertz,1));
	
	assert_clause(context,deref_local_var(context,get_first_arg(goal,NULL)),assertz);

	if (!(context->m_flags & (FLAG_HALT | FLAG_THROW)))
		(*gosub)(context);
}

void builtin_user_defined(context_t* context)
{

}
