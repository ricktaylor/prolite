#include "context.h"

typedef struct clause
{
	size_t       m_locals_count;
	term_t*      m_args;
	term_t*      m_body;

	debug_info_t m_debug_info;

} clause_t;

typedef struct predicate
{
	unsigned  m_dynamic : 1;
	size_t    m_clause_count;
	clause_t* m_clauses;
} predicate_t;

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

void assert_clause(context_t* context, const term_t* goal, int z, int dynamic)
{
	const term_t* head = goal;
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

	clause_t clause = { .m_locals_count = 0 };

	// Copy goal to heap
	size_t top = heap_top(context->m_heap);
	head = copy_term_to_heap(context,goal,&clause.m_locals_count);
	if (!head)
		return throw_out_of_memory_error(context,goal);

	if (body)
	{
		head = get_first_arg(head,NULL);
		body = get_next_arg(head);
	}

	if (get_term_type(head) == prolite_compound)
	{
		size_t arity = 0;
		const term_t* arg_start = get_first_arg(head,&arity);
		const term_t* arg_end = arg_start;

		while (arity--)
			arg_end = get_next_arg(arg_end);
		
		// TODO: Safely copy away args
	}

	if (body)
	{
		// TODO: Safely copy away body
	}
		
	const debug_info_t* debug_info = get_debug_info(head);
	{
		// TODO: Safely copy away debug info
		clause.m_debug_info = *debug_info;
	}
			
	predicate_t* pred = find_predicate(head);

	heap_reset(context->m_heap,top);
}

void builtin_assert(context_t* context)
{
	const void* gosub = (context->m_stack--)->m_pval;
	const term_t* goal = (context->m_stack--)->m_pval;

	int assertz = (goal->m_u64val == PACK_COMPOUND_BUILTIN(assertz,1));
	
	assert_clause(context,deref_local_var(context,get_first_arg(goal,NULL)),assertz,1);

	if (!(context->m_flags & (FLAG_HALT | FLAG_THROW)))
		call_continuation(context,gosub);
}

void builtin_user_defined(context_t* context)
{

}
