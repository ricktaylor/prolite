#include "context.h"

typedef struct clause
{
	unsigned  m_compiled : 1;

	size_t  m_head_vars;
	size_t  m_body_vars;
	term_t* m_head;
	term_t* m_body;

} clause_t;

typedef struct predicate
{
	unsigned  m_dynamic : 1;
	size_t    m_clause_count;
	clause_t* m_clauses;
} predicate_t;

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
				for (const term_t* p = get_first_arg(goal,&arity,NULL); arity--; p = get_next_arg(p,NULL))
				{
					if (!assert_is_callable(context,deref_var_term(context,p)))
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
		head = get_first_arg(head,NULL,NULL);
		body = deref_var_term(context,get_next_arg(head,NULL));
	}
	head = deref_var_term(context,head);

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

	
}

void builtin_assert(context_t* context)
{
	const term_t* goal = (context->m_stack--)->m_pval;
	int assertz = (goal->m_u64val == PACK_COMPOUND_BUILTIN(assertz,1));
	
	assert_clause(context,deref_var_term(context,get_first_arg(goal,NULL,NULL)),assertz,1);
}

void builtin_user_defined(context_t* context)
{

}
