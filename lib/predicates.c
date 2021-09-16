#include "predicates.h"
#include "fnv1a.h"

#include <string.h>
#include <assert.h>

static uint64_t predicate_key(const term_t* functor, int* is_sub_tree)
{
	prolite_type_t type = get_term_type(functor);
	assert(type == prolite_atom || type == prolite_compound);

	uint64_t key = functor->m_u64val;
	unsigned int sub_type = get_term_subtype(functor);
	if (sub_type == 0 || sub_type == 3)
	{
		string_t s;
		if (type == prolite_atom)
			s = get_string(functor,NULL);
		else
			s = get_predicate(functor,NULL,NULL);

		if (s.m_len <= 8)
		{
			union short_str_key
			{
				unsigned char m_chars[8];
				uint64_t      m_u64val;
			} sk = {0};
			memcpy(sk.m_chars,s.m_str,s.m_len);

			key = sk.m_u64val;
		}
		else
			key = fnv1a_64(s.m_str,s.m_len);

		*is_sub_tree = 1;
	}
	else
		*is_sub_tree = 0;

	return key;
}

predicate_base_t* predicate_map_lookup(predicate_map_t* pm, const term_t* functor)
{
	int is_sub_tree = 0;
	uint64_t key = predicate_key(functor,&is_sub_tree);

	void* p = btree_lookup(pm,functor->m_u64val);
	if (p && is_sub_tree)
	{
		btree_t sub_tree = 
		{
			.m_fn_malloc = pm->m_fn_malloc,
			.m_fn_free = pm->m_fn_free,
			.m_root = p
		};
		p = btree_lookup(&sub_tree,key);
	}

	return p;
}

predicate_base_t* predicate_map_insert(predicate_map_t* pm, predicate_base_t* pred)
{
	predicate_base_t* curr_pred;
	int is_sub_tree = 0;
	uint64_t key = predicate_key(pred->m_functor,&is_sub_tree);
	if (is_sub_tree)
	{
		// Get sub-tree
		btree_t sub_tree = 
		{
			.m_fn_malloc = pm->m_fn_malloc,
			.m_fn_free = pm->m_fn_free
		};
		sub_tree.m_root = btree_lookup(pm,pred->m_functor->m_u64val);

		if (!sub_tree.m_root)
		{
			curr_pred = btree_insert(&sub_tree,key,pred);
			if (!curr_pred)
				return NULL;

			if (!btree_insert(pm,pred->m_functor->m_u64val,sub_tree.m_root))
			{
				btree_clear(&sub_tree,NULL,NULL);
				return NULL;
			}
		}
		else
		{
			curr_pred = btree_insert(&sub_tree,key,pred);
			if (!curr_pred)
				return NULL;
		
			// TODO: Check for clashes...
			assert(predicate_compare(curr_pred->m_functor,pred->m_functor));
		}
	}
	else
	{
		curr_pred = btree_insert(pm,pred->m_functor->m_u64val,pred);
	}
	
	return curr_pred;
}

struct callback_param
{
	void (*m_callback)(void* param, predicate_base_t* pred);
	void* m_param;
	btree_t* m_bt;
};

static void clear_callback(void* p, uint64_t k, void* v)
{
	unsigned int sub_type = get_term_subtype(&(term_t){ .m_u64val = k });
	if (sub_type == 0 || sub_type == 3)
	{
		btree_t* bt = p;
		btree_t sub_tree = 
		{
			.m_fn_malloc = bt->m_fn_malloc,
			.m_fn_free = bt->m_fn_free,
			.m_root = v
		};
		btree_clear(&sub_tree,NULL,NULL);
	}
}

void predicate_map_clear(predicate_map_t* pm)
{
	btree_clear(pm,&clear_callback,&pm);
}

int predicate_is_builtin(const term_t* functor)
{
	switch (functor->m_u64val)
	{
#define DECLARE_BUILTIN_INTRINSIC(f,n) \
	case (n):

#define DECLARE_BUILTIN_FUNCTION(f,n) \
	case (n):

#include "builtin_functions.h"
		return 1;

	default:
		break;
	}

	return 0;
}

/*static int assert_is_callable(context_t* context, const term_t* goal)
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
#undef DECLARE_BUILTIN_INTRINSIC
#define DECLARE_BUILTIN_INTRINSIC(f,n) \
	case (n):

#undef DECLARE_BUILTIN_FUNCTION
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
}*/

static void assert_clause(context_t* context, const term_t* t, int z)
{
	//clause_t clause = {0};
	//term_to_clause(context,t,&clause);

	//predicate_t* pred = find_predicate(clause.m_head);


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
