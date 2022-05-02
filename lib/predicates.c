#include "predicates.h"
#include "fnv1a.h"

#include <string.h>
#include <assert.h>

static uint64_t predicate_key(const term_t* pred, int* is_sub_tree)
{
	prolite_type_t type = unpack_term_type(pred);
	assert(type == prolite_atom || type == prolite_compound);

	uint64_t key = pred->m_u64val;
	unsigned int sub_type = unpack_term_subtype(pred);
	if (sub_type == 0 || sub_type == 3)
	{
		string_t s;
		unpack_predicate(pred,&s,NULL);
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

predicate_base_t* predicate_map_lookup(predicate_map_t* pm, const term_t* pred)
{
	int is_sub_tree = 0;
	uint64_t key = predicate_key(pred,&is_sub_tree);

	void* p = btree_lookup(pm,pred->m_u64val);
	if (p && is_sub_tree)
		p = btree_lookup(&(btree_t){ .m_allocator = pm->m_allocator, .m_root = p},key);
	
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
		btree_t sub_tree = { .m_allocator = pm->m_allocator };
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

static void callback2(void* p, uint64_t k, void* v)
{
	struct callback_param* cp = p;
	(*cp->m_callback)(cp->m_param,v);
}

static void enum_callback(void* p, uint64_t k, void* v)
{
	struct callback_param* cp = p;	
	unsigned int sub_type = unpack_term_subtype(&(term_t){ .m_u64val = k });
	if (sub_type == 0 || sub_type == 3)
		btree_enum(&(btree_t){ .m_allocator = cp->m_bt->m_allocator, .m_root = v},&callback2,p);
	else
		(*cp->m_callback)(cp->m_param,v);

}

void predicate_map_enum(predicate_map_t* pm, void (*callback)(void* param, predicate_base_t* pred), void* param)
{
	struct callback_param p = {
		.m_bt = pm,
		.m_callback = callback,
		.m_param = param
	};
	btree_enum(pm,&enum_callback,&p);
}

static void clear_callback(void* p, uint64_t k, void* v)
{
	struct callback_param* cp = p;
	unsigned int sub_type = unpack_term_subtype(&(term_t){ .m_u64val = k });
	if (sub_type == 0 || sub_type == 3)
		btree_clear(&(btree_t){ .m_allocator = cp->m_bt->m_allocator, .m_root = v},cp->m_callback ? &callback2 : NULL,cp);
	else if (cp->m_callback)
		(*cp->m_callback)(cp->m_param,v);
}

void predicate_map_clear(predicate_map_t* pm, void (*callback)(void* param, predicate_base_t* pred), void* param)
{
	struct callback_param p = {
		.m_bt = pm,
		.m_callback = callback,
		.m_param = param
	};
	btree_clear(pm,&clear_callback,&p);
}

int predicate_is_builtin(const term_t* pred)
{
	switch (MASK_DEBUG_INFO(pred->m_u64val))
	{
#define DECLARE_BUILTIN_INTRINSIC(f,p) \
	case (p):

#define DECLARE_BUILTIN_FUNCTION(f,a,p) \
	case (p):

#include "builtin_functions.h"
		return 1;

	default:
		break;
	}

	return 0;
}

#if 0
#include "context.h"

typedef struct clause
{
	size_t              m_locals_count;
	const term_t*       m_head;
	const term_t*       m_body;

} clause_t;

typedef struct predicate
{
	predicate_base_t m_base;
	unsigned         m_dynamic : 1;
	size_t           m_clause_count;
	clause_t*        m_clauses;
} predicate_t;

/*static int assert_is_callable(context_t* context, const term_t* goal)
{
	switch (unpack_term_type(goal))
	{
	case prolite_atom:
	case prolite_var:
		return 1;

	case prolite_compound:
		switch (MASK_DEBUG_INFO(goal->m_u64val))
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

	if (MASK_DEBUG_INFO(head->m_u64val) == PACK_COMPOUND_EMBED_2(2,':','-'))
	{
		head = get_first_arg(head,NULL);
		body = deref_local_var(context,get_next_arg(head));
	}
	head = deref_local_var(context,head);

	switch (MASK_DEBUG_INFO(head->m_u64val))
	{
#undef DECLARE_BUILTIN_INTRINSIC
#define DECLARE_BUILTIN_INTRINSIC(f,p) \
	case (n):

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,a,p) \
	case (p):

#include "builtin_functions.h"

		return throw_permission_error(context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),head);

	default:
		if (unpack_term_type(head) == prolite_var)
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

void builtin_assert(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	const term_t* goal = (context->m_stack--)->m_pval;
	const builtin_fn_t gosub = (context->m_stack--)->m_pval;

	int assertz = (goal->m_u64val == PACK_COMPOUND_BUILTIN(assertz,1));

	assert_clause(context,deref_local_var(context,get_first_arg(goal,NULL)),assertz);
}

void builtin_user_defined(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	return !!(context->m_flags & FLAG_THROW);
}
#endif