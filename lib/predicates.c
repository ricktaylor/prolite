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
