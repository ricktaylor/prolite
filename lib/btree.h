#ifndef BTREE_H_
#define BTREE_H_

#include "heap.h"

// This isn't a 'btree', it's a b+tree

struct btree_kv
{
	uint64_t key;
	void*    value;
};

typedef enum btree_page_type
{
	bt_root = 0,
	bt_internal,
	bt_leaf
} btree_page_type_t;

struct btree_page
{
	btree_page_type_t  m_type;
	struct btree_page* m_parent;
	size_t             m_count;
	struct btree_kv    m_data[];
};

typedef struct btree
{
	struct btree_page* m_root;
	void*            (*m_fn_malloc)(size_t);
	void             (*m_fn_free)(void*);
} btree_t;

void* btree_lookup(btree_t* bt, uint64_t key);
void* btree_insert(btree_t* bt, uint64_t key, void* val);
void* btree_remove(btree_t* bt, uint64_t key);

#endif // BTREE_H_
