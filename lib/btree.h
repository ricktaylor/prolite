#ifndef BTREE_H_
#define BTREE_H_

#include <stdint.h>
#include <stddef.h>

// This isn't a 'btree', it's a b+tree

struct btree_page
{
	_Bool    m_internal;
	uint32_t m_count;
	uint64_t m_keys[];
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
void btree_clear(btree_t* bt);

#endif // BTREE_H_
