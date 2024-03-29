#ifndef BTREE_H_
#define BTREE_H_

#include "../include/prolite.h"

// This isn't a 'btree', it's a b+tree

struct btree_page
{
	_Bool    m_internal;
	uint32_t m_count;
	uint64_t m_keys[];
};

typedef struct btree
{
	struct btree_page*   m_root;
	prolite_allocator_t* m_allocator;
} btree_t;

_Bool btree_exists(const btree_t* bt, uint64_t key);
void* btree_lookup(const btree_t* bt, uint64_t key);
void* btree_insert(btree_t* bt, uint64_t key, void* val);
void* btree_replace(btree_t* bt, uint64_t key, void* val);
void* btree_remove(btree_t* bt, uint64_t key);
size_t btree_enum(const btree_t* bt, void (*callback)(void* param, uint64_t key, void* val), void* param);
size_t btree_clear(btree_t* bt, void (*callback)(void* param, uint64_t key, void* val), void* param);

#endif // BTREE_H_
