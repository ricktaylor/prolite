#ifndef HEAP_H_
#define HEAP_H_

#include "../include/prolite.h"

#include <stdint.h>
#include <stdlib.h>

// This is a bump allocated 'fast' heap

struct heap;

struct heap_page
{
	struct heap_page* m_prev;
	struct heap_page* m_next;
	uint32_t          m_count;
	uint32_t          m_top;
	size_t            m_base;
	uint64_t          m_data[];
};

typedef struct heap
{
	struct heap_page*    m_page;
	prolite_allocator_t* m_allocator;
} heap_t;

static inline void* allocator_malloc(prolite_allocator_t* a, size_t bytes)
{
	return a ? (*a->m_fn_malloc)(a->m_user_data,bytes) : malloc(bytes);
}

static inline void* allocator_realloc(prolite_allocator_t* a, void* ptr, size_t bytes)
{
	return a ? (*a->m_fn_realloc)(a->m_user_data,ptr,bytes) : realloc(ptr,bytes);
}

static inline void* allocator_free(prolite_allocator_t* a, void* ptr)
{
	if (ptr)
	{
		if (a)
			(*a->m_fn_free)(a->m_user_data,ptr);
		else
			free(ptr);
	}
	return NULL;
}

void heap_destroy(heap_t* s);

static inline size_t bytes_to_cells(size_t len, size_t cell_size)
{
	return (len + (cell_size-1)) / cell_size;
}

static inline size_t heap_top(const heap_t* heap)
{
	return ((!heap || !heap->m_page) ? 0 : (heap->m_page->m_base + heap->m_page->m_top));
}

void heap_reset(heap_t* heap, size_t pos);
void heap_compact(heap_t* heap);

heap_t heap_clone(heap_t* heap);
void heap_merge(heap_t* to, heap_t* from);

void* heap_malloc(heap_t* heap, size_t len);
void heap_free(heap_t* heap, void* ptr, size_t len);

void* heap_allocator_init(heap_t* heap);
void* heap_allocator_malloc(void* param, size_t bytes);
void* heap_allocator_realloc(void* param, void* ptr, size_t bytes);
void heap_allocator_free(void* param, void* ptr);
void heap_allocator_destroy(prolite_allocator_t* a);

#define heap_allocator(h) (prolite_allocator_t){ \
		.m_fn_malloc = &heap_allocator_malloc, \
		.m_fn_realloc = &heap_allocator_realloc, \
		.m_fn_free = &heap_allocator_free, \
		.m_user_data = heap_allocator_init(h) }

void* bump_allocator_malloc(void* param, size_t bytes);
void* bump_allocator_realloc(void* param, void* ptr, size_t bytes);
void bump_allocator_free(void* param, void* ptr);

#define bump_allocator(h) (prolite_allocator_t){ \
		.m_fn_malloc = &bump_allocator_malloc, \
		.m_fn_realloc = &bump_allocator_realloc, \
		.m_fn_free = &bump_allocator_free, \
		.m_user_data = (h) }

#endif /* HEAP_H_ */
