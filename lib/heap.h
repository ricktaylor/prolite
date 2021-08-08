#ifndef heap_H_INCLUDED_
#define heap_H_INCLUDED_

#include <stdint.h>
#include <stddef.h>

#if defined(_MSC_VER)
#define inline __inline
#endif

// This isn't a 'heap', it's just an arena allocator for the heap

struct heap;

struct heap_page
{
	struct heap*      m_heap;
	struct heap_page* m_prev;
	struct heap_page* m_next;
	uint32_t          m_count;
	uint32_t          m_top;
	size_t            m_base;
	uint64_t          m_data[];
};

typedef struct heap
{
	struct heap_page* m_page;
	void*           (*m_fn_malloc)(size_t);
	void            (*m_fn_free)(void*);
} heap_t;

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

void* heap_malloc(heap_t* heap, size_t len);

void heap_free(heap_t* heap, void* ptr, size_t len);

void* heap_realloc(heap_t* heap, void* ptr, size_t old_len, size_t new_len);

#endif /* heap_H_INCLUDED_ */
