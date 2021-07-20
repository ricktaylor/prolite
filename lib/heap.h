#ifndef heap_H_INCLUDED_
#define heap_H_INCLUDED_

#include <stdint.h>
#include <stdlib.h>

#if defined(_MSC_VER)
#define inline __inline
#endif

// This isn't a 'heap', it's just an arena allocator for the heap
struct heap_t
{
	struct heap_t* m_prev;
	struct heap_t* m_next;
	uint32_t       m_count;
	uint32_t       m_top;
	size_t         m_base;
	void*        (*m_fn_malloc)(size_t);
	void         (*m_fn_free)(void*);
	uint64_t       m_data[];
};

struct heap_t* heap_new(size_t size, void*(*fn_malloc)(size_t), void(*fn_free)(void*));
void heap_delete(struct heap_t* s);

static inline size_t heap_top(const struct heap_t* heap)
{
	return (!heap ? 0 : (heap->m_base + heap->m_top));
}

static inline const void* heap_at(const struct heap_t* heap, size_t pos)
{
	while (heap && heap->m_base + heap->m_top < pos)
		heap = heap->m_next;

	while (heap && heap->m_base > pos)
		heap = heap->m_prev;

	return &heap->m_data[pos - heap->m_base];
}

static inline void heap_reset(struct heap_t** heap, size_t pos)
{
	if (pos >= heap_top(*heap))
		return;

	pos = ((*heap)->m_base + (*heap)->m_top) - pos;
	while (pos)
	{
		if (pos <= (*heap)->m_top)
		{
			(*heap)->m_top -= pos;
			break;
		}

		pos -= (*heap)->m_top;
		*heap = (*heap)->m_prev;
	}
}

void heap_compact(struct heap_t* heap);

void* heap_malloc(struct heap_t** heap, size_t len);

void heap_free(struct heap_t* heap, void* ptr, size_t len);

void* heap_realloc(struct heap_t** heap, void* ptr, size_t old_len, size_t new_len);

#endif /* heap_H_INCLUDED_ */
