
#include "heap.h"

#include <stdlib.h>
#include <string.h>

static inline uint32_t next_pot(size_t s)
{
	--s;
	s |= s >> 1;
	s |= s >> 2;
	s |= s >> 4;
	s |= s >> 8;
	s |= s >> 16;
	++s;
	return s;
}

heap_t* heap_new(size_t size, void*(*fn_malloc)(size_t), void(*fn_free)(void*))
{
	size_t align_size = next_pot(size + sizeof(heap_t));

	heap_t* heap = (*fn_malloc)(align_size);
	if (heap)
	{
		heap->m_top = 0;
		heap->m_count = (align_size / sizeof(uint64_t)) - len_to_cells(sizeof(heap_t),sizeof(uint64_t));
		heap->m_prev = NULL;
		heap->m_next = NULL;
		heap->m_fn_malloc = fn_malloc;
		heap->m_fn_free = fn_free;
		heap->m_base = 0;
	}
	return heap;
}

static heap_t* heap_insert_page(size_t size, heap_t* after)
{
	size_t align_size = next_pot(size + sizeof(heap_t));

	heap_t* heap = after->m_next;
	if (heap && heap->m_count >= align_size / sizeof(uint64_t))
	{
		heap->m_top = 0;
		heap->m_base = after->m_base + after->m_top;
	}
	else
	{
		while (align_size <= after->m_count * sizeof(uint64_t))
			align_size *= 2;

		heap = (*after->m_fn_malloc)(align_size);
		if (heap)
		{
			heap->m_top = 0;
			heap->m_count = (align_size / sizeof(uint64_t)) - len_to_cells(sizeof(heap_t),sizeof(uint64_t));
			heap->m_fn_malloc = after->m_fn_malloc;
			heap->m_fn_free = after->m_fn_free;
			heap->m_prev = after;
			heap->m_next = NULL;
			if (heap->m_prev->m_next)
			{
				heap->m_next = heap->m_prev->m_next;
				heap->m_next->m_prev = heap;
			}
			heap->m_prev->m_next = heap;
			heap->m_base = heap->m_prev->m_base + heap->m_prev->m_top;
		}
	}
	return heap;
}

void heap_delete(heap_t* s)
{
	while (s)
	{
		heap_t* p = s->m_prev;
		(*s->m_fn_free)(s);
		s = p;
	}
}

void heap_compact(heap_t* heap)
{
	heap_t* next = heap->m_next;
	while (next)
	{
		heap_t* s = next->m_next;
		(*next->m_fn_free)(next);
		next = s;
	}
	heap->m_next = NULL;
}

void* heap_malloc(heap_t** heap, size_t len)
{
	void* ptr = NULL;
	uint32_t align_len = len_to_cells(len,sizeof(uint64_t));

	if ((*heap)->m_top + align_len >= (*heap)->m_count)
	{
		heap_t* s = heap_insert_page(len,*heap);
		if (!s)
			return NULL;

		*heap = s;
	}

	/* No rounding, just pointer bump */
	ptr = &(*heap)->m_data[(*heap)->m_top];
	(*heap)->m_top += align_len;
	return ptr;
}

void heap_free(heap_t* heap, void* ptr, size_t len)
{
	uint32_t align_len = len_to_cells(len,sizeof(uint64_t));
	while (heap)
	{
		if (ptr >= (void*)&heap->m_data[0] && ptr < (void*)&heap->m_data[heap->m_top])
		{
			if (heap->m_top >= align_len && ptr == &heap->m_data[heap->m_top - align_len])
				heap->m_top -= align_len;

			break;
		}

		heap = heap->m_prev;
	}
}

void* heap_realloc(heap_t** heap, void* ptr, size_t old_len, size_t new_len)
{
	uint32_t align_old_len = len_to_cells(old_len,sizeof(uint64_t));

	if ((*heap)->m_top >= align_old_len && ptr == &(*heap)->m_data[(*heap)->m_top - align_old_len])
	{
		uint32_t align_new_len = len_to_cells(new_len,sizeof(uint64_t));
		if ((*heap)->m_top + (align_new_len - align_old_len) <= (*heap)->m_count)
		{
			(*heap)->m_top += (align_new_len - align_old_len);
			return ptr;
		}
	}

	if (old_len < new_len)
	{
		void* new_ptr = heap_malloc(heap,new_len);
		if (!new_ptr)
			return NULL;

		if (ptr && old_len)
		{
			memcpy(new_ptr,ptr,old_len);
			heap_free(*heap,ptr,old_len);
		}
		ptr = new_ptr;
	}

	return ptr;
}

#if 0
int heap_copy(heap_t** dest, heap_t** src, size_t start)
{
	/* Bulk copy without extra splitting */
	if (start < heap_top(*src))
	{
		/* Rewind the start */
		heap_t* n;
		heap_t* s = *src;
		while (start < s->m_base)
			s = s->m_prev;

		/* Copy the first scrap off s */
		if (start == s->m_base)
			s = s->m_prev;
		else
		{
			uint64_t offset = start - s->m_base;
			uint64_t count = s->m_top - offset;
			void* p = heap_malloc(dest,count * sizeof(uint64_t));
			if (!p)
				return -1;

			memcpy(p,&(*src)->m_data[offset],count * sizeof(uint64_t));
			s->m_top = offset;
		}

		/* Swap the rest of the heaps */
		n = (*dest)->m_next;
		(*dest)->m_next = s->m_next;
		if ((*dest)->m_next)
			(*dest)->m_next->m_prev = *dest;

		s->m_next = n;
		if (s->m_next)
			s->m_next->m_prev = s;
		*src = s;
	}

	return 0;
}
#endif
