
#include "heap.h"

#include <stdlib.h>
#include <string.h>

static const size_t c_page_size = 0x10000;

static inline size_t next_pot(size_t s)
{
	--s;
	s |= s >> 1;
	s |= s >> 2;
	s |= s >> 4;
	s |= s >> 8;
	s |= s >> 16;

	if (sizeof(size_t) > 4)
		s |= s >> 32;

	++s;
	return s;
}

void heap_destroy(heap_t* heap)
{
	if (heap)
	{
		heap_compact(heap);
		
		while (heap->m_page)
		{
			struct heap_page* p = heap->m_page->m_prev;
			allocator_free(heap->m_allocator,heap->m_page);
			heap->m_page = p;
		}
	}
}

void heap_reset(heap_t* heap, size_t pos)
{
	if (heap && heap->m_page)
	{
		while (pos < heap->m_page->m_base)
		{
			heap->m_page->m_top = 0;
			heap->m_page = heap->m_page->m_prev;
		}

		heap->m_page->m_top = pos;
	}
}

void heap_compact(heap_t* heap)
{
	if (heap && heap->m_page)
	{
		while (heap->m_page->m_next)
		{
			struct heap_page* n = heap->m_page->m_next->m_next;
			allocator_free(heap->m_allocator,heap->m_page->m_next);
			heap->m_page->m_next = n;
		}
	}
}

void* heap_malloc(heap_t* heap, size_t len)
{
	void* ptr = NULL;
	if (heap && len)
	{
		size_t align_len = bytes_to_cells(len,sizeof(uint64_t));
		while (heap->m_page && heap->m_page->m_top + align_len >= heap->m_page->m_count && heap->m_page->m_next)
		{
			heap->m_page->m_next->m_base = heap->m_page->m_base + heap->m_page->m_top;
			heap->m_page = heap->m_page->m_next;
		}
		
		if (!heap->m_page || heap->m_page->m_top + align_len >= heap->m_page->m_count)
		{
			size_t alloc_size = next_pot(align_len * sizeof(uint64_t) + sizeof(struct heap_page));
			if (alloc_size < c_page_size)
				alloc_size = c_page_size;

			struct heap_page* new_page = allocator_malloc(heap->m_allocator,alloc_size);
			if (!new_page)
				return NULL;
			
			new_page->m_top = 0;
			new_page->m_count = (alloc_size / sizeof(uint64_t)) - bytes_to_cells(sizeof(struct heap_page),sizeof(uint64_t));
			new_page->m_prev = heap->m_page;
			new_page->m_next = NULL;
			new_page->m_base = 0;
			if (heap->m_page)
			{
				heap->m_page->m_next = new_page;
				new_page->m_base = heap->m_page->m_base + heap->m_page->m_top;
			}
			heap->m_page = new_page;
		}

		/* No rounding, just pointer bump */
		ptr = heap->m_page->m_data + heap->m_page->m_top;
		heap->m_page->m_top += align_len;
	}
	return ptr;
}

void heap_free(heap_t* heap, void* ptr, size_t len)
{
	if (heap && heap->m_page && ptr && len)
	{
		size_t align_len = bytes_to_cells(len,sizeof(uint64_t));
		
		if (heap->m_page->m_top >= align_len && ptr == heap->m_page->m_data + (heap->m_page->m_top - align_len))
			heap->m_page->m_top -= align_len;
	}
}

void* heap_realloc(heap_t* heap, void* ptr, size_t old_len, size_t new_len)
{
	if (!heap)
		return NULL;

	if (ptr)
	{
		if (new_len == 0)
		{
			heap_free(heap,ptr,old_len);
			return NULL;
		}

		size_t align_old_len = bytes_to_cells(old_len,sizeof(uint64_t));
		if (heap->m_page && heap->m_page->m_top >= align_old_len && ptr == heap->m_page->m_data + (heap->m_page->m_top - align_old_len))
		{
			size_t align_new_len = bytes_to_cells(new_len,sizeof(uint64_t));
			if (heap->m_page->m_top + (align_new_len - align_old_len) <= heap->m_page->m_count)
			{
				heap->m_page->m_top += (align_new_len - align_old_len);
				return ptr;
			}
		}
	}

	if (!ptr || old_len < new_len)
	{
		void* new_ptr = heap_malloc(heap,new_len);
		if (!new_ptr)
			return NULL;

		if (ptr && old_len)
		{
			memcpy(new_ptr,ptr,old_len);
			heap_free(heap,ptr,old_len);
		}
		ptr = new_ptr;
	}

	return ptr;
}

static void* heap_allocator_malloc(void* param, size_t bytes)
{
	uint64_t* p = heap_malloc((heap_t*)param,bytes + sizeof(uint64_t));
	if (p)
		*p++ = bytes;
	
	return p;
}

static void heap_allocator_free(void* param, void* ptr)
{
	if (ptr)
	{
		size_t bytes = *(((uint64_t*)ptr) - 1);
		heap_free((heap_t*)param,ptr,bytes);
	}
}

prolite_allocator_t heap_allocator(heap_t* h)
{
	return (prolite_allocator_t){
		.m_fn_malloc = &heap_allocator_malloc,
		.m_fn_free = &heap_allocator_free,
		.m_param = h
	};
}
