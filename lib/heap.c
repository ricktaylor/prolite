
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

static struct heap_page* heap_insert_page(void*(*fn_malloc)(size_t), size_t size, struct heap_page* after)
{
	size_t align_size = next_pot(size + sizeof(struct heap_page));
	if (align_size < 65536)
		align_size = 65536;

	struct heap_page* page = after ? after->m_next : NULL;
	if (page && page->m_count >= align_size / sizeof(uint64_t))
	{
		page->m_top = 0;
		page->m_base = after->m_base + after->m_top;
	}
	else
	{
		//while (align_size <= after->m_count * sizeof(uint64_t))
		//	align_size *= 2;

		page = (*fn_malloc)(align_size);
		if (page)
		{
			page->m_top = 0;
			page->m_count = (align_size / sizeof(uint64_t)) - bytes_to_cells(sizeof(heap_t),sizeof(uint64_t));
			page->m_prev = after;
			page->m_next = NULL;
			page->m_base = 0;
			if (page->m_prev)
			{
				if (page->m_prev->m_next)
				{
					page->m_next = page->m_prev->m_next;
					page->m_next->m_prev = page;
				}
				page->m_prev->m_next = page;
				page->m_base = page->m_prev->m_base + page->m_prev->m_top;
			}			
		}
	}
	return page;
}

void heap_delete(heap_t* heap)
{
	if (heap)
	{
		for (struct heap_page* page = heap->m_page; page;)
		{
			struct heap_page* p = page->m_prev;
			(*heap->m_fn_free)(page);
			page = p;
		}
	}
}

const void* heap_at(const heap_t* heap, size_t pos)
{
	struct heap_page* page = (heap ? heap->m_page : NULL);

	while (page && page->m_base + page->m_top < pos)
		page = page->m_next;

	while (page && page->m_base > pos)
		page = page->m_prev;

	return &page->m_data[pos - page->m_base];
}

void heap_reset(heap_t* heap, size_t pos)
{
	if (pos >= heap_top(heap))
		return;

	struct heap_page* page = heap->m_page;
	for (pos = page->m_base + page->m_top - pos; pos;)
	{
		if (pos <= page->m_top)
		{
			page->m_top -= pos;
			break;
		}

		pos -= page->m_top;
		page = page->m_prev;
	}
	heap->m_page = page;
}

void heap_compact(heap_t* heap)
{
	if (heap && heap->m_page)
	{
		for (struct heap_page* next = heap->m_page->m_next; next;)
		{
			struct heap_page* n = next->m_next;
			(*heap->m_fn_free)(next);
			next = n;
		}
		heap->m_page->m_next = NULL;
	}
}

void* heap_malloc(heap_t* heap, size_t len)
{
	void* ptr = NULL;
	if (heap && len)
	{
		uint32_t align_len = bytes_to_cells(len,sizeof(uint64_t));

		if (!heap->m_page || heap->m_page->m_top + align_len >= heap->m_page->m_count)
		{
			struct heap_page* page = heap_insert_page(heap->m_fn_malloc,len,heap->m_page);
			if (!page)
				return NULL;

			heap->m_page = page;
		}

		/* No rounding, just pointer bump */
		ptr = heap->m_page->m_data + heap->m_page->m_top;
		heap->m_page->m_top += align_len;
	}
	return ptr;
}

void heap_free(heap_t* heap, void* ptr, size_t len)
{
	if (heap && ptr && len)
	{
		uint32_t align_len = bytes_to_cells(len,sizeof(uint64_t));
		for (struct heap_page* page = heap->m_page; page; )
		{
			if (ptr >= (void*)page->m_data && ptr < (void*)(page->m_data + page->m_top))
			{
				if (page->m_top >= align_len && ptr == page->m_data + page->m_top - align_len)
					page->m_top -= align_len;

				heap->m_page = page;
				break;
			}

			page = page->m_prev;
		}
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

		uint32_t align_old_len = bytes_to_cells(old_len,sizeof(uint64_t));
		if (heap->m_page && heap->m_page->m_top >= align_old_len && ptr == heap->m_page->m_data + heap->m_page->m_top - align_old_len)
		{
			uint32_t align_new_len = bytes_to_cells(new_len,sizeof(uint64_t));
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
