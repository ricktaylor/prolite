
#include "heap.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

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
	if (heap)
	{
		while (heap->m_page && pos < heap->m_page->m_base)
		{
			heap->m_page->m_top = 0;
			heap->m_page = heap->m_page->m_prev;
		}

		if (heap->m_page)
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

heap_t heap_clone(heap_t* heap)
{
	assert(heap);

	heap_t h = { .m_allocator = heap->m_allocator };

	if (heap->m_page && heap->m_page->m_next)
	{
		h.m_page = heap->m_page->m_next;
		heap->m_page->m_next = NULL;

		h.m_page->m_prev = NULL;
	}

	return h;
}

void heap_merge(heap_t* to, heap_t* from)
{
	assert(to && from);
	assert(to->m_allocator == from->m_allocator);

	while (from->m_page && from->m_page->m_prev)
		from->m_page = from->m_page->m_prev;

	assert(!from->m_page || from->m_page->m_top == 0);

	struct heap_page* p = to->m_page;
	while (p && p->m_next)
		p = p->m_next;

	p->m_next = from->m_page;
	if (from->m_page)
		from->m_page->m_prev = p;

	from->m_page = NULL;
}

static void* heap_malloc_inner(heap_t* heap, size_t align_len)
{
	while (heap->m_page && heap->m_page->m_top + align_len >= heap->m_page->m_count && heap->m_page->m_next)
	{
		heap->m_page->m_next->m_base = heap->m_page->m_base + heap->m_page->m_top;
		heap->m_page = heap->m_page->m_next;
	}

	if (!heap->m_page || heap->m_page->m_top + align_len >= heap->m_page->m_count)
	{
		size_t alloc_size = align_len * sizeof(uint64_t) + sizeof(struct heap_page);
		if (alloc_size < c_page_size)
			alloc_size = c_page_size;
		else
			alloc_size = next_pot(alloc_size);

		struct heap_page* new_page = allocator_malloc(heap->m_allocator,alloc_size);
		if (!new_page)
			return NULL;

		*new_page = (struct heap_page){
			.m_count = (alloc_size - sizeof(struct heap_page))  / sizeof(uint64_t),
			.m_prev = heap->m_page,
		};

		assert(new_page->m_data + new_page->m_count == (uint64_t*)new_page + (alloc_size / sizeof(uint64_t)));

		if (heap->m_page)
		{
			heap->m_page->m_next = new_page;
			new_page->m_base = heap->m_page->m_base + heap->m_page->m_top;
		}
		heap->m_page = new_page;
	}

	/* No rounding, just pointer bump */
	void* ptr = heap->m_page->m_data + heap->m_page->m_top;
	heap->m_page->m_top += align_len;

	return ptr;
}

void* heap_malloc(heap_t* heap, size_t bytes)
{
	void* ptr = NULL;
	if (heap && bytes)
		ptr = heap_malloc_inner(heap,bytes_to_cells(bytes,sizeof(uint64_t)));

	return ptr;
}

#if !defined(NDEBUG)
static void write_deadbeef(void* p, size_t count)
{
	for (size_t i = 0; i < count; ++i)
		*((uint64_t*)p + i) = UINT64_C(0xEFBEADDEEFBEADDE);
}
#else
#define write_deadbeef(p,c)
#endif

static void heap_free_inner(heap_t* heap, void* ptr, size_t align_len)
{
	assert((uintptr_t)ptr % sizeof(uint64_t) == 0);

	for (struct heap_page* page = heap->m_page; page; page = page->m_prev)
	{
		if ((uint64_t*)ptr >= page->m_data && (uint64_t*)ptr < page->m_data + page->m_count)
		{
			assert((uint64_t*)ptr <= page->m_data + (page->m_top - align_len));

			write_deadbeef(ptr,align_len);

			if (ptr == page->m_data + (page->m_top - align_len))
			{
				page->m_top -= align_len;

				while (!heap->m_page->m_top && heap->m_page->m_prev)
					heap->m_page = heap->m_page->m_prev;
			}
			return;
		}
	}

	assert(0);
}

void heap_free(heap_t* heap, void* ptr, size_t bytes)
{
	if (heap && heap->m_page && ptr && bytes)
		heap_free_inner(heap,ptr,bytes_to_cells(bytes,sizeof(uint64_t)));
}

void* bump_allocator_malloc(void* param, size_t bytes)
{
	return heap_malloc((heap_t*)param,bytes);
}

void* bump_allocator_realloc(void* param, void* ptr, size_t bytes)
{
	assert((uintptr_t)ptr % sizeof(uint64_t) == 0);

	heap_t* heap = param;
	if (bytes == 0)
	{
		bump_allocator_free(heap,ptr);
		return NULL;
	}

	size_t align_new_len = bytes_to_cells(bytes,sizeof(uint64_t));
	if (!ptr)
		return heap_malloc_inner(heap,align_new_len);

	if (!heap || !heap->m_page)
		return NULL;

	if ((uint64_t*)ptr >= heap->m_page->m_data && (uint64_t*)ptr < heap->m_page->m_data + heap->m_page->m_top)
	{
		size_t align_old_len = heap->m_page->m_top - ((uint64_t*)ptr - heap->m_page->m_data);

		if (ptr == heap->m_page->m_data + heap->m_page->m_top - align_old_len &&
			heap->m_page->m_top + (align_new_len - align_old_len) <= heap->m_page->m_count)
		{
			heap->m_page->m_top += (align_new_len - align_old_len);
			return ptr;
		}

		void* new_ptr = heap_malloc_inner(heap,align_new_len);
		if (!new_ptr)
			return NULL;

		if (ptr && bytes)
		{
			memcpy(new_ptr,ptr,bytes);
			heap_free_inner(heap,ptr,align_old_len);
		}
		return new_ptr;
	}

	assert(0);
	return NULL;
}

void bump_allocator_free(void* param, void* ptr)
{
	assert((uintptr_t)ptr % sizeof(uint64_t) == 0);

	heap_t* heap = param;
	if (heap && heap->m_page && ptr)
	{
		if ((uint64_t*)ptr >= heap->m_page->m_data && (uint64_t*)ptr < heap->m_page->m_data + heap->m_page->m_top)
		{
			heap->m_page->m_top = ((uint64_t*)ptr - heap->m_page->m_data);
			while (!heap->m_page->m_top && heap->m_page->m_prev)
				heap->m_page = heap->m_page->m_prev;
		}

		assert(0);
	}
}

struct alloc_header
{
	uint64_t          m_free : 1;
	uint64_t          m_size : 63;
	struct heap_page* m_page;
	uint64_t          m_data[];
};
static const size_t c_alloc_header_size = offsetof(struct alloc_header,m_data) / sizeof(uint64_t);

struct free_header
{
	struct alloc_header m_header;
	struct free_header* m_prev;
	struct free_header* m_next;
};
static const size_t c_free_header_size = (sizeof(struct free_header) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);

struct heap_allocator
{
	heap_t*             m_heap;
	struct free_header* m_free_list;
};

void* heap_allocator_init(heap_t* heap)
{
	struct heap_allocator* h = heap_malloc(heap,sizeof(struct heap_allocator));
	if (h)
	{
		if (heap->m_page->m_count - heap->m_page->m_top >= c_free_header_size)
		{
			// Grab the rest of the heap page as free space
			struct free_header* f = (struct free_header*)(heap->m_page->m_data + heap->m_page->m_top);
			*f = (struct free_header){
				.m_header.m_free = 1,
				.m_header.m_size = heap->m_page->m_count - heap->m_page->m_top - c_alloc_header_size,
				.m_header.m_page = heap->m_page
			};
			h->m_free_list = f;
			heap->m_page->m_top = heap->m_page->m_count;

			write_deadbeef(f+1,f->m_header.m_size + c_alloc_header_size - c_free_header_size);
		}
		else
			h->m_free_list = NULL;

		h->m_heap = heap;
	}
	return h;
}

static void remove_block(struct heap_allocator* h, struct free_header* n)
{
	if (n->m_next)
		n->m_next->m_prev = n->m_prev;

	if (n->m_prev)
		n->m_prev->m_next = n->m_next;

	if (h->m_free_list == n)
		h->m_free_list = n->m_prev;
}

void* heap_allocator_malloc_inner(struct heap_allocator* h, size_t align_new_len)
{
	if (align_new_len < c_free_header_size - c_alloc_header_size)
		align_new_len = c_free_header_size - c_alloc_header_size;

	// Find best-fit free
	struct free_header* b = NULL;
	for (struct free_header* n = h->m_free_list; n; n = n->m_prev)
	{
		if (n->m_header.m_size >= align_new_len)
		{
			if (n->m_header.m_size >= align_new_len + c_free_header_size)
			{
				if (!b || b->m_header.m_size > n->m_header.m_size)
					b = n;
			}
			else
			{
				// Use this block
				remove_block(h,n);
				n->m_header.m_free = 0;
				return n->m_header.m_data;
			}
		}
	}

	if (b)
	{
		// Split the free block
		struct free_header* s = (struct free_header*)(b->m_header.m_data + align_new_len);
		*s = (struct free_header){
			.m_header.m_free = 1,
			.m_header.m_size = b->m_header.m_size - align_new_len - c_alloc_header_size,
			.m_header.m_page = b->m_header.m_page,
			.m_next = b->m_next,
			.m_prev = b->m_prev
		};

		if (s->m_next)
			s->m_next->m_prev = s;

		if (s->m_prev)
			s->m_prev->m_next = s;

		if (h->m_free_list == b)
			h->m_free_list = s;

		b->m_header.m_size = align_new_len;
		b->m_header.m_free = 0;
		return b->m_header.m_data;
	}

	// If we got here, we need more data
	struct alloc_header* p = heap_malloc_inner(h->m_heap,align_new_len + c_alloc_header_size);
	if (!p)
		return NULL;

	*p = (struct alloc_header){
		.m_size = align_new_len,
		.m_page = h->m_heap->m_page
	};

	if (p->m_page->m_count - p->m_page->m_top >= c_free_header_size)
	{
		// Grab the rest of the heap page as free space
		struct free_header* f = (struct free_header*)(p->m_page->m_data + p->m_page->m_top);
		*f = (struct free_header){
			.m_header.m_free = 1,
			.m_header.m_size = p->m_page->m_count - p->m_page->m_top - c_alloc_header_size,
			.m_header.m_page = p->m_page,
			.m_prev = h->m_free_list
		};
		if (f->m_prev)
			f->m_prev->m_next = f;
		h->m_free_list = f;

		write_deadbeef(f+1,f->m_header.m_size + c_alloc_header_size - c_free_header_size);
	}
	else
		p->m_size += p->m_page->m_count - p->m_page->m_top;

	p->m_page->m_top = p->m_page->m_count;

	return p->m_data;
}

void* heap_allocator_malloc(void* param, size_t bytes)
{
	if (!bytes)
		return NULL;

	return heap_allocator_malloc_inner(param,bytes_to_cells(bytes,sizeof(uint64_t)));
}

static struct free_header* get_next_free_block(struct alloc_header* p)
{
	struct free_header* n = NULL;

	if (p->m_data + p->m_size <= p->m_page->m_data + p->m_page->m_top - c_free_header_size)
	{
		n = (struct free_header*)(p->m_data + p->m_size);

		assert(n->m_header.m_page == p->m_page);

		if (!n->m_header.m_free)
			n = NULL;
	}

	return n;
}

static void heap_allocator_free_inner(struct heap_allocator* h, struct free_header* p)
{
	p->m_header.m_free = 1;

	// Merge with next
	struct free_header* n = get_next_free_block(&p->m_header);
	if (n)
	{
		p->m_next = n->m_next;
		if (p->m_next)
			p->m_next->m_prev = p;

		p->m_prev = n->m_prev;
		if (p->m_prev)
			p->m_prev->m_next = p;

		if (h->m_free_list == n)
			h->m_free_list = p;

		size_t s = p->m_header.m_size;
		p->m_header.m_size += n->m_header.m_size + c_alloc_header_size;
		write_deadbeef(p+1,s + c_alloc_header_size);

		// Find a prev to merge with
		struct free_header* q = p->m_prev;
		if (!q || q->m_header.m_data + q->m_header.m_size != (uint64_t*)p)
		{
			for (q = h->m_free_list; q; q = q->m_prev)
			{
				if (q != p && q->m_header.m_data + q->m_header.m_size == (uint64_t*)p)
					break;
			}
		}

		if (q)
		{
			// Remove p from free list
			remove_block(h,p);
			q->m_header.m_size += p->m_header.m_size + c_alloc_header_size;
			write_deadbeef(p,c_free_header_size);
		}
		return;
	}

	// Find a prev to merge with
	for (struct free_header* n = h->m_free_list; n; n = n->m_prev)
	{
		if (n->m_header.m_data + n->m_header.m_size == (uint64_t*)p)
		{
			n->m_header.m_size += p->m_header.m_size + c_alloc_header_size;
			write_deadbeef(p,p->m_header.m_size + c_alloc_header_size);
			return;
		}
	}

	// Add to free list
	p->m_next = NULL;
	p->m_prev = h->m_free_list;
	if (p->m_prev)
		p->m_prev->m_next = p;

	h->m_free_list = p;
	write_deadbeef(p+1,p->m_header.m_size + c_alloc_header_size - c_free_header_size);
}

void* heap_allocator_realloc(void* param, void* ptr, size_t bytes)
{
	assert((uintptr_t)ptr % sizeof(uint64_t) == 0);

	size_t align_new_len = bytes_to_cells(bytes,sizeof(uint64_t));

	if (!ptr)
		return heap_allocator_malloc_inner(param,align_new_len);

	struct alloc_header* p = (struct alloc_header*)ptr - 1;
	assert(!p->m_free);
	assert(p->m_data > p->m_page->m_data && p->m_data + p->m_size <= p->m_page->m_data + p->m_page->m_count);

	if (!bytes)
	{
		heap_allocator_free_inner(param,(struct free_header*)p);
		return NULL;
	}

	if (p->m_size >= align_new_len)
		return p->m_data;

	// Try to steal from next
	struct free_header* n = get_next_free_block(p);
	if (n)
	{
		size_t align_extra = align_new_len - p->m_size;
		if (align_extra < c_free_header_size)
			align_extra = c_free_header_size;

		if (n->m_header.m_size >= align_extra + c_free_header_size)
		{
			// Steal from next block
			struct free_header* s = (struct free_header*)(p->m_data + p->m_size + align_extra);
			*s = *n;
			s->m_header.m_size -= align_extra;

			if (s->m_next)
				s->m_next->m_prev = s;

			if (s->m_prev)
				s->m_prev->m_next = s;

			if (((struct heap_allocator*)param)->m_free_list == n)
				((struct heap_allocator*)param)->m_free_list = s;

			p->m_size += align_extra;
		}
		else
		{
			// Consume this block
			remove_block(param,n);
			p->m_size += n->m_header.m_size + c_alloc_header_size;
		}
		return ptr;
	}

	void* new_ptr = heap_allocator_malloc_inner(param,align_new_len);
	if (!new_ptr)
		return NULL;

	memcpy(new_ptr,ptr,p->m_size * sizeof(uint64_t));
	heap_allocator_free_inner(param,(struct free_header*)p);

	return new_ptr;
}

void heap_allocator_free(void* param, void* ptr)
{
	assert((uintptr_t)ptr % sizeof(uint64_t) == 0);

	if (ptr)
	{
		struct free_header* p = (struct free_header*)((struct alloc_header*)ptr - 1);

		assert(p->m_header.m_data > p->m_header.m_page->m_data && p->m_header.m_data + p->m_header.m_size <= p->m_header.m_page->m_data + p->m_header.m_page->m_count);
		assert(!p->m_header.m_free);

		heap_allocator_free_inner(param,p);
	}
}

void heap_allocator_destroy(prolite_allocator_t* a)
{
	struct heap_allocator* h = a->m_user_data;

	assert(!h->m_free_list->m_prev);

	heap_free(h->m_heap,h,sizeof(struct heap_allocator));
	a->m_user_data = NULL;
}
