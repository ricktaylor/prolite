
#include "stack.h"

#include <stdlib.h>
#include <strings.h>

static const uint64_t PAGE_SIZE = 4096 * sizeof(uint64_t);

#define ALIGN(x,y) (((x)+((y)-1)) & ~((y)-1))
#define ALIGN_DIV(x,y) (((x)+((y)-1)) / (y))

struct stack_t* stack_new(void*(*fn_malloc)(size_t), void(*fn_free)(void*))
{
	size_t align_size = PAGE_SIZE;
	size_t count = (align_size - sizeof(struct stack_t)) / sizeof(uint64_t);

	struct stack_t* stack = (*fn_malloc)(align_size);
	if (stack)
	{
		stack->m_top = 0;
		stack->m_count = count;
		stack->m_prev = NULL;
		stack->m_next = NULL;
		stack->m_fn_malloc = fn_malloc;
		stack->m_fn_free = fn_free;
		stack->m_base = 0;
	}
	return stack;
}

static struct stack_t* stack_insert_page(size_t size, struct stack_t* after)
{
	size_t align_size = ALIGN(size,PAGE_SIZE);
	size_t count = (align_size - sizeof(struct stack_t)) / sizeof(uint64_t);

	struct stack_t* stack = after->m_next;
	if (stack && stack->m_count >= count)
	{
		stack->m_top = 0;
		stack->m_base = after->m_base + after->m_top;
	}
	else
	{
		stack = (*after->m_fn_malloc)(align_size);
		if (stack)
		{
			stack->m_top = 0;
			stack->m_count = count;
			stack->m_fn_malloc = after->m_fn_malloc;
			stack->m_fn_free = after->m_fn_free;
			stack->m_prev = after;
			stack->m_next = NULL;
			if (stack->m_prev->m_next)
			{
				stack->m_next = stack->m_prev->m_next;
				stack->m_next->m_prev = stack;
			}
			stack->m_prev->m_next = stack;
			stack->m_base = stack->m_prev->m_base + stack->m_prev->m_top;
		}
	}
	return stack;
}

void stack_delete(struct stack_t* s)
{
	while (s)
	{
		struct stack_t* p = s->m_prev;
		(*s->m_fn_free)(s);
		s = p;
	}
}

void* stack_at(struct stack_t* stack, uint64_t pos)
{
	while (stack && stack->m_base + stack->m_top < pos)
		stack = stack->m_next;

	while (stack && stack->m_base > pos)
		stack = stack->m_prev;

	return &stack->m_data[pos - stack->m_base];
}

uint64_t stack_push(struct stack_t** stack, uint64_t val)
{
	if ((*stack)->m_top == (*stack)->m_count)
	{
		struct stack_t* s = stack_insert_page(sizeof(val),*stack);
		if (!s)
			return (uint64_t)-1;

		*stack = s;
	}

	/* No rounding, just pointer bump */
	(*stack)->m_data[(*stack)->m_top] = val;
	return (*stack)->m_base + (*stack)->m_top++;
}

void stack_reset_(struct stack_t** stack, uint64_t pos)
{
	if (pos >= stack_top(*stack))
		return;

	pos = ((*stack)->m_base + (*stack)->m_top) - pos;
	while (pos)
	{
		if (pos <= (*stack)->m_top)
		{
			(*stack)->m_top -= pos;
			break;
		}

		pos -= (*stack)->m_top;
		*stack = (*stack)->m_prev;
	}
}

void stack_compact(struct stack_t* stack)
{
	for (struct stack_t* next = stack->m_next;next;)
	{
		struct stack_t* s = next->m_next;
		(*next->m_fn_free)(next);
		next = s;
	}
	stack->m_next = NULL;
}

void* stack_malloc(struct stack_t** stack, size_t len)
{
	void* ptr = NULL;
	uint32_t align_len = ALIGN_DIV(len,sizeof(uint64_t));

	if ((*stack)->m_top + align_len > (*stack)->m_count)
	{
		struct stack_t* s = stack_insert_page(len,*stack);
		if (!s)
			return NULL;

		*stack = s;
	}

	/* No rounding, just pointer bump */
	ptr = &(*stack)->m_data[(*stack)->m_top];
	(*stack)->m_top += align_len;
	return ptr;
}

void stack_free(struct stack_t* stack, void* ptr, size_t len)
{
	while (stack && (ptr < (void*)&stack->m_data[0] || ptr >= (void*)&stack->m_data[stack->m_top]))
		stack = stack->m_prev;

	if (stack)
	{
		uint32_t align_len = ALIGN_DIV(len,sizeof(uint64_t));

		if (stack->m_top >= align_len && ptr == &stack->m_data[stack->m_top - align_len])
			stack->m_top -= align_len;
	}
}

void* stack_realloc(struct stack_t** stack, void* ptr, size_t old_len, size_t new_len)
{
	uint32_t align_old_len = ALIGN_DIV(old_len,sizeof(uint64_t));

	if ((*stack)->m_top >= align_old_len && ptr == &(*stack)->m_data[(*stack)->m_top - align_old_len])
	{
		uint32_t align_new_len = ALIGN_DIV(new_len,sizeof(uint64_t));
		if ((*stack)->m_top + (align_new_len - align_old_len) <= (*stack)->m_count)
		{
			(*stack)->m_top += (align_new_len - align_old_len);
			return ptr;
		}
	}

	if (old_len < new_len)
	{
		struct stack_t* s = *stack;
		void* new_ptr = stack_malloc(&s,new_len);
		if (old_len)
		{
			memcpy(new_ptr,ptr,old_len);
			stack_free(*stack,ptr,old_len);
		}
		*stack = s;
		ptr = new_ptr;
	}

	return ptr;
}

#if 0
int stack_copy(struct stack_t** dest, struct stack_t** src, size_t start)
{
	/* Bulk copy without extra splitting */
	if (start < stack_top(*src))
	{
		/* Rewind the start */
		struct stack_t* n;
		struct stack_t* s = *src;
		while (start < s->m_base)
			s = s->m_prev;

		/* Copy the first scrap off s */
		if (start == s->m_base)
			s = s->m_prev;
		else
		{
			uint64_t offset = start - s->m_base;
			uint64_t count = s->m_top - offset;
			void* p = stack_malloc(dest,count * sizeof(uint64_t));
			if (!p)
				return -1;

			memcpy(p,&(*src)->m_data[offset],count * sizeof(uint64_t));
			s->m_top = offset;
		}

		/* Swap the rest of the stacks */
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
