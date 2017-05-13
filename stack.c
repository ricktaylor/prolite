
#include "stack.h"

#include <stdlib.h>
#include <string.h>

static const uint64_t PAGE_SIZE = 4096 * sizeof(uint64_t);

#define ALIGN(x,y) (((x)+((y)-1)) & ~(y))
#define ALIGN_DIV(x,y) (((x)+((y)-1)) / (y))

static struct stack_t* stack_push_page(size_t size, struct stack_t* prev)
{
	size_t align_size = ALIGN(size,PAGE_SIZE);
	struct stack_t* stack = malloc(align_size);
	if (stack)
	{
		stack->m_top = 0;
		stack->m_count = (align_size - sizeof(struct stack_t)) / sizeof(uint64_t);
		stack->m_prev = prev;
		if (stack->m_prev)
			stack->m_base = stack->m_prev->m_base + stack->m_prev->m_top;
		else
			stack->m_base = 0;
	}
	return stack;
}

void stack_delete(struct stack_t* s)
{
	while (s)
	{
		struct stack_t* p = s->m_prev;
		free(s);
		s = p;
	}
}

uint64_t stack_push(struct stack_t** stack, uint64_t val)
{
	if (!(*stack) || (*stack)->m_top == (*stack)->m_count)
	{
		struct stack_t* s = stack_push_page(sizeof(val),*stack);
		if (!s)
			return (uint64_t)-1;

		*stack = s;
	}

	/* No rounding, just pointer bump */
	(*stack)->m_data[(*stack)->m_top++] = val;
	return (*stack)->m_base + (*stack)->m_top;
}

void stack_reset(struct stack_t** stack, uint64_t pos)
{
	if (!(*stack) || (*stack)->m_base + (*stack)->m_top <= pos)
		return;

	pos = ((*stack)->m_base + (*stack)->m_top) - pos;
	while (pos)
	{
		if (pos < (*stack)->m_top)
		{
			(*stack)->m_top -= pos;
			return;
		}

		pos -= (*stack)->m_top;

		struct stack_t* p = (*stack)->m_prev;
		free(*stack);
		*stack = p;
	}
}

void* stack_malloc(struct stack_t** stack, size_t len)
{
	void* ptr = NULL;
	uint32_t align_len = ALIGN_DIV(len,sizeof(uint64_t));

	if (!(*stack))
	{
		*stack = stack_push_page(len,NULL);
		if (!(*stack))
			return NULL;
	}

	if ((*stack)->m_top + align_len > (*stack)->m_count)
	{
		struct stack_t* s = stack_push_page(len,*stack);
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
	uint32_t align_len = ALIGN_DIV(len,sizeof(uint64_t));

	if (!stack)
		return;

	if (stack->m_top >= align_len && ptr == &stack->m_data[stack->m_top - align_len])
		stack->m_top -= align_len;
}

void* stack_realloc(struct stack_t** stack, void* ptr, size_t old_len, size_t new_len)
{
	struct stack_t* s;
	void* new_ptr;
	uint32_t align_old_len = ALIGN_DIV(old_len,sizeof(uint64_t));
	uint32_t align_new_len = ALIGN_DIV(new_len,sizeof(uint64_t));

	if (!(*stack))
		return stack_malloc(stack,new_len);

	if ((*stack)->m_top >= align_old_len && ptr == &(*stack)->m_data[(*stack)->m_top - align_old_len])
	{
		if ((*stack)->m_top + (align_new_len - align_old_len) <= (*stack)->m_count)
		{
			(*stack)->m_top += (align_new_len - align_old_len);
			return ptr;
		}

		(*stack)->m_top -= align_old_len;

		s = stack_push_page(new_len,*stack);
		if (!s)
		{
			(*stack)->m_top += align_old_len;
			return NULL;
		}
	}
	else
	{
		if (old_len >= new_len)
			return ptr;

		s = stack_push_page(new_len,*stack);
		if (!s)
			return NULL;
	}

	*stack = s;
	new_ptr = &(*stack)->m_data[(*stack)->m_top];
	(*stack)->m_top += align_new_len;

	memcpy(new_ptr,ptr,old_len);
	return new_ptr;
}
