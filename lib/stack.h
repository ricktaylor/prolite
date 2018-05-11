
#ifndef STACK_H_INCLUDED_
#define STACK_H_INCLUDED_

#include <stdint.h>
#include <stdlib.h>

#if defined(_MSC_VER)
#define inline __inline
#endif

struct stack_t
{
	struct stack_t* m_prev;
	struct stack_t* m_next;
	uint32_t        m_count;
	uint32_t        m_top;
	size_t          m_base;
	void*         (*m_fn_malloc)(size_t);
	void          (*m_fn_free)(void*);
	uint64_t        m_data[];
};

struct stack_t* stack_new(size_t size, void*(*fn_malloc)(size_t), void(*fn_free)(void*));
void stack_delete(struct stack_t* s);

static inline size_t stack_top(const struct stack_t* stack)
{
	return (!stack ? 0 : (stack->m_base + stack->m_top));
}

struct stack_t* stack_insert_page(size_t size, struct stack_t* after);

static inline size_t stack_push(struct stack_t** stack, uint64_t val)
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

static inline size_t stack_push_ptr(struct stack_t** stack, const void* ptr)
{
	return stack_push(stack,(uintptr_t)ptr);
}

static inline uint64_t stack_pop(struct stack_t** stack)
{
	while ((*stack) && (*stack)->m_top == 0)
		(*stack) = (*stack)->m_prev;

	return (((*stack) && (*stack)->m_top) ? (*stack)->m_data[--(*stack)->m_top] : 0);
}

static inline void* stack_pop_ptr(struct stack_t** stack)
{
	return (void*)(uintptr_t)stack_pop(stack);
}

static inline void* stack_at(struct stack_t* stack, size_t pos)
{
	while (stack && stack->m_base + stack->m_top < pos)
		stack = stack->m_next;

	while (stack && stack->m_base > pos)
		stack = stack->m_prev;

	return &stack->m_data[pos - stack->m_base];
}

static inline void stack_reset(struct stack_t** stack, size_t pos)
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

void stack_compact(struct stack_t* stack);

void* stack_malloc(struct stack_t** stack, size_t len);

void stack_free(struct stack_t* stack, void* ptr, size_t len);

void* stack_realloc(struct stack_t** stack, void* ptr, size_t old_len, size_t new_len);

#endif /* STACK_H_INCLUDED_ */
