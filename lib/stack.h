
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
	uint64_t        m_base;
	void*         (*m_fn_malloc)(size_t);
	void          (*m_fn_free)(void*);
	uint64_t        m_data[];
};

struct stack_t* stack_new(void*(*fn_malloc)(size_t), void(*fn_free)(void*));
void stack_delete(struct stack_t* s);

static inline uint64_t stack_top(const struct stack_t* stack)
{
	return (!stack ? 0 : (stack->m_base + stack->m_top));
}

uint64_t stack_push(struct stack_t** stack, uint64_t val);

static inline uint64_t stack_push_ptr(struct stack_t** stack, const void* ptr)
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

void* stack_at(struct stack_t* stack, uint64_t pos);

void stack_reset_(struct stack_t** stack, uint64_t pos);

static inline void stack_reset(struct stack_t** stack, uint64_t pos)
{
	if (pos < stack_top(*stack))
		return stack_reset_(stack,pos);
}

void stack_compact(struct stack_t* stack);

void* stack_malloc(struct stack_t** stack, size_t len);

void stack_free(struct stack_t* stack, void* ptr, size_t len);

void* stack_realloc(struct stack_t** stack, void* ptr, size_t old_len, size_t new_len);

int stack_copy(struct stack_t** dest, struct stack_t** src, size_t start);

#endif /* STACK_H_INCLUDED_ */
