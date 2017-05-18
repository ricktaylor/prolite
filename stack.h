
#include <stdint.h>

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
	uint64_t        m_data[];
};

void stack_delete(struct stack_t* s);

static inline uint64_t stack_top(const struct stack_t* stack)
{
	return (!stack ? 0 : stack->m_base + stack->m_top);
}

static inline void* stack_top_ptr(struct stack_t* stack)
{
	return (!stack ? NULL : &stack->m_data[stack->m_top]);
}

uint64_t stack_push(struct stack_t** stack, uint64_t val);

static inline uint64_t stack_pop(struct stack_t* stack)
{
	return ((stack && stack->m_top) ? stack->m_data[stack->m_top--] : 0);
}

uint64_t stack_at(const struct stack_t* stack, uint64_t pos);

void stack_reset(struct stack_t** stack, uint64_t pos);

void stack_reset_hard(struct stack_t** stack, uint64_t pos);

void* stack_malloc(struct stack_t** stack, size_t len);

void stack_free(struct stack_t* stack, void* ptr, size_t len);

void* stack_realloc(struct stack_t** stack, void* ptr, size_t old_len, size_t new_len);
