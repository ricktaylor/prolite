

struct stack_t
{
	struct stack_t* m_prev;
	uint32_t        m_count;
	uint32_t        m_top;
	uint64_t        m_base;
	uint64_t        m_data[];
};

void stack_delete(struct stack_t* s);

uint64_t stack_top(const struct stack_t* stack);

uint64_t stack_push(struct stack_t** stack, uint64_t val);

uint64_t stack_pop(struct stack_t* stack);

void stack_reset(struct stack_t** stack, uint64_t pos);

void* stack_malloc(struct stack_t** stack, size_t len);

void stack_free(struct stack_t* stack, void* ptr, size_t len);

void* stack_realloc(struct stack_t** stack, void* ptr, size_t old_len, size_t new_len);
