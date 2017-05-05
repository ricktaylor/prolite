

struct context_t
{
	int _blah;
};

struct context_string_t
{
	struct context_string_t* m_prev;
	size_t                   m_len;
	unsigned char            m_char[];
};

unsigned char* context_alloc_string(struct context_t* context, size_t len);
