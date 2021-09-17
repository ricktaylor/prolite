#ifndef STREAM_H_
#define STREAM_H_

#include "context.h"

typedef struct stream
{
	void (*m_fn_close)(struct stream* s);
	int64_t (*m_fn_read)(struct stream* s, void* dest, size_t len);
} stream_t;

static inline void stream_close(stream_t* s)
{
	if (s && s->m_fn_close)
		(*s->m_fn_close)(s);
}

static inline int64_t stream_read(stream_t* s, void* dest, size_t len)
{
	return (s && s->m_fn_read ? (*s->m_fn_read)(s,dest,len) : -1);
}

typedef void (*exception_handler_fn_t)(context_t* context, const term_t* term);

typedef struct stream_resolver
{
	stream_t* (*m_fn_open)(struct stream_resolver* r, exception_handler_fn_t* eh, const term_t* t);
} stream_resolver_t;

static inline stream_t* stream_resolver_open(struct stream_resolver* r, exception_handler_fn_t* eh, const term_t* t)
{
	stream_t* s = NULL;
	if (r && r->m_fn_open)
		s = (*r->m_fn_open)(r,eh,t);
	return s;
}

#endif /* STREAM_H_ */
