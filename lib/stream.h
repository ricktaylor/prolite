#ifndef STREAM_H_INCLUDED_
#define STREAM_H_INCLUDED_

#include "types.h"

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

typedef struct formatter
{
	void (*m_fn_write)(struct formatter* f, const term_t* term);
} formatter_t;

static inline void formatter_write(formatter_t* f, const term_t* t)
{
	if (f && f->m_fn_write)
		(*f->m_fn_write)(f,t);
}

typedef struct stream_resolver
{
	stream_t* (*m_fn_open)(struct stream_resolver* r, formatter_t* f, const term_t* t);
} stream_resolver_t;

static inline stream_t* stream_resolver_open(struct stream_resolver* r, formatter_t* f, const term_t* t)
{
	stream_t* s = NULL;
	if (r && r->m_fn_open)
		s = (*r->m_fn_open)(r,f,t);
	return s;
}

#endif /* STREAM_H_INCLUDED_ */
