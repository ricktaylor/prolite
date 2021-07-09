
#ifndef STREAM_H_INCLUDED_
#define STREAM_H_INCLUDED_

#include "types.h"

struct stream_t
{
	void (*m_fn_close)(struct stream_t* s);
	int64_t (*m_fn_read)(struct stream_t* s, void* dest, size_t len);
};

static inline void stream_close(struct stream_t* s)
{
	if (s && s->m_fn_close)
		(*s->m_fn_close)(s);
}

static inline int64_t stream_read(struct stream_t* s, void* dest, size_t len)
{
	return (s && s->m_fn_read ? (*s->m_fn_read)(s,dest,len) : -1);
}

enum eEmitStatus
{
	EMIT_NOMEM = -1,
	EMIT_OK = 0,
	EMIT_EOF,
	EMIT_THROW
};

enum eEmitStatus emit_term(struct context_t* context, struct stream_t* s, union packed_t** term, const char*** varnames);

#endif /* STREAM_H_INCLUDED_ */
