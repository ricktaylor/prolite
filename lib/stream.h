
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

enum eParseStatus
{
	PARSE_OK = 0,
	PARSE_EOF,
	PARSE_THROW
};

enum eParseStatus read_term(context_t* context, stream_t* s);

#endif /* STREAM_H_INCLUDED_ */
