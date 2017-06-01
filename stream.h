
#ifndef STREAM_H_INCLUDED_
#define STREAM_H_INCLUDED_

#include "types.h"

struct stream_t
{
	int64_t (*m_fn_read)(void* dest, size_t len);
};

struct text_stream_t
{
	struct stream_t m_proto;

	int stuff;
};

static inline int64_t stream_read(struct stream_t* s, void* dest, size_t len)
{
	return (!s ? -1 : (*s->m_fn_read)(dest,len));
}

void text_stream_init(struct text_stream_t* ts);

#endif /* STREAM_H_INCLUDED_ */
