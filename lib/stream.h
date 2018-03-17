
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
	if (s)
		(*s->m_fn_close)(s);
}

static inline int64_t stream_read(struct stream_t* s, void* dest, size_t len)
{
	return (!s ? -1 : (*s->m_fn_read)(s,dest,len));
}

struct stream_t* new_text_stream(const char** str, size_t len);

#endif /* STREAM_H_INCLUDED_ */
