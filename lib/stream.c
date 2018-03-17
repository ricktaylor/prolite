
#include "stream.h"

#include <string.h>

struct text_stream_t
{
	struct stream_t m_proto;

	const char** m_str;
	const char*  m_end;
};

static void text_stream_close(struct stream_t* s)
{
	struct text_stream_t* ts = (struct text_stream_t*)s;
	free(ts);
}

static int64_t text_stream_read(struct stream_t* s, void* dest, size_t len)
{
	struct text_stream_t* ts = (struct text_stream_t*)s;
	int64_t r = (ts->m_end - *ts->m_str);
	if (r > len)
		r = len;

	memcpy(dest,*ts->m_str,r);
	*ts->m_str += r;
	return r;
}

struct stream_t* new_text_stream(const char** str, size_t len)
{
	struct text_stream_t* ts = malloc(sizeof(struct text_stream_t));
	if (ts)
	{
		ts->m_proto.m_fn_close = &text_stream_close;
		ts->m_proto.m_fn_read = &text_stream_read;

		ts->m_str = str;
		if (len == -1)
			len = strlen(*str);

		ts->m_end = *ts->m_str + len;
	}

	return ts ? &ts->m_proto : NULL;
}
