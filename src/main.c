/*
 * main.c
 *
 *  Created on: 13 Mar 2018
 *      Author: taylorr
 */

#include "../lib/stream.h"

void compile(context_t* context, stream_t* s);

#include <string.h>
#include <assert.h>
#include <stdlib.h>

typedef struct prolite_env
{
	int opaque;
}* prolite_env_t;

struct text_stream
{
	stream_t m_proto;

	const char** m_str;
	const char*  m_end;
};

static int64_t text_stream_read(stream_t* s, void* dest, size_t len)
{
	struct text_stream* ts = (struct text_stream*)s;
	size_t r = (ts->m_end - *ts->m_str);
	if (r > len)
		r = len;

	memcpy(dest,*ts->m_str,r);
	*ts->m_str += r;
	return r;
}

int main(int argc, char* argv[])
{
	const char* cmd = argc > 1 ? argv[1] : "true.";

	heap_t h = { .m_fn_malloc = &malloc, .m_fn_free = &free };
	context_t* c = context_new(&h);
	if (c)
	{
		struct text_stream ts = {0};
		ts.m_proto.m_fn_read = &text_stream_read;
		ts.m_str = &cmd;
		ts.m_end = *ts.m_str + strlen(*ts.m_str);

		// Read a term and prepare it for execution
		compile(c,&ts.m_proto);
		
		context_delete(c);
	}

	heap_destroy(&h);

	return 0;
}
