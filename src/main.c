/*
 * main.c
 *
 *  Created on: 13 Mar 2018
 *      Author: taylorr
 */

#include "../lib/stream.h"

void compile(struct context_t* context, struct stream_t* s);

#include <string.h>
#include <assert.h>

typedef struct prolite_env
{
	int opaque;
}* prolite_env_t;

struct text_stream_t
{
	struct stream_t m_proto;

	const char** m_str;
	const char*  m_end;
};

static int64_t text_stream_read(struct stream_t* s, void* dest, size_t len)
{
	struct text_stream_t* ts = (struct text_stream_t*)s;
	size_t r = (ts->m_end - *ts->m_str);
	if (r > len)
		r = len;

	memcpy(dest,*ts->m_str,r);
	*ts->m_str += r;
	return r;
}

struct context_t* context_new(void);
void context_delete(struct context_t* c);

int main(int argc, char* argv[])
{
	const char* cmd = argc > 1 ? argv[1] : "true.";

	struct context_t* c = context_new();
	if (c)
	{
		struct text_stream_t ts = {0};
		ts.m_proto.m_fn_read = &text_stream_read;
		ts.m_str = &cmd;
		ts.m_end = *ts.m_str + strlen(*ts.m_str);

		// Read a term and prepare it for execution
		compile(c,&ts.m_proto);
		
		context_delete(c);
	}

	return 0;
}
