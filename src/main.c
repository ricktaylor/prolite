/*
 * main.c
 *
 *  Created on: 13 Mar 2018
 *      Author: taylorr
 */

#include "../lib/parser.h"
#include "../lib/btree.h"

void compile_term(context_t* context, const term_t* goal, size_t var_count);

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

void btree_tests(void);

int main(int argc, char* argv[])
{
	btree_tests();

	const char* cmd = argc > 1 ? argv[1] : "true.";

	heap_t h = { .m_fn_malloc = &malloc, .m_fn_free = &free };
	context_t* context = context_new(&h);
	if (context)
	{
		struct text_stream ts = {0};
		ts.m_proto.m_fn_read = &text_stream_read;
		ts.m_str = &cmd;
		ts.m_end = *ts.m_str + strlen(*ts.m_str);

		// Read a term and prepare it for execution
		parse_status_t result = read_term(context,&ts.m_proto);
		if (result == PARSE_OK)
		{		
			// Pop varinfo
			size_t varcount = 0;
			{
				const term_t* sp = context->m_stack;
				varcount = (sp++)->m_u64val;
				for (size_t i = 0; i < varcount; ++i)
					sp = get_next_arg(sp) + 1;

				context->m_stack = (term_t*)sp;
			}

			compile_term(context,context->m_stack,varcount);
		}
		
		context_delete(context);
	}

	heap_destroy(&h);

	return 0;
}
