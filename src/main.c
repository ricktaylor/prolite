/*
 * main.c
 *
 *  Created on: 13 Mar 2018
 *      Author: taylorr
 */

#include "../lib/types.h"
#include "../lib/stream.h"

#include <string.h>
#include <assert.h>
#include <stdio.h>

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

struct module_t* module_new(struct context_t* context, const char* name)
{
	// TODO: Much more here!!

	struct module_t* module = NULL;
	struct stack_t* s = stack_new(8000,&malloc,&free);
	if (s)
	{
		module = stack_malloc(&s,sizeof(struct module_t));
		if (module)
		{
			memset(module,0,sizeof(struct module_t));
			module->m_flags.char_conversion = 1;
			module->m_flags.back_quotes = 1;
			module->m_stack = s;
		}

		if (!module)
			stack_delete(s);
	}

	return module;
}

void module_delete(struct module_t* module)
{
	// TODO
}

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

struct query_t
{
	struct context_t   m_context;
	const char**       m_varnames;
	size_t             m_varcount;
	const char*        m_error_text;
};

static struct query_t* query_new(prolite_env_t env)
{
	// Create a new context
	struct query_t* retval = NULL;
	struct stack_t* s = stack_new(8000,&malloc,&free);
	if (s)
	{
		struct query_t* q = stack_malloc(&s,sizeof(struct query_t));
		if (q)
		{
			memset(q,0,sizeof(struct query_t));
			q->m_context.m_call_stack = s;
			q->m_context.m_scratch_stack = stack_new(900,&malloc,&free);
			if (q->m_context.m_scratch_stack)
			{
				q->m_context.m_module = module_new(&q->m_context,"user");
				if (q->m_context.m_module)
				{
					retval = q;
				}

				if (!retval)
					stack_delete(q->m_context.m_scratch_stack);
			}
		}

		if (!retval)
			stack_delete(s);
	}

	return retval;
}

static void query_delete(struct query_t* q)
{
	module_delete(q->m_context.m_module);
	stack_delete(q->m_context.m_scratch_stack);
	stack_delete(q->m_context.m_call_stack);
}

void prolite_pl2mir(struct query_t* q, const char* query_text, size_t query_len, const char** tail)
{
	struct text_stream_t ts = {0};
	ts.m_proto.m_fn_read = &text_stream_read;
	ts.m_str = &query_text;
	ts.m_end = *ts.m_str + (query_len == -1 ? strlen(*ts.m_str) : query_len);
	if (tail)
		*tail = query_text;

	// Read a term and prepare it for execution
	union packed_t* goal = NULL;
	enum eEmitStatus result = prepare_term(&q->m_context,&ts.m_proto,&goal,&q->m_varnames);
	if (result == EMIT_OK)
	{
		
	}
}

int main(int argc, char* argv[])
{
	const char* cmd = argc > 1 ? argv[1] : "true.";
	prolite_env_t dummy = {0};
	struct query_t* q = query_new(dummy);
	if (q)
	{
		prolite_pl2mir(q,cmd,-1,NULL);
		
		query_delete(q);
	}

	return 0;
}
