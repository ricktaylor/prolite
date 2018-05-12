
#include "types.h"
#include "stream.h"

#include "../include/prolite.h"

#include <string.h>
#include <assert.h>

struct text_stream_t
{
	struct stream_t m_proto;

	const char** m_str;
	const char*  m_end;
};

enum eSolveResult context_init(struct context_t* context);
enum eSolveResult context_prepare(struct context_t* context, struct stream_t* s, struct var_info_t** varinfo);
enum eSolveResult context_solve(struct context_t* context);
enum eSolveResult context_reset(struct context_t* context);

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

static struct module_t* module_new(struct context_t* context, const char* name)
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

static void module_delete(struct module_t* module)
{
	// TODO
}

struct query_t
{
	struct context_t   m_context;
	struct var_info_t* m_varinfo;
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

prolite_query_t prolite_new_query(prolite_env_t env)
{
	struct query_t* q = query_new(env);
	if (q)
	{
		if (context_init(&q->m_context) != SOLVE_TRUE)
		{
			query_delete(q);
			q = NULL;
		}
	}

	return (prolite_query_t)q;
}

enum eProliteResult prolite_prepare(prolite_query_t query, const char* query_text, size_t query_len, const char** tail)
{
	enum eProliteResult result = PROLITE_FAIL;
	struct query_t* q = (struct query_t*)query;
	if (q)
	{
		struct text_stream_t ts = {0};
		ts.m_proto.m_fn_read = &text_stream_read;
		ts.m_str = &query_text;
		ts.m_end = *ts.m_str + (query_len == -1 ? strlen(*ts.m_str) : query_len);
		if (tail)
			*tail = query_text;

		// Read a term and prepare it for execution
		switch (context_prepare(&q->m_context,&ts.m_proto,&q->m_varinfo))
		{
		case SOLVE_TRUE:

			// TODO: Do something with varinfo!

			result = PROLITE_TRUE;
			break;

		case SOLVE_THROW:
			result = PROLITE_ERROR;
			break;

		case SOLVE_NOMEM:
			result = PROLITE_NOMEM;
			break;

		default:
			// Few of these are actually reported
			result = PROLITE_FAIL;
			break;
		}
	}
	return result;
}

enum eProliteResult prolite_solve(prolite_query_t query)
{
	enum eProliteResult result = PROLITE_FAIL;
	struct query_t* q = (struct query_t*)query;
	if (q)
	{
		switch (context_solve(&q->m_context))
		{
		case SOLVE_TRUE:
			result = PROLITE_TRUE;
			break;

		case SOLVE_THROW:
			result = PROLITE_ERROR;
			break;

		case SOLVE_NOMEM:
			result = PROLITE_NOMEM;
			break;

		case SOLVE_HALT:
			// TODO: Check halt handler
			result = PROLITE_FAIL;
			break;

		default:
			// Few of these are actually reported
			result = PROLITE_FAIL;
			break;
		}
	}
	return result;
}

enum eProliteResult prolite_reset(prolite_query_t query)
{
	enum eProliteResult result = PROLITE_TRUE;
	struct query_t* q = (struct query_t*)query;
	if (q)
		context_reset(&q->m_context);

	return result;
}

void prolite_delete_query(prolite_query_t query)
{
	struct query_t* q = (struct query_t*)query;
	if (q)
	{
		context_reset(&q->m_context);
		query_delete(q);
	}
}
