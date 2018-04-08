
#include "context.h"
#include "stream.h"

#include "../include/prolite.h"

#include <string.h>

enum eSolveResult read_term(struct context_t* context, struct stream_t* s, struct term_t* term);
enum eSolveResult call(struct context_t* context, struct term_t* goal);
enum eSolveResult redo(struct context_t* context);

struct text_stream_t
{
	struct stream_t m_proto;

	const char** m_str;
	const char*  m_end;
};

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

static enum eSolveResult solve_start(struct context_t* context)
{
	struct term_t goal;
	uint64_t top = stack_top(context->m_exec_stack);
	goal.m_value = *(union box_t**)stack_at(context->m_exec_stack,top-1);
	goal.m_vars = *(struct var_info_t**)stack_at(context->m_exec_stack,top-2);
	return call(context,&goal);
}

struct query_t
{
	struct context_t m_context;
	size_t           m_start;
};

enum eProliteResult prolite_prepare(prolite_env_t env, const char* query_text, size_t query_len, prolite_query_t* query, const char** tail)
{
	enum eSolveResult result = SOLVE_TRUE;
	struct text_stream_t ts = {0};
	struct term_t term = {0};

	struct stack_t* s = NULL;
	struct query_t* q = stack_malloc(&s,sizeof(struct query_t));
	if (!q)
		return PROLITE_NOMEM;

	memset(q,0,sizeof(struct query_t));
	q->m_context.m_exec_stack = s;

	ts.m_proto.m_fn_read = &text_stream_read;
	ts.m_str = &query_text;
	ts.m_end = *ts.m_str + (query_len == -1 ? strlen(*ts.m_str) : query_len);
	if (tail)
		*tail = query_text;

	// Read a term and prepare it
	result = read_term(&q->m_context,&ts.m_proto,&term);
	if (result == SOLVE_TRUE)
	{
		if (stack_push_ptr(&q->m_context.m_exec_stack,term.m_vars) == -1 ||
			stack_push_ptr(&q->m_context.m_exec_stack,term.m_value) == -1)
		{
			result = SOLVE_NOMEM;
		}
		else
		{
			q->m_start = stack_top(q->m_context.m_exec_stack);

			if (stack_push_ptr(&q->m_context.m_exec_stack,&solve_start) == -1)
				result = SOLVE_NOMEM;
		}
	}

	switch (result)
	{
	case SOLVE_TRUE:
		*query = (prolite_query_t)q;
		return PROLITE_TRUE;

	case SOLVE_NOMEM:
		return PROLITE_NOMEM;

	case SOLVE_THROW:
		*query = (prolite_query_t)q;
		return PROLITE_ERROR;

	default:
		return PROLITE_FALSE;
	}
}

enum eProliteResult prolite_solve(prolite_query_t query)
{
	struct query_t* q = (struct query_t*)query;
	if (!q)
		return SOLVE_FAIL;

	if (stack_top(q->m_context.m_exec_stack) == q->m_start)
		return PROLITE_FALSE;

	switch (redo(&q->m_context))
	{
	case SOLVE_TRUE:
		return PROLITE_TRUE;

	case SOLVE_NOMEM:
		return PROLITE_NOMEM;

	case SOLVE_THROW:
		return PROLITE_ERROR;

	default:
		return PROLITE_FALSE;
	}
}

enum eProliteResult prolite_reset(prolite_query_t query)
{
	enum eProliteResult result = PROLITE_FALSE;
	struct query_t* q = (struct query_t*)query;
	if (q)
	{
		result = PROLITE_TRUE;
		if (stack_top(q->m_context.m_exec_stack) > q->m_start)
		{
			// In order to rewind stack safely, force a halt and step
			q->m_context.m_flags.halt = 1;
			redo(&q->m_context);
			q->m_context.m_flags.halt = 0;

			context_reset(&q->m_context,q->m_start);
		}

		stack_compact(q->m_context.m_exec_stack);
		if (stack_push_ptr(&q->m_context.m_exec_stack,&solve_start) == -1)
			result = SOLVE_NOMEM;

		stack_reset(&q->m_context.m_scratch_stack,0);
		stack_compact(q->m_context.m_scratch_stack);
	}
	return result;
}

void prolite_finalize(prolite_query_t query)
{
	struct query_t* q = (struct query_t*)query;
	if (q)
	{
		prolite_reset(query);
		stack_delete(q->m_context.m_exec_stack);
	}
}
