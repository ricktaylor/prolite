
#include "types.h"
#include "stream.h"

#include "../include/prolite.h"

#include <string.h>
#include <assert.h>

enum eSolveResult read_term(struct context_t* context, struct stream_t* s, struct term_t* term);
enum eSolveResult solve(struct context_t* context, struct term_t* goal);
enum eSolveResult redo(struct context_t* context, int unwind);

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
	return solve(context,&goal);
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

	// Create a new context
	struct query_t* q = NULL;
	{
		struct stack_t* s = stack_new(&malloc,&free);
		if (!s)
			return PROLITE_NOMEM;

		q = stack_malloc(&s,sizeof(struct query_t));
		if (!q)
		{
			stack_delete(s);
			return PROLITE_NOMEM;
		}

		memset(q,0,sizeof(struct query_t));
		q->m_context.m_exec_stack = s;
	}

	q->m_context.m_scratch_stack = stack_new(&malloc,&free);
	if (!q->m_context.m_scratch_stack)
	{
		stack_delete(q->m_context.m_exec_stack);
		return PROLITE_NOMEM;
	}

	q->m_context.m_module = stack_malloc(&q->m_context.m_exec_stack,sizeof(struct module_t));
	if (!q->m_context.m_module)
	{
		stack_delete(q->m_context.m_scratch_stack);
		stack_delete(q->m_context.m_exec_stack);
		return PROLITE_NOMEM;
	}
	memset(q->m_context.m_module,0,sizeof(struct module_t));
	q->m_context.m_module->m_flags.char_conversion = 1;
	q->m_context.m_module->m_flags.back_quotes = 1;

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
			if ((q->m_start = stack_push_ptr(&q->m_context.m_exec_stack,&solve_start)) == -1)
				result = SOLVE_NOMEM;
		}
	}

	// We have just made heavy use of the scratch stack
	stack_compact(q->m_context.m_scratch_stack);

	switch (result)
	{
	case SOLVE_TRUE:
		*query = (prolite_query_t)q;
		return PROLITE_TRUE;

	case SOLVE_NOMEM:
		stack_delete(q->m_context.m_scratch_stack);
		stack_delete(q->m_context.m_exec_stack);
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
	if (q)
	{
		if (stack_top(q->m_context.m_exec_stack) > q->m_start)
		{
			redo_fn_t fn = stack_pop_ptr(&q->m_context.m_exec_stack);
			switch ((*fn)(&q->m_context,0))
			{
			case SOLVE_TRUE:
				return PROLITE_TRUE;

			case SOLVE_NOMEM:
				return PROLITE_NOMEM;

			case SOLVE_THROW:
				return PROLITE_ERROR;

			case SOLVE_HALT:
				return PROLITE_HALT;

			default:
				break;
			}
		}
	}
	return PROLITE_FALSE;
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
			result = redo(&q->m_context,1);

			assert(stack_top(q->m_context.m_exec_stack) == q->m_start);
		}

		stack_compact(q->m_context.m_exec_stack);
		if (stack_push_ptr(&q->m_context.m_exec_stack,&solve_start) == -1)
			result = PROLITE_NOMEM;

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
		stack_delete(q->m_context.m_scratch_stack);
		stack_delete(q->m_context.m_exec_stack);
	}
}
