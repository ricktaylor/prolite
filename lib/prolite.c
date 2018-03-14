
#include "types.h"
#include "stream.h"

#include "../include/prolite.h"

#include <string.h>

int interpreter_setup_stack(struct context_t* context, struct term_t* term);
int read_term(struct context_t* context, struct stream_t* s, struct term_t* term);

struct query_t
{
	struct context_t m_context;
	uint64_t         m_start;
};

static enum eProliteResult prolite_error(struct query_t* q, enum eSolveResult err)
{
	switch (err)
	{
	case SOLVE_TRUE:
		return PROLITE_TRUE;

	case SOLVE_FAIL:
	case SOLVE_CUT:
		return PROLITE_FALSE;

	default:
		break;
	}

	if (err == SOLVE_NOMEM)
	{
		// Try to allocate a no mem error
	}

	// Do something sensible with the error

	return PROLITE_ERROR;
}

int prolite_finalize(prolite_query_t query)
{
	struct query_t* q = (struct query_t*)query;

	// Clean up query

	free(q);

	return PROLITE_TRUE;
}

int prolite_prepare(prolite_env_t env, const char* query_text, size_t query_len, prolite_query_t* query, const char** tail)
{
	enum eSolveResult err = 0;
	struct query_t* q = malloc(sizeof(struct query_t));
	if (q)
		err = SOLVE_NOMEM;
	else
	{
		struct term_t term = {0};
		struct text_stream_t ts;

		// Init the context
		memset(q,0,sizeof(struct query_t));

		// Build a stream around query

		text_stream_init(&ts);

		// Read a term

		switch (read_term(&q->m_context,&ts.m_proto,&term))
		{
		case 1:
			err = SOLVE_THROW;
			break;

		case 0:
			err = SOLVE_TRUE;
			break;

		default:
			err = SOLVE_NOMEM;
			break;
		}

		// Set tail to the remainder of the stream

		if (!err)
		{
			// Setup first call
			if (interpreter_setup_stack(&q->m_context,&term) == -1)
				err = SOLVE_NOMEM;
			else
			{
				q->m_start = stack_top(q->m_context.m_exec_stack);
			}
		}

		if (err)
		{
			prolite_finalize((prolite_query_t)q);
			q = NULL;
		}
	}

	*query = (prolite_query_t)q;
	if (q)
		return prolite_error(q,err);

	return PROLITE_ERROR;
}

int prolite_step(prolite_query_t query)
{
	struct query_t* q = (struct query_t*)query;
	solve_fn_t* fn = stack_pop_ptr(&q->m_context.m_exec_stack);
	enum eSolveResult err = (*fn)(&q->m_context);

	return prolite_error(q,err);
}

int prolite_reset(prolite_query_t query)
{
	struct query_t* q = (struct query_t*)query;
	int err = SOLVE_TRUE;

	stack_reset(&q->m_context.m_exec_stack,q->m_start);
	stack_compact(q->m_context.m_exec_stack);

	return prolite_error(q,err);
}
