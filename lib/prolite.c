
#include "types.h"
#include "stream.h"

#include "../include/prolite.h"

#include <string.h>

int read_term(struct context_t* context, struct stream_t* s, struct term_t* term);
enum eSolveResult solve_goal(struct context_t* context, struct term_t* goal);

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
	struct query_t* q = calloc(1,sizeof(struct query_t));
	if (!q)
		err = SOLVE_NOMEM;
	else
	{
		struct stream_t* s = new_text_stream(&query_text,query_len);
		if (tail)
			*tail = query_text;

		if (!s)
			err = SOLVE_NOMEM;
		else
		{
			// Read a term
			struct term_t term = {0};
			switch (read_term(&q->m_context,s,&term))
			{
			case 1:
				err = SOLVE_THROW;
				break;

			case 0:
				err = solve_goal(&q->m_context,&term);
				break;

			default:
				err = SOLVE_NOMEM;
				break;
			}

			if (err)
			{
				prolite_finalize((prolite_query_t)q);
				q = NULL;
			}

			stream_close(s);
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
