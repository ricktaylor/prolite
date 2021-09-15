#include "../lib/parser.h"
#include "../lib/predicates.h"

#include <assert.h>
#include <string.h>

typedef struct consult_clause
{
	struct consult_clause* m_next;

	const term_t*   m_term;

} consult_clause_t;

typedef struct consult_predicate
{
	predicate_base_t  m_base;

	unsigned          m_public : 1;
	unsigned          m_dynamic : 1;
	unsigned          m_multifile : 1;
	unsigned          m_discontiguous : 1;

	consult_clause_t* m_clauses;

} consult_predicate_t;

typedef struct consult_context
{
	unsigned                m_failed : 1;

	context_t*              m_context;
	exception_handler_fn_t* m_eh;
	stream_resolver_t*      m_resolver;

	predicate_map_t         m_predicates;
	consult_predicate_t*    m_current_predicate;

} consult_context_t;

static int catch_exception(consult_context_t* context)
{
	if (context->m_context->m_flags & FLAG_THROW)
	{
		// TODO
		assert(0);
		const term_t* t = NULL;
		(*context->m_eh)(context->m_context,t);

		context->m_context->m_flags &= ~FLAG_THROW;
		context->m_failed = 1;
		return 1;
	}

	return 0;
}

static void report_exception(consult_context_t* context)
{
	// TODO
	assert(0);
	const term_t* t = NULL;
	(*context->m_eh)(context->m_context,t);

	context->m_failed = 1;
}

static void report_out_of_memory_error(consult_context_t* context, const term_t* t)
{
    // 't' just gives us debug info

	// TODO
	report_exception(context);
}

static void report_permission_error(consult_context_t* context, uint64_t p1, uint64_t p2, const term_t* t)
{
	// TODO
	report_exception(context);
}

static void report_type_error(consult_context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	report_exception(context);
}

static void report_domain_error(consult_context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	report_exception(context);
}

static void report_representation_error(consult_context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	report_exception(context);
}

static consult_predicate_t* new_predicate(consult_context_t* context, const term_t* t, int public, int dynamic, int multifile, int discontiguous, int* is_new_pred)
{
	consult_predicate_t new_pred =
	{
		.m_base.m_functor = t,
		.m_dynamic = dynamic,
		.m_multifile = multifile,
		.m_discontiguous = discontiguous
	};

	term_t* sp = context->m_context->m_stack;
	sp -= bytes_to_cells(sizeof(consult_predicate_t),sizeof(term_t));
	memcpy(sp,&new_pred,sizeof(consult_predicate_t));

	predicate_base_t* pred = predicate_map_insert(&context->m_predicates,(predicate_base_t*)sp);
	if (!pred)
		report_out_of_memory_error(context,t);

	if (is_new_pred)
		*is_new_pred = (pred == (predicate_base_t*)sp);

	if (!pred || pred != (predicate_base_t*)sp)
		context->m_context->m_stack = sp;
	
	return (consult_predicate_t*)pred;
}

static void public(consult_context_t* context, const term_t* t, const term_t* pi)
{
	consult_predicate_t* pred = new_predicate(context,t,1,0,0,0,NULL);
	if (!pred)
		return report_out_of_memory_error(context,t);
		
	if (!pred->m_public)
	{
		if (pred->m_clauses)
		{
			// TODO: Must error somehow...
			//return report_permission_error(context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),pi);
		}

		pred->m_public = 1;
	}
}

static void dynamic(consult_context_t* context, const term_t* t, const term_t* pi)
{
	consult_predicate_t* pred = new_predicate(context,t,1,1,0,0,NULL);
	if (!pred)
		return report_out_of_memory_error(context,t);
		
	if (!pred->m_dynamic)
	{
		if (pred->m_clauses)
			return report_permission_error(context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),pi);
			
		pred->m_dynamic = 1;
		pred->m_public = 1;
	}
}

static void discontiguous(consult_context_t* context, const term_t* t, const term_t* pi)
{
	consult_predicate_t* pred = new_predicate(context,t,0,0,0,1,NULL);
	if (!pred)
		return report_out_of_memory_error(context,t);
		
	if (!pred->m_discontiguous)
	{
		if (pred->m_clauses)
		{
			// TODO: Must error somehow...
			//return report_permission_error(context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),pi);
		}
			
		pred->m_discontiguous = 1;
	}
}

static void multifile(consult_context_t* context, const term_t* t, const term_t* pi)
{
	consult_predicate_t* pred = new_predicate(context,t,0,0,1,0,NULL);
	if (!pred)
		return report_out_of_memory_error(context,t);
		
	if (!pred->m_multifile)
	{
		if (pred->m_clauses)
		{
			// TODO: Must error somehow...
			//return report_permission_error(context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),pi);
		}
			
		pred->m_multifile = 1;
	}
}

static void pi_directive_inner(consult_context_t* context, const term_t* pi, void (*fn)(consult_context_t*,const term_t*,const term_t*))
{
	if (pi->m_u64val == PACK_COMPOUND_EMBED_1(2,'/'))
	{
		const term_t* functor = get_first_arg(pi,NULL);
		if (get_term_type(functor) == prolite_atom)
		{
			const term_t* arity = get_next_arg(functor);
			if (get_term_type(arity) == prolite_integer)
			{
				int64_t a = get_integer(arity);
				if (a < 0)
					return report_domain_error(context,PACK_ATOM_BUILTIN(not_less_than_zero),arity);
					
				if (a > MAX_ARITY)
					return report_representation_error(context,PACK_ATOM_BUILTIN(max_arity),arity);
					
				term_t* sp = context->m_context->m_stack;
				if (a > 0)
				{
					string_t f = get_string(functor,NULL);
					context->m_context->m_stack = push_predicate(context->m_context->m_stack,a,f.m_str,f.m_len,1);
					functor = context->m_context->m_stack;
				}

				if (predicate_is_builtin(functor))
				{
					report_permission_error(context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),pi);
					context->m_context->m_stack = sp;
					return;
				}
				return (*fn)(context,functor,pi);
			}
		}
	}

	report_type_error(context,PACK_ATOM_BUILTIN(predicate_indicator),pi);
}

static void pi_directive(consult_context_t* context, const term_t* directive, void (*fn)(consult_context_t*,const term_t*,const term_t*))
{
	size_t arity = 0;
	const term_t* arg = get_first_arg(directive,&arity);
	if (arity == 1)
	{
		while (arg->m_u64val == PACK_COMPOUND_EMBED_1(2,'.'))
		{
			arg = get_first_arg(arg,NULL);

			pi_directive_inner(context,arg,fn);

			arg = get_next_arg(arg);
		}

		if (arg->m_u64val != PACK_ATOM_EMBED_2('[',']'))
			pi_directive_inner(context,arg,fn);
	}
	else
	{
		// PI_Sequence (I have assumed this is parsed as directive/N )
		while (arity--)
		{
			pi_directive_inner(context,arg,fn);

			arg = get_next_arg(arg);
		}
	}
}

static void assert_initializer(consult_context_t* context, const term_t* goal)
{
	// Initializer clears the current predicate
	context->m_current_predicate = NULL;

	// Only add if we are actually going to do something about it
	if (!context->m_failed)
	{
		term_t atom_null = { .m_u64val = PACK_ATOM_NUL };
		consult_predicate_t* pred = new_predicate(context,&atom_null,0,0,0,0,NULL);
		if (!pred)
			return report_out_of_memory_error(context,goal);

		// TODO - Add the goal to the predicate
		assert(0);
	}
}

static void assert_term(consult_context_t* context, const term_t* t)
{
	int is_current_pred = 0;
	if (context->m_current_predicate)
		is_current_pred = predicate_compare(context->m_current_predicate->m_base.m_functor,t);
	
	if (!is_current_pred)
	{
		int new_pred = 0;
		consult_predicate_t* pred = new_predicate(context,t,0,0,0,0,&new_pred);
		if (!pred)
			return report_out_of_memory_error(context,t);

		if (!new_pred)
		{
			if (!pred->m_discontiguous)
			{
				// TODO: Some kind of discontiguous warning
			}

			// TODO: Multifile check
		}

		context->m_current_predicate = pred;
	}

	//clause_t clause = {0};
	//term_to_clause(context->m_context,t,&clause);
	if (!catch_exception(context))
	{





		if (!context->m_failed)
		{
			// Actually assert the clause if everything looks good so far
			assert(0);
		}
	}
}

static void include(consult_context_t* context, const term_t* t);

static void ensure_loaded(consult_context_t* context, const term_t* t)
{
	assert(0);
}

static void load_file_inner(consult_context_t* context, stream_t* input)
{
	for (parser_t parser = { .m_s = input, .m_line_info.m_end_line = 1, .m_multiterm = 1 };;)
	{
		term_t* sp = context->m_context->m_stack;

		parse_status_t status = consult_term(context->m_context,&parser);
		if (status == PARSE_EOF)
			break;

		const term_t* term = context->m_context->m_stack;

		if (status == PARSE_THROW)
		{
			(*context->m_eh)(context->m_context,term);
			context->m_failed = 1;

			// Reset stack
			context->m_context->m_stack = sp;
		}
		else
		{
			if (term->m_u64val == PACK_COMPOUND_EMBED_2(1,':','-'))
			{
				term = get_first_arg(term,NULL);
				switch (term->m_u64val)
				{
				case PACK_COMPOUND_BUILTIN(include,1):
					include(context,get_first_arg(term,NULL));
					break;

				case PACK_COMPOUND_BUILTIN(ensure_loaded,1):
					ensure_loaded(context,get_first_arg(term,NULL));
					break;

				case PACK_COMPOUND_BUILTIN(initialization,1):
					assert_initializer(context,get_first_arg(term,NULL));
					break;

				case PACK_COMPOUND_BUILTIN(public,1):
					pi_directive(context,term,&public);
					break;

				case PACK_COMPOUND_BUILTIN(dynamic,1):
					pi_directive(context,term,&dynamic);
					break;

				case PACK_COMPOUND_BUILTIN(multifile,1):
					pi_directive(context,term,&multifile);
					break;

				case PACK_COMPOUND_BUILTIN(discontiguous,1):
					pi_directive(context,term,&discontiguous);
					break;

				default:
					if ((term->m_u64val & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(public,0))
						pi_directive(context,term,&public);
					else if ((term->m_u64val & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(dynamic,0))
						pi_directive(context,term,&dynamic);
					else if ((term->m_u64val & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(multifile,0))
						pi_directive(context,term,&multifile);
					else if ((term->m_u64val & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(discontiguous,0))
						pi_directive(context,term,&discontiguous);
					else if (get_term_type(term) == prolite_compound)
					{
						if (term[1].m_u64val == PACK_ATOM_BUILTIN(public))
							pi_directive(context,term,&public);
						else if (term[1].m_u64val == PACK_ATOM_BUILTIN(dynamic))
							pi_directive(context,term,&dynamic);
						else if (term[1].m_u64val == PACK_ATOM_BUILTIN(multifile))
							pi_directive(context,term,&multifile);
						else if (term[1].m_u64val == PACK_ATOM_BUILTIN(discontiguous))
							pi_directive(context,term,&discontiguous);
						else
							assert(0);
					}
					else
					{
						assert(0);
					}
					break;
				}
			}
			else
			{
				/* Assert the term */
				assert_term(context,term);
			}
		}
	}
}

static void include(consult_context_t* context, const term_t* t)
{
	stream_t* s = stream_resolver_open(context->m_resolver,context->m_eh,t);
	if (s)
	{
		/* TODO: Twiddle with context */

		load_file_inner(context,s);

		/* TODO: Untwiddle context */

		stream_close(s);
	}
}

void consult(context_t* context, stream_t* input, exception_handler_fn_t* eh, stream_resolver_t* resolver)
{
	consult_context_t cc =
	{
		.m_context = context,
		.m_eh = eh,
		.m_resolver = resolver,
		.m_predicates.m_fn_malloc = context->m_heap->m_fn_malloc,
		.m_predicates.m_fn_free = context->m_heap->m_fn_free
	};

	load_file_inner(&cc,input);
	if (!cc.m_failed)
	{
		// TODO: Call initializers
	}

	// We have just done a load of heap allocation
	heap_compact(context->m_heap);
}
