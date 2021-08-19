#include "parser.h"

#include <assert.h>

typedef struct consult_predicate
{
	term_t* t;

} consult_predicate_t;

typedef struct consult_context
{
	context_t*              m_context;
	exception_handler_fn_t* m_eh;
	stream_resolver_t*      m_resolver;

} consult_context_t;

static void dynamic(consult_context_t* context, const term_t* t)
{
	assert(0);
}

static void multifile(consult_context_t* context, const term_t* t)
{
	assert(0);
}

static void discontiguous(consult_context_t* context, const term_t* t)
{
	assert(0);
}

static void pi_directive_inner(consult_context_t* context, const term_t* t, void (*fn)(consult_context_t*,const term_t*))
{
	if (t->m_u64val != PACK_COMPOUND_EMBED_1(2,'/'))
		assert(0);

	const term_t* functor = get_first_arg(t,NULL);
	if (get_term_type(functor) != prolite_atom)
		assert(0);

	const term_t* arity = get_next_arg(functor);
	if (get_term_type(arity) != prolite_int32)
		assert(0);

	(*fn)(context,t);
}

static void pi_directive(consult_context_t* context, const term_t* directive, void (*fn)(consult_context_t*,const term_t*))
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
		// PI_Sequence (TODO: I have assumed this is parsed as directive/N )
		while (arity--)
		{
			pi_directive_inner(context,arg,fn);

			arg = get_next_arg(arg);
		}
	}
}

static void compile_initializer(consult_context_t* context, const term_t* goal)
{
	assert(0);
}

static void assert_clause(consult_context_t* context, const term_t* clause)
{
	assert(0);
}

static void include(consult_context_t* context, const term_t* t);

static void ensure_loaded(consult_context_t* context, const term_t* t)
{
	assert(0);
}

static void load_file_inner(consult_context_t* context, stream_t* input)
{
	parser_t parser = { .m_s = input, .m_line_info.m_end_line = 1 };
	
	for (parse_status_t status = PARSE_OK; status != PARSE_EOF;)
	{
		term_t* sp = context->m_context->m_stack;
		status = consult_term(context->m_context,&parser);

		const term_t* term = context->m_context->m_stack;

		if (status == PARSE_THROW)
		{
			(*context->m_eh)(context->m_context,term);
		}
		else if (status == PARSE_OK)
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
					compile_initializer(context,get_first_arg(term,NULL));
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
					if ((term->m_u64val & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(dynamic,0))
						pi_directive(context,term,&dynamic);
					else if ((term->m_u64val & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(multifile,0))
						pi_directive(context,term,&multifile);
					else if ((term->m_u64val & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(discontiguous,0))
						pi_directive(context,term,&discontiguous);
					else if (get_term_type(term) == prolite_compound)
					{
						if (term[1].m_u64val == PACK_ATOM_BUILTIN(dynamic))
							pi_directive(context,term,&dynamic);
						if (term[1].m_u64val == PACK_ATOM_BUILTIN(multifile))
							pi_directive(context,term,&multifile);
						if (term[1].m_u64val == PACK_ATOM_BUILTIN(discontiguous))
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
				assert_clause(context,term);
			}
		}

		// Reset stack
		context->m_context->m_stack = sp;
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
	size_t heap_start = heap_top(context->m_heap);

	consult_context_t cc = 
	{
		.m_context = context,
		.m_eh = eh,
		.m_resolver = resolver
	};

	load_file_inner(&cc,input);

	// TODO: Call initializers

	// Reset the heap
	heap_reset(context->m_heap,heap_start);
	
	// We have just done a load of heap allocation
	heap_compact(context->m_heap);
}
