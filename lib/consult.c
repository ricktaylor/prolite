#include "parser.h"
#include "predicates.h"
#include "export.h"

#include <assert.h>
#include <string.h>

typedef struct consult_clause
{
	struct consult_clause* m_next;
	const term_t*          m_term;
	size_t                 m_varcount;
	const term_t*          m_varinfo;

} consult_clause_t;

typedef struct consult_file
{
	struct consult_file* m_next;
	const term_t*        m_filename;
	
} consult_file_t;

typedef struct consult_predicate
{
	predicate_base_t  m_base;

	unsigned          m_public : 1;
	unsigned          m_dynamic : 1;
	unsigned          m_multifile : 1;
	unsigned          m_discontiguous : 1;

	const consult_file_t* m_file;
	consult_clause_t*     m_clauses;
		
} consult_predicate_t;

typedef struct consult_initializer
{
	struct consult_initializer* m_next;
	const term_t*               m_term;
	
} consult_initializer_t;

typedef struct consult_module
{
	struct consult_module* m_next;
	module_t               m_module;

} consult_module_t;

typedef struct consult_context
{
	unsigned                m_failed : 1;
	unsigned                m_critical_failure : 1;

	context_t*              m_context;
	exception_handler_fn_t* m_eh;
	stream_resolver_t*      m_resolver;
	consult_file_t*         m_files;

	predicate_map_t         m_predicates;
	consult_predicate_t*    m_current_predicate;
	consult_initializer_t*  m_initializers;
	consult_module_t*       m_modules;

} consult_context_t;

/*static int catch_exception(consult_context_t* context)
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
}*/

static void report_exception(consult_context_t* context)
{
	// TODO
	assert(0);
	
	(*context->m_eh)(context->m_context);
	context->m_failed = 1;
}

static void report_out_of_memory_error(consult_context_t* context, const term_t* t)
{
    // 't' just gives us debug info

	// TODO
	report_exception(context);

	context->m_critical_failure = 1;
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
	consult_predicate_t* new_pred = heap_malloc(context->m_context->m_heap,sizeof(consult_predicate_t));
	if (!new_pred)
		return NULL;
	
	*new_pred = (consult_predicate_t){ 
		.m_base.m_functor = t,
		.m_dynamic = dynamic,
		.m_multifile = multifile,
		.m_discontiguous = discontiguous,
		.m_file = context->m_files
	};

	predicate_base_t* pred = predicate_map_insert(&context->m_predicates,&new_pred->m_base);
	
	if (is_new_pred)
		*is_new_pred = (pred == &new_pred->m_base);

	if (!pred || pred != &new_pred->m_base)
		heap_free(context->m_context->m_heap,new_pred,sizeof(consult_predicate_t));
	
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

static void append_clause(consult_context_t* context, consult_predicate_t* pred, const consult_clause_t* c)
{
	// Push a clause frame on the stack, and add to linked list
	consult_clause_t** tail = &pred->m_clauses;
	while (*tail)
		tail = &(*tail)->m_next;

	consult_clause_t* new_clause = heap_malloc(context->m_context->m_heap,sizeof(consult_clause_t));
	if (!new_clause)
		return report_out_of_memory_error(context,c->m_term);

	*new_clause = *c;
	*tail = new_clause;
}

static void assert_initializer(consult_context_t* context, const term_t* goal)
{
	// Initializer clears the current predicate
	context->m_current_predicate = NULL;

	// Only add if we are actually going to do something about it
	if (!context->m_failed)
	{
		// Push an initializer frame on the stack, and add to linked list
		consult_initializer_t** tail = &context->m_initializers;
		while (*tail)
			tail = &(*tail)->m_next;

		consult_initializer_t* new_init = heap_malloc(context->m_context->m_heap,sizeof(consult_initializer_t));
		if (!new_init)
			return report_out_of_memory_error(context,goal);

		*new_init = (consult_initializer_t){ .m_term = goal };
		*tail = new_init;
	}
}

static void include(consult_context_t* context, const term_t* t);
static void ensure_loaded(consult_context_t* context, const term_t* t);

static void directive(consult_context_t* context, const term_t* term)
{
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

static void assert_clause(consult_context_t* context, const consult_clause_t* c)
{
	int is_current_pred = 0;
	if (context->m_current_predicate)
		is_current_pred = predicate_compare(context->m_current_predicate->m_base.m_functor,c->m_term);
	
	if (!is_current_pred)
	{
		int is_new_pred = 0;
		consult_predicate_t* pred = new_predicate(context,c->m_term,0,0,0,0,&is_new_pred);
		if (!pred)
			return report_out_of_memory_error(context,c->m_term);

		// The current predicate changes, no matter what happens next
		context->m_current_predicate = pred;

		if (!is_new_pred)
		{
			if (!pred->m_discontiguous && pred->m_clauses)
			{
				// TODO: Some kind of discontiguous warning
			}

			if (!pred->m_multifile && !term_compare(pred->m_file->m_filename,context->m_files->m_filename))
			{
				// TODO: Some kind of multifile warning
			}
		}
	}

	append_clause(context,context->m_current_predicate,c);
}

static void load_file(consult_context_t* context, const term_t* filename)
{
	parser_t parser = { .m_line_info.m_end_line = 1, .m_multiterm = 1 };
	parser.m_s = stream_resolver_open(context->m_resolver,context->m_eh,filename);
	if (!parser.m_s)
		context->m_failed = 1;
	else
	{
		while (!context->m_critical_failure)
		{
			term_t* sp = context->m_context->m_stack;

			parse_status_t status = read_term(context->m_context,&parser);
			if (status == PARSE_EOF)
				break;

			if (status == PARSE_THROW)
			{
				report_exception(context);

				// Reset stack
				context->m_context->m_stack = sp;
			}
			else
			{
				// Unpack clause term
				consult_clause_t clause = { .m_term = context->m_context->m_stack };
				clause.m_varcount = (clause.m_term++)->m_u64val;
				if (clause.m_varcount)
				{
					clause.m_varinfo = clause.m_term;
					for (size_t i = 0; i < clause.m_varcount; ++i)
						clause.m_term = get_next_arg(clause.m_term) + 1;
				}
			
				if (clause.m_term->m_u64val == PACK_COMPOUND_EMBED_2(1,':','-'))
					directive(context,get_first_arg(clause.m_term,NULL));
				else
					assert_clause(context,&clause);
			}
		}

		stream_close(parser.m_s);
	}
}

static void include(consult_context_t* context, const term_t* t)
{
	load_file(context,t);
}

static void ensure_loaded(consult_context_t* context, const term_t* t)
{
	// Check if we have attempted to load it already
	for (consult_file_t* f = context->m_files;f != NULL; f = f->m_next)
	{
		if (term_compare(f->m_filename,t))
			return;
	}

	consult_file_t* new_file = heap_malloc(context->m_context->m_heap,sizeof(consult_file_t));
	if (!new_file)
		report_out_of_memory_error(context,t);
	else
	{
		*new_file = (consult_file_t){ .m_filename = t, .m_next = context->m_files };
		context->m_files = new_file;

		load_file(context,t);

		// Reset current file
		context->m_files = new_file->m_next;
	}
}

static void* heap_allocator_malloc(void* param, size_t bytes)
{
	uint64_t* p = heap_malloc((heap_t*)param,bytes + sizeof(uint64_t));
	if (p)
		*p++ = bytes;
	
	return p;
}

static void heap_allocator_free(void* param, void* ptr)
{
	if (ptr)
	{
		size_t bytes = *(((uint64_t*)ptr) - 1);
		heap_free((heap_t*)param,ptr,bytes);
	}
}

int consult(context_t* context, const term_t* filename, stream_resolver_t* resolver, exception_handler_fn_t* eh)
{
	consult_context_t cc =
	{
		.m_context = context,
		.m_eh = eh,
		.m_resolver = resolver,
		.m_predicates.m_allocator = &(prolite_allocator_t){
			.m_fn_malloc = &heap_allocator_malloc,
			.m_fn_free = &heap_allocator_free,
			.m_param = context->m_heap
		},
		.m_modules = &(consult_module_t){ .m_module = *context->m_module },
		.m_files = &(consult_file_t){ .m_filename = filename }
	};

	size_t heap_start = heap_top(cc.m_context->m_heap);

	load_file(&cc,filename);
	if (!cc.m_failed)
	{
		// TODO: We now have a map of predicates...
	}
	
	// Clear the predicate map - not needed if using local heap
	//predicate_map_clear(&cc.m_predicates);

	// Reset heap
	heap_reset(cc.m_context->m_heap,heap_start);

	// We have just done a load of heap allocation
	heap_compact(cc.m_context->m_heap);

	return cc.m_failed;
}