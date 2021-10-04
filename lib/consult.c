#include "parser.h"
#include "compile.h"

#include <string.h>

typedef struct consult_file
{
	struct consult_file* m_next;
	const term_t*        m_filename;
	
} consult_file_t;

typedef struct consult_predicate
{
	compile_predicate_t m_base;

	unsigned          m_public : 1;
	unsigned          m_dynamic : 1;
	unsigned          m_multifile : 1;
	unsigned          m_discontiguous : 1;

	const term_t*     m_filename;
	compile_clause_t* m_clauses;
		
} consult_predicate_t;

static_assert(offsetof(consult_predicate_t,m_base) == 0,"structure members reorganised");

typedef struct consult_initializer
{
	struct consult_initializer* m_next;
	const term_t*               m_goal;
	size_t                      m_varcount;
	
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
	parser_t*               m_parser;
	consult_file_t*         m_includes;
	consult_file_t*         m_loaded_files;
	consult_predicate_t*    m_current_predicate;
	consult_module_t*       m_modules;

	// TODO: The following are per-module
	predicate_map_t         m_predicates;
	prolog_flags_t          m_flags;
	operator_table_t        m_operators;
	char_conv_table_t       m_char_conversion;
	consult_initializer_t*  m_initializers;
	
} consult_context_t;

void directive_op(context_t* context, operator_table_t* ops, const term_t* goal);
void directive_set_prolog_flag(context_t* context, const term_t* flag);
void directive_char_conversion(context_t* context, char_conv_table_t* cc, const term_t* goal);

static void report_exception(consult_context_t* context)
{
	const term_t* arg1 = context->m_context->m_stack;
	term_t* sp = (term_t*)get_next_arg(arg1);
	
	// TODO
	assert(0);
	//(*context->m_eh)(context->m_context);
	
	context->m_context->m_stack = sp;
	context->m_context->m_flags &= ~FLAG_THROW;
	context->m_failed = 1;
}

static void check_exception(consult_context_t* context)
{
	if (context->m_context->m_flags & FLAG_THROW)
		report_exception(context);
}

static void report_out_of_memory_error(consult_context_t* context, const term_t* t)
{
    push_out_of_memory_error(context->m_context,t);
	report_exception(context);

	context->m_critical_failure = 1;
}

static void report_permission_error(consult_context_t* context, uint64_t p1, uint64_t p2, const term_t* t)
{
	push_permission_error(context->m_context,p1,p2,t);
	report_exception(context);
}

static void report_type_error(consult_context_t* context, uint64_t p1, const term_t* t)
{
	push_type_error(context->m_context,p1,t);
	report_exception(context);
}

static void report_domain_error(consult_context_t* context, uint64_t p1, const term_t* t)
{
	push_domain_error(context->m_context,p1,t);
	report_exception(context);
}

static void report_representation_error(consult_context_t* context, uint64_t p1, const term_t* t)
{
	push_representation_error(context->m_context,p1,t);
	report_exception(context);
}

static consult_predicate_t* new_predicate(consult_context_t* context, const term_t* t, int public, int dynamic, int multifile, int discontiguous, int* is_new_pred)
{
	consult_predicate_t* new_pred = heap_malloc(&context->m_context->m_heap,sizeof(consult_predicate_t));
	if (!new_pred)
		return NULL;
	
	*new_pred = (consult_predicate_t){ 
		.m_base.m_base.m_functor = t,
		.m_dynamic = dynamic,
		.m_multifile = multifile,
		.m_discontiguous = discontiguous,
		.m_filename = context->m_includes->m_filename
	};

	predicate_base_t* pred = predicate_map_insert(&context->m_predicates,&new_pred->m_base.m_base);
	
	if (is_new_pred)
		*is_new_pred = (pred == &new_pred->m_base.m_base);

	if (!pred || pred != &new_pred->m_base.m_base)
		heap_free(&context->m_context->m_heap,new_pred,sizeof(consult_predicate_t));
	
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
	if (MASK_DEBUG_INFO(pi->m_u64val) == PACK_COMPOUND_EMBED_1(2,'/'))
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
					string_t f;
					get_string(functor,&f,NULL);
					context->m_context->m_stack = push_predicate(context->m_context->m_stack,a,f.m_str,f.m_len,1,NULL);
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
		while (MASK_DEBUG_INFO(arg->m_u64val) == PACK_COMPOUND_EMBED_1(2,'.'))
		{
			arg = get_first_arg(arg,NULL);

			pi_directive_inner(context,arg,fn);

			arg = get_next_arg(arg);
		}

		if (MASK_DEBUG_INFO(arg->m_u64val) != PACK_ATOM_EMBED_2('[',']'))
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

static void append_clause(consult_context_t* context, consult_predicate_t* pred, const compile_clause_t* c)
{
	// Push a clause frame on the stack, and add to linked list
	compile_clause_t** tail = &pred->m_clauses;
	while (*tail)
		tail = &(*tail)->m_next;

	compile_clause_t* new_clause = heap_malloc(&context->m_context->m_heap,sizeof(compile_clause_t));
	if (!new_clause)
		return report_out_of_memory_error(context,c->m_head);

	*new_clause = *c;
	*tail = new_clause;
}

static void assert_initializer(consult_context_t* context, const term_t* goal, size_t varcount)
{
	// Only add if we are actually going to do something about it
	if (!context->m_failed)
	{
		// Push an initializer frame on the stack, and add to linked list
		consult_initializer_t** tail = &context->m_initializers;
		while (*tail)
			tail = &(*tail)->m_next;

		consult_initializer_t* new_init = heap_malloc(&context->m_context->m_heap,sizeof(consult_initializer_t));
		if (!new_init)
			return report_out_of_memory_error(context,goal);

		*new_init = (consult_initializer_t){ .m_goal = goal, .m_varcount = varcount };
		*tail = new_init;
	}
}

static void include(consult_context_t* context, const term_t* t);
static void ensure_loaded(consult_context_t* context, const term_t* t);

static void directive(consult_context_t* context, const term_t* term, size_t varcount)
{
	uint64_t d = MASK_DEBUG_INFO(term->m_u64val);
	switch (d)
	{
	case PACK_COMPOUND_BUILTIN(include,1):
		include(context,get_first_arg(term,NULL));
		break;

	case PACK_COMPOUND_BUILTIN(ensure_loaded,1):
		ensure_loaded(context,get_first_arg(term,NULL));
		break;

	case PACK_COMPOUND_BUILTIN(initialization,1):
		assert_initializer(context,get_first_arg(term,NULL),varcount);
		break;

	case PACK_COMPOUND_BUILTIN(set_prolog_flag,2):
		directive_set_prolog_flag(context->m_context,get_first_arg(term,NULL));
		check_exception(context);
		break;

	case PACK_COMPOUND_EMBED_2(3,'o','p'):
		directive_op(context->m_context,&context->m_operators,term);
		check_exception(context);
		break;

	case PACK_COMPOUND_BUILTIN(char_conversion,2):
		directive_char_conversion(context->m_context,&context->m_char_conversion,term);
		check_exception(context);
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
		if ((d & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(public,0))
			pi_directive(context,term,&public);
		else if ((d & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(dynamic,0))
			pi_directive(context,term,&dynamic);
		else if ((d & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(multifile,0))
			pi_directive(context,term,&multifile);
		else if ((d & PACK_COMPOUND_BUILTIN_MASK) == PACK_COMPOUND_BUILTIN(discontiguous,0))
			pi_directive(context,term,&discontiguous);
		else if (get_term_type(term) == prolite_compound)
		{
			d = MASK_DEBUG_INFO(term[1].m_u64val);
			if (d == PACK_ATOM_BUILTIN(public))
				pi_directive(context,term,&public);
			else if (d == PACK_ATOM_BUILTIN(dynamic))
				pi_directive(context,term,&dynamic);
			else if (d == PACK_ATOM_BUILTIN(multifile))
				pi_directive(context,term,&multifile);
			else if (d == PACK_ATOM_BUILTIN(discontiguous))
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

static void assert_clause(consult_context_t* context, const compile_clause_t* c)
{
	int is_current_pred = 0;
	if (context->m_current_predicate)
		is_current_pred = predicate_compare(context->m_current_predicate->m_base.m_base.m_functor,c->m_head);
	
	if (!is_current_pred)
	{
		int is_new_pred = 0;
		consult_predicate_t* pred = new_predicate(context,c->m_head,0,0,0,0,&is_new_pred);
		if (!pred)
			return report_out_of_memory_error(context,c->m_head);

		// The current predicate changes, no matter what happens next
		context->m_current_predicate = pred;

		if (!is_new_pred)
		{
			if (!pred->m_discontiguous && pred->m_clauses)
			{
				// TODO: Some kind of discontiguous warning
			}

			if (!pred->m_multifile && !term_compare(pred->m_filename,context->m_includes->m_filename))
			{
				// TODO: Some kind of multifile warning
			}
		}
	}

	append_clause(context,context->m_current_predicate,c);
}

static prolite_stream_t* stream_open(consult_context_t* context, const term_t* t)
{
	prolite_stream_t* s = NULL;
	prolite_stream_resolver_error_t err = prolite_stream_resolver_error_permission;
	if (context->m_context->m_resolver)
	{
		if (context->m_context->m_resolver->m_fn_open_relative && context->m_parser->m_s)
		{
			string_t str;
			get_string(t,&str,NULL);
			s = (*context->m_context->m_resolver->m_fn_open_relative)(context->m_context->m_resolver,context->m_parser->m_s,(const char*)str.m_str,str.m_len,&err);
			if (!s)
				context->m_failed = 1;
		}
		else if (context->m_context->m_resolver->m_fn_open)
		{
			string_t str;
			get_string(t,&str,NULL);
			s = (*context->m_context->m_resolver->m_fn_open)(context->m_context->m_resolver,(const char*)str.m_str,str.m_len,&err);
			if (!s)
				context->m_failed = 1;
		}
	}
	
	if (!s)
	{
		switch (err)
		{
		default:
			report_permission_error(context,PACK_ATOM_EMBED_4('o','p','e','n'),PACK_ATOM_BUILTIN(source_sink),t);
			break;
		}
	}

	return s;
}

static void load_file(consult_context_t* context, const term_t* filename)
{
	// Check for recursive inclusion
	for (consult_file_t* f = context->m_includes;f != NULL; f = f->m_next)
	{
		if (term_compare(f->m_filename,filename))
		{
			// TODO - Some kind of error?
			assert(0);
			return;
		}			
	}

	parser_t parser = 
	{
		.m_context = context->m_context,
		.m_flags = context->m_parser->m_flags,
		.m_operators = context->m_parser->m_operators,
		.m_char_conversion = context->m_parser->m_char_conversion,
		.m_multiterm = 1,
		.m_line_info.m_start_col = 1,
		.m_line_info.m_end_col = 1,
		.m_line_info.m_start_line = 1,
		.m_line_info.m_end_line = 1,
		.m_s = stream_open(context,filename)
	};

	if (parser.m_s)
	{
		parser_t* prev_parser = context->m_parser;
		context->m_parser = &parser;

		consult_file_t include = { 
			.m_filename = filename,
			.m_next = context->m_includes
		};
		context->m_includes = &include;

		while (!context->m_critical_failure)
		{
			term_t* sp = context->m_context->m_stack;

			parse_status_t status = read_term(&parser);
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
				compile_clause_t clause = { .m_head = context->m_context->m_stack };
				clause.m_varcount = (clause.m_head++)->m_u64val;
				for (size_t i = 0; i < clause.m_varcount; ++i)
					clause.m_head = get_next_arg(clause.m_head) + 1;
							
				if (MASK_DEBUG_INFO(clause.m_head->m_u64val) == PACK_COMPOUND_EMBED_2(1,':','-'))
					directive(context,get_first_arg(clause.m_head,NULL),clause.m_varcount);
				else
				{
					if (MASK_DEBUG_INFO(clause.m_head->m_u64val) == PACK_COMPOUND_EMBED_2(2,':','-'))
					{
						clause.m_head = get_first_arg(clause.m_head,NULL);
						clause.m_body = get_next_arg(clause.m_head);
					}
					
					assert_clause(context,&clause);
				}
			}
		}

		context->m_includes = include.m_next;
		context->m_parser = prev_parser;

		(*parser.m_s->m_fn_close)(parser.m_s);
	}
}

static void include(consult_context_t* context, const term_t* t)
{
	load_file(context,t);
}

static void ensure_loaded(consult_context_t* context, const term_t* t)
{
	// Check if we have attempted to load it already
	for (consult_file_t* f = context->m_loaded_files;f != NULL; f = f->m_next)
	{
		if (term_compare(f->m_filename,t))
			return;
	}

	// Remember that we have loaded the file
	consult_file_t* f = heap_malloc(&context->m_context->m_heap,sizeof(consult_file_t));
	if (!f)
		return report_out_of_memory_error(context,t);
	
	*f = (consult_file_t){ 
		.m_filename = t,
		.m_next = context->m_loaded_files
	};
	context->m_loaded_files = f;

	// Stash the parse context, as this is a different "prolog text" being prepared for execution
	prolog_flags_t old_flags = context->m_flags;
	context->m_flags = g_default_prolog_flags;

	operator_table_t old_ops = context->m_operators;
	context->m_operators.m_root = NULL;
	
	char_conv_table_t old_conv = context->m_char_conversion;
	context->m_char_conversion.m_root = NULL;
		
	load_file(context,t);

	context->m_operators = old_ops;
	context->m_char_conversion = old_conv;
	context->m_flags = old_flags;	
}

static int consult(context_t* context, const term_t* filename)
{
	size_t heap_start = heap_top(&context->m_heap);

	prolite_allocator_t local_allocator = heap_allocator(&context->m_heap);

	consult_context_t cc =
	{
		.m_context = context,
		.m_predicates.m_allocator = &local_allocator,
		.m_operators.m_allocator = &local_allocator,
		.m_char_conversion.m_allocator = &local_allocator,
		.m_flags = g_default_prolog_flags,
		.m_parser = &(parser_t){0}
	};
	cc.m_parser->m_flags = &cc.m_flags;
	cc.m_parser->m_operators = &cc.m_operators;
	cc.m_parser->m_char_conversion = &cc.m_char_conversion;

	load_file(&cc,filename);
	if (!cc.m_failed)
	{
		// TODO: We now have a map of predicates...

		// Initializer context flags must be the consult flags
		for (consult_initializer_t* init = cc.m_initializers; init ; init = init->m_next)
		{
			compile_goal(context,init->m_goal,init->m_varcount);
		}
	}
	
	// Clear the predicate map - not needed if using local heap
	//predicate_map_clear(&cc.m_predicates);

	// Reset heap
	heap_reset(&context->m_heap,heap_start);

	// We have just done a load of heap allocation
	heap_compact(&context->m_heap);

	return cc.m_failed;
}

PROLITE_EXPORT prolite_context_t prolite_context_load(void* user_data, const prolite_environment_t* env, const char* source)
{
	if (!env)
		env = &g_default_env;

	context_t* context = context_new(user_data,env);
	if (context)
	{
		term_t* filename = context->m_stack = push_string(context->m_stack,prolite_atom,(const unsigned char*)source,strlen(source),1,NULL);
		if (consult(context,filename))
		{
			context_delete(context);
			context = NULL;
		}
	}
	return (prolite_context_t)context;
}
