#include "parser.h"
#include "compile.h"

#include <string.h>
#include <math.h>

typedef struct consult_file
{
	struct consult_file* m_next;
	const term_t*        m_filename;

} consult_file_t;

typedef struct consult_predicate
{
	compile_predicate_t m_base;

	unsigned          m_public : 1;
	unsigned          m_multifile : 1;
	unsigned          m_discontiguous : 1;

	const term_t*     m_filename;

} consult_predicate_t;

static_assert(offsetof(consult_predicate_t,m_base) == 0,"structure members reorganised");

typedef struct consult_initializer
{
	struct consult_initializer* m_next;
	const term_t*               m_goal;
	size_t                      m_var_count;

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
	heap_t                  m_heap;

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

static consult_predicate_t* new_predicate(consult_context_t* context, const term_t* t, int public, int dynamic, int multifile, int discontiguous, int* is_new_pred)
{
	size_t heap_start = heap_top(&context->m_heap);
	consult_predicate_t* new_pred = heap_malloc(&context->m_heap,sizeof(consult_predicate_t));
	if (!new_pred)
		return NULL;

	*new_pred = (consult_predicate_t){
		.m_base.m_dynamic = dynamic,
		.m_multifile = multifile,
		.m_discontiguous = discontiguous,
		.m_filename = context->m_includes->m_filename
	};

	string_t f;
	const debug_info_t* di = NULL;
	size_t arity = unpack_predicate(t,&f,&di);
	new_pred->m_base.m_base.m_functor = emit_predicate(&(emit_buffer_t){ .m_a = &heap_allocator(&context->m_heap) },arity,f.m_str,f.m_len,0,di);
	if (!new_pred->m_base.m_base.m_functor)
	{
		heap_reset(&context->m_heap,heap_start);
		return NULL;
	}

	predicate_base_t* pred = predicate_map_insert(&context->m_predicates,&new_pred->m_base.m_base);

	if (is_new_pred)
		*is_new_pred = (pred == &new_pred->m_base.m_base);

	if (!pred || pred != &new_pred->m_base.m_base)
		heap_reset(&context->m_heap,heap_start);

	return (consult_predicate_t*)pred;
}

static void public(consult_context_t* context, const term_t* t, const term_t* pi)
{
	consult_predicate_t* pred = new_predicate(context,t,1,0,0,0,NULL);
	if (!pred)
		return throw_out_of_memory_error(context->m_context,t);

	if (!pred->m_public)
	{
		if (pred->m_base.m_clauses)
		{
			// TODO: Must error somehow...
			//return throw_permission_error(context->m_context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),pi);
		}

		pred->m_public = 1;
	}
}

static void dynamic(consult_context_t* context, const term_t* t, const term_t* pi)
{
	consult_predicate_t* pred = new_predicate(context,t,1,1,0,0,NULL);
	if (!pred)
		return throw_out_of_memory_error(context->m_context,t);

	if (!pred->m_base.m_dynamic)
	{
		if (pred->m_base.m_clauses)
			return throw_permission_error(context->m_context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),pi);

		pred->m_base.m_dynamic = 1;
		pred->m_public = 1;
	}
}

static void discontiguous(consult_context_t* context, const term_t* t, const term_t* pi)
{
	consult_predicate_t* pred = new_predicate(context,t,0,0,0,1,NULL);
	if (!pred)
		return throw_out_of_memory_error(context->m_context,t);

	if (!pred->m_discontiguous)
	{
		if (pred->m_base.m_clauses)
		{
			// TODO: Must error somehow...
			//return throw_permission_error(context->m_context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),pi);
		}

		pred->m_discontiguous = 1;
	}
}

static void multifile(consult_context_t* context, const term_t* t, const term_t* pi)
{
	consult_predicate_t* pred = new_predicate(context,t,0,0,1,0,NULL);
	if (!pred)
		return throw_out_of_memory_error(context->m_context,t);

	if (!pred->m_multifile)
	{
		if (pred->m_base.m_clauses)
		{
			// TODO: Must error somehow...
			//return throw_permission_error(context->m_context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),pi);
		}

		pred->m_multifile = 1;
	}
}

static void pi_directive_inner(consult_context_t* context, const term_t* pi, void (*fn)(consult_context_t*,const term_t*,const term_t*))
{
	if (MASK_DEBUG_INFO(pi->m_u64val) == PACK_COMPOUND_EMBED_1(2,'/'))
	{
		const term_t* functor = get_first_arg(pi,NULL);
		if (unpack_term_type(functor) == prolite_atom)
		{
			const term_t* arity = get_next_arg(functor);
			if (unpack_term_type(arity) == prolite_number && nearbyint(arity->m_dval) == arity->m_dval)
			{
				size_t a = arity->m_dval;
				if (a < 0)
					return throw_domain_error(context->m_context,PACK_ATOM_BUILTIN(not_less_than_zero),arity);

				if (a > MAX_ARITY)
					return throw_representation_error(context->m_context,PACK_ATOM_BUILTIN(max_arity),arity);

				size_t trail_start = heap_top(&context->m_context->m_trail);

				if (arity->m_dval > 0)
				{
					string_t f;
					unpack_string(functor,&f,NULL);
					functor = emit_predicate(&(emit_buffer_t){ .m_a = &heap_allocator(&context->m_context->m_trail) },a,f.m_str,f.m_len,1,NULL);
				}

				if (predicate_is_builtin(functor))
					throw_permission_error(context->m_context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(static_procedure),pi);
				else
					(*fn)(context,functor,pi);

				heap_reset(&context->m_context->m_trail,trail_start);
				return;
			}
		}
	}

	throw_type_error(context->m_context,PACK_ATOM_BUILTIN(predicate_indicator),pi);
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

static compile_clause_t* append_clause(consult_context_t* context, consult_predicate_t* pred, const term_t* t, size_t var_count)
{
	// Push a clause frame on the heap, and add to linked list

	size_t heap_start = heap_top(&context->m_heap);
	compile_clause_t* new_clause = heap_malloc(&context->m_heap,sizeof(compile_clause_t));
	if (!new_clause)
		return NULL;

	*new_clause = (compile_clause_t){ .m_var_count = var_count };

	prolite_allocator_t a = heap_allocator(&context->m_heap);
	new_clause->m_head = copy_term(context->m_context,&a,t,0,0,NULL);
	if (!new_clause->m_head)
	{
		heap_reset(&context->m_heap,heap_start);
		return NULL;
	}

	if (MASK_DEBUG_INFO(new_clause->m_head->m_u64val) == PACK_COMPOUND_EMBED_2(2,':','-'))
	{
		new_clause->m_head = get_first_arg(new_clause->m_head,NULL);
		new_clause->m_body = get_next_arg(new_clause->m_head);
	}

	compile_clause_t** tail = &pred->m_base.m_clauses;
	while (*tail)
		tail = &(*tail)->m_next;
	*tail = new_clause;

	return new_clause;
}

static void assert_initializer(consult_context_t* context, const term_t* goal, size_t var_count)
{
	// Only add if we are actually going to do something about it
	if (!context->m_failed)
	{
		// Push an initializer frame on the heap, and add to linked list

		size_t heap_start = heap_top(&context->m_heap);
		consult_initializer_t* new_init = heap_malloc(&context->m_heap,sizeof(consult_initializer_t));
		if (!new_init)
			return throw_out_of_memory_error(context->m_context,goal);

		*new_init = (consult_initializer_t){ .m_var_count = var_count };

		new_init->m_goal = copy_term(context->m_context,&heap_allocator(&context->m_heap),goal,0,0,NULL);
		if (!new_init->m_goal)
		{
			heap_reset(&context->m_heap,heap_start);
			return throw_out_of_memory_error(context->m_context,goal);
		}

		consult_initializer_t** tail = &context->m_initializers;
		while (*tail)
			tail = &(*tail)->m_next;
		*tail = new_init;
	}
}

static void include(consult_context_t* context, const term_t* t);
static void ensure_loaded(consult_context_t* context, const term_t* t);

static void directive(consult_context_t* context, const term_t* term, size_t var_count)
{
	uint64_t d = MASK_DEBUG_INFO(term->m_u64val);
	switch (d)
	{
	case PACK_COMPOUND_BUILTIN(include,1):
		return include(context,get_first_arg(term,NULL));

	case PACK_COMPOUND_BUILTIN(ensure_loaded,1):
		return ensure_loaded(context,get_first_arg(term,NULL));

	case PACK_COMPOUND_BUILTIN(initialization,1):
		return assert_initializer(context,get_first_arg(term,NULL),var_count);

	case PACK_COMPOUND_BUILTIN(set_prolog_flag,2):
		return directive_set_prolog_flag(context->m_context,get_first_arg(term,NULL));

	case PACK_COMPOUND_EMBED_2(3,'o','p'):
		return directive_op(context->m_context,&context->m_operators,term);

	case PACK_COMPOUND_BUILTIN(char_conversion,2):
		return directive_char_conversion(context->m_context,&context->m_char_conversion,term);

	case PACK_COMPOUND_BUILTIN(public,1):
		return pi_directive(context,term,&public);

	case PACK_COMPOUND_BUILTIN(dynamic,1):
		return pi_directive(context,term,&dynamic);

	case PACK_COMPOUND_BUILTIN(multifile,1):
		return pi_directive(context,term,&multifile);

	case PACK_COMPOUND_BUILTIN(discontiguous,1):
		return pi_directive(context,term,&discontiguous);

	default:
		switch (d & PACK_COMPOUND_BUILTIN_MASK)
		{
		case PACK_COMPOUND_BUILTIN(public,0):
			return pi_directive(context,term,&public);

		case PACK_COMPOUND_BUILTIN(dynamic,0):
			return pi_directive(context,term,&dynamic);

		case PACK_COMPOUND_BUILTIN(multifile,0):
			return pi_directive(context,term,&multifile);

		case PACK_COMPOUND_BUILTIN(discontiguous,0):
			return pi_directive(context,term,&discontiguous);

		default:
			if (unpack_term_type(term) == prolite_compound)
			{
				switch (MASK_DEBUG_INFO(term[1].m_u64val))
				{
				case PACK_ATOM_BUILTIN(public):
					return pi_directive(context,term,&public);

				case PACK_ATOM_BUILTIN(dynamic):
					return pi_directive(context,term,&dynamic);

				case PACK_ATOM_BUILTIN(multifile):
					return pi_directive(context,term,&multifile);

				case PACK_ATOM_BUILTIN(discontiguous):
					return pi_directive(context,term,&discontiguous);

				default:
					break;
				}
			}
			break;
		}
	}

	assert(0);
}

static compile_clause_t* assert_clause(consult_context_t* context, const term_t* t, size_t var_count)
{
	const term_t* head = t;
	if (MASK_DEBUG_INFO(head->m_u64val) == PACK_COMPOUND_EMBED_2(2,':','-'))
		head = get_first_arg(head,NULL);

	int is_current_pred = 0;
	if (context->m_current_predicate)
		is_current_pred = predicate_compare(context->m_current_predicate->m_base.m_base.m_functor,head);

	if (!is_current_pred)
	{
		int is_new_pred = 0;
		consult_predicate_t* pred = new_predicate(context,head,0,0,0,0,&is_new_pred);
		if (!pred)
			return NULL;

		// The current predicate changes, no matter what happens next
		context->m_current_predicate = pred;

		if (!is_new_pred)
		{
			if (!pred->m_discontiguous && pred->m_base.m_clauses)
			{
				// TODO: Some kind of discontiguous warning
			}

			if (!pred->m_multifile && !term_compare(pred->m_filename,context->m_includes->m_filename))
			{
				// TODO: Some kind of multifile warning
			}
		}
	}

	return append_clause(context,context->m_current_predicate,t,var_count);
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
			unpack_string(t,&str,NULL);
			s = (*context->m_context->m_resolver->m_fn_open_relative)(context->m_context->m_resolver,context->m_parser->m_s,(const char*)str.m_str,str.m_len,&err);
			if (!s)
				context->m_failed = 1;
		}
		else if (context->m_context->m_resolver->m_fn_open)
		{
			string_t str;
			unpack_string(t,&str,NULL);
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
			throw_permission_error(context->m_context,PACK_ATOM_EMBED_4('o','p','e','n'),PACK_ATOM_BUILTIN(source_sink),t);
			break;
		}
	}

	return s;
}

static void consult_term(context_t* context, void* param, const term_t* term, size_t var_count, const var_info_t* var_info)
{
	consult_context_t* cc = param;
	if (!term)
	{
		if (context->m_flags & FLAG_THROW)
			throw_out_of_memory_error(context,NULL);
	}
	else if (!(context->m_flags & FLAG_THROW))
	{
		if (MASK_DEBUG_INFO(term->m_u64val) == PACK_COMPOUND_EMBED_2(1,':','-'))
			directive(cc,get_first_arg(term,NULL),var_count);
		else if (!assert_clause(cc,term,var_count))
			throw_out_of_memory_error(context,term);
	}

	if (context->m_flags & FLAG_THROW)
	{
		cc->m_failed = 1;

		if (is_out_of_memory_exception(context))
			cc->m_critical_failure = 1;

		unhandled_exception(context,cc->m_parser->m_operators);
	}
}

static void load_file(consult_context_t* context, const term_t* filename, token_t* buffer)
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

	parser_t parser = {
		.m_context = context->m_context,
		.m_buffer = buffer,
		.m_flags = context->m_parser->m_flags,
		.m_operators = context->m_parser->m_operators,
		.m_char_conversion = context->m_parser->m_char_conversion,
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

		while (!parser.m_eof && !context->m_critical_failure)
			read_term(&parser,context,&consult_term,1);

		context->m_includes = include.m_next;
		context->m_parser = prev_parser;

		(*parser.m_s->m_fn_close)(parser.m_s);
	}
}

static void include(consult_context_t* context, const term_t* t)
{
	load_file(context,t,context->m_parser->m_buffer);
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
	consult_file_t* f = heap_malloc(&context->m_heap,sizeof(consult_file_t));
	if (!f)
		return throw_out_of_memory_error(context->m_context,t);

	*f = (consult_file_t){ .m_next = context->m_loaded_files };

	prolite_allocator_t a = heap_allocator(&context->m_heap);
	f->m_filename = copy_term(context->m_context,&a,t,0,0,NULL);
	if (!f->m_filename)
		return throw_out_of_memory_error(context->m_context,t);

	context->m_loaded_files = f;

	// Stash the parse context, as this is a different "prolog text" being prepared for execution
	prolog_flags_t old_flags = context->m_flags;
	context->m_flags = g_default_prolog_flags;

	operator_table_t old_ops = context->m_operators;
	context->m_operators.m_root = NULL;

	char_conv_table_t old_conv = context->m_char_conversion;
	context->m_char_conversion.m_root = NULL;

	token_t buffer = {0};
	load_file(context,t,&buffer);
	allocator_free(context->m_context->m_heap.m_allocator,buffer.m_str);

	context->m_operators = old_ops;
	context->m_char_conversion = old_conv;
	context->m_flags = old_flags;
}

static void* inline_call(void* context, void* param, const term_t* goal, const void* cont)
{
	return inline_predicate_call(context,(const compile_predicate_t*)predicate_map_lookup(&((consult_context_t*)param)->m_predicates,goal),goal,cont);
}

#include <stdio.h>

static void compile_statics(void* param, predicate_base_t* p)
{
	consult_context_t* context = param;
	consult_predicate_t* pred = (consult_predicate_t*)p;

	if (!pred->m_base.m_dynamic)
	{
		string_t s;
		size_t arity = unpack_predicate(pred->m_base.m_base.m_functor,&s,NULL);
		fprintf(stdout,"=======================\nCompiling %.*s/%zu\n",(int)s.m_len,s.m_str,arity);

		// For each clause
		for (const compile_clause_t* clause = pred->m_base.m_clauses; clause; clause = clause->m_next)
		{
			if (clause->m_body)
			{
				fprintf(stdout,"-----\n");

				compile_goal(context->m_context,&inline_call,context,clause->m_body,clause->m_var_count);
			}
		}
	}
}

static int consult(context_t* context, const term_t* filename)
{
	size_t heap_start = heap_top(&context->m_heap);
	consult_context_t cc = {
		.m_context = context,
		.m_flags = g_default_prolog_flags,
		.m_heap = { .m_allocator = context->m_heap.m_allocator },
		.m_parser = &(parser_t){
			.m_buffer = &(token_t){0}
		}

	};

	prolite_allocator_t local_allocator = heap_allocator(&cc.m_heap);
	cc.m_predicates.m_allocator = &local_allocator,
	cc.m_operators.m_allocator = &local_allocator,
	cc.m_char_conversion.m_allocator = &local_allocator,

	cc.m_parser->m_flags = &cc.m_flags;
	cc.m_parser->m_operators = &cc.m_operators;
	cc.m_parser->m_char_conversion = &cc.m_char_conversion;

	load_file(&cc,filename,cc.m_parser->m_buffer);
	allocator_free(context->m_heap.m_allocator,cc.m_parser->m_buffer->m_str);
	if (!cc.m_failed)
	{
		predicate_map_enum(&cc.m_predicates,&compile_statics,&cc);

		// TODO: We now have a map of predicates...

		// Initializer context flags must be the consult flags
		for (consult_initializer_t* init = cc.m_initializers; init ; init = init->m_next)
		{
			string_t s;
			size_t arity = unpack_predicate(init->m_goal,&s,NULL);
			fprintf(stdout,"=======================\nCompiling %.*s/%zu\n",(int)s.m_len,s.m_str,arity);

			compile_goal(context,&inline_call,&cc,init->m_goal,init->m_var_count);
		}
	}

	heap_destroy(&cc.m_heap);

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
		term_t filename[2];
		pack_string(filename,(const unsigned char*)source,source ? strlen(source) : 0);
		if (consult(context,filename))
		{
			context_delete(context);
			context = NULL;
		}
	}
	return (prolite_context_t)context;
}
