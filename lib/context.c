/*
 * context.c
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#include "context.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

const prolog_flags_t g_default_prolog_flags =
{
	.char_conversion = 1,
	.back_quotes = 1
};

static void push_flag_value_error(context_t* context, const term_t* flag, const term_t* value)
{
	// TODO
	
	context->m_flags = FLAG_THROW;
}

static void set_prolog_flag_inner(context_t* context, const term_t* flag, const term_t* value)
{
	switch (MASK_DEBUG_INFO(flag->m_u64val))
	{
	case PACK_ATOM_BUILTIN(char_conversion):
		switch (MASK_DEBUG_INFO(value->m_u64val))
		{
		case PACK_ATOM_EMBED_2('o','n'):
			context->m_module->m_flags.char_conversion = 1;
			break;

		case PACK_ATOM_EMBED_3('o','f','f'):
			context->m_module->m_flags.char_conversion = 0;
			break;

		default:
			push_flag_value_error(context,flag,value);
			break;
		}
		break;

	case PACK_ATOM_EMBED_5('d','e','b','u','g'):
		switch (MASK_DEBUG_INFO(value->m_u64val))
		{
		case PACK_ATOM_EMBED_2('o','n'):
			context->m_module->m_flags.debug = 1;
			break;

		case PACK_ATOM_EMBED_3('o','f','f'):
			context->m_module->m_flags.debug = 0;
			break;

		default:
			push_flag_value_error(context,flag,value);
			break;
		}
		break;

	case PACK_ATOM_BUILTIN(unknown):
		switch (MASK_DEBUG_INFO(value->m_u64val))
		{
		case PACK_ATOM_EMBED_5('e','r','r','o','r'):
			context->m_module->m_flags.unknown = 0;
			break;

		case PACK_ATOM_EMBED_4('f','a','i','l'):
			context->m_module->m_flags.unknown = 1;
			break;

		case PACK_ATOM_BUILTIN(warning):
			context->m_module->m_flags.unknown = 2;
			break;

		default:
			push_flag_value_error(context,flag,value);
			break;
		}
		break;

	case PACK_ATOM_BUILTIN(double_quotes):
		switch (MASK_DEBUG_INFO(value->m_u64val))
		{
		case PACK_ATOM_EMBED_5('c','h','a','r','s'):
			context->m_module->m_flags.double_quotes = 0;
			break;

		case PACK_ATOM_EMBED_5('c','o','d','e','s'):
			context->m_module->m_flags.double_quotes = 1;
			break;

		case PACK_ATOM_EMBED_4('a','t','o','m'):
			context->m_module->m_flags.double_quotes = 2;
			break;
		
		default:
			push_flag_value_error(context,flag,value);
			break;
		}
		break;

	case PACK_ATOM_BUILTIN(back_quotes):
		switch (MASK_DEBUG_INFO(value->m_u64val))
		{
		case PACK_ATOM_EMBED_5('c','h','a','r','s'):
			context->m_module->m_flags.double_quotes = 0;
			break;

		case PACK_ATOM_EMBED_5('c','o','d','e','s'):
			context->m_module->m_flags.double_quotes = 1;
			break;

		case PACK_ATOM_EMBED_4('a','t','o','m'):
			context->m_module->m_flags.double_quotes = 2;
			break;
		
		default:
			push_flag_value_error(context,flag,value);
			break;
		}
		break;

	default:
		if (get_term_type(flag) == prolite_var)
			push_instantiation_error(context,flag);
		else if (get_term_type(flag) != prolite_atom)
			push_type_error(context,PACK_ATOM_EMBED_4('a','t','o','m'),flag);
		else
			push_domain_error(context,PACK_ATOM_BUILTIN(prolog_flag),flag);
		break;
	}	
}

void directive_set_prolog_flag(context_t* context, const term_t* flag)
{
	const term_t* value = get_next_arg(flag);
	set_prolog_flag_inner(context,flag,value);
}

void builtin_set_prolog_flag(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[])
{
	set_prolog_flag_inner(context,argv[0],argv[1]);
	if (!(context->m_flags & FLAG_THROW))
		(*gosub)(context);
}

const term_t* deref_local_var(context_t* context, const term_t* t)
{
	if (get_term_type(t) == prolite_var)
	{
		assert(context->m_locals && get_var_index(t) < context->m_locals->m_count);
		const term_t* g = context->m_locals->m_vals[get_var_index(t)];
		if (g)
			t = deref_local_var(context,t);
	}
	return t;
}

static int unify(context_t* context, const term_t* t1, const term_t* t2, int with_occurs_check)
{
	return 0;
}

static void builtin_unify(context_t* context, int with_occurs_check) 
{
	term_t* sp = context->m_stack;
	builtin_fn_t gosub = (sp++)->m_pval;
	size_t var_count = (sp++)->m_u64val;
	const term_t* args = sp;
	
	// There are var_count * 2 args on the stack
	context->m_stack += (var_count * 2);

	// Now we can unify...
	int unified = 1;
	for (size_t i = 0; unified && i < var_count; ++i)
	{
		unified = unify(context,args,args + 1,with_occurs_check);
		
		args += 2;
	}

	if (unified)
		(*gosub)(context);
	
	context->m_stack = sp;
}

PROLITE_EXPORT void prolite_builtin_unify(context_t* context) 
{
	builtin_unify(context,0);
}

PROLITE_EXPORT void prolite_builtin_unify_with_occurs_check(context_t* context)
{
	builtin_unify(context,1);
}

module_t* module_new(context_t* context, const term_t* name)
{
	// TODO: Much more here!!

	module_t* module = heap_malloc(&context->m_heap,sizeof(module_t));
	if (module)
	{
		*module = (module_t){
			.m_name = name,
			.m_flags = g_default_prolog_flags
		};
	}

	return module;
}

void module_delete(module_t* module)
{
	// TODO
}

static void default_exception_handler(const char* err_msg, size_t err_len)
{
	fprintf(stderr,"Uncaught exception: %.*s\n",(int)err_len,err_msg);

	assert(0);
	exit(EXIT_FAILURE);
}

context_t* context_new(void* user_data, const prolite_environment_t* env)
{
	heap_t heap = { .m_allocator = env->m_allocator };

	context_t* c = heap_malloc(&heap,sizeof(context_t));
	if (c)
	{
		*c = (context_t){
			.m_user_data = user_data,
			.m_heap = heap,
			.m_eh = env->m_handler,
			.m_resolver = env->m_resolver
		};
		if (!c->m_eh)
			c->m_eh = &default_exception_handler;

		size_t stack_size = env->m_stack_size;
		if (!stack_size)
			stack_size = g_default_env.m_stack_size;

		stack_size = bytes_to_cells(stack_size,sizeof(term_t));

		c->m_stack = allocator_malloc(env->m_allocator,stack_size * sizeof(term_t));
		if (!c->m_stack)
		{
			heap_destroy(&heap);
			return NULL;
		}		
		c->m_stack += (stack_size - 1);

		term_t user = { .m_u64val = PACK_ATOM_EMBED_4('u','s','e','r') };
		c->m_module = module_new(c,&user);
	}

	return c;
}

void context_delete(context_t* c)
{
	module_delete(c->m_module);
	//stack_delete(c->m_call_stack);
	
	heap_t h = c->m_heap;
	heap_destroy(&h);
}

const prolite_environment_t g_default_env = 
{
	.m_stack_size = 0x10000
};

PROLITE_EXPORT prolite_context_t prolite_context_new(void* user_data, const prolite_environment_t* env)
{
	if (!env)
		env = &g_default_env;
	
	context_t* c = context_new(user_data,env);
	if (!c && env && env->m_handler)
	{
		// TODO: Report an error!
	}

	assert(c == (void*)&c->m_user_data);

	return (prolite_context_t)c;
}

PROLITE_EXPORT void prolite_context_destroy(prolite_context_t context)
{
	context_delete((context_t*)context);
}



// MOVE THIS!!

void builtin_user_defined(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[])
{
}

void builtin_asserta(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[])
{
	
}

void builtin_assertz(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[])
{
	
}


