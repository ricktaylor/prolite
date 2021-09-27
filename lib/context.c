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

uint32_t convert_char(context_t* context, uint32_t in_char)
{
	/* TODO */

	return in_char;
}

#ifdef UNUSED
static uint32_t atom_to_code(const term_t* b)
{
	uint64_t all48 = UNPACK_MANT_48(b->m_u64val);
	unsigned int len = ((all48 >> 40) & 0x07);
	unsigned int count = 0;
	uint32_t val = 0;
	
	uint8_t c[5] =
	{
		(all48 >> 32) & 0xFF,
		(all48 >> 24) & 0xFF,
		(all48 >> 16) & 0xFF,
		(all48 >> 8) & 0xFF,
		all48 & 0xFF
	};

	if (((all48 >> 32) & 0xC000) != 0x8000)
		return -1;

	if (c[0] <= 0x7f)
		return len == 1 ? c[0] : -1;

	if (c[0] < 0xC2 || c[0] > 0xF4)
		return -1;

	if ((c[0] & 0xE0) == 0xC0)
	{
		count = 2;
		val = (c[0] & 0x1F);
	}
	else if ((c[0] & 0xF0) == 0xE0)
	{
		if ((c[0] == 0xE0 && c[1] >= 0x80 && c[1] <= 0x9F) ||
			(c[0] == 0xED && c[1] >= 0xA0 && c[1] <= 0xBF))
		{
			return -1;
		}

		count = 3;
		val = (c[0] & 0x0F);
	}
	else if ((c[0] & 0xF8) == 0xF0)
	{
		if ((c[0] == 0xF0 && c[1] >= 0x80 && c[1] <= 0x8F) ||
			(c[0] == 0xF4 && c[1] >= 0x90 && c[1] <= 0xBF))
		{
			return -1;
		}

		count = 4;
		val = (c[0] & 0x7);
	}
	else
		return -1;

	if (len != count)
		return -1;
	else
	{
		unsigned int i;
		for (i=1;i<count;++i)
		{
			if ((c[i] & 0xC0) != 0x80)
				return -1;

			val = (val << 6) | (c[i] & 0x3F);
		}
	}

	return val;
}
#endif

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

void builtin_set_prolog_flag(context_t* context)
{
}

void builtin_char_conversion(context_t* context)
{
}

void builtin_current_char_conversion(context_t* context)
{
}

void builtin_op(context_t* context)
{
}

void builtin_current_op(context_t* context)
{
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

static void default_exception_handler(prolite_context_t context)
{
	// TODO - Print error?

	fprintf(stderr,"Unhandled prolite exception\n");
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
