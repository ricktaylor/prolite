/*
 * main.c
 *
 *  Created on: 13 Mar 2018
 *      Author: taylorr
 */

#include "settings.h"

#include "../lib/stream.h"

void consult(context_t* context, const term_t* filename, stream_resolver_t* resolver, exception_handler_fn_t* eh);

#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

// TODO: Move these around later...

module_t* module_new(context_t* context, const term_t* name)
{
	// TODO: Much more here!!

	module_t* module = heap_malloc(context->m_heap,sizeof(module_t));
	if (module)
	{
		memset(module,0,sizeof(module_t));
		module->m_flags.char_conversion = 1;
		module->m_flags.back_quotes = 1;
	}

	/* &(consult_module_t){
				.m_module.m_name = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('u','s','e','r') },
				.m_module.m_flags.char_conversion = 1,
				.m_module.m_flags.back_quotes = 1
			}, */

	return module;
}

void module_delete(module_t* module)
{
	// TODO
}

context_t* context_new(heap_t* heap)
{
	const size_t stack_size = 65536;

	context_t* c = heap_malloc(heap,sizeof(context_t));
	if (c)
	{
		memset(c,0,sizeof(context_t));			
		c->m_stack = malloc(stack_size * sizeof(term_t));
		c->m_stack += stack_size;
		c->m_heap = heap;

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

int main(int argc, char* argv[])
{
	settings_t settings = {0};
	argc = parse_cmd_args(argc,argv,&settings);

	heap_t h = {0};
	context_t* context = context_new(&h);
	if (context)
	{
		/*struct text_stream ts = {
			.m_proto.m_fn_read = &text_stream_read,
			.m_str = &cmd,
			.m_end = *ts.m_str + strlen(*ts.m_str)
		};

		// Read a term and prepare it for execution
		parse_status_t result = read_term(context,&ts.m_proto);
		if (result == PARSE_OK)
		{		
			// Pop varinfo
			size_t varcount = 0;
			{
				const term_t* sp = context->m_stack;
				varcount = (sp++)->m_u64val;
				for (size_t i = 0; i < varcount; ++i)
					sp = get_next_arg(sp) + 1;

				context->m_stack = (term_t*)sp;
			}

			compile_term(context,context->m_stack,varcount);
		}*/
		
		context_delete(context);
	}

	heap_destroy(&h);

	return 0;
}
