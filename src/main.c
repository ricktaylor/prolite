/*
 * main.c
 *
 *  Created on: 13 Mar 2018
 *      Author: taylorr
 */

#include "settings.h"

#include "../include/prolite.h"

#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
	settings_t settings = {0};
	argc = parse_cmd_args(argc,argv,&settings);

	prolite_context_t context = prolite_context_load(NULL,NULL,argv[0]);
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
		
		prolite_context_destroy(context);
	}

	return 0;
}
