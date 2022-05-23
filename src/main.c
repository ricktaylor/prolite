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

prolite_stream_resolver_t* fs_resolver_new(void);
void fs_resolver_destroy(prolite_stream_resolver_t* r);

static void exception_handler(const char* err_msg, size_t err_len)
{
	fprintf(stderr,"%.*s\n",(int)err_len,err_msg);
}

int main(int argc, char* argv[])
{
	settings_t settings = {0};
	argc = parse_cmd_args(argc,argv,&settings);

	prolite_environment_t env = {
		.m_resolver = fs_resolver_new(),
		.m_handler = &exception_handler
	};
	if (env.m_resolver)
	{
		prolite_context_t context = prolite_context_load(NULL,&env,argv[0]);
		if (context)
		{
			// TODO!

			prolite_context_destroy(context);
		}

		fs_resolver_destroy(env.m_resolver);
	}

	return 0;
}
