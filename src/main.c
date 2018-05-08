/*
 * main.c
 *
 *  Created on: 13 Mar 2018
 *      Author: taylorr
 */

#include "../include/prolite.h"

#include <stdio.h>

static void dump(enum eProliteResult r)
{
	switch (r)
	{
	case PROLITE_TRUE:
		printf("Succeeds.");
		printf("\n");
		break;

	case PROLITE_NOMEM:
		printf("resource_error(memory).\n");
		break;

	case PROLITE_ERROR:
		printf("ERROR!\n");
		break;

	case PROLITE_FAIL:
		printf("Fails.\n");
		break;
	}

	fflush(stdout);
}

int main(int argc, char* argv[])
{
	const char* cmd = argc > 1 ? argv[1] : "true.";
	prolite_env_t dummy = {0};
	prolite_query_t q = prolite_new_query(dummy);
	if (q)
	{
		enum eProliteResult r = prolite_prepare(q,cmd,-1,NULL);
		if (r == PROLITE_TRUE)
		{
			do
			{
				r = prolite_solve(q);
				dump(r);
			}
			while (r == PROLITE_TRUE);

			prolite_reset(q);
		}
		else
			dump(r);

		prolite_delete_query(q);
	}

	return 0;
}
