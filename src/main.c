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
		printf("\r\n");
		break;

	case PROLITE_HALT:
		printf("HALT\r\n");
		break;

	case PROLITE_NOMEM:
		printf("resource_error(memory).\r\n");
		break;

	case PROLITE_ERROR:
		printf("ERROR!\r\n");
		break;

	case PROLITE_FALSE:
		printf("Fails.\r\n");
		break;
	}
}

int main(int argc, char* argv[])
{
	prolite_env_t dummy;
	prolite_query_t q;

	enum eProliteResult r = prolite_prepare(dummy,"X = 1,X = 1.",-1,&q,NULL);
	if (r == PROLITE_TRUE)
	{
		do
		{
			r = prolite_solve(q);
			dump(r);
		}
		while (r == PROLITE_TRUE);

		prolite_finalize(q);
	}
	else
		dump(r);

	return 0;
}
