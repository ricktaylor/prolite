/*
 * main.c
 *
 *  Created on: 13 Mar 2018
 *      Author: taylorr
 */

#include "../include/prolite.h"

#include <stdio.h>

int main(int argc, char* argv[])
{
	prolite_env_t dummy;
	prolite_query_t q;

	if (prolite_prepare(dummy,"repeat,!,true.",-1,&q,NULL) == PROLITE_TRUE)
	{
		enum eProliteResult r;
		do
		{
			r = prolite_solve(q);
			switch (r)
			{
			case PROLITE_TRUE:
				printf("TRUE\r\n");
				break;

			case PROLITE_HALT:
				printf("HALT\r\n");
				break;

			case PROLITE_NOMEM:
				printf("NOMEM\r\n");
				break;

			case PROLITE_ERROR:
				printf("ERROR\r\n");
				break;

			case PROLITE_FALSE:
				printf("FALSE\r\n");
				break;
			}
		}
		while (r == PROLITE_TRUE);

		prolite_finalize(q);
	}

	return 0;
}
