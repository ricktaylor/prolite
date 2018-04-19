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

	if (prolite_prepare(dummy,"true,true.",-1,&q,NULL) == PROLITE_TRUE)
	{
		while (prolite_solve(q) == PROLITE_TRUE)
		{
			printf("TRUE\r\n");
		}

		printf("RESET\r\n\r\n");

		prolite_reset(q);

		while (prolite_solve(q) == PROLITE_TRUE)
		{
			printf("TRUE\r\n");
		}

		prolite_finalize(q);

		printf("DONE\r\n");
	}

	return 0;
}
