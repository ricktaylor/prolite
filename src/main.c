/*
 * main.c
 *
 *  Created on: 13 Mar 2018
 *      Author: taylorr
 */

#include "../include/prolite.h"

int main(int argc, char* argv[])
{
	prolite_env_t dummy;
	prolite_query_t q;

	if (prolite_prepare(dummy,"true,halt(1).",-1,&q,NULL) == PROLITE_TRUE)
	{
		while (prolite_step(q) == PROLITE_TRUE)
			;

		prolite_reset(q);

		while (prolite_step(q) == PROLITE_TRUE)
			;

		prolite_finalize(q);
	}

	return 0;
}
