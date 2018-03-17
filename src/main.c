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

	if (prolite_prepare(dummy,"test(X),test(Y).",-1,&q,NULL) == PROLITE_TRUE)
	{
		prolite_step(q);

		prolite_finalize(q);
	}

	return 0;
}
