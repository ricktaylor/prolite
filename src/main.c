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

	prolite_prepare(&dummy,"test(X).",-1,&q,NULL);

	return 0;
}
