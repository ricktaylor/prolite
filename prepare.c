
#include <stdlib.h>

typedef struct prolite_env_s
{
	int opaque;
}* prolite_env_t;

typedef struct prolite_query_s
{
	int opaque;
}* prolite_query_t;

int read_term_string(const char* str);

prolite_query_t prolite_prepare(prolite_env_t env, const char* text)
{
	return NULL;
}
