
#include <stddef.h>

enum eProliteResult
{
	PROLITE_ERROR = -1,
	PROLITE_FALSE = 0,
	PROLITE_TRUE = 1
};

typedef struct prolite_env
{
	int opaque;
}* prolite_env_t;

typedef struct prolite_query
{
	int opaque;
}* prolite_query_t;

int prolite_prepare(prolite_env_t env, const char* query_text, size_t query_len, prolite_query_t* query, const char** tail);
