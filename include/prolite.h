
#include <stddef.h>

enum eProliteResult
{
	PROLITE_NOMEM = -2,
	PROLITE_ERROR = -1,
	PROLITE_FALSE = 0,
	PROLITE_TRUE = 1,
	PROLITE_HALT = 2
};

typedef struct prolite_env
{
	int opaque;
}* prolite_env_t;

typedef struct prolite_query* prolite_query_t;

enum eProliteResult prolite_prepare(prolite_env_t env, const char* query_text, size_t query_len, prolite_query_t* query, const char** tail);
enum eProliteResult prolite_step(prolite_query_t query);
enum eProliteResult prolite_reset(prolite_query_t query);
void prolite_finalize(prolite_query_t query);
