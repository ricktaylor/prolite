
#include <stddef.h>

enum eProliteResult
{
	PROLITE_NOMEM = -2,
	PROLITE_ERROR = -1,
	PROLITE_FAIL = 0,
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

prolite_query_t prolite_new_query(prolite_env_t env);
enum eProliteResult prolite_prepare(prolite_query_t query, const char* query_text, size_t query_len, const char** tail);
enum eProliteResult prolite_solve(prolite_query_t query);
enum eProliteResult prolite_reset(prolite_query_t query);
void prolite_delete_query(prolite_query_t query);
