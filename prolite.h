

typedef struct prolite_env
{
	int opaque;
}* prolite_env_t;

typedef struct prolite_query
{
	int opaque;
}* prolite_query_t;

prolite_query_t prolite_prepare(prolite_env_t env, const char* text);

