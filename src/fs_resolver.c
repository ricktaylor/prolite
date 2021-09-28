#include "../include/prolite.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <string.h>

typedef struct fs_resolver
{
	prolite_stream_resolver_t m_base;

	size_t m_include_count;
	char** m_includes;
} fs_resolver_t;

static_assert(offsetof(fs_resolver_t,m_base) == 0,"structure members reorganised");

typedef struct fs_stream
{
	prolite_stream_t m_base;

	FILE*          m_f;
	fs_resolver_t* m_resolver;
	char*          m_name;

} fs_stream_t;

static_assert(offsetof(fs_stream_t,m_base) == 0,"structure members reorganised");

static prolite_stream_t* resolver_open(fs_resolver_t* r, prolite_context_t context, prolite_exception_handler_fn_t eh, const char* dir, const char* name, size_t name_len);

static struct prolite_stream* fs_stream_open_relative(struct prolite_stream* s, prolite_context_t context, prolite_exception_handler_fn_t eh, const char* name, size_t name_len)
{
	fs_stream_t* stream = (fs_stream_t*)s;

	char* dir = strrchr(stream->m_name,'/');
	if (dir)
	{
		dir = strndup(stream->m_name,dir - stream->m_name);
		if (!dir)
		{
			// TODO - memory error via EH
			return NULL;
		}
	}

	s = resolver_open(stream->m_resolver,context,eh,dir,name,name_len);

	free(dir);

	return s;
}

static void fs_stream_close(struct prolite_stream* s)
{
	fs_stream_t* stream = (fs_stream_t*)s;

	fclose(stream->m_f);
	free(stream->m_name);
	free(stream);
}

static int64_t fs_stream_read(struct prolite_stream* s, void* dest, size_t len)
{
	FILE* f = ((fs_stream_t*)s)->m_f;

	int64_t r = fread(dest,1,len,f);
	if (r == 0 && ferror(f))
		r = -1;

	/*if (r > 0)
	{
		fwrite(dest,1,r,stdout);
		fflush(stdout);
	}*/

	return r;
}

static prolite_stream_t* resolver_open(fs_resolver_t* r, prolite_context_t context, prolite_exception_handler_fn_t eh, const char* dir, const char* name, size_t name_len)
{
	size_t dir_len = 0;
	if (dir)
	{
		dir_len = strlen(dir);
		for (;dir_len;--dir_len)
		{
			if (dir[dir_len-1] != '/')
				break;
		}
		if (dir_len)
			++dir_len;
	}

	char* new_name = malloc(dir_len + name_len + 1);
	if (!new_name)
	{
		// TODO - memory error via EH
		return NULL;
	}
		
	if (dir_len)
	{
		memcpy(new_name,dir,dir_len - 1);
		new_name[dir_len - 1] = '/';
	}
	memcpy(new_name + dir_len,name,name_len);
	new_name[dir_len + name_len] = '\0';
	
	name_len = dir_len + name_len;
	
	fs_stream_t* s = malloc(sizeof(fs_stream_t));
	if (!s)
	{
		// TODO - memory error via EH
	}
	else
	{
		*s = (fs_stream_t){
			.m_base.m_fn_close = &fs_stream_close,
			.m_base.m_fn_read = &fs_stream_read,
			.m_base.m_fn_open_relative = &fs_stream_open_relative,
			.m_resolver = r
		};

		s->m_f = fopen(new_name,"r");
		if (!s->m_f)
		{
			if (errno != ENOENT)
			{
				// TODO - error via EH
			}
			else
			{
				// Try appending .pl
				char* new_name2 = realloc(new_name,name_len + 4);
				if (!new_name2)
				{
					// TODO - memory error via EH
				}
				else
				{
					new_name = new_name2;
					memcpy(new_name + name_len,".pl\0",4);
					s->m_f = fopen(new_name,"r");
					if (!s->m_f)
					{
						if (errno != ENOENT)
						{
							// TODO - error via EH
						}
					}
				}
			}
		}

		if (!s->m_f)
		{			
			free(s);
			s = NULL;
		}
		else
		{
			s->m_name = realpath(new_name,NULL);
			if (!s->m_name)
			{
				// TODO - memory error via EH

				fclose(s->m_f);
				free(s);
				s = NULL;
			}
		}
	}

	free(new_name);

	return (prolite_stream_t*)s;
}

static prolite_stream_t* fs_resolver_open(struct prolite_stream_resolver* r, prolite_context_t context, prolite_exception_handler_fn_t eh, const char* name, size_t name_len)
{
	fs_resolver_t* res = (fs_resolver_t*)r;
	prolite_stream_t* s = resolver_open(res,context,eh,NULL,name,name_len);

	for (size_t i = 0; !s && i < res->m_include_count; ++i)
		s = resolver_open(res,context,eh,res->m_includes[i],name,name_len);
	
	if (!s)
	{
		// TODO: existence error!
	}

	return s;
}

prolite_stream_resolver_t* fs_resolver_new(void)
{
	fs_resolver_t* r = malloc(sizeof(fs_resolver_t));
	if (r)
	{
		*r = (fs_resolver_t){
			.m_base.m_fn_open = &fs_resolver_open
		};
	}

	return (prolite_stream_resolver_t*)r;
}

void fs_resolver_destroy(prolite_stream_resolver_t* r)
{
	free(r);
}
