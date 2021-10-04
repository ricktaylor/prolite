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

static void fs_stream_close(prolite_stream_t* s)
{
	fs_stream_t* stream = (fs_stream_t*)s;

	fclose(stream->m_f);
	free(stream->m_name);
	free(stream);
}

static int64_t fs_stream_read(prolite_stream_t* s, void* dest, size_t len, prolite_stream_error_t* err)
{
	FILE* f = ((fs_stream_t*)s)->m_f;

	int64_t r = fread(dest,1,len,f);
	if (r == 0)
	{
		r = -1;
		if (err)
		{
			if (feof(f))
				*err = prolite_stream_error_eof;
			else
			{
				// TODO!
				*err = prolite_stream_error_no_room;
			}
		}
	}
	
	/*if (r > 0)
	{
		fwrite(dest,1,r,stdout);
		fflush(stdout);
	}*/

	return r;
}

static prolite_stream_t* resolver_open(fs_resolver_t* r, const char* dir, const char* name, size_t name_len, prolite_stream_resolver_error_t* err)
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

static prolite_stream_t* stream_open_relative(struct fs_stream* stream, const char* name, size_t name_len, prolite_stream_resolver_error_t* err)
{
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

	prolite_stream_t* s = resolver_open(stream->m_resolver,dir,name,name_len,err);

	free(dir);

	return s;
}

static prolite_stream_t* fs_resolver_open(prolite_stream_resolver_t* r, const char* name, size_t name_len, prolite_stream_resolver_error_t* err)
{
	fs_resolver_t* res = (fs_resolver_t*)r;
	prolite_stream_resolver_error_t local_err = prolite_stream_resolver_error_none;
	prolite_stream_t* s = resolver_open(res,NULL,name,name_len,&local_err);
	
	for (size_t i = 0; !s && i < res->m_include_count; ++i)
		s = resolver_open(res,res->m_includes[i],name,name_len,&local_err);
	
	if (!s && err)
		*err = local_err;
	
	return s;
}

static prolite_stream_t* fs_resolver_open_relative(prolite_stream_resolver_t* r, prolite_stream_t* s, const char* name, size_t name_len, prolite_stream_resolver_error_t* err)
{
	fs_stream_t* stream = (fs_stream_t*)s;

	return stream_open_relative(stream,name,name_len,err);
}

prolite_stream_resolver_t* fs_resolver_new(void)
{
	fs_resolver_t* r = malloc(sizeof(fs_resolver_t));
	if (r)
	{
		*r = (fs_resolver_t){
			.m_base.m_fn_open = &fs_resolver_open,
			.m_base.m_fn_open_relative = &fs_resolver_open_relative,
		};
	}

	return (prolite_stream_resolver_t*)r;
}

void fs_resolver_destroy(prolite_stream_resolver_t* r)
{
	free(r);
}
