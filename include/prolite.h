#ifndef PROLITE_H_INCLUDED
#define PROLITE_H_INCLUDED

#include <stddef.h>
#include <stdint.h>

#if !defined(PROLITE_EXPORT)
#if defined(_WIN32)
#define PROLITE_EXPORT __declspec(dllimport)
#else
#define PROLITE_EXPORT
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct prolite_context
{
    void* m_user_data;
}* prolite_context_t;

typedef struct prolite_allocator
{
    void*  m_user_data;
    void* (*m_fn_malloc)(void* user_data, size_t bytes);
	void  (*m_fn_free)(void* user_data, void* ptr);
} prolite_allocator_t;

typedef void (*prolite_exception_handler_fn_t)(prolite_context_t context);

typedef struct prolite_stream
{
    void (*m_fn_close)(struct prolite_stream* s);
	int64_t (*m_fn_read)(struct prolite_stream* s, void* dest, size_t len);
    int (*m_fn_write)(struct prolite_stream* s, const void* src, size_t len);

} prolite_stream_t;

typedef struct prolite_stream_resolver
{
	prolite_stream_t* (*m_fn_open)(struct prolite_stream_resolver* r, prolite_context_t context, prolite_exception_handler_fn_t eh, const char* name, size_t name_len);
    struct prolite_stream* (*m_fn_open_relative)(struct prolite_stream_resolver* r, struct prolite_stream* s, prolite_context_t context, prolite_exception_handler_fn_t eh, const char* name, size_t name_len);
	
} prolite_stream_resolver_t;

typedef struct prolite_environment
{
    prolite_allocator_t*           m_allocator;
    size_t                         m_stack_size;
    prolite_stream_resolver_t*     m_resolver;
    prolite_exception_handler_fn_t m_handler;

} prolite_environment_t;

PROLITE_EXPORT prolite_context_t prolite_context_new(/* optional */ void* user_data, /* optional */ const prolite_environment_t* env);
PROLITE_EXPORT prolite_context_t prolite_context_load(/* optional */ void* user_data, /* optional */ const prolite_environment_t* env, const char* source);
PROLITE_EXPORT void prolite_context_destroy(prolite_context_t context);

#ifdef __cplusplus
}
#endif

#endif // PROLITE_H_INCLUDED
