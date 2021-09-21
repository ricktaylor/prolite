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
    int opaque;
}* prolite_context_t;

typedef struct prolite_allocator
{
    void*  m_param;
    void* (*m_fn_malloc)(void* param, size_t bytes);
	void  (*m_fn_free)(void* param, void* ptr);
} prolite_allocator_t;

typedef void (*prolite_exception_handler_fn_t)(prolite_context_t context);

typedef struct prolite_stream
{
	void (*m_fn_close)(struct prolite_stream* s);
	int64_t (*m_fn_read)(struct prolite_stream* s, void* dest, size_t len);
} prolite_stream_t;

typedef struct prolite_stream_resolver
{
	prolite_stream_t* (*m_fn_open)(struct prolite_stream_resolver* r, prolite_context_t context, prolite_exception_handler_fn_t* eh, const char* name);
} prolite_stream_resolver_t;

typedef struct prolite_environment
{
    prolite_allocator_t*            m_allocator;
    size_t                          m_stack_size;
    prolite_stream_resolver_t*      m_resolver;
    prolite_exception_handler_fn_t* m_handler;

} prolite_environment_t;

PROLITE_EXPORT prolite_context_t prolite_context_new(/* optional */ prolite_environment_t* env);
PROLITE_EXPORT prolite_context_t prolite_context_load(/* optional */ prolite_environment_t* env, const char* source);
PROLITE_EXPORT void prolite_context_destroy(prolite_context_t context);

#ifdef __cplusplus
}
#endif

#endif // PROLITE_H_INCLUDED
