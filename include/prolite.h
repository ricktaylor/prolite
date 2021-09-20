#ifndef PROLITE_H_INCLUDED
#define PROLITE_H_INCLUDED

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct prolite_allocator
{
    void*  m_param;
    void* (*m_fn_malloc)(void* param, size_t bytes);
	void  (*m_fn_free)(void* param, void* ptr);
} prolite_allocator_t;

#ifdef __cplusplus
}
#endif

#endif // PROLITE_H_INCLUDED
