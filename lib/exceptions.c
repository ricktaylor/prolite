#include "context.h"
#include "write_term.h"

#include <string.h>

void push_out_of_memory_error(context_t* context, const term_t* t)
{
	// 't' just gives us debug info

	// It would be nice to be able to avoid malloc calls here

	// TODO
	assert(0);
	
	context->m_flags = FLAG_THROW;
}

void push_instantiation_error(context_t* context, const term_t* t)
{
	// 't' just gives us debug info

	// TODO
	assert(0);
	
	context->m_flags = FLAG_THROW;
}

void push_permission_error(context_t* context, uint64_t p1, uint64_t p2, const term_t* t)
{
	// TODO
	assert(0);
	
	context->m_flags = FLAG_THROW;
}

void push_type_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	assert(0);
	
	context->m_flags = FLAG_THROW;
}

void push_domain_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	assert(0);
	
	context->m_flags = FLAG_THROW;
}

void push_representation_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	assert(0);
	
	context->m_flags = FLAG_THROW;
}

void builtin_throw(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[]) 
{
	prolite_allocator_t a = heap_allocator(&context->m_heap);
	size_t var_count = 0;
	term_t* ball = copy_term(&a,context,argv[0],0,&var_count);
	if (!ball)
		return push_out_of_memory_error(context,argv[0]);
	
	if (var_count)
	{
		allocator_free(&a,ball);
		return push_instantiation_error(context,argv[0]);
	}
	
	// Don't set FLAG_THROW - it gets set in the gosub
	context->m_exception = ball;
	(*gosub)(context);
}

void builtin_catch(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[]) 
{
	assert(context->m_exception);

	// Clear throw flag
	context->m_flags &= ~FLAG_THROW;

	term_t* exception = context->m_exception;
	context->m_exception = NULL;

	term_t* ball = push_term(context,exception,0,NULL);
	if (!ball)
	{
		prolite_allocator_t a = heap_allocator(&context->m_heap);
		allocator_free(&a,exception);
		return push_out_of_memory_error(context,argv[0]);
	}
	
	// if (unify_terms(context,ball,arg1))
	// (*gosub)(context);
	// else
	{
		// Rethrow...
		context->m_flags = FLAG_THROW;

		context->m_exception = exception;
	}
}

struct stack_output
{
	prolite_stream_t m_base;
	heap_t*          m_heap;
	char*            m_ptr;
	size_t           m_len;
	size_t           m_alloc;
};

static int stack_stream_write(struct prolite_stream* s, const void* src, size_t len, prolite_stream_error_t* err)
{
	struct stack_output* stream = (struct stack_output*)s;

	if (len)
	{
		if (stream->m_len + len >= stream->m_alloc)
		{
			size_t new_size = (stream->m_alloc ? stream->m_alloc * 2 : 32);
			void* p = heap_realloc(stream->m_heap,stream->m_ptr,stream->m_alloc,new_size);
			if (!p)
			{
				if (err)
					*err = prolite_stream_error_no_room;
				return 0;
			}
			
			stream->m_alloc = new_size;
			stream->m_ptr = p;
		}

		memcpy(stream->m_ptr + stream->m_len,src,len);
		stream->m_len += len;
	}
	return 1;
}

void unhandled_exception(context_t* context, const operator_table_t* ops)
{
	struct stack_output s = 
	{
		.m_base.m_fn_write = &stack_stream_write,
		.m_heap = &context->m_heap
	};

	const term_t* exception = context->m_stack;
	term_t* sp = (term_t*)get_next_arg(exception);
			
	do
	{
		context->m_flags &= ~FLAG_THROW;
		write_term(context,&s.m_base,exception,NULL,ops);

		if (context->m_flags & FLAG_THROW)
			exception = context->m_stack;
	}
	while (context->m_flags & FLAG_THROW);

	(*context->m_eh)(s.m_ptr,s.m_len);

	if (s.m_alloc)
		heap_free(s.m_heap,s.m_ptr,s.m_alloc);
		
	context->m_stack = sp;
}