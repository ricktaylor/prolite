#include "context.h"

void push_out_of_memory_error(context_t* context, const term_t* t)
{
	// 't' just gives us debug info

	// It would be nice to be able to avoid malloc calls here

	// TODO
	assert(0);
	
	context->m_flags |= FLAG_THROW;
}

void push_instantiation_error(context_t* context, const term_t* t)
{
	// 't' just gives us debug info

	// TODO
	assert(0);
	
	context->m_flags |= FLAG_THROW;
}

void push_permission_error(context_t* context, uint64_t p1, uint64_t p2, const term_t* t)
{
	// TODO
	assert(0);
	
	context->m_flags |= FLAG_THROW;
}

void push_type_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	assert(0);
	
	context->m_flags |= FLAG_THROW;
}

void push_domain_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	assert(0);
	
	context->m_flags |= FLAG_THROW;
}

void push_representation_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	assert(0);
	
	context->m_flags |= FLAG_THROW;
}

void builtin_throw(context_t* context, const term_t* arg1) 
{
	prolite_allocator_t a = heap_allocator(&context->m_heap);
	size_t var_count = 0;
	term_t* ball = copy_term(&a,context,arg1,0,&var_count);
	if (!ball)
		push_out_of_memory_error(context,arg1);
	else if (var_count)
	{
		allocator_free(&a,ball);
		push_instantiation_error(context,arg1);
	}
	else
		context->m_exception = ball;
}

void builtin_catch(context_t* context, const term_t* arg1) 
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
		push_out_of_memory_error(context,arg1);
	}
	else
	{
		// TODO - unify_terms(context,ball,arg1);

		if (context->m_flags & FLAG_FAIL)
		{
			// Rethrow...
			context->m_flags &= ~FLAG_FAIL;
			context->m_flags |= FLAG_THROW;

			context->m_exception = exception;
		}
	}
}
