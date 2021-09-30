#include "context.h"

static void push_exception(context_t* context)
{
	// TODO
	assert(0);
	
    context->m_flags |= FLAG_THROW;
}

void push_out_of_memory_error(context_t* context, const term_t* t)
{
    // 't' just gives us debug info

	// TODO
	push_exception(context);
}

void push_instantiation_error(context_t* context, const term_t* t)
{
    // 't' just gives us debug info

	// TODO
	push_exception(context);
}

void push_permission_error(context_t* context, uint64_t p1, uint64_t p2, const term_t* t)
{
	// TODO
	push_exception(context);
}

void push_type_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	push_exception(context);
}

void push_domain_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	push_exception(context);
}

void push_representation_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	push_exception(context);
}

void throw_exception(context_t* context)
{
    // TODO - Exception is on the stack!
}
