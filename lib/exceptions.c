#include "context.h"
#include "write_term.h"

#include <string.h>
#include <math.h>

static const exception_t s_oom = {
	.m_term = (term_t[]){
		{ .m_u64val = PACK_COMPOUND_EMBED_5(2,'e','r','r','o','r') },
		{ .m_u64val = PACK_COMPOUND_BUILTIN(resource_error,1) },
		{ .m_u64val = PACK_ATOM_EMBED_4('h','e','a','p') },
		{ .m_u64val = PACK_ATOM_EMBED_2('[',']') }
	}
};

static term_t* emit_error_debug_info(emit_buffer_t* out, const term_t* t)
{
	const debug_info_t* di = NULL;
	if (t)
		di = unpack_debug_info(t);

	term_t* r = NULL;
	if (di)
	{
		// TODO: Line info
		r = emit_buffer_append(out,1);
		if (r)
			r->m_u64val = PACK_ATOM_EMBED_4('t','o','d','o');
	}
	else
	{
		r = emit_buffer_append(out,1);
		if (r)
			r->m_u64val = PACK_ATOM_EMBED_2('[',']');
	}

	if (!r)
		allocator_free(out->m_allocator,out->m_buf);

	return r;
}

static exception_t* exception_create(context_t* context)
{
	heap_t eh = heap_clone(&context->m_heap);
	exception_t* e = heap_malloc(&eh,sizeof(exception_t));
	if (e)
		*e = (exception_t){ .m_heap = eh };

	return e;
}

static exception_t* exception_destroy(context_t* context, exception_t* e)
{
	if (e && e != &s_oom)
	{
		heap_reset(&e->m_heap,0);
		heap_merge(&context->m_heap,&e->m_heap);
	}

	return NULL;
}

void throw_out_of_memory_error(context_t* context, const term_t* t)
{
	// 't' just gives us debug info

	assert(!context->m_exception);

	if (t)
	{
		const debug_info_t* di = unpack_debug_info(t);
		if (di)
		{
			exception_t* x = exception_create(context);
			if (x)
			{
				emit_buffer_t out = { .m_allocator = &bump_allocator(&x->m_heap) };
				term_t* e = emit_buffer_append(&out,3);
				if (e)
				{
					e[0].m_u64val = s_oom.m_term[0].m_u64val;
					e[1].m_u64val = s_oom.m_term[1].m_u64val;
					e[2].m_u64val = s_oom.m_term[2].m_u64val;

					if (emit_error_debug_info(&out,t))
					{
						x->m_term = out.m_buf;
						context->m_exception = x;
					}
				}

				if (!context->m_exception)
					exception_destroy(context,x);
			}
		}
	}

	if (!context->m_exception)
		context->m_exception = (exception_t*)&s_oom;

	context->m_flags |= FLAG_THROW;
}

void throw_instantiation_error(context_t* context, const term_t* t)
{
	assert(t);
	assert(unpack_term_type(t) == prolite_var);
	assert(!context->m_exception);

	context->m_exception = exception_create(context);
	if (!context->m_exception)
		return throw_out_of_memory_error(context,t);

	emit_buffer_t out = { .m_allocator = &bump_allocator(&context->m_exception->m_heap) };
	term_t* e = emit_buffer_append(&out,3);
	if (!e)
	{
		context->m_exception = exception_destroy(context,context->m_exception);
		return throw_out_of_memory_error(context,t);
	}

	e[0].m_u64val = PACK_COMPOUND_EMBED_5(2,'e','r','r','o','r');
	e[1].m_u64val = PACK_COMPOUND_BUILTIN(instantiation_error,1);
	e[2].m_u64val = t->m_u64val;

	if (!emit_error_debug_info(&out,t))
	{
		context->m_exception = exception_destroy(context,context->m_exception);
		return throw_out_of_memory_error(context,t);
	}

	context->m_exception->m_term = out.m_buf;
	context->m_flags |= FLAG_THROW;
}

void throw_permission_error(context_t* context, uint64_t p1, uint64_t p2, const term_t* t)
{
	// TODO
	assert(0);
}

void throw_type_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	assert(0);
}

void throw_domain_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	assert(0);
}

void throw_representation_error(context_t* context, uint64_t p1, const term_t* t)
{
	// TODO
	assert(0);
}

PROLITE_EXPORT void prolite_builtin_throw_evaluable(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(argc == 1);
}

PROLITE_EXPORT void prolite_builtin_throw_zero_div(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(argc == 1);
}

PROLITE_EXPORT void prolite_builtin_throw_underflow(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(argc == 1);
}

PROLITE_EXPORT void prolite_builtin_throw_integer_overflow(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(argc == 1);
}

PROLITE_EXPORT void prolite_builtin_throw_float_overflow(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(argc == 1);
}

PROLITE_EXPORT void prolite_builtin_throw_callable(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(argc == 1);
}

void throw_term(context_t* context, const term_t* ball)
{
	assert(!context->m_exception);

	context->m_exception = exception_create(context);
	if (!context->m_exception)
		return throw_out_of_memory_error(context,ball);

	size_t var_count = 0;
	context->m_exception->m_term = copy_term(context,&bump_allocator(&context->m_exception->m_heap),&bump_allocator(&context->m_heap),ball,0,1,&var_count);
	if (!context->m_exception->m_term)
	{
		context->m_exception = exception_destroy(context,context->m_exception);
		return throw_out_of_memory_error(context,ball);
	}

	if (var_count)
	{
		context->m_exception = exception_destroy(context,context->m_exception);
		return throw_instantiation_error(context,ball);
	}

	context->m_flags |= FLAG_THROW;
}

PROLITE_EXPORT void prolite_builtin_throw(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(argc == 1);

	throw_term(context,argv[0]);
}

PROLITE_EXPORT void prolite_builtin_catch(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(argc == 1);

	if (context->m_flags & FLAG_THROW)
	{
		assert(context->m_exception);

		size_t heap_start = heap_top(&context->m_heap);
		const term_t* ball = copy_term(context,&bump_allocator(&context->m_heap),&bump_allocator(&context->m_exception->m_heap),context->m_exception->m_term,0,0,NULL);
		if (!ball)
		{
			context->m_exception = exception_destroy(context,context->m_exception);
			throw_out_of_memory_error(context,argv[0]);
		}
		else
		{
			exception_t* prev_exception = context->m_exception;
			context->m_exception = NULL;
			context->m_flags &= ~FLAG_THROW;

			substitutions_t* prev_substs = context->m_substs;
			if (context->m_substs)
			{
				substitutions_t* new_substs = allocator_malloc(&bump_allocator(&context->m_heap),sizeof(substitutions_t) + context->m_substs->m_count * sizeof(const term_t*));
				if (!new_substs)
					throw_out_of_memory_error(context,argv[0]);
				else
				{
					memcpy(new_substs,context->m_substs,sizeof(substitutions_t) + context->m_substs->m_count * sizeof(const term_t*));
					context->m_substs = new_substs;
				}
			}

			if (unify_terms(context,ball,argv[0],0))
			{
				exception_destroy(context,prev_exception);

				builtin_gosub(context,gosub);
			}
			else
			{
				if (context->m_flags & FLAG_THROW)
					context->m_exception = prev_exception;
				else
					exception_destroy(context,prev_exception);

				context->m_flags |= FLAG_THROW;
			}

			context->m_substs = prev_substs;
		}

		heap_reset(&context->m_heap,heap_start);
	}
}

PROLITE_EXPORT void prolite_builtin_halt(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(!context->m_exception);

	double d = 0.0;
	if (argc == 1)
	{
		const term_t* t = deref_local_var(context,argv[0]);
		switch (unpack_term_type(t))
		{
		case prolite_var:
			return throw_instantiation_error(context,t);

		case prolite_number:
			if (nearbyint(t->m_dval) != t->m_dval)
				return throw_type_error(context,PACK_ATOM_BUILTIN(integer),t);
			break;

		default:
			return throw_type_error(context,PACK_ATOM_BUILTIN(integer),t);
		}

		d = t->m_dval;
	}

	context->m_exception = exception_create(context);
	if (!context->m_exception)
		return throw_out_of_memory_error(context,argv[0]);

	context->m_exception->m_term = heap_malloc(&context->m_exception->m_heap,sizeof(term_t));
	if (!context->m_exception->m_term)
	{
		context->m_exception = exception_destroy(context,context->m_exception);
		return throw_out_of_memory_error(context,argv[0]);
	}

	context->m_exception->m_term->m_dval = d;

	context->m_flags = FLAG_HALT;
}

struct heap_output
{
	prolite_stream_t     m_base;
	prolite_allocator_t* m_allocator;
	char*                m_ptr;
	size_t               m_len;
	size_t               m_alloc;
};

static int heap_stream_write(struct prolite_stream* s, const void* src, size_t len, prolite_stream_error_t* err)
{
	struct heap_output* stream = (struct heap_output*)s;

	if (len)
	{
		if (stream->m_len + len >= stream->m_alloc)
		{
			size_t new_size = (stream->m_alloc ? stream->m_alloc * 2 : 32);
			while (new_size < stream->m_len + len)
				new_size *= 2;

			void* p = allocator_realloc(stream->m_allocator,stream->m_ptr,new_size);
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

int is_out_of_memory_exception(context_t* context)
{
	return (context->m_exception == &s_oom);
}

void unhandled_exception(context_t* context, prolite_allocator_t* a, const operator_table_t* ops)
{
	assert(context->m_exception);

	struct heap_output s = {
		.m_base.m_fn_write = &heap_stream_write,
		.m_allocator = a
	};

	if (context->m_flags == FLAG_HALT)
		heap_stream_write(&s.m_base,"Goal halted with code ",22,NULL);
	else
		heap_stream_write(&s.m_base,"Unhandled exception: ",21,NULL);

	if (write_term(context,&s.m_base,context->m_exception->m_term,&(write_options_t){ .quoted = 1 },ops) == prolite_stream_error_none)
		(*context->m_eh)(s.m_ptr,s.m_len);

	allocator_free(a,s.m_ptr);

	context->m_exception = exception_destroy(context,context->m_exception);
	context->m_flags = 0;
}
