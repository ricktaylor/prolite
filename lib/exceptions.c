#include "context.h"
#include "write_term.h"

#include <string.h>
#include <math.h>

static const term_t s_oom[] = {
	{ .m_u64val = PACK_COMPOUND_EMBED_5(2,'e','r','r','o','r') },
	{ .m_u64val = PACK_COMPOUND_BUILTIN(resource_error,1) },
	{ .m_u64val = PACK_ATOM_EMBED_4('h','e','a','p') },
	{ .m_u64val = PACK_ATOM_EMBED_2('[',']') }
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
		allocator_free(out->m_a,out->m_buf);

	return r;
}

void throw_out_of_memory_error(context_t* context, const term_t* t)
{
	// 't' just gives us debug info

	assert(!context->m_exception);

	if (t)
	{
		emit_buffer_t out = { .m_a = context->m_trail.m_allocator };
		term_t* e = emit_buffer_append(&out,3);
		if (e)
		{
			e[0].m_u64val = s_oom[0].m_u64val;
			e[1].m_u64val = s_oom[1].m_u64val;
			e[2].m_u64val = s_oom[2].m_u64val;

			if (emit_error_debug_info(&out,t))
				context->m_exception = out.m_buf;
		}

		if (!context->m_exception)
			allocator_free(context->m_trail.m_allocator,out.m_buf);
	}

	if (!context->m_exception)
		context->m_exception = (term_t*)s_oom;

	context->m_flags |= FLAG_THROW;
}

void throw_instantiation_error(context_t* context, const term_t* t)
{
	assert(t);
	assert(unpack_term_type(t) == prolite_var);
	assert(!context->m_exception);

	emit_buffer_t out = { .m_a = context->m_trail.m_allocator };
	term_t* e = emit_buffer_append(&out,3);
	if (!e)
		return throw_out_of_memory_error(context,t);

	e[0].m_u64val = PACK_COMPOUND_EMBED_5(2,'e','r','r','o','r');
	e[1].m_u64val = PACK_COMPOUND_BUILTIN(instantiation_error,1);
	e[2].m_u64val = t->m_u64val;

	if (!emit_error_debug_info(&out,t))
	{
		allocator_free(context->m_trail.m_allocator,out.m_buf);
		return throw_out_of_memory_error(context,t);
	}

	context->m_exception = out.m_buf;
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

void throw_evaluable_error(context_t* context, const term_t* t)
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

PROLITE_EXPORT void prolite_builtin_throw(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(argc == 1);
	assert(!context->m_exception);

	size_t var_count = 0;
	context->m_exception = copy_term(context,context->m_trail.m_allocator,argv[0],0,1,&var_count);
	if (!context->m_exception)
		return throw_out_of_memory_error(context,argv[0]);

	if (var_count)
	{
		context->m_exception = allocator_free(context->m_trail.m_allocator,context->m_exception);
		return throw_instantiation_error(context,argv[0]);
	}

	context->m_flags |= FLAG_THROW;
}

PROLITE_EXPORT void prolite_builtin_catch(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(argc == 1);

	if (context->m_exception)
	{
		size_t trail_start = heap_top(&context->m_trail);
		const term_t* ball = copy_term(context,&bump_allocator(&context->m_trail),context->m_exception,0,0,NULL);
		if (!ball)
		{
			if (context->m_exception != s_oom)
				allocator_free(context->m_trail.m_allocator,context->m_exception);
			context->m_exception = (term_t*)s_oom;
		}
		else
		{
			substitutions_t* prev_substs = context->m_substs;
			if (context->m_substs)
			{
				substitutions_t* new_substs = alloca(sizeof(substitutions_t) + context->m_substs->m_count * sizeof(const term_t*));
				memcpy(new_substs,context->m_substs,sizeof(substitutions_t) + context->m_substs->m_count * sizeof(const term_t*));
				context->m_substs = new_substs;
			}

			if (unify_terms(context,ball,argv[0],0))
			{
				context->m_flags &= ~FLAG_THROW;

				if (context->m_exception != s_oom)
					allocator_free(context->m_trail.m_allocator,context->m_exception);
				context->m_exception = NULL;

				builtin_gosub(context,gosub);
			}

			context->m_substs = prev_substs;
		}

		heap_reset(&context->m_trail,trail_start);
	}
}

PROLITE_EXPORT void prolite_builtin_halt(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(argc == 1);
	assert(!context->m_exception);

	switch (unpack_term_type(argv[0]))
	{
	case prolite_var:
		return throw_instantiation_error(context,argv[0]);

	case prolite_number:
		if (nearbyint(argv[0]->m_dval) != argv[0]->m_dval)
			return throw_type_error(context,PACK_ATOM_BUILTIN(integer),argv[0]);
		break;

	default:
		return throw_type_error(context,PACK_ATOM_BUILTIN(integer),argv[0]);
	}

	context->m_exception = allocator_malloc(context->m_trail.m_allocator,sizeof(term_t));
	if (!context->m_exception)
		return throw_out_of_memory_error(context,argv[0]);

	context->m_exception->m_dval = argv[0]->m_dval;

	context->m_flags |= FLAG_HALT;
}

struct trail_output
{
	prolite_stream_t m_base;
	heap_t*          m_trail;
	char*            m_ptr;
	size_t           m_len;
	size_t           m_alloc;
};

static int trail_stream_write(struct prolite_stream* s, const void* src, size_t len, prolite_stream_error_t* err)
{
	struct trail_output* stream = (struct trail_output*)s;

	if (len)
	{
		if (stream->m_len + len >= stream->m_alloc)
		{
			size_t new_size = (stream->m_alloc ? stream->m_alloc * 2 : 32);
			void* p = bump_allocator_realloc(stream->m_trail,stream->m_ptr,new_size);
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
	return (context->m_exception &&
			context->m_exception[0].m_u64val == s_oom[0].m_u64val &&
			context->m_exception[1].m_u64val == s_oom[1].m_u64val &&
			context->m_exception[2].m_u64val == s_oom[2].m_u64val);
}

void unhandled_exception(context_t* context, const operator_table_t* ops)
{
	assert(context->m_exception);

	struct trail_output s = {
		.m_base.m_fn_write = &trail_stream_write,
		.m_trail = &context->m_trail
	};

	size_t trail_start = heap_top(&context->m_trail);

	if (write_term(context,&s.m_base,context->m_exception,NULL,ops) == prolite_stream_error_none)
		(*context->m_eh)(s.m_ptr,s.m_len);

	heap_reset(&context->m_trail,trail_start);

	if (context->m_exception != s_oom)
		allocator_free(context->m_trail.m_allocator,context->m_exception);

	context->m_exception = NULL;
	context->m_flags &= ~FLAG_THROW;
}
