
#include "box_types.h"
#include "context.h"

#include <stdlib.h>
#include <string.h>

struct builtin_string_t
{
	size_t               m_len;
	const unsigned char* m_str;
};

#define BUILTIN_STRING(s) { sizeof(s),(const unsigned char*)(s) }

/* These MUST be sorted by length then text */
struct builtin_string_t s_builtin_strings[] =
{
	BUILTIN_STRING("missing"),
	BUILTIN_STRING("max_arity"),
	BUILTIN_STRING("underflow"),
	BUILTIN_STRING("max_integer"),
	BUILTIN_STRING("min_integer"),
	BUILTIN_STRING("invalid_utf8"),
	BUILTIN_STRING("syntax_error"),
	BUILTIN_STRING("system_error"),
	BUILTIN_STRING("out_of_memory"),
	BUILTIN_STRING("float_overflow"),
	BUILTIN_STRING("invalid_escape"),
	BUILTIN_STRING("evaluation_error"),
	BUILTIN_STRING("invalid_argument"),
	BUILTIN_STRING("unexpected_token"),
	BUILTIN_STRING("invalid_character"),
	BUILTIN_STRING("past_end_of_stream"),
	BUILTIN_STRING("representation_error"),
};

static int box_string_ptr(struct context_t* context, union box_t* b, const unsigned char* str, size_t len)
{
	struct string_ptr_t* s;
	for (s = context->m_strings; s; s = s->m_prev)
	{
		if (s->m_len == len && memcmp(s->m_str,str,len) == 0)
		{
			box_pointer(b,s);
			return 1;
		}
	}

	if (box_string_builtin(b,str,len))
		return 1;

	s = stack_malloc(&context->m_exec_stack,sizeof(struct string_ptr_t) + len);
	if (!s)
		return 0;

	s->m_prev = context->m_strings;
	s->m_len = len;
	memcpy(s->m_str,str,len);
	context->m_strings = s;

	return 1;
}

static int box_string_embed(union box_t* b, const unsigned char* str, size_t len)
{
	switch (len)
	{
	case 5:
		b->m_uval |= (uint64_t)(*str++) << 32;
	case 4:
		b->m_uval |= (*str++) << 24;
	case 3:
		b->m_uval |= (*str++) << 16;
	case 2:
		b->m_uval |= (*str++) << 8;
	case 1:
		b->m_uval |= *str;
	default:
		b->m_uval |= ((UINT64_C(0x8) << 44) | ((len & UINT64_C(0xF)) << 40));
		break;
	}
	return 1;
}

int box_string(struct context_t* context, union box_t* b, const unsigned char* str, size_t len)
{
	if (len > 5)
		return box_string_ptr(context,b,str,len);

	return box_string_embed(b,str,len);
}

static int builtin_string_compare(const void* p1, const void* p2)
{
	const struct builtin_string_t* s1 = p1;
	const struct builtin_string_t* s2 = p2;

	if (s1->m_len != s2->m_len)
		return s1->m_len - s2->m_len;

	return memcmp(s1->m_str,s2->m_str,s1->m_len);
}

int box_string_builtin(union box_t* b, const unsigned char* str, size_t len)
{
	if (len <= 5)
		return box_string_embed(b,str,len);
	else
	{
		struct builtin_string_t f, *s;
		f.m_len = len;
		f.m_str = str;

		s = bsearch(&f,s_builtin_strings,sizeof(s_builtin_strings) / sizeof(s_builtin_strings[0]),sizeof(s_builtin_strings),&builtin_string_compare);
		if (!s)
			return 0;

		b->m_uval |= (UINT64_C(0x4) << 44);
		box_pointer(b,s);
	}
	return 1;
}

const unsigned char* unbox_string(struct context_t* context, const union box_t* b, size_t* len)
{
	unsigned int mask = ((b->m_uval >> 44) & 0xC);
	if (mask == 0)
	{
		struct string_ptr_t const* s = unbox_pointer(b);
		*len = s->m_len;
		return s->m_str;
	}
	else if (mask == 4)
	{
		struct builtin_string_t const* s = unbox_pointer(b);
		*len = s->m_len;
		return s->m_str;
	}

	*len = (size_t)(b->m_uval & (UINT64_C(0xF) << 40));
	return ((const unsigned char*)b) + 3;
}
