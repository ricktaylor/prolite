
#include "types.h"

#include <stdlib.h>
#include <string.h>

struct builtin_string_t
{
	size_t               m_len;
	const unsigned char* m_str;
};

#undef DECLARE_BUILTIN_STRING
#define DECLARE_BUILTIN_STRING(s) { sizeof(#s),(const unsigned char*)(#s) },

const struct builtin_string_t s_builtin_strings[] =
{
#include "builtin_strings.h"
};

static int builtin_string_compare(const void* p1, const void* p2)
{
	const struct builtin_string_t* s1 = p1;
	const struct builtin_string_t* s2 = p2;

	if (s1->m_len != s2->m_len)
		return s1->m_len - s2->m_len;

	return memcmp(s1->m_str,s2->m_str,s1->m_len);
}

static int box_string_builtin(union box_t* b, const unsigned char* str, size_t len)
{
	struct builtin_string_t f, *s;
	f.m_len = len;
	f.m_str = str;

	s = bsearch(&f,s_builtin_strings,sizeof(s_builtin_strings) / sizeof(s_builtin_strings[0]),sizeof(s_builtin_strings),&builtin_string_compare);
	if (!s)
		return 0;

	b->m_uval |= (UINT64_C(0x4) << 44) | (uint32_t)(s - s_builtin_strings);
	return 1;
}

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
		b->m_uval |= (UINT64_C(0x8) << 44) | ((uint64_t)len << 40);
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
		const struct builtin_string_t* s = &s_builtin_strings[(uint32_t)b->m_uval];
		*len = s->m_len;
		return s->m_str;
	}

	*len = (size_t)((b->m_uval & (UINT64_C(0x7) << 40)) >> 40);
	return ((const unsigned char*)b) + 3;
}

const unsigned char* unbox_compound(struct context_t* context, const union box_t* b, uint64_t* arity, size_t* flen)
{
	if ((b->m_uval & BOX_TAG_COMPOUND_EMBED) == BOX_TAG_COMPOUND_EMBED)
	{
		*flen = (size_t)((b->m_uval & (UINT64_C(0x7) << 44)) >> 44);
		*arity = (b->m_uval & (UINT64_C(0xF) << 40)) >> 40;
		return ((const unsigned char*)b) + 3;
	}
	*arity = (b->m_uval & ~(UINT64_C(0xFFFF8) << 44));
	return unbox_string(context,b+1,flen);
}

uint64_t compound_arity(const union box_t* b)
{
	if ((b->m_uval & BOX_TAG_COMPOUND_EMBED) == BOX_TAG_COMPOUND_EMBED)
		return (b->m_uval & (UINT64_C(0xF) << 40)) >> 40;
	return (b->m_uval & ~(UINT64_C(0xFFFF8) << 44));
}

uint32_t embed_string_code(const union box_t* b)
{
	unsigned int len = (unsigned int)((b->m_uval & (UINT64_C(0x7) << 40)) >> 40);
	const unsigned char* c = ((const unsigned char*)b) + 3;
	unsigned int count = 0;
	uint32_t val = 0;

	if (c[0] <= 0x7f)
		return c[0];

	if (c[0] < 0xC2 || c[0] > 0xF4)
		return -1;

	if ((c[0] & 0xE0) == 0xC0)
	{
		count = 2;
		val = (c[0] & 0x1F);
	}
	else if ((c[0] & 0xF0) == 0xE0)
	{
		if (len == 1)
			return -1;

		if ((c[0] == 0xE0 && c[1] >= 0x80 && c[1] <= 0x9F) ||
			(c[0] == 0xED && c[1] >= 0xA0 && c[1] <= 0xBF))
		{
			return -1;
		}

		count = 3;
		val = (c[0] & 0x0F);
	}
	else if ((c[0] & 0xF8) == 0xF0)
	{
		if (len == 1)
			return -1;

		if ((c[0] == 0xF0 && c[1] >= 0x80 && c[1] <= 0x8F) ||
			(c[0] == 0xF4 && c[1] >= 0x90 && c[1] <= 0xBF))
		{
			return -1;
		}

		count = 4;
		val = (c[0] & 0x7);
	}
	else
		return -1;

	if (len != count)
		return -1;
	else
	{
		unsigned int i;
		for (i=1;i<count;++i)
		{
			if ((c[i] & 0xC0) != 0x80)
				return -1;

			val = (val << 6) | (c[i] & 0x3F);
		}
	}

	return val;
}
