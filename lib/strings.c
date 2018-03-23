
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

	b->m_u64val = BOX_TYPE(prolite_atom) | BOX_HI48(0x4000) | BOX_U32((uint32_t)(s - s_builtin_strings));
	return 1;
}

static int box_string_ptr(struct context_t* context, union box_t* b, const unsigned char* str, size_t len)
{
	// TODO: THIS WHOLE THING LOOKS DODGY!!

	struct string_ptr_t* s;
	for (s = context->m_strings; s; s = s->m_prev)
	{
		if (s->m_len == len && memcmp(s->m_str,str,len) == 0)
		{
			b->m_u64val = BOX_TYPE(prolite_atom);
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
	uint64_t c[5] = {0};
	if (len > 0)
		c[4] = *str++;

	if (len > 1)
		c[3] = *str++;

	if (len > 2)
		c[2] = *str++;

	if (len > 3)
		c[1] = *str++;

	if (len > 4)
		c[0] = *str++;

	b->m_u64val = BOX_TYPE_EMBED(prolite_atom,0,len,c[4],c[3],c[2],c[1],c[0]);

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
	unsigned int hi48 = UNBOX_HI48(b->m_u64val);
	unsigned int mask = hi48 & 0xC000;
	if (mask == 0)
	{
		struct string_ptr_t const* s = unbox_pointer(b);
		*len = s->m_len;
		return s->m_str;
	}
	else if (mask == 0x4000)
	{
		const struct builtin_string_t* s = &s_builtin_strings[UNBOX_U32(b->m_u64val)];
		*len = s->m_len;
		return s->m_str;
	}

	*len = (hi48 & 0x0700) >> 8;

	// TODO: NEED TO UNPACK!!
	return NULL;
}

const unsigned char* unbox_compound(struct context_t* context, const union box_t* b, uint64_t* arity, size_t* flen)
{
	uint64_t all48 = UNBOX_MANT_48(b->m_u64val);
	unsigned int hi48 = (all48 >> 32);
	if (hi48 & 0x8000)
	{
		*flen = (hi48 & 0x700) >> 8;
		*arity = (hi48 & 0x7800) >> 11;

		// TODO: NEED TO UNPACK
		return NULL;//((const unsigned char*)b) + 3;
	}
	*arity = all48 & MAX_ARITY;
	return unbox_string(context,b+1,flen);
}

uint64_t compound_arity(const union box_t* b)
{
	uint64_t all48 = UNBOX_MANT_48(b->m_u64val);
	unsigned int hi48 = (all48 >> 32);
	if (hi48 & 0x8000)
		return (hi48 & 0x7800) >> 11;
	return all48 & MAX_ARITY;
}

uint32_t embed_string_code(const union box_t* b)
{
	struct pun
	{
		uint32_t u32val;
		int8_t   c[4];
	} p;
	unsigned int len = (UNBOX_HI48(b->m_u64val) & 0x0700) >> 8;
	unsigned int count = 0;
	uint32_t val = 0;

	p.u32val = UNBOX_U32(b->m_u64val);

	if (p.c[0] <= 0x7f)
		return p.c[0];

	if (p.c[0] < 0xC2 || p.c[0] > 0xF4)
		return -1;

	if ((p.c[0] & 0xE0) == 0xC0)
	{
		count = 2;
		val = (p.c[0] & 0x1F);
	}
	else if ((p.c[0] & 0xF0) == 0xE0)
	{
		if (len == 1)
			return -1;

		if ((p.c[0] == 0xE0 && p.c[1] >= 0x80 && p.c[1] <= 0x9F) ||
			(p.c[0] == 0xED && p.c[1] >= 0xA0 && p.c[1] <= 0xBF))
		{
			return -1;
		}

		count = 3;
		val = (p.c[0] & 0x0F);
	}
	else if ((p.c[0] & 0xF8) == 0xF0)
	{
		if (len == 1)
			return -1;

		if ((p.c[0] == 0xF0 && p.c[1] >= 0x80 && p.c[1] <= 0x8F) ||
			(p.c[0] == 0xF4 && p.c[1] >= 0x90 && p.c[1] <= 0xBF))
		{
			return -1;
		}

		count = 4;
		val = (p.c[0] & 0x7);
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
			if ((p.c[i] & 0xC0) != 0x80)
				return -1;

			val = (val << 6) | (p.c[i] & 0x3F);
		}
	}

	return val;
}
