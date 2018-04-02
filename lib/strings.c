
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

static void box_pointer(union box_t* b, void* ptr)
{
#if UINTPTR_MAX == UINT32_MAX
	b->m_u64val |= BOX_LOW32((uintptr_t)ptr);
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	b->m_u64val |= BOX_MANT_48(((uintptr_t)ptr >> 3) & UINT64_C(0x1FFFFFFFFFFF));
#else
#error No idea what to do with addresses on your architecture!
#endif
}

static void* unbox_pointer(const union box_t* b)
{
#if UINTPTR_MAX == UINT32_MAX
	return (void*)(uintptr_t)UNOX_U32(b->m_u64val);
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	/* Sign extend to make an x86_64 canonical address */
	struct pun { uint64_t u45 : 45; } p;
	return (void*)((uintptr_t)(p.u45 = UNBOX_MANT_48(b->m_u64val) << 3));
#else
#error No idea what to do with addresses on your architecture!
#endif
}

static int builtin_string_compare(const void* p1, const void* p2)
{
	const struct builtin_string_t* s1 = p1;
	const struct builtin_string_t* s2 = p2;

	if (s1->m_len != s2->m_len)
		return s1->m_len - s2->m_len;

	return memcmp(s1->m_str,s2->m_str,s1->m_len);
}

int box_string(enum tag_type_t type, struct context_t* context, union box_t* b, const unsigned char* str, size_t len)
{
	if (len <= 5)
	{
		uint8_t c[5] = {0};
		switch (len)
		{
		case 5:
			c[4] = str[4];
		case 4:
			c[3] = str[3];
		case 3:
			c[2] = str[2];
		case 2:
			c[1] = str[1];
		case 1:
			c[0] = str[0];
		default:
			break;
		}
		memcpy(c,str,len);
		b->m_u64val = BOX_TYPE_EMBED(type,0,len,c[0],c[1],c[2],c[3],c[4]);
	}
	else
	{
		struct builtin_string_t f, *r;
		f.m_len = len;
		f.m_str = str;

		r = bsearch(&f,s_builtin_strings,sizeof(s_builtin_strings) / sizeof(s_builtin_strings[0]),sizeof(s_builtin_strings),&builtin_string_compare);
		if (r)
			b->m_u64val = BOX_TYPE(type) | BOX_HI16(0x4000) | BOX_LOW32((uint32_t)(r - s_builtin_strings));
		else
		{
			struct string_ptr_t* s;
			for (s = context->m_strings; s; s = s->m_prev)
			{
				if (s->m_len == len && memcmp(s->m_str,str,len) == 0)
					break;
			}

			if (!s)
			{
				s = stack_malloc(&context->m_exec_stack,sizeof(struct string_ptr_t) + len);
				if (!s)
					return 0;

				s->m_prev = context->m_strings;
				s->m_len = len;
				memcpy(s->m_str,str,len);
				context->m_strings = s;
			}

			// TODO: Do something smarter here by using 46-bit stack offsets
			b->m_u64val = BOX_TYPE(type);
			box_pointer(b,s);
		}
	}

	return 1;
}

const unsigned char* unbox_string(struct context_t* context, const union box_t* b, size_t* len)
{
	unsigned int hi48 = UNBOX_HI16(b->m_u64val);
	unsigned int mask = hi48 & 0xC000;
	if (mask == 0)
	{
		struct string_ptr_t const* s = unbox_pointer(b);
		*len = s->m_len;
		return s->m_str;
	}
	else if (mask == 0x4000)
	{
		const struct builtin_string_t* s = &s_builtin_strings[UNBOX_LOW32(b->m_u64val)];
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
