
#include "types.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

struct builtin_string_t
{
	size_t               m_len;
	const unsigned char* m_str;
};

#undef DECLARE_BUILTIN_STRING
#define DECLARE_BUILTIN_STRING(s) { sizeof(#s)-1,(const unsigned char*)(#s) },

const struct builtin_string_t s_builtin_strings[] =
{
#include "builtin_strings.h"
};

uint64_t box_pointer(void* ptr)
{
#if UINTPTR_MAX == UINT32_MAX
	return BOX_LOW32((uintptr_t)ptr);
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	return BOX_MANT_48(((uintptr_t)ptr >> 3) & UINT64_C(0x1FFFFFFFFFFF));
#else
#error No idea what to do with addresses on your architecture!
#endif
}

static void* unbox_pointer(const union box_t* b)
{
#if UINTPTR_MAX == UINT32_MAX
	return (void*)(uintptr_t)UNBOX_LOW32(b->m_u64val);
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

struct string_ptr_t* box_stack_string(struct stack_t** stack, struct string_ptr_t** strings, const unsigned char* str, size_t len)
{
	struct string_ptr_t* s;
	for (s = *strings; s; s = s->m_prev)
	{
		if (s->m_len == len && memcmp(s->m_str,str,len) == 0)
			break;
	}

	if (!s)
	{
		s = stack_malloc(stack,sizeof(struct string_ptr_t) + len);
		if (s)
		{
			s->m_prev = *strings;
			s->m_len = len;
			memcpy(s->m_str,str,len);
			*strings = s;
		}
	}

	return s;
}

uint32_t is_builtin_string(const unsigned char* str, size_t len)
{
	uint32_t ret = -1;
	struct builtin_string_t f, *r;
	f.m_len = len;
	f.m_str = str;

	r = bsearch(&f,s_builtin_strings,sizeof(s_builtin_strings) / sizeof(s_builtin_strings[0]),sizeof(s_builtin_strings[0]),&builtin_string_compare);
	if (r)
		ret = (uint32_t)(r - s_builtin_strings);

	return ret;
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
	assert(0);
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
		assert(0);
		return NULL;//((const unsigned char*)b) + 3;
	}
	*arity = all48 & MAX_ARITY;
	return unbox_string(context,b+1,flen);
}
