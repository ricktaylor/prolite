
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

void* unbox_pointer(uint64_t v)
{
#if UINTPTR_MAX == UINT32_MAX
	return (void*)(uintptr_t)UNBOX_LOW32(v);
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	/* Sign extend to make an x86_64 canonical address */
	struct pun { uint64_t u45 : 45; } p;
	return (void*)((uintptr_t)(p.u45 = UNBOX_MANT_48(v) << 3));
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
		struct string_ptr_t const* s = unbox_pointer(b->m_u64val);
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

/*static inline int copy_term_part(struct stack_t** stack, union box_t const** v, union box_t** term, size_t* term_size)
{
	*term = stack_realloc(stack,*term,*term_size * sizeof(union box_t),((*term_size)+1) * sizeof(union box_t));
	if (!*term)
		return -1;

	(*term)[(*term_size)++] = *((*v)++);
	return 0;
}

int copy_term_append(struct substs_t* substs, struct stack_t** stack, struct string_ptr_t** strings, union box_t const** v, union box_t** new_term, size_t* term_size)
{
	int r = 0;
	enum tag_type_t type = UNBOX_TYPE((*v)->m_u64val);
	switch (type)
	{
	case prolite_compound:
		{
			uint64_t arity = UNBOX_MANT_48((*v)->m_u64val);
			unsigned int hi16 = (arity >> 32);
			if (hi16 & 0x8000)
				arity = (hi16 & (MAX_ARITY_EMBED << 11)) >> 11;
			else if ((hi16 & 0xC000) == 0x4000)
				arity = (hi16 & MAX_ARITY_BUILTIN);
			else
			{
				// Copy functor atom
				if (copy_term_part(stack,v,new_term,term_size))
					return -1;
			}

			if (copy_term_part(stack,v,new_term,term_size))
				return -1;

			if (!r && UNBOX_TYPE((*v)->m_u64val) == PROLITE_DEBUG_INFO)
			{
				// TODO: Debug info
			}

			while (arity--)
			{
				if (copy_term_append(substs,stack,strings,v,new_term,term_size) != 0)
					return -1;
			}
		}
		break;

	case prolite_var:
		{
			const union box_t* ptr = deref_term(substs,*v);
			if (ptr != *v)
				return copy_term_append(substs,stack,strings,&ptr,new_term,term_size);

			r = copy_term_part(stack,v,new_term,term_size);
		}
		break;

	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		{
			unsigned int hi16 = UNBOX_HI16((*v)->m_u64val);
			if (hi16 & 0xC000)
				r = copy_term_part(stack,v,new_term,term_size);
			else
			{
				union box_t b[2];

				struct string_ptr_t* s = unbox_pointer((*v)->m_u64val);
				s = box_stack_string(stack,strings,s->m_str,s->m_len);
				if (!s)
					return -1;

				b[0].m_u64val = BOX_TYPE(type) | box_pointer(s);
				b[1].m_u64val = 0;
				r = copy_term_part(stack,(const union box_t**)&b,new_term,term_size);
			}
		}
		break;

	default:
		r = copy_term_part(stack,v,new_term,term_size);
	}

	if (!r && UNBOX_TYPE((*v)->m_u64val) == PROLITE_DEBUG_INFO)
	{
		// TODO: Debug info
	}

	return r;
}

union box_t* copy_term(struct substs_t* substs, struct stack_t** stack, struct string_ptr_t** strings, const union box_t* v)
{
	size_t term_size = 0;
	union box_t* r = NULL;
	if (copy_term_append(substs,stack,strings,&v,&r,&term_size) != 0)
		r = NULL;
	return r;
}*/
