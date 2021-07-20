
#include "types.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#undef DECLARE_BUILTIN_STRING
#define DECLARE_BUILTIN_STRING(s) { sizeof(#s)-1,(const unsigned char*)(#s) },

const struct string_t s_builtin_strings[] =
{
#include "builtin_strings"
};

static int string_compare(const void* p1, const void* p2)
{
	const struct string_t* s1 = p1;
	const struct string_t* s2 = p2;

	int r = (s1->m_len - s2->m_len);
	if (r == 0)
		r = memcmp(s1->m_str,s2->m_str,s1->m_len);
	
	return r;
}

static uint32_t is_builtin_string(const unsigned char* str, size_t len)
{
	uint32_t ret = -1;
	struct string_t f, *r;
	f.m_len = len;
	f.m_str = str;

	r = bsearch(&f,s_builtin_strings,sizeof(s_builtin_strings) / sizeof(s_builtin_strings[0]),sizeof(s_builtin_strings[0]),&string_compare);
	if (r)
		ret = (uint32_t)(r - s_builtin_strings);

	return ret;
}

union packed_t* push_string(union packed_t* stack, enum tag_type_t type, const unsigned char* str, size_t len)
{
	uint32_t builtin;
	switch (len)
	{
	case 5:
		(--stack)->m_u64val = PACK_TYPE_EMBED(type,0,5,str[0],str[1],str[2],str[3],str[4]);
		break;

	case 4:
		(--stack)->m_u64val = PACK_TYPE_EMBED(type,0,4,str[0],str[1],str[2],str[3],0);
		break;

	case 3:
		(--stack)->m_u64val = PACK_TYPE_EMBED(type,0,3,str[0],str[1],str[2],0,0);
		break;

	case 2:
		(--stack)->m_u64val = PACK_TYPE_EMBED(type,0,2,str[0],str[1],0,0,0);
		break;

	case 1:
		(--stack)->m_u64val = PACK_TYPE_EMBED(type,0,1,str[0],0,0,0,0);
		break;

	case 0:
		(--stack)->m_u64val = PACK_TYPE_EMBED(type,0,0,0,0,0,0,0);
		break;

	default:
		if ((builtin = is_builtin_string(str,len)) != -1)
		{
			(--stack)->m_u64val = (PACK_TYPE(type) | PACK_MANT_48((UINT64_C(0x4000) << 32) | builtin));
		}
		else
		{
			stack -= ((len + sizeof(union packed_t)-1) / sizeof(union packed_t));
			memcpy(stack,str,len);
			(--stack)->m_u64val = PACK_TYPE(type) | PACK_MANT_48(len);	
		}
		break;
	}

	return stack;
}

union packed_t* push_compound(union packed_t* stack, uint64_t arity, const unsigned char* functor, size_t functor_len)
{
	uint32_t builtin;
	if (arity <= MAX_ARITY_EMBED && functor_len <= 5)
	{
		switch (functor_len)
		{
		case 5:
			(--stack)->m_u64val = PACK_TYPE_EMBED(prolite_compound,arity,5,functor[0],functor[1],functor[2],functor[3],functor[4]);
			break;

		case 4:
			(--stack)->m_u64val = PACK_TYPE_EMBED(prolite_compound,arity,4,functor[0],functor[1],functor[2],functor[3],0);
			break;

		case 3:
			(--stack)->m_u64val = PACK_TYPE_EMBED(prolite_compound,arity,3,functor[0],functor[1],functor[2],0,0);
			break;

		case 2:
			(--stack)->m_u64val = PACK_TYPE_EMBED(prolite_compound,arity,2,functor[0],functor[1],0,0,0);
			break;

		case 1:
			(--stack)->m_u64val = PACK_TYPE_EMBED(prolite_compound,arity,1,functor[0],0,0,0,0);
			break;

		default:
			(--stack)->m_u64val = PACK_TYPE_EMBED(prolite_compound,arity,0,0,0,0,0,0);
			break;
		}
	}
	else if (arity <= MAX_ARITY_BUILTIN && (builtin = is_builtin_string(functor,functor_len)) != -1)
	{
		(--stack)->m_u64val = (PACK_TYPE(prolite_compound) | PACK_MANT_48(((UINT64_C(0x4000) | ((uint16_t)(arity) & MAX_ARITY_BUILTIN)) << 32) | builtin));
	}
	else
	{
		stack = push_string(stack,prolite_atom,functor,functor_len);
		(--stack)->m_u64val = PACK_TYPE(prolite_compound) | PACK_MANT_48(arity);		
	}

	return stack;
}

static const union packed_t* get_debug_info(const union packed_t* p, struct debug_info_t* debug_info)
{
	// TODO

	return p;
}

struct string_t get_string(const union packed_t** b, struct debug_info_t* debug_info)
{
	struct string_t ret;
	uint64_t all48 = ((*b)++)->m_u64val;
	uint16_t hi16;

	all48 = UNPACK_MANT_48(all48);
	hi16 = (all48 >> 32);
	if (hi16 & 0x8000)
	{
		ret.m_len = (hi16 & 0x0700) >> 8;
		ret.m_data[0] = all48 >> 32;
		ret.m_data[1] = all48 >> 24;
		ret.m_data[2] = all48 >> 16;
		ret.m_data[3] = all48 >> 8;
		ret.m_data[4] = all48;
		ret.m_str = ret.m_data;
	}
	else if (hi16 & 0x4000)
	{
		ret = s_builtin_strings[(uint32_t)all48];
	}
	else
	{
		ret.m_len = (size_t)(all48 & MAX_ATOM_LEN);
		ret.m_str = (const unsigned char*)b;
		*b += ((ret.m_len + sizeof(union packed_t)-1) / sizeof(union packed_t));
	}

	*b = get_debug_info(*b,debug_info);

	return ret;
}

struct string_t get_compound(const union packed_t** b, uint64_t* arity, struct debug_info_t* debug_info)
{
	struct string_t ret;
	uint64_t all48 = ((*b)++)->m_u64val;
	uint16_t hi16;

	all48 = UNPACK_MANT_48(all48);
	hi16 = (all48 >> 32);
	if (hi16 & 0x8000)
	{
		if (arity)
			*arity = (hi16 & 0x7800) >> 11;

		ret.m_len = (hi16 & 0x0700) >> 8;
		ret.m_data[0] = all48 >> 32;
		ret.m_data[1] = all48 >> 24;
		ret.m_data[2] = all48 >> 16;
		ret.m_data[3] = all48 >> 8;
		ret.m_data[4] = all48;
		ret.m_str = ret.m_data;

		*b = get_debug_info(*b,debug_info);
	}
	else if (hi16 & 0x4000)
	{
		if (arity)
			*arity = hi16 & MAX_ARITY_BUILTIN;

		ret = s_builtin_strings[(uint32_t)all48];
	}
	else
	{
		if (arity)
			*arity = (all48 & MAX_ARITY);

		ret = get_string(b,debug_info);
	}

	return ret;
}

const union packed_t* get_next_arg(const union packed_t* p, struct debug_info_t* debug_info)
{
	uint64_t all48 = (p++)->m_u64val;
	uint16_t hi16;
	enum tag_type_t type = UNPACK_TYPE(all48);
	all48 = UNPACK_MANT_48(all48);
	hi16 = (all48 >> 32);

	switch (type)
	{
	case prolite_double:
	case prolite_int32:
	case prolite_var:
		break;

	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		if (!(hi16 & 0xC000))
		{
			size_t len = (size_t)(all48 & MAX_ATOM_LEN);
			p += ((len + sizeof(union packed_t)-1) / sizeof(union packed_t));
		}
		break;

	case prolite_compound:
		{
			uint64_t arity;
			if (hi16 & 0x8000)
			{
				arity = (hi16 & 0x7800) >> 11;
				p = get_debug_info(p,debug_info);
			}
			else if (hi16 & 0x4000)
			{
				arity = hi16 & MAX_ARITY_BUILTIN;
				p = get_debug_info(p,debug_info);
			}
			else
			{
				arity = (all48 & MAX_ARITY);
				p = get_next_arg(p,debug_info);
			}

			/* Skip args */
			while (arity--)
				p = get_next_arg(p,NULL);
		}
		return p;
		
	default:
		assert(0);
		break;
	}

	return get_debug_info(p,debug_info);
}

const union packed_t* get_first_arg(const union packed_t* compound, uint64_t* arity, struct debug_info_t* debug_info)
{
	uint64_t all48 = (compound++)->m_u64val;
	uint16_t hi16;

	assert(UNPACK_TYPE(all48) == prolite_compound);

	all48 = UNPACK_MANT_48(all48);
	hi16 = (all48 >> 32);
	if (hi16 & 0x8000)
	{
		if (arity)
			*arity = (hi16 & 0x7800) >> 11;

		compound = get_debug_info(compound,debug_info);
	}
	else if (hi16 & 0x4000)
	{
		if (arity)
			*arity = hi16 & MAX_ARITY_BUILTIN;

		compound = get_debug_info(compound,debug_info);
	}
	else
	{
		if (arity)
			*arity = (all48 & MAX_ARITY);
		
		// Functor is next
		compound = get_next_arg(compound,debug_info);
	}

	return compound;
}
