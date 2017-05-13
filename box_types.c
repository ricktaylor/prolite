
#include "box_types.h"
#include "context.h"

#include <string.h>

static int box_string_ptr(struct context_t* context, union box_t* b, const unsigned char* str, size_t len)
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
			return -1;

		s->m_prev = context->m_strings;
		s->m_len = len;
		memcpy(s->m_str,str,len);
		context->m_strings = s;
	}

	box_pointer(b,s);
	return 0;
}

int box_string(struct context_t* context, union box_t* b, const unsigned char* str, size_t len)
{
	if (len > 5)
		return box_string_ptr(context,b,str,len);

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

const unsigned char* unbox_string(struct context_t* context, const union box_t* b, size_t* len)
{
	if ((b->m_uval & (UINT64_C(0x8) << 44)) == 0)
	{
		struct string_ptr_t const* s = unbox_pointer(b);
		*len = s->m_len;
		return s->m_str;
	}

	*len = (size_t)(b->m_uval & (UINT64_C(0xF) << 40));
	return ((const unsigned char*)b) + 3;
}
