/*
 * context.c
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#include "context.h"

#include <assert.h>

uint32_t convert_char(struct context_t* context, uint32_t in_char)
{
	return in_char;
}

/* Try to find a infix/suffix op, otherwise find prefix */
struct operator_t* lookup_op(struct context_t* context, const union box_t* b)
{
	return NULL;
}

/* Try to find a prefix op, otherwise find infix/suffix */
struct operator_t* lookup_prefix_op(struct context_t* context, const union box_t* b)
{
	return NULL;
}

static int define_op(struct context_t* context, struct term_t* term)
{
	return -1;
}

static uint64_t directive_atom(const unsigned char* atom, const size_t alen)
{
	union box_t b;
	b.m_uval = BOX_TAG_ATOM;
#if defined(NDEBUG)
	box_string_builtin(&b,atom,alen);
#else
	assert(box_string_builtin(&b,atom,alen) == 1);
#endif
	return b.m_uval;
}

#define STRING_AND_LEN(s) (const unsigned char*)(s),sizeof(s)

/* 'Do' a directive */
int directive(struct context_t* context, struct term_t* term)
{
	static uint64_t dynamic = directive_atom(STRING_AND_LEN("dynamic"));
	static uint64_t multifile = directive_atom(STRING_AND_LEN("multifile"));
	static uint64_t discontiguous = directive_atom(STRING_AND_LEN("discontiguous"));
	static uint64_t initialization= directive_atom(STRING_AND_LEN("initialization"));
	static uint64_t include = directive_atom(STRING_AND_LEN("include"));
	static uint64_t ensure_loaded = directive_atom(STRING_AND_LEN("ensure_loaded"));
	static uint64_t char_conversion = directive_atom(STRING_AND_LEN("char_conversion"));
	static uint64_t set_prolog_flag = directive_atom(STRING_AND_LEN("set_prolog_flag"));

	switch (term->m_value->m_uval)
	{
	case BOX_COMPOUND_EMBED_2(3,'o','p'):  /* op/3 */
		return define_op(context,term);

	case (UINT64_C(0xFFF6) << 48 | 1):
		if (term->m_value->m_uval == dynamic)
		{
		}
		if (term->m_value->m_uval == multifile)
		{
		}
		if (term->m_value->m_uval == discontiguous)
		{
		}
		if (term->m_value->m_uval == initialization)
		{
		}
		if (term->m_value->m_uval == include)
		{
		}
		if (term->m_value->m_uval == ensure_loaded)
		{
		}
		break;

	case (UINT64_C(0xFFF6) << 48 | 2):
		if (term->m_value->m_uval == char_conversion)
		{
		}
		if (term->m_value->m_uval == set_prolog_flag)
		{
		}
		break;

	default:
		break;
	}

	/* TODO: push error */
	return 1;
}
