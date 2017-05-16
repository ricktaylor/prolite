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

/* 'Do' a directive */
int directive(struct context_t* context, struct term_t* term)
{
	if (term->m_value->m_uval == BOX_COMPOUND_EMBED_2(3,'o','p'))
	{
		++term->m_value;
		return define_op(context,term);
	}

	if (term->m_value->m_uval == (UINT64_C(0xFFF6) << 48 | 1))
	{
		switch (term->m_value[1].m_uval)
		{
		case BUILTIN_ATOM(dynamic):
		case BUILTIN_ATOM(multifile):
		case BUILTIN_ATOM(discontiguous):
		case BUILTIN_ATOM(initialization):
		case BUILTIN_ATOM(include):
		case BUILTIN_ATOM(ensure_loaded):
		default:
			break;
		}
	}
	else if (term->m_value->m_uval == (UINT64_C(0xFFF6) << 48 | 2))
	{
		switch (term->m_value[1].m_uval)
		{
		case BUILTIN_ATOM(char_conversion):
		case BUILTIN_ATOM(set_prolog_flag):
		default:
			break;
		}
	}

	/* TODO: push error */
	return 1;
}
