/*
 * context.c
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#include "types.h"

#include <assert.h>

uint32_t convert_char(struct context_t* context, uint32_t in_char)
{
	/* TODO */

	return in_char;
}

#ifdef UNUSED
static uint32_t atom_to_code(const union packed_t* b)
{
	uint16_t hi16 = UNPACK_HI16(b->m_u64val);
	uint32_t lo32 = UNPACK_LOW32(b->m_u64val);
	unsigned int len = (hi16 & 0x0700) >> 8;
	unsigned int count = 0;
	uint32_t val = 0;
	uint8_t c[5] =
	{
		(hi16 & 0xFF),
		(lo32 >> 24),
		(lo32 >> 16) & 0xFF,
		(lo32 >> 8) & 0xFF,
		lo32 & 0xFF
	};

	if (c[0] <= 0x7f)
		return len == 1 ? c[0] : -1;

	if (c[0] < 0xC2 || c[0] > 0xF4)
		return -1;

	if ((c[0] & 0xE0) == 0xC0)
	{
		count = 2;
		val = (c[0] & 0x1F);
	}
	else if ((c[0] & 0xF0) == 0xE0)
	{
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
#endif

static struct operator_t* find_op(struct context_t* context, const union packed_t* b)
{
	static struct operator_t s_builtins[] =
	{
		/* 0 */ { NULL, eXFX, 1200 },
		/* 1 */ { NULL, eXFY, 1100 },
		/* 2 */ { NULL, eXFY, 1050 },
		/* 3 */ { NULL, eXFY, 1000 },
		/* 4 */ { NULL, eXFX, 700 },
		/* 5 */ { NULL, eYFX, 500 },
		/* 6 */ { NULL, eYFX, 400 },
		/* 7 */ { NULL, eXFX, 200 },
		/* 8 */ { NULL, eXFY, 200 },
	};

	if (context->m_module->m_operators)
	{
		// TODO: Use the module table
		assert(0);
	}

	switch (b->m_u64val)
	{
	case PACK_ATOM_EMBED_2(':','-'):
	case PACK_ATOM_EMBED_3('-','-','>'):
		return &s_builtins[0];

	case PACK_ATOM_EMBED_1(';'):
		return &s_builtins[1];

	case PACK_ATOM_EMBED_2('-','>'):
		return &s_builtins[2];

	case PACK_ATOM_EMBED_1(','):
		return &s_builtins[3];

	case PACK_ATOM_EMBED_1('='):
	case PACK_ATOM_EMBED_2('\\','='):
	case PACK_ATOM_EMBED_2('=','='):
	case PACK_ATOM_EMBED_3('\\','=','='):
	case PACK_ATOM_EMBED_2('@','<'):
	case PACK_ATOM_EMBED_3('@','=','<'):
	case PACK_ATOM_EMBED_2('@','>'):
	case PACK_ATOM_EMBED_3('@','>','='):
	case PACK_ATOM_EMBED_3('=','.','.'):
	case PACK_ATOM_EMBED_2('i','s'):
	case PACK_ATOM_EMBED_3('=',':','='):
	case PACK_ATOM_EMBED_3('=','\\','='):
	case PACK_ATOM_EMBED_1('<'):
	case PACK_ATOM_EMBED_2('=','<'):
	case PACK_ATOM_EMBED_1('>'):
	case PACK_ATOM_EMBED_2('>','='):
		return &s_builtins[4];

	case PACK_ATOM_EMBED_1('+'):
	case PACK_ATOM_EMBED_1('-'):
	case PACK_ATOM_EMBED_2('/','\\'):
	case PACK_ATOM_EMBED_2('\\','/'):
		return &s_builtins[5];

	case PACK_ATOM_EMBED_1('*'):
	case PACK_ATOM_EMBED_1('/'):
	case PACK_ATOM_EMBED_2('/','/'):
	case PACK_ATOM_EMBED_3('r','e','m'):
	case PACK_ATOM_EMBED_3('m','o','d'):
	case PACK_ATOM_EMBED_2('<','<'):
	case PACK_ATOM_EMBED_2('>','>'):
	case PACK_ATOM_EMBED_3('d','i','v'):
		return &s_builtins[6];

	case PACK_ATOM_EMBED_2('*','*'):
		return &s_builtins[7];

	case PACK_ATOM_EMBED_1('^'):
		return &s_builtins[8];

	default:
		return NULL;
	}
}

static struct operator_t* find_prefix_op(struct context_t* context, const union packed_t* b)
{
	static struct operator_t s_builtins[] =
	{
		/* 0 */ { NULL, eFX, 1200 },
		/* 1 */ { NULL, eFY, 900 },
		/* 2 */ { NULL, eFY, 200 },
	};

	if (context->m_module->m_operators)
	{
		// TODO: Use the module table
		assert(0);
	}

	switch (b->m_u64val)
	{
	case PACK_ATOM_EMBED_2(':','-'):
	case PACK_ATOM_EMBED_2('?','-'):
		return &s_builtins[0];

	case PACK_ATOM_EMBED_2('\\','+'):
		return &s_builtins[1];

	case PACK_ATOM_EMBED_1('-'):
	case PACK_ATOM_EMBED_1('\\'):
	case PACK_ATOM_EMBED_1('+'):
		return &s_builtins[2];

	default:
		return NULL;
	}
}

/* Try to find a infix/postfix op, otherwise find prefix */
struct operator_t* lookup_op(struct context_t* context, const union packed_t* b)
{
	struct operator_t* op = find_op(context,b);
	if (!op)
		op = find_prefix_op(context,b);
	return op;
}

/* Try to find a prefix op, otherwise find infix/suffix */
struct operator_t* lookup_prefix_op(struct context_t* context, const union packed_t* b)
{
	struct operator_t* op = find_prefix_op(context,b);
	if (!op)
		op = find_op(context,b);
	return op;
}
