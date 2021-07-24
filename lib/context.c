/*
 * context.c
 *
 *  Created on: 13 May 2017
 *      Author: rick
 */

#include "types.h"

#include <assert.h>

uint32_t convert_char(context_t* context, uint32_t in_char)
{
	/* TODO */

	return in_char;
}

#ifdef UNUSED
static uint32_t atom_to_code(const packed_t* b)
{
	uint64_t all48 = UNPACK_MANT_48(b->m_u64val);
	unsigned int len = ((all48 >> 40) & 0x07);
	unsigned int count = 0;
	uint32_t val = 0;
	
	uint8_t c[5] =
	{
		(all48 >> 32) & 0xFF,
		(all48 >> 24) & 0xFF,
		(all48 >> 16) & 0xFF,
		(all48 >> 8) & 0xFF,
		all48 & 0xFF
	};

	if (!((all48 >> 32) & 0x8000))
		return -1;

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

static operator_t* find_op(context_t* context, const unsigned char* name, size_t name_len)
{
	static operator_t s_builtins[] =
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

	switch (name_len)
	{
	case 1:
		switch (name[0])
		{
		case ';':
			return &s_builtins[1];

		case ',':
			return &s_builtins[3];

		case '=':
		case '<':
		case '>':
			return &s_builtins[4];

		case '+':
		case '-':
			return &s_builtins[5];

		case '*':
		case '/':
			return &s_builtins[6];
	
		case '^':
			return &s_builtins[8];

		default:
			break;
		}
		break;

	case 2:
		switch (name[0])
		{
		case ':':
			if (name[1] == '-')
				return &s_builtins[0];
			break;

		case '-':
			if (name[1] == '>')
				return &s_builtins[2];
			break;

		case '\\':
			if (name[1] == '=')
				return &s_builtins[4];
			if (name[1] == '/')
				return &s_builtins[5];
			break;

		case '=':
			if (name[1] == '=' || name[1] == '<')
				return &s_builtins[4];
			break;

		case '@':
			if (name[1] == '<' || name[1] == '>')
				return &s_builtins[4];
			break;

		case 'i':
			if (name[1] == 's')
				return &s_builtins[4];
			break;

		case '>':
			if (name[1] == '=')
				return &s_builtins[4];
			if (name[1] == '>')
				return &s_builtins[6];
			break;

		case '/':
			if (name[1] == '\\')
				return &s_builtins[5];
			if (name[1] == '/')
				return &s_builtins[6];
			break;

		case '<':
			if (name[1] == '<')
				return &s_builtins[6];
			break;

		case '*':
			if (name[1] == '*')
				return &s_builtins[7];
			break;

		default:
			break;
		}
		break;

	case 3:
		switch (name[0])
		{
		case '-':
			if (name[1] == '-' && name[2] == '>')
				return &s_builtins[0];
			break;

		case '\\':
			if (name[1] == '=' && name[2] == '=')
				return &s_builtins[4];
			break;

		case '@':
			if ((name[1] == '=' && name[2] == '<') ||
				(name[1] == '>' && name[2] == '='))
					return &s_builtins[4];
			break;

		case '=':
			if ((name[1] == '.' && name[2] == '.') ||
				(name[1] == ':' && name[2] == '=') ||
				(name[1] == '\\' && name[2] == '='))
					return &s_builtins[4];
			break;

		case 'r':
			if (name[1] == 'e' && name[2] == 'm')
				return &s_builtins[6];
			break;

		case 'm':
			if (name[1] == 'o' && name[2] == 'd')
				return &s_builtins[6];
			break;

		case 'd':
			if (name[1] == 'i' && name[2] == 'v')
				return &s_builtins[6];
			break;

		default:
			break;
		}
		break;

	default:
		break;
	}

	return NULL;
}

static operator_t* find_prefix_op(context_t* context, const unsigned char* name, size_t name_len)
{
	static operator_t s_builtins[] =
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

	switch (name_len)
	{
	case 1:
		switch (name[0])
		{
		case '-':
		case '\\':
		case '+':
			return &s_builtins[2];

		default:
			break;
		}
		break;

	case 2:
		switch (name[0])
		{
		case ':':
		case '?':
			if (name[1] == '-')
				return &s_builtins[0];
			break;

		case '\\':
			if (name[1] == '+')
				return &s_builtins[1];
			break;

		default:
			break;
		}
		break;

	default:
		break;
	}

	return NULL;
}

/* Try to find a infix/postfix op, otherwise find prefix */
operator_t* lookup_op(context_t* context, const unsigned char* name, size_t name_len)
{
	operator_t* op = find_op(context,name,name_len);
	if (!op)
		op = find_prefix_op(context,name,name_len);
	return op;
}

/* Try to find a prefix op, otherwise find infix/suffix */
operator_t* lookup_prefix_op(context_t* context, const unsigned char* name, size_t name_len)
{
	operator_t* op = find_prefix_op(context,name,name_len);
	if (!op)
		op = find_op(context,name,name_len);
	return op;
}

// TODO: Move these around later...

#include <string.h>

module_t* module_new(context_t* context, const char* name)
{
	// TODO: Much more here!!

	module_t* module = heap_malloc(&context->m_heap,sizeof(module_t));
	if (module)
	{
		memset(module,0,sizeof(module_t));
		module->m_flags.char_conversion = 1;
		module->m_flags.back_quotes = 1;
	}

	return module;
}

void module_delete(module_t* module)
{
	// TODO
}

context_t* context_new(void)
{
	const size_t stack_size = 65536;

	// Create a new context
	context_t* retval = NULL;
	heap_t* h = heap_new(8000,&malloc,&free);
	if (h)
	{
		context_t* c = heap_malloc(&h,sizeof(context_t));
		if (c)
		{
			memset(c,0,sizeof(context_t));			
			c->m_stack = malloc(stack_size * sizeof(packed_t));
			c->m_stack += stack_size;
			c->m_heap = h;
			c->m_module = module_new(c,"user");
			if (c->m_module)
			{
				retval = c;
			}
		}

		if (!retval)
			heap_delete(h);
	}

	return retval;
}

void context_delete(context_t* c)
{
	module_delete(c->m_module);
	//stack_delete(c->m_call_stack);
	heap_delete(c->m_heap);
}
