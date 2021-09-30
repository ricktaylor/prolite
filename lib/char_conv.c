#include "context.h"

uint32_t convert_char(const char_conv_table_t* cc, uint32_t in_char)
{
	if (in_char == 0)
		in_char = -1;

	uint32_t out_char = (uintptr_t)btree_lookup(cc,in_char);
	if (!out_char)
		out_char = in_char;

	if (out_char == -1)
		out_char = 0;

	return out_char;
}

static uint32_t atom_to_code(const term_t* a)
{
	string_t s;
	get_string(a,&s,NULL);
	if (s.m_len == 0)
		return -1;

	const unsigned char* c = s.m_str;
	if (c[0] <= 0x7f)
		return s.m_len == 1 ? c[0] : -1;

	if (c[0] < 0xC2 || c[0] > 0xF4)
		return -1;

	size_t count = 0;
	uint32_t val = 0;
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

	if (s.m_len != count)
		return -1;

	for (size_t i = 1; i < count; ++i)
	{
		if ((c[i] & 0xC0) != 0x80)
			return -1;

		val = (val << 6) | (c[i] & 0x3F);
	}

	return val;
}

static void set_char_conversion(context_t* context, char_conv_table_t* cc, const term_t* in_char, const term_t* out_char)
{
	switch (get_term_type(in_char))
	{
	case prolite_var:
		return push_instantiation_error(context,in_char);

	case prolite_atom:
		switch (get_term_type(out_char))
		{
		case prolite_var:
			return push_instantiation_error(context,out_char);

		case prolite_atom:
			{
				uint32_t in_code = atom_to_code(in_char);
				if (in_code == -1)
					return push_representation_error(context,PACK_ATOM_BUILTIN(character),in_char);

				if (in_code == 0)
					in_code = -1;

				uint32_t out_code = atom_to_code(out_char);
				if (out_code == -1)
					return push_representation_error(context,PACK_ATOM_BUILTIN(character),out_char);

				if (out_code == 0)
					out_code = -1;

				if (!btree_replace(cc,in_code,(void*)(uintptr_t)out_code))
					return push_out_of_memory_error(context,in_char);
			}
			break;

		default:
			return push_representation_error(context,PACK_ATOM_BUILTIN(character),out_char);
		}
		break;

	default:
		return push_representation_error(context,PACK_ATOM_BUILTIN(character),in_char);
	}
}

void directive_char_conversion(context_t* context, char_conv_table_t* cc, const term_t* goal)
{
	const term_t* in_char = get_first_arg(goal,NULL);
	const term_t* out_char = get_next_arg(in_char);

	set_char_conversion(context,cc,in_char,out_char);
}

PROLITE_EXPORT void prolite_builtin_char_conversion(context_t* context, const term_t* arg1, const term_t* arg2)
{
	set_char_conversion(context,&context->m_module->m_char_conversion,arg1,arg2);
}

PROLITE_EXPORT void prolite_builtin_current_char_conversion(context_t* context, const term_t* arg1, const term_t* arg2)
{
}
