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

	size_t count = 0;
	uint32_t val = 0;
	if (s.m_str[0] <= 0x7F)
	{
		count = 1;
		val = s.m_str[0];
	}
	else if ((s.m_str[0] & 0xE0) == 0xC0)
	{
		count = 2;
		val = (s.m_str[0] & 0x1F);
	}
	else if ((s.m_str[0] & 0xF0) == 0xE0)
	{
		count = 3;
		val = (s.m_str[0] & 0x0F);
	}
	else if ((s.m_str[0] & 0xF8) == 0xF0)
	{
		count = 4;
		val = (s.m_str[0] & 0x7);
	}
	
	if (s.m_len != count)
		return -1;

	for (size_t i = 1; i < count; ++i)
		val = (val << 6) | (s.m_str[i] & 0x3F);
	
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

void builtin_char_conversion(context_t* context, const void* gosub, size_t argc, const term_t* argv[])
{
	set_char_conversion(context,&context->m_module->m_char_conversion,argv[0],argv[1]);
	if (!(context->m_flags & FLAG_THROW))
		builtin_gosub(context,gosub);
}

void builtin_current_char_conversion(context_t* context, const void* gosub, size_t argc, const term_t* argv[])
{
}
