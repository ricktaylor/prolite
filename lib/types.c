
#include "compile.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#undef DECLARE_BUILTIN_STRING
#define DECLARE_BUILTIN_STRING(s) { sizeof(#s)-1,(const unsigned char*)(#s) },

const string_t s_builtin_strings[] =
{
#include "builtin_strings.h"
};

static int string_compare(const void* p1, const void* p2)
{
	const string_t* s1 = p1;
	const string_t* s2 = p2;

	int r = (s1->m_len - s2->m_len);
	if (r == 0)
		r = memcmp(s1->m_str,s2->m_str,s1->m_len);

	return r;
}

static uint32_t is_builtin_string(const unsigned char* str, size_t len)
{
	uint32_t ret = -1;
	string_t f = 
	{
		.m_len = len,
		.m_str = str
	};

	string_t* r = bsearch(&f,s_builtin_strings,sizeof(s_builtin_strings) / sizeof(s_builtin_strings[0]),sizeof(s_builtin_strings[0]),&string_compare);
	if (r)
		ret = (uint32_t)(r - s_builtin_strings);

	return ret;
}

term_t* push_string(term_t* stack, prolite_type_t type, const unsigned char* str, size_t len)
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
			stack -= ((len + sizeof(term_t)-1) / sizeof(term_t));
			memcpy(stack,str,len);
			(--stack)->m_u64val = PACK_TYPE(type) | PACK_MANT_48(len);
		}
		break;
	}

	return stack;
}

term_t* push_compound(term_t* stack, uint64_t arity, const unsigned char* functor, size_t functor_len)
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

static const term_t* get_debug_info(const term_t* p, debug_info_t* debug_info)
{
	// TODO

	return p;
}

string_t get_string(const term_t* b, debug_info_t* debug_info)
{
	string_t ret;
	uint64_t all48 = (b++)->m_u64val;
	int have_debug_info = (UNPACK_TYPE(all48) & prolite_debug_info);
	all48 = UNPACK_MANT_48(all48);

	uint16_t hi16 = (all48 >> 32);
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
		b += ((ret.m_len + sizeof(term_t)-1) / sizeof(term_t));
	}

	if (have_debug_info && debug_info)
		get_debug_info(b,debug_info);

	return ret;
}

string_t get_compound(const term_t* b, uint64_t* arity, debug_info_t* debug_info)
{
	string_t ret;
	uint64_t all48 = (b++)->m_u64val;
	int have_debug_info = (UNPACK_TYPE(all48) & prolite_debug_info);
	all48 = UNPACK_MANT_48(all48);

	uint16_t hi16 = (all48 >> 32);
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

		if (have_debug_info && debug_info)
			get_debug_info(b,debug_info);
	}
	else if (hi16 & 0x4000)
	{
		if (arity)
			*arity = hi16 & MAX_ARITY_BUILTIN;

		ret = s_builtin_strings[(uint32_t)all48];

		if (have_debug_info && debug_info)
			get_debug_info(b,debug_info);
	}
	else
	{
		if (arity)
			*arity = (all48 & MAX_ARITY);

		ret = get_string(b,debug_info);
	}

	return ret;
}

const term_t* get_next_arg(const term_t* p, debug_info_t* debug_info)
{
	uint64_t all48 = (p++)->m_u64val;
	uint16_t hi16 = UNPACK_TYPE(all48);
	prolite_type_t type = hi16 & 0x7;
	int have_debug_info = (hi16 & prolite_debug_info);
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
			p += ((len + sizeof(term_t)-1) / sizeof(term_t));
		}
		break;

	case prolite_compound:
		{
			uint64_t arity;
			if (hi16 & 0x8000)
			{
				arity = (hi16 & 0x7800) >> 11;
				if (have_debug_info)
					p = get_debug_info(p,debug_info);
			}
			else if (hi16 & 0x4000)
			{
				arity = hi16 & MAX_ARITY_BUILTIN;
				if (have_debug_info)
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

	if (have_debug_info)
		p = get_debug_info(p,debug_info);
	return p;
}

const term_t* get_first_arg(const term_t* compound, uint64_t* arity, debug_info_t* debug_info)
{
	uint64_t all48 = (compound++)->m_u64val;
	int have_debug_info = (UNPACK_TYPE(all48) & prolite_debug_info);
	
	assert((UNPACK_TYPE(all48) & 0x7) == prolite_compound);

	all48 = UNPACK_MANT_48(all48);
	uint16_t hi16 = (all48 >> 32);
	if (hi16 & 0x8000)
	{
		if (arity)
			*arity = (hi16 & 0x7800) >> 11;

		if (have_debug_info)
			compound = get_debug_info(compound,debug_info);
	}
	else if (hi16 & 0x4000)
	{
		if (arity)
			*arity = hi16 & MAX_ARITY_BUILTIN;

		if (have_debug_info)
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

static int atom_compare(const term_t* a1, const term_t* a2)
{
	// TODO
	return 1;
}

static int atom_precedes(const term_t* a1, const term_t* a2)
{
	// TODO
	return 0;
}

int compound_compare(const term_t* c1, const term_t* c2)
{
	int r = 0;
	if (c1->m_u64val == c2->m_u64val)
	{
		uint16_t hi16 = (UNPACK_MANT_48(c1->m_u64val) >> 32);
		if (!(hi16 & 0xC000))
		{
			// Check functors
			r = atom_compare(c1+1,c2+1);
		}
		else
			r = 1;
	}

	return r;
}

static int compound_precedes(const term_t* c1, const term_t* c2)
{
	int r = 0;

	// TODO - Check term_precedes

	if (c1->m_u64val != c2->m_u64val)
	{
		
	}
	else
	{
		uint16_t hi16 = (UNPACK_MANT_48(c1->m_u64val) >> 32);
		if (!(hi16 & 0xC000))
		{
			// Check functors
			r = atom_precedes(c1+1,c2+1);
		}
	}

	return r;
}

static int type_precedes(prolite_type_t t)
{
	switch (t)
	{
	case prolite_var:
		return 0;

	case prolite_double:
		return 1;

	case prolite_int32:
	case prolite_atom:
	case prolite_compound:
		return t;

	case prolite_chars:
	case prolite_charcodes:
		return prolite_compound;

	default:
		assert(0);
		return 100;
	}
}

int term_compare(const term_t* t1, const term_t* t2)
{
	int r = 0;
	prolite_type_t type = get_term_type(t1);
	if (type == get_term_type(t2))
	{
		switch (type)
		{
		case prolite_atom:
			r = atom_compare(t1,t2);
			break;

		case prolite_compound:
			if ((r = compound_compare(t1,t2)))
			{
				uint64_t arity;
				const term_t* p1 = get_first_arg(t1,&arity,NULL);
				const term_t* p2 = get_first_arg(t2,NULL,NULL);
				while (arity--)
				{
					if (!(r = term_compare(p1,p2)))
						break;

					p1 = get_next_arg(p1,NULL);
					p2 = get_next_arg(p2,NULL);
				}
			}
			break;

		// TODO

		default:
			r = (t1->m_u64val == t2->m_u64val);
			break;
		}
	}

	return r;
}

int term_precedes(const term_t* t1, const term_t* t2)
{
	prolite_type_t type = get_term_type(t1);
	int r = (type_precedes(type) - type_precedes(get_term_type(t2)));
	if (r == 0)
	{
		switch (type)
		{
		case prolite_atom:
			r = atom_precedes(t1,t2);
			break;

		case prolite_compound:
			r = compound_precedes(t1,t2);
			if (r == 0)
			{
				uint64_t arity;
				const term_t* p1 = get_first_arg(t1,&arity,NULL);
				const term_t* p2 = get_first_arg(t2,NULL,NULL);
				while (arity--)
				{
					r = term_precedes(p1,p2);
					if (r != 0)
						break;

					p1 = get_next_arg(p1,NULL);
					p2 = get_next_arg(p2,NULL);
				}
			}
			break;

		// TODO

		default:
			r = (t1->m_u64val - t2->m_u64val);
			break;
		}
	}

	return r;
}

continuation_t* compile_var(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	if (get_term_type(g1) == prolite_var)
		return compile_builtin(context,cont,&builtin_var,1,g1);

	return compile_false(context,cont,goal);
}

continuation_t* compile_atom(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,&builtin_atom,1,g1);

	case prolite_atom:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

continuation_t* compile_integer(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,&builtin_integer,1,g1);

	case prolite_int32:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

continuation_t* compile_float(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,&builtin_float,1,g1);

	case prolite_double:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

continuation_t* compile_atomic(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,&builtin_atomic,1,g1);

	case prolite_compound:
		return compile_false(context,cont,goal);

	default:
		return compile_true(context,cont,goal);
	}
}

continuation_t* compile_compound(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,&builtin_compound,1,g1);

	case prolite_compound:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

continuation_t* compile_nonvar(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	if (get_term_type(g1) == prolite_var)
		return compile_builtin(context,cont,&builtin_nonvar,1,g1);

	return compile_true(context,cont,goal);
}

continuation_t* compile_number(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = deref_var(context,get_first_arg(goal,NULL,NULL));
	switch (get_term_type(g1))
	{
	case prolite_var:
		return compile_builtin(context,cont,&builtin_number,1,g1);

	case prolite_int32:
	case prolite_double:
		return compile_true(context,cont,goal);

	default:
		return compile_false(context,cont,goal);
	}
}

static int compile_is_ground(compile_context_t* context, const term_t* goal)
{
	goal = deref_var(context,goal);
	switch (get_term_type(goal))
	{
	case prolite_var:
		return -1;

	case prolite_atom:
		return 1;

	case prolite_compound:
		{
			uint64_t arity;
			for (const term_t* p = get_first_arg(goal,&arity,NULL); arity--; p = get_next_arg(p,NULL))
			{
				int r = compile_is_ground(context,p);
				if (r != 1)
					return r;
			}
		}
		return 1;

	default:
		return 0;
	}
}

continuation_t* compile_ground(compile_context_t* context, continuation_t* cont, const term_t* goal)
{
	const term_t* g1 = get_first_arg(goal,NULL,NULL);
	switch (compile_is_ground(context,g1))
	{
	case 1:
		return compile_true(context,cont,g1);

	case 0:
		return compile_false(context,cont,g1);

	default:
		return compile_builtin(context,cont,&builtin_ground,1,g1);
	}
}
