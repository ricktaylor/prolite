#include "context.h"

#include <stdlib.h>
#include <string.h>
#include <math.h>

#undef DECLARE_BUILTIN_STRING
#define DECLARE_BUILTIN_STRING(s) { sizeof(#s)-1,(const unsigned char*)(#s) },

const string_t s_builtin_strings[] = {
#include "builtin_strings.h"
	{ 0, NULL }
};

static int string_compare(const void* p1, const void* p2)
{
	const string_t* s1 = p1;
	const string_t* s2 = p2;

	int r = (s1->m_len - s2->m_len);
	if (r == 0 && s1->m_len)
		r = memcmp(s1->m_str,s2->m_str,s1->m_len);

	return r;
}

static builtin_atom_id_t is_builtin_string(const unsigned char* str, size_t len)
{
	builtin_atom_id_t ret = MAX_BUILTIN_ATOM;
	string_t f = {
		.m_len = len,
		.m_str = str
	};

	string_t* r = bsearch(&f,s_builtin_strings,sizeof(s_builtin_strings) / sizeof(s_builtin_strings[0]),sizeof(s_builtin_strings[0]),&string_compare);
	if (r)
		ret = (builtin_atom_id_t)(r - s_builtin_strings);

	return ret;
}

term_t* emit_buffer_append(emit_buffer_t* out, size_t count)
{
	term_t* r;
	if (out->m_count + count > out->m_alloc)
	{
		size_t new_count = out->m_count + count;

		r = allocator_realloc(out->m_a,out->m_buf,new_count * sizeof(term_t));
		if (!r)
			return NULL;

		out->m_alloc = new_count;
		out->m_buf = r;
	}

	r = out->m_buf + out->m_count;
	out->m_count += count;

	return r;
}

static term_t* emit_debug_info(emit_buffer_t* out, const debug_info_t* debug_info)
{
	// TODO
	return out->m_buf + out->m_count;
}

static void emit_string_type(term_t* out, prolite_type_t type, const unsigned char* str, size_t len, builtin_atom_id_t builtin)
{
	switch (len)
	{
	case 5:
		out[0].m_u64val = PACK_TYPE_EMBED(type,0,5,str[0],str[1],str[2],str[3],str[4]);
		break;

	case 4:
		out[0].m_u64val = PACK_TYPE_EMBED(type,0,4,str[0],str[1],str[2],str[3],0);
		break;

	case 3:
		out[0].m_u64val = PACK_TYPE_EMBED(type,0,3,str[0],str[1],str[2],0,0);
		break;

	case 2:
		out[0].m_u64val = PACK_TYPE_EMBED(type,0,2,str[0],str[1],0,0,0);
		break;

	case 1:
		out[0].m_u64val = PACK_TYPE_EMBED(type,0,1,str[0],0,0,0,0);
		break;

	case 0:
		out[0].m_u64val = PACK_TYPE_EMBED(type,0,0,0,0,0,0,0);
		break;

	default:
		if (builtin != MAX_BUILTIN_ATOM)
			out[0].m_u64val = (PACK_TYPE(type) | PACK_MANT_48((UINT64_C(0x4000) << 32) | builtin));
		else
		{
			out[0].m_u64val = PACK_TYPE(type) | PACK_MANT_48((UINT64_C(0xC000) << 32) | len);
			out[1].m_pval = str;
		}
		break;
	}
}

void pack_string(term_t out[2], const unsigned char* str, size_t len)
{
	assert(len <= MAX_ATOM_LEN);

	builtin_atom_id_t builtin = MAX_BUILTIN_ATOM;
	if (len > 5)
		builtin = is_builtin_string(str,len);

	emit_string_type(out,prolite_atom,str,len,builtin);
}

const term_t* emit_string(emit_buffer_t* out, prolite_type_t type, const unsigned char* str, size_t len, int external, const debug_info_t* debug_info)
{
	size_t offset = out->m_count;
	term_t* r = NULL;

	if (debug_info)
		type |= prolite_debug_info;

	builtin_atom_id_t builtin = MAX_BUILTIN_ATOM;
	if (len <= 5 || (builtin = is_builtin_string(str,len)) != MAX_BUILTIN_ATOM)
	{
		if ((r = emit_buffer_append(out,1)))
			emit_string_type(r,type,str,len,builtin);
	}
	else if (len <= MAX_ATOM_LEN)
	{
		if (external)
		{
			if ((r = emit_buffer_append(out,2)))
				emit_string_type(r,type,str,len,builtin);
		}
		else
		{
			if (out->m_pool)
			{
				// TODO - String re-use
			}

			if ((r = emit_buffer_append(out,1 + bytes_to_cells(len,sizeof(term_t)))))
			{
				r->m_u64val = PACK_TYPE(type) | PACK_MANT_48(len);
				memcpy(r+1,str,len);
			}
		}
	}

	if (r && debug_info)
		r = emit_debug_info(out,debug_info);

	return (r ? out->m_buf + offset : NULL);
}

const term_t* emit_predicate(emit_buffer_t* out, size_t arity, const unsigned char* functor, size_t functor_len, int external, const debug_info_t* debug_info)
{
	if (arity == 0)
		return emit_string(out,prolite_atom,functor,functor_len,external,debug_info);

	size_t offset = out->m_count;
	term_t* r = emit_buffer_append(out,1);
	if (r)
	{
		prolite_type_t type = prolite_compound;
		builtin_atom_id_t builtin;
		if (arity <= MAX_ARITY_EMBED && functor_len <= 5)
		{
			if (debug_info)
				type |= prolite_debug_info;

			switch (functor_len)
			{
			case 5:
				r->m_u64val = PACK_TYPE_EMBED(type,arity,5,functor[0],functor[1],functor[2],functor[3],functor[4]);
				break;

			case 4:
				r->m_u64val = PACK_TYPE_EMBED(type,arity,4,functor[0],functor[1],functor[2],functor[3],0);
				break;

			case 3:
				r->m_u64val = PACK_TYPE_EMBED(type,arity,3,functor[0],functor[1],functor[2],0,0);
				break;

			case 2:
				r->m_u64val = PACK_TYPE_EMBED(type,arity,2,functor[0],functor[1],0,0,0);
				break;

			case 1:
				r->m_u64val = PACK_TYPE_EMBED(type,arity,1,functor[0],0,0,0,0);
				break;

			default:
				r->m_u64val = PACK_TYPE_EMBED(type,arity,0,0,0,0,0,0);
				break;
			}

			if (debug_info)
				r = emit_debug_info(out,debug_info);
		}
		else if (arity <= MAX_ARITY_BUILTIN && (builtin = is_builtin_string(functor,functor_len)) != MAX_BUILTIN_ATOM)
		{
			if (debug_info)
				type |= prolite_debug_info;

			r->m_u64val = (PACK_TYPE(type) | PACK_MANT_48((UINT64_C(0x4000) << 32) | ((arity & MAX_ARITY_BUILTIN) << 32) | builtin));

			if (debug_info)
				r = emit_debug_info(out,debug_info);
		}
		else if (arity <= MAX_ARITY)
		{
			r->m_u64val = PACK_TYPE(type) | PACK_MANT_48(arity);
			if (!emit_string(out,prolite_atom,functor,functor_len,external,debug_info))
				r = NULL;
		}
		else
			r = NULL;
	}

	return (r ? out->m_buf + offset : NULL);
}

const term_t* emit_number(emit_buffer_t* out, double d, const debug_info_t* debug_info)
{
	size_t offset = out->m_count;
	term_t* r = NULL;

	/*if (debug_info)
	{
		// TODO !

		prolite_type_t type = prolite_integer;

		type |= prolite_debug_info;
		(--stack)->m_u64val = PACK_TYPE(type) | PACK_MANT_48(i);

		emit_debug_info(stack,debug_info);
	}*/

	if ((r = emit_buffer_append(out,1)))
		r->m_dval = d;

	return (r ? out->m_buf + offset : NULL);
}

const term_t* emit_var(emit_buffer_t* out, size_t idx, const debug_info_t* debug_info)
{
	size_t offset = out->m_count;
	term_t* r = NULL;

	if (idx <= MAX_VAR_INDEX && (r = emit_buffer_append(out,1)))
	{
		prolite_type_t type = prolite_var;
		if (debug_info)
			type |= prolite_debug_info;

		r->m_u64val = PACK_TYPE(type) | PACK_MANT_48(idx);

		if (debug_info)
			r = emit_debug_info(out,debug_info);
	}

	return (r ? out->m_buf + offset : NULL);
}

static const term_t* unpack_term(const term_t* t, prolite_type_t* type, int* have_debug_info, uint64_t* all48)
{
	uint16_t exp = UNPACK_EXP_16(t->m_u64val);
	if ((exp & 0x7FF0) != 0x7FF0)
	{
		*type = prolite_number;
		*have_debug_info = 0;
		*all48 = 0;
	}
	else
	{
		*type = exp & 0x7;
		*have_debug_info = (exp & prolite_debug_info);
		*all48 = UNPACK_MANT_48(t->m_u64val);
	}
	return t+1;
}

void unpack_string(const term_t* t, string_t* str, const debug_info_t** debug_info)
{
	uint64_t all48;
	prolite_type_t type;
	int have_debug_info;
	t = unpack_term(t,&type,&have_debug_info,&all48);

	assert(type == prolite_atom);

	uint16_t hi16 = (all48 >> 32);
	switch (hi16 >> 14)
	{
	case 3:
		str->m_len = (size_t)(all48 & MAX_ATOM_LEN);
		str->m_str = (t++)->m_pval;
		break;

	case 2:
		str->m_len = (hi16 & 0x0700) >> 8;
		str->m_data[0] = all48 >> 32;
		str->m_data[1] = all48 >> 24;
		str->m_data[2] = all48 >> 16;
		str->m_data[3] = all48 >> 8;
		str->m_data[4] = all48;
		str->m_str = str->m_data;
		break;

	case 1:
		*str = s_builtin_strings[(uint32_t)all48];
		break;

	case 0:
		str->m_len = (size_t)(all48 & MAX_ATOM_LEN);
		str->m_str = (const unsigned char*)t;
		t += bytes_to_cells(str->m_len,sizeof(term_t));
		break;
	}

	if (debug_info && have_debug_info)
		*debug_info = (const debug_info_t*)t;
}

size_t unpack_predicate(const term_t* t, string_t* str, const debug_info_t** debug_info)
{
	uint64_t all48;
	prolite_type_t type;
	int have_debug_info;
	const term_t* t1 = unpack_term(t,&type,&have_debug_info,&all48);

	if (type == prolite_atom)
	{
		unpack_string(t,str,debug_info);
		return 0;
	}

	assert(type == prolite_compound);

	uint16_t hi16 = (all48 >> 32);
	switch (hi16 >> 14)
	{
	case 3:
	default:
		assert(0);
		return 0;

	case 2:
		str->m_len = (hi16 & 0x0700) >> 8;
		str->m_data[0] = all48 >> 32;
		str->m_data[1] = all48 >> 24;
		str->m_data[2] = all48 >> 16;
		str->m_data[3] = all48 >> 8;
		str->m_data[4] = all48;
		str->m_str = str->m_data;

		if (debug_info && have_debug_info)
			*debug_info = (const debug_info_t*)t1;

		return (hi16 & 0x7800) >> 11;

	case 1:
		*str = s_builtin_strings[(uint32_t)all48];

		if (debug_info && have_debug_info)
			*debug_info = (const debug_info_t*)t1;

		return hi16 & MAX_ARITY_BUILTIN;

	case 0:
		unpack_string(t1,str,debug_info);
		return all48 & MAX_ARITY;
	}
}

double unpack_number(const term_t* t, const debug_info_t** debug_info)
{
	uint16_t exp = UNPACK_EXP_16(t->m_u64val);
	if ((exp & 0x7FF0) != 0x7FF0)
		return t->m_dval;

	assert(0);
	return 0.0;
}

static const term_t* skip_debug_info(const term_t* t, const debug_info_t** debug_info)
{
	if (debug_info)
		*debug_info = (const debug_info_t*)t;

	return t + bytes_to_cells(sizeof(debug_info_t),sizeof(term_t));
}

const debug_info_t* unpack_debug_info(const term_t* t)
{
	const debug_info_t* di = NULL;
	uint64_t all48;
	prolite_type_t type;
	int have_debug_info;
	t = unpack_term(t,&type,&have_debug_info,&all48);
	uint16_t sub_type = (all48 >> 46);

	switch (type)
	{
	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		switch (sub_type)
		{
		case 3:
			++t;
			break;

		case 0:
			t += bytes_to_cells(all48 & MAX_ATOM_LEN,sizeof(term_t));
			break;
		}
		if (have_debug_info)
			di = (const debug_info_t*)t;
		break;

	case prolite_compound:
		switch (sub_type)
		{
		case 3:
			assert(0);
			break;

		case 2:
			if (have_debug_info)
				di = (const debug_info_t*)t;
			break;

		case 1:
			if (have_debug_info)
				di = (const debug_info_t*)t;
			break;

		case 0:
			di = unpack_debug_info(t);
			break;
		}
		break;

	case prolite_var:
		if (have_debug_info)
			di = (const debug_info_t*)t;
		break;

	default:
		// TODO;
		break;
	}

	return di;
}

const term_t* get_next_arg(const term_t* t)
{
	uint64_t all48;
	prolite_type_t type;
	int have_debug_info;
	t = unpack_term(t,&type,&have_debug_info,&all48);
	uint16_t hi16 = (all48 >> 32);

	switch (type)
	{
	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		switch (hi16 >> 14)
		{
		case 3:
			++t;
			break;

		case 0:
			t += bytes_to_cells(all48 & MAX_ATOM_LEN,sizeof(term_t));
			break;
		}
		if (have_debug_info)
			t = skip_debug_info(t,NULL);
		break;

	case prolite_compound:
		{
			size_t arity;
			switch (hi16 >> 14)
			{
			case 3:
				arity = 0;
				assert(0);
				break;

			case 2:
				arity = (hi16 & 0x7800) >> 11;
				if (have_debug_info)
					t = skip_debug_info(t,NULL);
				break;

			case 1:
				arity = hi16 & MAX_ARITY_BUILTIN;
				if (have_debug_info)
					t = skip_debug_info(t,NULL);
				break;

			case 0:
				arity = (all48 & MAX_ARITY);
				t = get_next_arg(t);
				break;
			}

			/* Skip args */
			while (arity--)
				t = get_next_arg(t);
		}
		break;

	default:
		if (have_debug_info)
			t = skip_debug_info(t,NULL);
		break;
	}

	return t;
}

const term_t* get_first_arg(const term_t* compound, size_t* arity)
{
	uint64_t all48;
	prolite_type_t type;
	int have_debug_info;
	compound = unpack_term(compound,&type,&have_debug_info,&all48);

	assert(type == prolite_compound);

	uint16_t hi16 = (all48 >> 32);
	switch (hi16 >> 14)
	{
	case 3:
		if (arity)
			*arity = 0;
		assert(0);
		break;

	case 2:
		if (arity)
			*arity = (hi16 & 0x7800) >> 11;

		if (have_debug_info)
			compound = skip_debug_info(compound,NULL);
		break;

	case 1:
		if (arity)
			*arity = hi16 & MAX_ARITY_BUILTIN;

		if (have_debug_info)
			compound = skip_debug_info(compound,NULL);
		break;

	case 0:
		if (arity)
			*arity = (all48 & MAX_ARITY);

		// Functor is next
		compound = get_next_arg(compound);
		break;
	}

	return compound;
}

static int atom_compare(const term_t* a1, const term_t* a2)
{
	int r = (a1->m_u64val == a2->m_u64val);
	if (!r)
	{
		unsigned int t1 = unpack_term_subtype(a1);
		unsigned int t2 = unpack_term_subtype(a2);
		if (t1 == 3 || t1 == 0 || t2 == 3 || t2 == 0)
		{
			string_t s1,s2;
			unpack_string(a1,&s1,NULL);
			unpack_string(a2,&s2,NULL);

			if (s1.m_len == s2.m_len && memcmp(s1.m_str,s2.m_str,s1.m_len) == 0)
				r = 1;
		}
	}

	return r;
}

static int atom_precedes(const term_t* a1, const term_t* a2)
{
	string_t s1,s2;
	unpack_string(a1,&s1,NULL);
	unpack_string(a2,&s2,NULL);

	int r = memcmp(s1.m_str,s2.m_str,s1.m_len < s2.m_len ? s1.m_len : s2.m_len);
	if (r == 0)
		r = s1.m_len - s2.m_len;

	return r;
}

static int functor_compare(const term_t* c1, const term_t* c2)
{
	int r = 0;

	if (c1->m_u64val == c2->m_u64val)
	{
		if (unpack_term_subtype(c1) == 0)
		{
			// Check functors
			r = atom_compare(c1+1,c2+1);
		}
		else
			r = 1;
	}

	return r;
}

int predicate_compare(const term_t* c1, const term_t* c2)
{
	prolite_type_t type1 = unpack_term_type(c1);
	prolite_type_t type2 = unpack_term_type(c2);
	if (type1 == type2)
	{
		switch (type1)
		{
		case prolite_atom:
			return atom_compare(c1,c2);

		case prolite_compound:
			return functor_compare(c1,c2);

		default:
			break;
		}
	}
	return 0;
}

static int compound_precedes(const term_t* c1, const term_t* c2)
{
	string_t s1,s2;
	size_t a1 = unpack_predicate(c1,&s1,NULL);
	size_t a2 = unpack_predicate(c2,&s2,NULL);

	int r = (a1 - a2);
	if (r == 0)
	{
		r = memcmp(s1.m_str,s2.m_str,s1.m_len < s2.m_len ? s1.m_len : s2.m_len);
		if (r == 0)
			r = s1.m_len - s2.m_len;
	}
	return r;
}

static int type_precedes(prolite_type_t t)
{
	switch (t)
	{
	case prolite_var:
		return 0;

	case prolite_number:
		return 1;

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
	prolite_type_t type1 = unpack_term_type(t1);
	prolite_type_t type2 = unpack_term_type(t2);
	if (type1 != type2)
	{
		if (type1 == prolite_chars ||
			type1 == prolite_charcodes ||
			type1 == prolite_compound)
		{
			if (type2 == prolite_chars ||
				type2 == prolite_charcodes ||
				type2 == prolite_compound)
			{
				// Try again as compound
				type1 = prolite_compound;
				type2 = prolite_compound;
			}
		}
	}

	if (type1 == type2)
	{
		switch (type1)
		{
		case prolite_atom:
			r = atom_compare(t1,t2);
			break;

		case prolite_compound:
			if ((r = functor_compare(t1,t2)))
			{
				size_t arity;
				const term_t* p1 = get_first_arg(t1,&arity);
				const term_t* p2 = get_first_arg(t2,NULL);
				while (arity--)
				{
					if (!(r = term_compare(p1,p2)))
						break;

					p1 = get_next_arg(p1);
					p2 = get_next_arg(p2);
				}
			}
			break;

		case prolite_number:
			r = (t1->m_dval == t2->m_dval);
			break;

		case prolite_var:
		default:
			r = (t1->m_u64val == t2->m_u64val);
			break;
		}
	}
	return r;
}

int term_precedes(const term_t* t1, const term_t* t2)
{
	prolite_type_t type1 = unpack_term_type(t1);
	prolite_type_t type2 = unpack_term_type(t2);
	int r = (type_precedes(type1) - type_precedes(type2));
	if (r != 0)
	{
		if (type1 == prolite_chars ||
			type1 == prolite_charcodes ||
			type1 == prolite_compound)
		{
			if (type2 == prolite_chars ||
				type2 == prolite_charcodes ||
				type2 == prolite_compound)
			{
				// Try again as compound
				type1 = prolite_compound;
				type2 = prolite_compound;
				r = 0;
			}
		}
	}

	if (r == 0)
	{
		switch (type1)
		{
		case prolite_atom:
			r = atom_precedes(t1,t2);
			break;

		case prolite_compound:
			r = compound_precedes(t1,t2);
			if (r == 0)
			{
				size_t arity;
				const term_t* p1 = get_first_arg(t1,&arity);
				const term_t* p2 = get_first_arg(t2,NULL);
				while (arity--)
				{
					r = term_precedes(p1,p2);
					if (r != 0)
						break;

					p1 = get_next_arg(p1);
					p2 = get_next_arg(p2);
				}
			}
			break;

		case prolite_number:
			r = isgreater(t1->m_dval,t2->m_dval);
			if (!r)
				r = -isless(t1->m_dval,t2->m_dval);
			break;

		case prolite_var:
		default:
			r = (t1->m_u64val - t2->m_u64val);
			break;
		}
	}
	return r;
}

PROLITE_EXPORT void prolite_builtin_type_test(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	// TODO:
	assert(0);
}

PROLITE_EXPORT void prolite_builtin_callable(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	// TODO:
	assert(0);
}

PROLITE_EXPORT void prolite_builtin_ground(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	// TODO:
	assert(0);
}

static const term_t* copy_string(emit_buffer_t* out, const term_t* src, int shallow, prolite_type_t type, uint64_t all48, int have_debug_info, jmp_buf* jmp)
{
	term_t* r = emit_buffer_append(out,1);
	if (!r)
		longjmp(*jmp,1);

	r->m_u64val = src->m_u64val;
	const term_t* n = src + 1;

	unsigned int sub_type = (all48 >> 32) >> 14;
	if (sub_type == 0)
	{
		size_t len = (size_t)(all48 & MAX_ATOM_LEN);
		size_t cells = bytes_to_cells(len,sizeof(term_t));

		const unsigned char* str = (const unsigned char*)n;
		n += cells;

		if (out->m_pool)
		{
			// TODO - String re-use
		}

		if (!(r = emit_buffer_append(out,cells)))
			longjmp(*jmp,1);

		memcpy(r,str,len);
	}
	else if (sub_type == 3)
	{
		++n;

		if (!(r = emit_buffer_append(out,1)))
			longjmp(*jmp,1);

		r->m_pval = src->m_pval;
	}

	if (have_debug_info)
	{
		const debug_info_t* di = NULL;
		n = skip_debug_info(n,&di);

		if (!emit_debug_info(out,di))
			longjmp(*jmp,1);
	}

	return n;
}

static void copy_term_inner(context_t* context, emit_buffer_t* out, const term_t* src, int shallow, int deref, uint64_t** var_mapping, size_t* var_count, jmp_buf* jmp)
{
	uint64_t all48;
	prolite_type_t type;
	int have_debug_info;
	const term_t* n = unpack_term(src,&type,&have_debug_info,&all48);

	switch (type)
	{
	case prolite_var:
		{
			const term_t* v = src;
			if (deref)
				v = deref_local_var(context,src);
			if (v != src)
				copy_term_inner(context,out,v,shallow,deref,var_mapping,var_count,jmp);
			else
			{
				size_t idx = unpack_var_index(src);
				size_t i = 0;
				while (i < *var_count && (*var_mapping)[i] != idx)
					++i;

				if (i == *var_count)
				{
					uint64_t* v = heap_realloc(&context->m_heap,*var_mapping,(*var_count) * sizeof(uint64_t),(*var_count + 1) * sizeof(uint64_t));
					if (!v)
						longjmp(*jmp,1);

					*var_mapping = v;
					(*var_mapping)[(*var_count)++] = idx;
				}

				if (!emit_var(out,i,unpack_debug_info(src)))
					longjmp(*jmp,1);
			}
		}
		break;

	case prolite_compound:
		{
			term_t* r = emit_buffer_append(out,1);
			if (!r)
				longjmp(*jmp,1);

			r->m_u64val = src->m_u64val;

			size_t arity = 0;
			unsigned int sub_type = (all48 >> 32) >> 14;

			if (sub_type == 0)
			{
				arity = all48 & MAX_ARITY;

				unpack_term(n,&type,&have_debug_info,&all48);
				n = copy_string(out,n,shallow,type,all48,have_debug_info,jmp);
			}
			else
			{
				unsigned int hi16 = (all48 >> 32);
				if (sub_type == 2)
					arity = (hi16 & 0x7800) >> 11;
				else
					arity = hi16 & MAX_ARITY_BUILTIN;

				if (have_debug_info)
				{
					const debug_info_t* di = NULL;
					n = skip_debug_info(n,&di);

					if (!emit_debug_info(out,di))
						longjmp(*jmp,1);
				}
			}

			for (; arity--; n = get_next_arg(n))
				copy_term_inner(context,out,n,shallow,deref,var_mapping,var_count,jmp);
		}
		break;

	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		{
			const debug_info_t* di = NULL;
			if (have_debug_info)
				skip_debug_info(n,&di);

			copy_string(out,src,shallow,type,all48,have_debug_info,jmp);
		}
		break;

	case prolite_number:
		{
			const debug_info_t* di = NULL;
			double dVal = unpack_number(src,&di);
			if (!emit_number(out,dVal,di))
				longjmp(*jmp,1);
		}
		break;
	}
}

term_t* copy_term(context_t* context, prolite_allocator_t* a, const term_t* src, int shallow, int deref, size_t* var_count)
{
	emit_buffer_t out = { .m_a = a };
	size_t heap_start = heap_top(&context->m_heap);

	jmp_buf jmp;
	if (!setjmp(jmp))
	{
		size_t vc = 0;
		uint64_t* var_mapping = NULL;
		copy_term_inner(context,&out,src,shallow,deref,&var_mapping,&vc,&jmp);

		if (var_count)
			*var_count = vc;
	}
	else
		out.m_buf = allocator_free(a,out.m_buf);

	heap_reset(&context->m_heap,heap_start);

	return out.m_buf;
}
