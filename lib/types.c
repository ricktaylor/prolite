#include "context.h"

#include <stdlib.h>
#include <string.h>

#undef DECLARE_BUILTIN_STRING
#define DECLARE_BUILTIN_STRING(s) { sizeof(#s)-1,(const unsigned char*)(#s) },

const string_t s_builtin_strings[] =
{
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
	builtin_atom_id_t ret = BUILTIN_ATOM_;
	string_t f = 
	{
		.m_len = len,
		.m_str = str
	};

	string_t* r = bsearch(&f,s_builtin_strings,sizeof(s_builtin_strings) / sizeof(s_builtin_strings[0]),sizeof(s_builtin_strings[0]),&string_compare);
	if (r)
		ret = (builtin_atom_id_t)(r - s_builtin_strings);

	return ret;
}

term_t* push_string(term_t* stack, prolite_type_t type, const unsigned char* str, size_t len, int external)
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
		if ((builtin = is_builtin_string(str,len)) != BUILTIN_ATOM_)
		{
			(--stack)->m_u64val = (PACK_TYPE(type) | PACK_MANT_48((UINT64_C(0x4000) << 32) | builtin));
		}
		else if (external)
		{
			(--stack)->m_pval = (void*)str;
			(--stack)->m_u64val = PACK_TYPE(type) | PACK_MANT_48((UINT64_C(0xC000) << 32) | len);
		}
		else
		{
			stack -= bytes_to_cells(len,sizeof(term_t));
			memcpy(stack,str,len);
			(--stack)->m_u64val = PACK_TYPE(type) | PACK_MANT_48(len);
		}
		break;
	}

	return stack;
}

term_t* push_predicate(term_t* stack, uint64_t arity, const unsigned char* functor, size_t functor_len, int external)
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
	else if (arity <= MAX_ARITY_BUILTIN && (builtin = is_builtin_string(functor,functor_len)) != BUILTIN_ATOM_)
	{
		(--stack)->m_u64val = (PACK_TYPE(prolite_compound) | PACK_MANT_48(((UINT64_C(0x4000) | ((uint16_t)(arity) & MAX_ARITY_BUILTIN)) << 32) | builtin));
	}
	else
	{
		stack = push_string(stack,prolite_atom,functor,functor_len,external);
		(--stack)->m_u64val = PACK_TYPE(prolite_compound) | PACK_MANT_48(arity);
	}

	return stack;
}

void get_string(const term_t* t, string_t* str, const debug_info_t** debug_info)
{
	assert(get_term_type(t) == prolite_atom);
	
	uint64_t all48 = (t++)->m_u64val;
	int have_debug_info = (UNPACK_TYPE(all48) & prolite_debug_info);
	all48 = UNPACK_MANT_48(all48);

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

void get_predicate(const term_t* t, string_t* str, size_t* arity, const debug_info_t** debug_info)
{
	assert(get_term_type(t) == prolite_compound);

	uint64_t all48 = (t++)->m_u64val;
	int have_debug_info = (UNPACK_TYPE(all48) & prolite_debug_info);
	all48 = UNPACK_MANT_48(all48);

	uint16_t hi16 = (all48 >> 32);
	switch (hi16 >> 14)
	{
	case 3:
		assert(0);
		break;

	case 2:
		if (arity)
			*arity = (hi16 & 0x7800) >> 11;

		str->m_len = (hi16 & 0x0700) >> 8;
		str->m_data[0] = all48 >> 32;
		str->m_data[1] = all48 >> 24;
		str->m_data[2] = all48 >> 16;
		str->m_data[3] = all48 >> 8;
		str->m_data[4] = all48;
		str->m_str = str->m_data;

		if (debug_info && have_debug_info)
			*debug_info = (const debug_info_t*)t;
		break;
	
	case 1:
		if (arity)
			*arity = hi16 & MAX_ARITY_BUILTIN;

		*str = s_builtin_strings[(uint32_t)all48];

		if (debug_info && have_debug_info)
			*debug_info = (const debug_info_t*)t;
		break;
	
	case 0:
		if (arity)
			*arity = (all48 & MAX_ARITY);

		get_string(t,str,debug_info);
		break;
	}
}

static const term_t* skip_debug_info(const term_t* t, const debug_info_t** debug_info)
{
	if (debug_info)
		*debug_info = (const debug_info_t*)t;
	
	return t + bytes_to_cells(sizeof(debug_info_t),sizeof(term_t));
}

const term_t* get_next_arg(const term_t* t)
{
	uint64_t all48 = (t++)->m_u64val;
	uint16_t hi16 = UNPACK_TYPE(all48);
	prolite_type_t type = hi16 & 0x7;
	int have_debug_info = (hi16 & prolite_debug_info);
	all48 = UNPACK_MANT_48(all48);
	hi16 = (all48 >> 32);

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
	assert(get_term_type(compound) == prolite_compound);

	uint64_t all48 = (compound++)->m_u64val;
	int have_debug_info = (UNPACK_TYPE(all48) & prolite_debug_info);
	all48 = UNPACK_MANT_48(all48);
	uint16_t hi16 = (all48 >> 32);
	switch (hi16 >> 14)
	{
	case 3:
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

const debug_info_t* get_debug_info(const term_t* t)
{
	const debug_info_t* di = NULL;
	uint64_t all48 = (t++)->m_u64val;
	uint16_t hi16 = UNPACK_TYPE(all48);
	prolite_type_t type = hi16 & 0x7;
	int have_debug_info = (hi16 & prolite_debug_info);
	all48 = UNPACK_MANT_48(all48);
	hi16 = (all48 >> 32);

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
			di = (const debug_info_t*)t;
		break;

	case prolite_compound:
		switch (hi16 >> 14)
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
			di = get_debug_info(t);
			break;
		}
		break;

	default:
		if (have_debug_info)
			di = (const debug_info_t*)t;
		break;
	}

	return di;
}

static int atom_compare(const term_t* a1, const term_t* a2)
{
	unsigned int t1 = get_term_subtype(a1);
	unsigned int t2 = get_term_subtype(a2);

	int r = 0;
	if (t1 == 3 || t1 == 0 || t2 == 3 || t2 == 0)
	{
		string_t s1,s2;
		get_string(a1,&s1,NULL);
		get_string(a2,&s2,NULL);

		if (s1.m_len == s2.m_len && memcmp(s1.m_str,s2.m_str,s1.m_len) == 0)
			r = 1;
	}
	else
		r = (a1->m_u64val == a2->m_u64val);

	return r;
}

static int atom_precedes(const term_t* a1, const term_t* a2)
{
	string_t s1,s2;
	get_string(a1,&s1,NULL);
	get_string(a2,&s2,NULL);

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
		if (get_term_subtype(c1) == 0)
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
	prolite_type_t type1 = get_term_type(c1);
	prolite_type_t type2 = get_term_type(c2);
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
	size_t a1,a2;
	string_t s1,s2;
	get_predicate(c1,&s1,&a1,NULL);
	get_predicate(c2,&s2,&a2,NULL);

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

	case prolite_double:
		return 1;

	case prolite_integer:
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
	prolite_type_t type1 = get_term_type(t1);
	prolite_type_t type2 = get_term_type(t2);
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

		case prolite_double:
			r = (t1->m_dval == t2->m_dval);
			break;
		
		case prolite_integer:	
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
	prolite_type_t type1 = get_term_type(t1);
	prolite_type_t type2 = get_term_type(t2);
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

		case prolite_double:
			// Warning - Here be dragons with epsilon
			// See: https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
			r = (int)(t1->m_dval - t2->m_dval);
			break;
		
		case prolite_integer:
			r = get_integer(t1) - get_integer(t2);
			break;
		
		case prolite_var:
		default:
			r = (t1->m_u64val - t2->m_u64val);
			break;
		}
	}
	return r;
}

static term_t* append_term(prolite_allocator_t* a, const term_t* src, term_t* dst, size_t* old_len, size_t count)
{
	term_t* new_dst = allocator_realloc(a,dst,*old_len + (count * sizeof(term_t)));
	if (new_dst)
	{
		memcpy(new_dst + (*old_len / sizeof(term_t)),src,count * sizeof(term_t));
		*old_len += count * sizeof(term_t);
	}
	return new_dst;
}

static int copy_term_inner(prolite_allocator_t* a, context_t* context, const term_t* src, term_t** dst, size_t* dst_len, const term_t* var_mapping, size_t* var_count)
{
	const term_t* p = src;
	uint64_t all48 = (p++)->m_u64val;
	uint16_t hi16 = UNPACK_TYPE(all48);
	prolite_type_t type = hi16 & 0x7;
	int have_debug_info = (hi16 & prolite_debug_info);
	all48 = UNPACK_MANT_48(all48);
	hi16 = (all48 >> 32);

	switch (type)
	{
	case prolite_var:
		{
			size_t i = 0;
			while (i < *var_count && var_mapping[i].m_u64val != all48)
				++i;

			if (i == *var_count)
			{
				(--context->m_stack)->m_u64val = all48;
				++(*var_count);
			}

			term_t new_var = (term_t){ .m_u64val = PACK_TYPE(prolite_var) | PACK_MANT_48(i) };
			term_t* new_dst = append_term(a,&new_var,*dst,dst_len,1);
			if (!new_dst)
				return 0;
			*dst = new_dst;

			if (have_debug_info)
			{
				src = p;
				p = skip_debug_info(p,NULL);
				new_dst = append_term(a,p,*dst,dst_len,src - p);
				if (!new_dst)
					return 0;
				*dst = new_dst;
			}
		}
		return 1;

	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		switch (hi16 >> 14)
		{
		case 3:
			++p;
			break;

		case 0:
			p += bytes_to_cells(all48 & MAX_ATOM_LEN,sizeof(term_t));
			break;
		}
		if (have_debug_info)
			p = skip_debug_info(p,NULL);
		break;

	case prolite_compound:
		{
			size_t arity;
			switch (hi16 >> 14)
			{
			case 3:
				assert(0);
				break;

			case 2:
				arity = (hi16 & 0x7800) >> 11;
				if (have_debug_info)
					p = skip_debug_info(p,NULL);
				break;
			
			case 1:
				arity = hi16 & MAX_ARITY_BUILTIN;
				if (have_debug_info)
					p = skip_debug_info(p,NULL);
				break;
			
			case 0:
				arity = (all48 & MAX_ARITY);
				p = get_next_arg(p);
				break;
			}

			term_t* new_dst = append_term(a,src,*dst,dst_len,p - src);
			if (!new_dst)
				return 0;
			*dst = new_dst;
			
			/* Skip args */
			while (arity--)
			{
				p = get_next_arg(p);
				if (!copy_term_inner(a,context,deref_local_var(context,p),dst,dst_len,var_mapping,var_count))
					return 0;
			}
		}
		return 1;

	default:
		if (have_debug_info)
			p = skip_debug_info(p,NULL);
		break;
	}

	term_t* new_dst = append_term(a,src,*dst,dst_len,p - src);
	if (!new_dst)
		return 0;
	*dst = new_dst;
	return 1;
}

term_t* copy_term(prolite_allocator_t* a, context_t* context, const term_t* t, size_t* var_count)
{
	term_t* sp = context->m_stack;

	size_t vc = 0;
	size_t dst_len = 0;
	term_t* dst = NULL;
	if (!copy_term_inner(a,context,deref_local_var(context,t),&dst,&dst_len,context->m_stack,&vc))
	{
		allocator_free(a,dst);
		dst = NULL;
	}
	else if (var_count)
		*var_count = vc;

	context->m_stack = sp;

	return dst;
}
