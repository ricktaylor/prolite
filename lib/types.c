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
	builtin_atom_id_t ret = MAX_BUILTIN_ATOM;
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

term_t* push_string(term_t* stack, prolite_type_t type, const unsigned char* str, size_t len, int external, const debug_info_t* debug_info)
{
	if (debug_info)
	{
		stack = push_debug_info(stack,debug_info);
		type |= prolite_debug_info;
	}

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
		if ((builtin = is_builtin_string(str,len)) != MAX_BUILTIN_ATOM)
		{
			(--stack)->m_u64val = (PACK_TYPE(type) | PACK_MANT_48((UINT64_C(0x4000) << 32) | builtin));
		}
		else if (external)
		{
			(--stack)->m_pval = str;
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

term_t* push_predicate(term_t* stack, uint64_t arity, const unsigned char* functor, size_t functor_len, int external, const debug_info_t* debug_info)
{
	prolite_type_t type = prolite_compound;
	uint32_t builtin;
	if (arity <= MAX_ARITY_EMBED && functor_len <= 5)
	{
		if (debug_info)
		{
			stack = push_debug_info(stack,debug_info);
			type |= prolite_debug_info;
		}

		switch (functor_len)
		{
		case 5:
			(--stack)->m_u64val = PACK_TYPE_EMBED(type,arity,5,functor[0],functor[1],functor[2],functor[3],functor[4]);
			break;

		case 4:
			(--stack)->m_u64val = PACK_TYPE_EMBED(type,arity,4,functor[0],functor[1],functor[2],functor[3],0);
			break;

		case 3:
			(--stack)->m_u64val = PACK_TYPE_EMBED(type,arity,3,functor[0],functor[1],functor[2],0,0);
			break;

		case 2:
			(--stack)->m_u64val = PACK_TYPE_EMBED(type,arity,2,functor[0],functor[1],0,0,0);
			break;

		case 1:
			(--stack)->m_u64val = PACK_TYPE_EMBED(type,arity,1,functor[0],0,0,0,0);
			break;

		default:
			(--stack)->m_u64val = PACK_TYPE_EMBED(type,arity,0,0,0,0,0,0);
			break;
		}
	}
	else if (arity <= MAX_ARITY_BUILTIN && (builtin = is_builtin_string(functor,functor_len)) != MAX_BUILTIN_ATOM)
	{
		if (debug_info)
		{
			stack = push_debug_info(stack,debug_info);
			type |= prolite_debug_info;
		}

		(--stack)->m_u64val = (PACK_TYPE(type) | PACK_MANT_48(((UINT64_C(0x4000) | ((uint16_t)(arity) & MAX_ARITY_BUILTIN)) << 32) | builtin));
	}
	else
	{
		stack = push_string(stack,prolite_atom,functor,functor_len,external,debug_info);
		(--stack)->m_u64val = PACK_TYPE(type) | PACK_MANT_48(arity);
	}

	return stack;
}

static const term_t* unpack_term(const term_t* t, prolite_type_t* type, int* have_debug_info, uint64_t* all48)
{
	uint16_t exp = UNPACK_EXP_16(t->m_u64val);
	if ((exp & 0x7FF0) != 0x7FF0)
	{
		*type = prolite_double;
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

void get_string(const term_t* t, string_t* str, const debug_info_t** debug_info)
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

void get_predicate(const term_t* t, string_t* str, size_t* arity, const debug_info_t** debug_info)
{
	uint64_t all48;
	prolite_type_t type;
	int have_debug_info;
	const term_t* t1 = unpack_term(t,&type,&have_debug_info,&all48);

	if (type == prolite_atom)
	{
		if (arity)
			*arity = 0;
		return get_string(t,str,debug_info);
	}

	assert(type == prolite_compound);

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
			*debug_info = (const debug_info_t*)t1;
		break;

	case 1:
		if (arity)
			*arity = hi16 & MAX_ARITY_BUILTIN;

		*str = s_builtin_strings[(uint32_t)all48];

		if (debug_info && have_debug_info)
			*debug_info = (const debug_info_t*)t1;
		break;

	case 0:
		if (arity)
			*arity = (all48 & MAX_ARITY);

		get_string(t1,str,debug_info);
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

term_t* push_debug_info(term_t* stack, const debug_info_t* debug_info)
{
	// TODO
	return stack;
}

const debug_info_t* get_debug_info(const term_t* t)
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
	int r = (a1->m_u64val == a2->m_u64val);
	if (!r)
	{
		unsigned int t1 = get_term_subtype(a1);
		unsigned int t2 = get_term_subtype(a2);
		if (t1 == 3 || t1 == 0 || t2 == 3 || t2 == 0)
		{
			string_t s1,s2;
			get_string(a1,&s1,NULL);
			get_string(a2,&s2,NULL);

			if (s1.m_len == s2.m_len && memcmp(s1.m_str,s2.m_str,s1.m_len) == 0)
				r = 1;
		}
	}

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

void builtin_type_test(context_t* context, const void* gosub, size_t argc, const term_t* argv[]) 
{
	// TODO:
}

static const term_t* push_term_inner(context_t* context, const term_t* src, int allow_external, uint64_t** var_mapping, size_t* var_count, jmp_buf* jmp)
{
	uint64_t all48;
	prolite_type_t type;
	int have_debug_info;
	const term_t* p = unpack_term(src,&type,&have_debug_info,&all48);
	uint16_t hi16 = (all48 >> 32);

	switch (type)
	{
	case prolite_var:
		{
			const term_t* v = deref_local_var(context,src);
			if (v != src)
				push_term_inner(context,v,allow_external,var_mapping,var_count,jmp);
			else
			{
				size_t i = 0;
				while (i < *var_count && (*var_mapping)[i] != all48)
					++i;

				if (i == *var_count)
				{
					uint64_t* v = heap_realloc(&context->m_heap,var_mapping,(*var_count) * sizeof(uint64_t),(*var_count + 1) * sizeof(uint64_t));
					if (!v)
						longjmp(*jmp,1);
						
					*var_mapping = v;
					(*var_mapping)[(*var_count)++] = all48;
				}

				const debug_info_t* di = NULL;
				if (have_debug_info)
					p = skip_debug_info(p,&di);
				
				context->m_stack = push_var(context->m_stack,i,di);
			}
		}
		return p;

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
				break;
			}

			// Push args
			const term_t* q = p;
			if (arity == 1)
				p = push_term_inner(context,p,allow_external,var_mapping,var_count,jmp);
			else
			{
				const term_t** rev = heap_malloc(&context->m_heap,arity * sizeof(term_t*));
				if (!rev)
					longjmp(*jmp,1);

				for (size_t i = 0; i < arity; ++i)
				{
					rev[i] = p;
					p = get_next_arg(p);
				}
				
				for (size_t i = arity; i--;)
					push_term_inner(context,rev[i],allow_external,var_mapping,var_count,jmp);
				
				heap_free(&context->m_heap,rev,arity * sizeof(term_t*));
			}

			if ((hi16 >> 14) == 0)
				push_term_inner(context,q,allow_external,var_mapping,var_count,jmp);

			context->m_stack -= (q - src);
			memcpy(context->m_stack,src,(q - src) * sizeof(term_t));
		}
		return p;

	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		switch (hi16 >> 14)
		{
		case 3:
			if (!allow_external)
			{
				// Copy the external data

				size_t len = (size_t)(all48 & MAX_ATOM_LEN);
				const unsigned char* str = p->m_pval;

				const debug_info_t* di = NULL;
				if (have_debug_info)
					p = skip_debug_info(p,&di);
				
				context->m_stack = push_string(context->m_stack,type,str,len,0,di);
				return p;
			}
			++p;
			break;

		case 0:
			p += bytes_to_cells(all48 & MAX_ATOM_LEN,sizeof(term_t));
			break;
		}
		break;

	default:
		break;
	}

	if (have_debug_info)
		p = skip_debug_info(p,NULL);
	
	context->m_stack -= (p - src);
	memcpy(context->m_stack,src,(p - src) * sizeof(term_t));
	return p;
}

term_t* push_term(context_t* context, const term_t* src, int allow_external, size_t* var_count)
{
	term_t* sp = context->m_stack;
	size_t heap_start = heap_top(&context->m_heap);
	
	jmp_buf jmp;
	if (!setjmp(jmp))
	{
		size_t vc = 0;
		uint64_t* var_mapping = NULL;
		push_term_inner(context,src,allow_external,&var_mapping,&vc,&jmp);

		if (var_count)
			*var_count = vc;

		sp = context->m_stack;
	}
	else
	{
		context->m_stack = sp;
		sp = NULL;
	}

	heap_reset(&context->m_heap,heap_start);

	return sp;
}

term_t* copy_term(prolite_allocator_t* a, context_t* context, const term_t* src, int allow_external, size_t* var_count)
{
	term_t* sp = context->m_stack;

	size_t vc = 0;
	term_t* dst = push_term(context,src,allow_external,&vc);
	if (dst && dst != sp)
	{
		term_t* cp = allocator_malloc(a,(sp - dst) * sizeof(term_t));
		if (cp)
			memcpy(cp,dst,(sp - dst) * sizeof(term_t));
		
		dst = cp;
	
		if (dst && var_count)
			*var_count = vc;
	}

	context->m_stack = sp;
	return dst;
}
