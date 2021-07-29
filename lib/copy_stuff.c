

static term_t* alloc_heap_term(compile_context_t* context, size_t count, term_t** new_term, size_t* term_count)
{
	term_t* t1 = heap_realloc(&context->m_heap,*new_term,*term_count * sizeof(term_t),(*term_count + count) * sizeof(term_t));
	if (!t1)
		longjmp(context->m_jmp,1);

	*new_term = t1;
	t1 += *term_count;
	*term_count += count;

	return t1;
}

static const term_t* copy_term_to_heap(compile_context_t* context, const term_t* t, size_t count, term_t** new_term, size_t* term_count)
{
	term_t* t1 = alloc_heap_term(context,count,new_term,term_count);
	memcpy(t1,t,count * sizeof(term_t));
	return t + count;
}

static const term_t* copy_term(compile_context_t* context, const term_t* t, term_t** new_term, size_t* term_count)
{
	int have_debug_info = has_debug_info(t);

	switch (get_term_type(t))
	{
	case prolite_var:
		copy_term(context,deref_var(context,t),new_term,term_count);
		t += 1 + have_debug_info;
		break;

	case prolite_double:
	case prolite_int32:
		t = copy_term_to_heap(context,t,1 + have_debug_info,new_term,term_count);
		break;

	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		switch (get_term_subtype(t))
		{
		case 0:
			{
				// Need to externalise the string data
				term_t* t1 = alloc_heap_term(context,2,new_term,term_count);
				(t1++)->m_u64val = (t->m_u64val | PACK_MANT_48(UINT64_C(0xC000) << 32));
				(t1++)->m_u64val = (uint64_t)(t+1);
				
				t += 1 + ((t->m_u64val & MAX_ATOM_LEN) + sizeof(term_t)-1) / sizeof(term_t);
				
				if (have_debug_info)
					t = copy_term_to_heap(context,t,1,new_term,term_count);
			}
			break;

		case 1:
		case 2:
			t = copy_term_to_heap(context,t,1 + have_debug_info,new_term,term_count);
			break;

		case 3:
			t = copy_term_to_heap(context,t,2 + have_debug_info,new_term,term_count);
			break;
		}
		break;
		
	case prolite_compound:
		{
			uint64_t arity = 0;
			uint64_t all48 = UNPACK_MANT_48(t->m_u64val);
			uint16_t hi16 = (all48 >> 32);

			switch (hi16 >> 14)
			{
			case 3:
				assert(0);
				break;

			case 2:
				arity = (hi16 & 0x7800) >> 11;
				t = copy_term_to_heap(context,t,1,new_term,term_count);
				if (have_debug_info)
					t = copy_term_to_heap(context,t,1,new_term,term_count);
				break;
			
			case 1:
				arity = hi16 & MAX_ARITY_BUILTIN;
				t = copy_term_to_heap(context,t,1,new_term,term_count);
				if (have_debug_info)
					t = copy_term_to_heap(context,t,1,new_term,term_count);
				break;
			
			case 0:
				arity = (all48 & MAX_ARITY);
				t = copy_term_to_heap(context,t,1,new_term,term_count);
				t = copy_term(context,t,new_term,term_count);
				break;
			}
			
			while (arity--)
				t = copy_term(context,t,new_term,term_count);			
		}
		break;	

	case prolite_userdata:
		// TODO
		assert(0);
		break;
	}

	return t;
}

static int needs_copy(compile_context_t* context, const term_t* t)
{
	switch (get_term_type(t))
	{
	case prolite_var:
		return (deref_var(context,t) != t);
			
	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		return (get_term_subtype(t) == 0);
		
	case prolite_compound:
		{
			uint64_t arity;
			for (const term_t* p = get_first_arg(t,&arity,NULL); arity--; p = get_next_arg(p,NULL))
			{
				if (needs_copy(context,p))
					return 1;
			}
		}
		break;

	default:
		break;
	}
	return 0;
}

static void push_copy_term(compile_context_t* context, cfg_block_t* b, const term_t* t)
{
	if (needs_copy(context,t))
	{
		term_t* new_term = NULL;
		size_t term_count = 0;
		copy_term(context,t,&new_term,&term_count);

		if (term_count)
		{
			// TODO: Could de-duplicate data blocks here...

			opcode_t* ops = append_opcodes(context,b,3);
			(ops++)->m_opcode = OP_DATA;
			(ops++)->m_u64val = term_count * sizeof(term_t);
			ops->m_pval = new_term;
		}
		t = new_term;
	}

	opcode_t* ops = append_opcodes(context,b,2);
	(ops++)->m_opcode = OP_PUSH_TERM_REF;
	ops->m_pval = t;
}
