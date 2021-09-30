#include "context.h"
#include "fnv1a.h"

#include <string.h>

typedef struct dynamic_operator
{
	term_t*    m_name;
	operator_t m_prefix;
	operator_t m_other;
} dynamic_operator_t;

static uint64_t operator_key(const term_t* name, int* is_sub_tree)
{
	assert(get_term_type(name) == prolite_atom);

	uint64_t key = name->m_u64val;
	unsigned int sub_type = get_term_subtype(name);
	if (sub_type == 0 || sub_type == 3)
	{
		string_t s;
		get_string(name,&s,NULL);

		if (s.m_len <= 8)
		{
			union short_str_key
			{
				unsigned char m_chars[8];
				uint64_t      m_u64val;
			} sk = {0};
			memcpy(sk.m_chars,s.m_str,s.m_len);

			key = sk.m_u64val;
		}
		else
			key = fnv1a_64(s.m_str,s.m_len);

		*is_sub_tree = 1;
	}
	else
		*is_sub_tree = 0;

	return key;
}

static const dynamic_operator_t* op_map_lookup(const btree_t* om, const term_t* name)
{
	int is_sub_tree = 0;
	uint64_t key = operator_key(name,&is_sub_tree);

	void* p = btree_lookup(om,name->m_u64val);
	if (p && is_sub_tree)
		p = btree_lookup(&(btree_t){ .m_allocator = om->m_allocator, .m_root = p},key);

	return p;
}

static const operator_t* static_op(const term_t* name)
{
	static const operator_t s_builtins[] =
	{
		/* 0 */ { eXFX, 1200 },
		/* 1 */ { eXFY, 1100 },
		/* 2 */ { eXFY, 1050 },
		/* 3 */ { eXFY, 1000 },
		/* 4 */ { eXFX, 700 },
		/* 5 */ { eXFY, 600 },
		/* 6 */ { eYFX, 500 },
		/* 7 */ { eYFX, 400 },
		/* 8 */ { eXFX, 200 },
		/* 9 */ { eXFY, 200 },
	};

	switch (name->m_u64val)
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
	case PACK_ATOM_EMBED_1('<'):
	case PACK_ATOM_EMBED_1('>'):
	case PACK_ATOM_EMBED_2('\\','='):
	case PACK_ATOM_EMBED_2('=','='):
	case PACK_ATOM_EMBED_2('=','<'):
	case PACK_ATOM_EMBED_2('@','<'):
	case PACK_ATOM_EMBED_2('@','>'):
	case PACK_ATOM_EMBED_2('i','s'):
	case PACK_ATOM_EMBED_2('>','='):
	case PACK_ATOM_EMBED_3('\\','=','='):
	case PACK_ATOM_EMBED_3('@','=','<'):
	case PACK_ATOM_EMBED_3('@','>','='):
	case PACK_ATOM_EMBED_3('=','.','.'):
	case PACK_ATOM_EMBED_3('=',':','='):
	case PACK_ATOM_EMBED_3('=','\\','='):
		return &s_builtins[4];

	case PACK_ATOM_EMBED_1(':'):
		return &s_builtins[5];

	case PACK_ATOM_EMBED_1('+'):
	case PACK_ATOM_EMBED_1('-'):
	case PACK_ATOM_EMBED_2('\\','/'):
	case PACK_ATOM_EMBED_2('/','\\'):
		return &s_builtins[6];

	case PACK_ATOM_EMBED_1('*'):
	case PACK_ATOM_EMBED_1('/'):
	case PACK_ATOM_EMBED_2('>','>'):
	case PACK_ATOM_EMBED_2('/','/'):
	case PACK_ATOM_EMBED_2('<','<'):
	case PACK_ATOM_EMBED_3('r','e','m'):
	case PACK_ATOM_EMBED_3('m','o','d'):
	case PACK_ATOM_EMBED_3('d','i','v'):
		return &s_builtins[7];

	case PACK_ATOM_EMBED_2('*','*'):
		return &s_builtins[8];

	case PACK_ATOM_EMBED_1('^'):
		return &s_builtins[9];

	default:
		break;
	}

	return NULL;
}

static const operator_t* static_prefix_op(const term_t* name)
{
	static const operator_t s_builtins[] =
	{
		/* 0 */ { eFX, 1200 },
		/* 1 */ { eFY, 900 },
		/* 2 */ { eFY, 200 },
	};

	switch (name->m_u64val)
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
		break;
	}

	return NULL;
}

/* Try to find a infix/postfix op, otherwise find prefix */
const operator_t* lookup_op(context_t* context, const btree_t* ops, const unsigned char* name, size_t name_len)
{
	term_t* sp = context->m_stack;
	context->m_stack = push_string(context->m_stack,prolite_atom,name,name_len,1);

	const operator_t* op = NULL;
	const dynamic_operator_t* dyn_op = ops ? op_map_lookup(ops,context->m_stack) : NULL;
	if (dyn_op && dyn_op->m_other.m_precedence)
		op = &dyn_op->m_other;
	else
		op = static_op(context->m_stack);

	if (!op)
	{
		if (dyn_op && dyn_op->m_prefix.m_precedence)
			op = &dyn_op->m_prefix;
		else
			op = static_prefix_op(context->m_stack);
	}

	context->m_stack = sp;	
	return op;
}

/* Try to find a prefix op, otherwise find infix/suffix */
const operator_t* lookup_prefix_op(context_t* context, const btree_t* ops, const unsigned char* name, size_t name_len)
{
	term_t* sp = context->m_stack;
	context->m_stack = push_string(context->m_stack,prolite_atom,name,name_len,1);

	const operator_t* op = NULL;
	const dynamic_operator_t* dyn_op = ops ? op_map_lookup(ops,context->m_stack) : NULL;
	if (dyn_op && dyn_op->m_prefix.m_precedence)
		op = &dyn_op->m_prefix;
	else
		op = static_prefix_op(context->m_stack);

	if (!op)
	{
		if (dyn_op && dyn_op->m_other.m_precedence)
			op = &dyn_op->m_other;
		else
			op = static_op(context->m_stack);
	}
	
	context->m_stack = sp;	
	return op;
}

static void remove_operator(operator_table_t* ops, operator_specifier_t specifier, const term_t* name)
{
	int is_sub_tree = 0;
	uint64_t key = operator_key(name,&is_sub_tree);

	dynamic_operator_t* curr_op;
	void* p = btree_lookup(ops,name->m_u64val);
	if (p && is_sub_tree)
		curr_op = btree_lookup(&(btree_t){ .m_allocator = ops->m_allocator, .m_root = p},key);
	else
		curr_op = p;

	if (curr_op)
	{
		if (specifier == eFX || specifier == eFY)
			curr_op->m_prefix.m_precedence = 0;
		else
		{
			if (curr_op->m_other.m_precedence)
			{
				int new_postfix = (specifier == eXF || specifier == eYF);
				int curr_postfix = (curr_op->m_other.m_specifier == eXF || curr_op->m_other.m_specifier == eYF);

				// Get out early - mismatched 'operator class'
				if (new_postfix != curr_postfix)
					return;
			}
			
			curr_op->m_other.m_precedence = 0;
		}

		if (!curr_op->m_prefix.m_precedence && !curr_op->m_other.m_precedence)
		{
			if (is_sub_tree)
				btree_remove(&(btree_t){ .m_allocator = ops->m_allocator, .m_root = p},key);
			else
				btree_remove(ops,name->m_u64val);

			allocator_free(ops->m_allocator,curr_op->m_name);
			allocator_free(ops->m_allocator,curr_op);
		}
	}
}

static int update_operator(context_t* context, operator_table_t* ops, int64_t precendence, operator_specifier_t specifier, const term_t* name)
{
	// return -1 on OOM, 0 on failure, 1 on success

	if (precendence == 0)
	{
		remove_operator(ops,specifier,name);
		return 1;
	}

	dynamic_operator_t* new_op = allocator_malloc(ops->m_allocator,sizeof(dynamic_operator_t));
	if (!new_op)
		return -1;
	
	*new_op = (dynamic_operator_t){ .m_name = (term_t*)name };

	// Only copy the name if we need to
	term_t* name_copy = NULL;
	if (ops->m_allocator)
	{
		name_copy = copy_term(ops->m_allocator,context,name,NULL);
		if (!name_copy)
		{
			allocator_free(ops->m_allocator,new_op);
			return -1;
		}
		new_op->m_name = name_copy;
	}	

	if (specifier == eFX || specifier == eFY)
	{
		new_op->m_prefix.m_precedence = precendence;
		new_op->m_prefix.m_specifier = specifier;
	}
	else
	{
		new_op->m_other.m_precedence = precendence;
		new_op->m_other.m_specifier = specifier;
	}

	dynamic_operator_t* curr_op;
	int is_sub_tree = 0;
	uint64_t key = operator_key(name,&is_sub_tree);
	if (is_sub_tree)
	{
		// Get sub-tree
		btree_t sub_tree = { .m_allocator = ops->m_allocator };
		sub_tree.m_root = btree_lookup(ops,name->m_u64val);
		if (!sub_tree.m_root)
		{
			curr_op = btree_insert(&sub_tree,key,new_op);
			if (!curr_op)
			{
				allocator_free(ops->m_allocator,new_op->m_name);
				allocator_free(ops->m_allocator,new_op);
				return -1;
			}

			if (!btree_insert(ops,name->m_u64val,sub_tree.m_root))
			{
				btree_clear(&sub_tree,NULL,NULL);
				allocator_free(ops->m_allocator,new_op->m_name);
				allocator_free(ops->m_allocator,new_op);
				return -1;
			}
		}
		else
		{
			curr_op = btree_insert(&sub_tree,key,new_op);
			if (!curr_op)
			{
				allocator_free(ops->m_allocator,new_op->m_name);
				allocator_free(ops->m_allocator,new_op);
				return -1;
			}
		
			// TODO: Check for clashes...
			assert(term_compare(curr_op->m_name,name));
		}
	}
	else
	{
		curr_op = btree_insert(ops,name->m_u64val,new_op);
		if (!curr_op)
		{
			allocator_free(ops->m_allocator,new_op->m_name);
			allocator_free(ops->m_allocator,new_op);
			return -1;
		}
	}

	if (curr_op != new_op)
	{
		allocator_free(ops->m_allocator,new_op);

		if (specifier == eFX || specifier == eFY)
		{
			curr_op->m_prefix.m_precedence = precendence;
			curr_op->m_prefix.m_specifier = specifier;		
		}
		else
		{
			if (curr_op->m_other.m_precedence)
			{
				int new_postfix = (specifier == eXF || specifier == eYF);
				int curr_postfix = (curr_op->m_other.m_specifier == eXF || curr_op->m_other.m_specifier == eYF);

				if (new_postfix != curr_postfix)
					return 0;
			}
			
			curr_op->m_other.m_precedence = precendence;
			curr_op->m_other.m_specifier = specifier;
		}
	}

	return 1;
}

static void set_op_inner(context_t* context, operator_table_t* ops, int64_t precendence, operator_specifier_t specifier, const term_t* op)
{
	if (get_term_type(op) != prolite_atom)
		push_type_error(context,PACK_ATOM_EMBED_4('a','t','o','m'),op);
	else if (op->m_u64val == PACK_ATOM_EMBED_1(','))
		push_permission_error(context,PACK_ATOM_BUILTIN(modify),PACK_ATOM_BUILTIN(operator),op);
	else
	{
		int i = update_operator(context,ops,precendence,specifier,op);
		if (i == 0)
			push_permission_error(context,PACK_ATOM_BUILTIN(create),PACK_ATOM_BUILTIN(operator),op);
		else if (i == -1)
			push_out_of_memory_error(context,op);
	}
}

static void set_op(context_t* context, operator_table_t* ops, const term_t* p, const term_t* s, const term_t* op)
{
	int64_t precendence = 0;
	switch (get_term_type(p))
	{
	case prolite_var:
		return push_instantiation_error(context,p);

	case prolite_integer:
		precendence = get_integer(p);
		if (precendence < 0 || precendence > 1200)
			return push_domain_error(context,PACK_ATOM_BUILTIN(operator_priority),p);
		break;

	default:
		return push_type_error(context,PACK_ATOM_BUILTIN(integer),p);
	}
	
	operator_specifier_t specifier;
	switch (get_term_type(s))
	{
	case prolite_var:
		return push_instantiation_error(context,s);

	case prolite_atom:
		switch (s->m_u64val)
		{
		case PACK_ATOM_EMBED_2('x','f'):
			specifier = eXF;
			break;

		case PACK_ATOM_EMBED_2('y','f'):
			specifier = eYF;
			break;

		case PACK_ATOM_EMBED_2('f','x'):
			specifier = eFX;
			break;

		case PACK_ATOM_EMBED_2('f','y'):
			specifier = eFY;
			break;

		case PACK_ATOM_EMBED_3('x','f','x'):
			specifier = eXFX;
			break;

		case PACK_ATOM_EMBED_3('x','f','y'):
			specifier = eXFY;
			break;

		case PACK_ATOM_EMBED_3('y','f','x'):
			specifier = eYFX;
			break;

		default:
			return push_domain_error(context,PACK_ATOM_BUILTIN(operator_specifier),s);
		}
		break;

	default:
		return push_type_error(context,PACK_ATOM_EMBED_4('a','t','o','m'),s);
	}

	switch (get_term_type(op))
	{
	case prolite_var:
		return push_instantiation_error(context,op);

	case prolite_atom:
		return set_op_inner(context,ops,precendence,specifier,op);

	case prolite_compound:
		while (op->m_u64val == PACK_COMPOUND_EMBED_1(2,'.'))
		{
			op = get_first_arg(op,NULL);
			set_op_inner(context,ops,precendence,specifier,op);
			op = get_next_arg(op);
		}

		if (op->m_u64val != PACK_ATOM_EMBED_2('[',']'))
			set_op_inner(context,ops,precendence,specifier,op);
		break;

	default:
		return push_type_error(context,PACK_ATOM_EMBED_4('l','i','s','t'),op);
	}
}

void directive_op(context_t* context, operator_table_t* ops, const term_t* goal)
{
	const term_t* p = get_first_arg(goal,NULL);
	const term_t* s = get_next_arg(p);
	const term_t* op = get_next_arg(s);

	set_op(context,ops,p,s,op);
}

void builtin_op(context_t* context)
{
	// Pop 2 terms
	term_t* op = context->m_stack;
	const term_t* s = get_next_arg(op);
	const term_t* p = get_next_arg(s);
	context->m_stack = (term_t*)get_next_arg(p);

	set_op(context,&context->m_module->m_operators,p,s,op);
}

void builtin_current_op(context_t* context)
{
}
