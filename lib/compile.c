
#include "stream.h"

#include <stdio.h>
#include <stdarg.h>
#include <setjmp.h>

enum Flags
{
	FLAG_FAIL = 1,
	FLAG_CUT = 2,
	FLAG_THROW = 4,
	FLAG_HALT = 8
};

enum OpCodes
{
	OP_END,
	OP_TRUE,
	OP_JMP,
	OP_CALL,
	OP_RET,
	OP_BUILTIN,
	OP_THROW,
	OP_SET_FLAGS,
	OP_CLEAR_FLAGS,
	OP_PUSH_CUT,
	OP_POP_CUT,
	OP_BRANCH
};

union opcode_t
{
	enum OpCodes m_opcode;
	double       m_dval;
	uint64_t     m_u64val;
	const void*  m_pval;
};

struct cfg_block_t
{
	size_t          m_len;  //< in sizeof(m_ops[0])
	union opcode_t* m_ops;
};

struct continuation_t
{
	const struct cfg_block_t* m_entry_point;
	struct cfg_block_t*       m_tail;
	uint8_t                   m_set_flags;
	unsigned                  m_call_site : 1;
};

struct compile_context_t
{
	struct heap_t* m_heap;
	jmp_buf        m_jmp;
};

static struct cfg_block_t* new_cfg_block(struct compile_context_t* context)
{
	struct cfg_block_t* b = heap_malloc(&context->m_heap,sizeof(struct cfg_block_t));
	if (!b)
		longjmp(context->m_jmp,1);
	
	b->m_len = 0;
	b->m_ops = NULL;
	return b;
}

static union opcode_t* append_opcodes(struct compile_context_t* context, struct cfg_block_t* blk, size_t count)
{
	union opcode_t* ret = blk->m_ops;
	if (count)
	{
		blk->m_ops = heap_realloc(&context->m_heap,blk->m_ops,blk->m_len * sizeof(union opcode_t),(blk->m_len + count) * sizeof(union opcode_t));
		if (!blk->m_ops)
			longjmp(context->m_jmp,1);

		ret = blk->m_ops + blk->m_len;
		blk->m_len += count;		
	}
	return ret;
}

static struct continuation_t* new_continuation(struct compile_context_t* context)
{
	struct continuation_t* c = heap_malloc(&context->m_heap,sizeof(struct continuation_t));
	if (!c)
		longjmp(context->m_jmp,1);
	
	c->m_call_site = 0;
	c->m_set_flags = 0;
	c->m_tail = new_cfg_block(context);
	c->m_entry_point = c->m_tail;
	
	return c;
}

typedef const char* builtin_fn_t;

#define DECLARE_BUILTIN_FUNCTION(f,n) \
builtin_fn_t builtin_##f = #f;

#include "builtin_functions"

builtin_fn_t builtin_call = "call";
builtin_fn_t throw_call = "call";
builtin_fn_t builtin_throw = "throw";

static struct continuation_t* set_flags(struct compile_context_t* context, struct continuation_t* c, uint8_t flags)
{
	if ((c->m_set_flags | flags) != c->m_set_flags)
	{
		c->m_set_flags |= flags;

		if (c->m_tail->m_len >= 2 &&
			c->m_tail->m_ops[c->m_tail->m_len-2].m_opcode == OP_SET_FLAGS)
		{
			c->m_tail->m_ops[c->m_tail->m_len-1].m_opcode = c->m_set_flags;
		}
		else if (c->m_set_flags)
		{
			union opcode_t* ops = append_opcodes(context,c->m_tail,2);
			(ops++)->m_opcode = OP_SET_FLAGS;
			ops->m_u64val = c->m_set_flags;
		}
	}
	return c;
}

static struct continuation_t* clear_flags(struct compile_context_t* context, struct continuation_t* c, uint8_t flags)
{
	if (c->m_tail->m_len >= 2 &&
		c->m_tail->m_ops[c->m_tail->m_len-2].m_opcode == OP_SET_FLAGS &&
		(c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val & FLAG_FAIL))
	{
		c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val &= ~FLAG_FAIL;
	}
	else if (c->m_tail->m_len < 2 ||
		c->m_tail->m_ops[c->m_tail->m_len-2].m_opcode != OP_CLEAR_FLAGS ||
		!(c->m_tail->m_ops[c->m_tail->m_len-1].m_u64val & FLAG_FAIL))
	{
		union opcode_t* ops = append_opcodes(context,c->m_tail,2);
		(ops++)->m_opcode = OP_CLEAR_FLAGS;
		ops->m_u64val = FLAG_FAIL;
	}

	c->m_set_flags &= ~flags;

	return c;
}

static struct continuation_t* prune_branches(struct continuation_t* c, uint8_t flags)
{
	// Remove spurious branches
	if (c->m_entry_point->m_len == 5 &&
		c->m_entry_point->m_ops[0].m_opcode == OP_BRANCH &&
		c->m_entry_point->m_ops[3].m_opcode == OP_JMP)
	{
		c->m_entry_point->m_ops[1].m_u64val &= ~flags;
		
		if (c->m_entry_point->m_ops[1].m_u64val == 0)
			c->m_entry_point = c->m_entry_point->m_ops[4].m_pval;
	}
	return c;
}

static struct continuation_t* goto_next(struct compile_context_t* context, struct continuation_t* c, struct continuation_t* next)
{
	union opcode_t* ops = append_opcodes(context,c->m_tail,2);
	if (next->m_call_site)
	{
		(ops++)->m_opcode = OP_CALL;
		while (next->m_entry_point->m_len == 3 &&
			next->m_entry_point->m_ops[0].m_opcode == OP_CALL &&
			next->m_entry_point->m_ops[2].m_opcode == OP_RET)
		{
			next->m_entry_point = next->m_entry_point->m_ops[1].m_pval;
		}

		ops->m_pval = next->m_entry_point;
	}
	else
	{
		(ops++)->m_opcode = OP_JMP;
		while (next->m_entry_point->m_len == 2 && next->m_entry_point->m_ops[0].m_opcode == OP_JMP)
			next->m_entry_point = next->m_entry_point->m_ops[1].m_pval;

		ops->m_pval = next->m_entry_point;
		c->m_tail = next->m_tail;
	}
	return c;
}

static struct continuation_t* make_call_site(struct compile_context_t* context, struct continuation_t* c)
{
	if (!c->m_call_site)
	{
		union opcode_t* ops = append_opcodes(context,c->m_tail,1);
		ops->m_opcode = OP_RET;
		c->m_call_site = 1;
	}
	return c;
}

static struct continuation_t* convert_to_call(struct compile_context_t* context, struct continuation_t* c)
{
	// Convert c to a call site
	c = make_call_site(context,c);
	
	// Convert cont to a call
	if (c->m_entry_point->m_len != 3 ||
		c->m_entry_point->m_ops[0].m_opcode != OP_CALL ||
		c->m_entry_point->m_ops[2].m_opcode != OP_RET)
	{
		struct continuation_t* c1 = new_continuation(context);
		
		union opcode_t* ops = append_opcodes(context,c1->m_tail,2);
		(ops++)->m_opcode = OP_CALL;
		ops->m_pval = c->m_entry_point;

		c1->m_set_flags = c->m_set_flags;
		c = c1;
	}
	
	return c;
}

static struct continuation_t* conditional_goal(struct compile_context_t* context, struct continuation_t* c, uint8_t flags)
{
	struct continuation_t* c1 = new_continuation(context);
	struct continuation_t* c2 = new_continuation(context);
	
	union opcode_t* ops = append_opcodes(context,c1->m_tail,3);
	
	(ops++)->m_opcode = OP_BRANCH;
	(ops++)->m_u64val = flags;
	ops->m_pval = c2->m_entry_point;

	flags = c->m_set_flags;

	c = goto_next(context,c1,c);
	c = goto_next(context,c,c2);
	
	c->m_set_flags = flags;
	
	return c;
}

static struct continuation_t* compile_goal(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal);

static struct continuation_t* compile_true(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	return prune_branches(cont,FLAG_FAIL | FLAG_CUT | FLAG_THROW | FLAG_HALT);
}

static struct continuation_t* compile_false(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	if (cont->m_set_flags & FLAG_FAIL)
		return cont;

	struct continuation_t* c = new_continuation(context);
	
	c = set_flags(context,c,FLAG_FAIL);
	
	return c;
}

static struct continuation_t* compile_cut(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	if (cont->m_set_flags & FLAG_CUT)
		return cont;
	
	cont = prune_branches(cont,FLAG_FAIL | FLAG_THROW | FLAG_HALT);

	struct continuation_t* c = cont;
	if (cont->m_call_site)
		c = new_continuation(context);
		
	c = set_flags(context,c,FLAG_CUT);
	c->m_set_flags |= cont->m_set_flags;

	if (cont->m_call_site)
		c = goto_next(context,c,cont);
	
	return c;
}

static struct continuation_t* compile_and(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	const union packed_t* g1 = get_first_arg(goal,NULL,NULL);
	const union packed_t* g2 = get_next_arg(g1,NULL);

	return compile_goal(context,compile_goal(context,cont,g2),g1);
}

static struct continuation_t* compile_if_then_else(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal, const union packed_t* g_else)
{
#if 0
	const union packed_t* g_if = get_first_arg(goal,NULL,NULL);
	const union packed_t* g_then = get_next_arg(g_if,NULL);

	struct cfg_block_t* b = new_cfg_block(context);
	struct cfg_block_t* b1 = new_cfg_block(context);
	struct cfg_block_t* b2 = new_cfg_block(context);
	if (b && b1 && !b2)
	{
		union opcode_t* ops = append_opcodes(context,b1,3);
		if (ops)
		{
			(ops++)->m_opcode = OP_POP_CUT;
			(ops++)->m_opcode = OP_JMP;
			ops->m_pval = compile_goal(context,cont,g_then);
			if (ops->m_pval)
			{
				ops = append_opcodes(context,b2,3);
				if (ops)
				{
					(ops++)->m_opcode = OP_POP_CUT;
					(ops++)->m_opcode = OP_JMP;
					if (g_else)
						ops->m_pval = compile_goal(context,cont,g_else);

					if (ops->m_pval)
					{
						ops = append_opcodes(context,b,4);
						if (ops)
						{
							(ops++)->m_opcode = OP_PUSH_CUT;
							(ops++)->m_opcode = OP_SET_CUT;
							(ops++)->m_opcode = OP_JMP;
							ops->m_pval = compile_goal(context,b1,g_if);
							if (ops->m_pval)
								return b;
						}
					}
				}
			}
		}
	}
#endif
	return NULL;
}

static struct continuation_t* compile_or(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	const union packed_t* g1 = get_first_arg(goal,NULL,NULL);
	const union packed_t* g2 = get_next_arg(g1,NULL);

	if (g1->m_u64val == PACK_COMPOUND_EMBED_2(2,'-','>'))
		return compile_if_then_else(context,cont,g1,g2);

	// Compile g1
	struct continuation_t* c = compile_goal(context,convert_to_call(context,cont),g1);
	
	// Compile g2
	if (!(c->m_set_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
	{
		struct continuation_t* c2 = compile_goal(context,convert_to_call(context,cont),g2);
			
		uint8_t flags = c->m_set_flags;
		c = clear_flags(context,c,FLAG_FAIL);
		
		if (!(flags & FLAG_FAIL))
			c2 = conditional_goal(context,c2,FLAG_CUT | FLAG_THROW | FLAG_HALT);
		
		c = goto_next(context,c,c2);
		c->m_set_flags = c2->m_set_flags;
	}
	return c;
}

static struct continuation_t* compile_if_then(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	return compile_if_then_else(context,cont,goal,NULL);
}

static struct continuation_t* compile_builtin(struct compile_context_t* context, struct continuation_t* cont, builtin_fn_t fn)
{
	cont = prune_branches(cont,FLAG_FAIL | FLAG_THROW | FLAG_HALT);

	// Convert cont to a call site
	cont = make_call_site(context,cont);

	while (cont->m_entry_point->m_len == 3 &&
		cont->m_entry_point->m_ops[0].m_opcode == OP_CALL &&
		cont->m_entry_point->m_ops[2].m_opcode == OP_RET)
	{
		cont->m_entry_point = cont->m_entry_point->m_ops[1].m_pval;
	}

	struct continuation_t* c = new_continuation(context);
		
	union opcode_t* ops = append_opcodes(context,c->m_tail,3);		
	(ops++)->m_opcode = OP_BUILTIN;
	(ops++)->m_pval = fn;	
	ops->m_pval = cont->m_entry_point;
	
	return c;
}

struct continuation_t* compile_user_defined(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	return compile_builtin(context,cont,"user_defined");
}

struct continuation_t* compile_callN(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	return compile_builtin(context,cont,"Call/N");
}

static struct continuation_t* compile_throw(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	cont = prune_branches(cont,FLAG_FAIL | FLAG_CUT | FLAG_HALT);

	struct continuation_t* c = new_continuation(context);
	c->m_set_flags = FLAG_THROW;
	
	union opcode_t* ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode = OP_THROW;
	ops->m_pval = builtin_throw;
	
	return c;
}

static struct continuation_t* wrap_cut(struct compile_context_t* context, struct continuation_t* c)
{
	struct continuation_t* c1 = new_continuation(context);
	c1->m_set_flags = (c->m_set_flags & ~FLAG_CUT);

	union opcode_t* ops = append_opcodes(context,c1->m_tail,1);
	ops->m_opcode = OP_PUSH_CUT;

	ops = append_opcodes(context,c->m_tail,1);
	ops->m_opcode = OP_POP_CUT;	
								
	return goto_next(context,c1,c);
}

static struct continuation_t* compile_call(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	const union packed_t* g1 = get_first_arg(goal,NULL,NULL);
	uint16_t type = UNPACK_TYPE(g1->m_u64val);

	struct continuation_t* c;
	if (type == prolite_atom || type == prolite_compound)
		c = wrap_cut(context,compile_goal(context,cont,g1));
	else if (type == prolite_var)
		c = wrap_cut(context,compile_builtin(context,cont,builtin_call));
	else
	{
		// We know this throws...
		c = new_continuation(context);
		c->m_set_flags = FLAG_THROW;
		
		union opcode_t* ops = append_opcodes(context,c->m_tail,2);
		(ops++)->m_opcode = OP_THROW;
		ops->m_pval = throw_call;		
	}

	return c;
}

static struct continuation_t* compile_once(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	struct continuation_t* c = new_continuation(context);
	c = set_flags(context,c,FLAG_CUT);
		
	c = compile_call(context,c,goal);
			
	struct continuation_t* c1 = conditional_goal(context,cont,FLAG_FAIL);
				
	c = goto_next(context,c,c1);
	c->m_set_flags = c1->m_set_flags;
	
	return c;
}

static struct continuation_t* compile_repeat(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	cont = prune_branches(cont,FLAG_FAIL | FLAG_CUT | FLAG_THROW | FLAG_HALT);

	if (cont->m_set_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT))
		return cont;

	if (cont->m_call_site)
		cont = goto_next(context,new_continuation(context),cont);
		
	struct continuation_t* c1 = new_continuation(context);
		
	union opcode_t* ops = append_opcodes(context,cont->m_tail,5);
	(ops++)->m_opcode = OP_BRANCH;
	(ops++)->m_u64val = FLAG_CUT | FLAG_THROW | FLAG_HALT;
	(ops++)->m_pval = c1->m_entry_point;
	//(ops++)->m_opcode = OP_CLEAR_FLAGS;
	//(ops++)->m_u64val = FLAG_FAIL;
	(ops++)->m_opcode = OP_JMP;
	ops->m_pval = cont->m_entry_point;

	cont->m_tail = c1->m_tail;
		
	return cont;
}

static struct continuation_t* compile_not_proveable(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	struct continuation_t* c = new_continuation(context);
	c = set_flags(context,c,FLAG_CUT);
	c = compile_call(context,c,goal);
	
	struct continuation_t* c1 = new_continuation(context);
	struct continuation_t* c2 = new_continuation(context);
	
	union opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode = OP_BRANCH;
	(ops++)->m_u64val = FLAG_FAIL;
	ops->m_pval = c2->m_entry_point;

	c = set_flags(context,c,FLAG_FAIL);
	c = goto_next(context,c,c1);
	
	c2 = clear_flags(context,c2,FLAG_FAIL);
	c2 = goto_next(context,c2,cont);
	c2 = goto_next(context,c2,c1);
	
	c->m_tail = c2->m_tail;
	c->m_set_flags = c2->m_set_flags;
		
	return c;
}

static struct continuation_t* compile_goal(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	int debug = 0;

	struct continuation_t* c;
	switch (goal->m_u64val)
	{
#define DECLARE_BUILTIN_STATIC(f,n) \
	case (n): c = compile_##f(context,cont,goal); break;

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,n) \
	case (n): c = compile_builtin(context,cont,builtin_##f); break;

#include "builtin_functions"

	default:
		switch (UNPACK_TYPE(goal->m_u64val))
		{
		case prolite_compound:
			if ((goal->m_u64val & PACK_COMPOUND_EMBED_4(0,'c','a','l','l')) == PACK_COMPOUND_EMBED_4(0,'c','a','l','l') ||
				(goal->m_u64val & PACK_COMPOUND_BUILTIN(call,0)) == PACK_COMPOUND_BUILTIN(call,0) ||
				goal[1].m_u64val == PACK_ATOM_EMBED_4('c','a','l','l'))
			{
				c = compile_callN(context,cont,goal);
			}
			else
				c = compile_user_defined(context,cont,goal);
			break;

		case prolite_atom:
			c = compile_user_defined(context,cont,goal);
			break;

		default:
			c = compile_call(context,cont,goal);
			break;
		}
		break;
	}

	if (debug)
	{
		// TODO: Emit tracepoints
	}

	return c;
}

static void dumpCFG(const struct cfg_block_t* s, FILE* f);

static void compile_term(struct compile_context_t* context, struct continuation_t* cont, const union packed_t* goal)
{
	struct continuation_t* c = compile_goal(context,cont,goal);
	
	union opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode = OP_SET_FLAGS;
	(ops++)->m_u64val = c->m_set_flags;
	ops->m_opcode = OP_END;

	// TODO: Link!!

	dumpCFG(c->m_entry_point,stdout);
}

void compile(struct context_t* context, struct stream_t* s)
{
	// Read a term and prepare it for execution
	enum eParseStatus result = read_term(context,s);
	if (result != PARSE_OK)
		fprintf(stderr,"Parser failure\n");
	else
	{
		// Pop varinfo
		const union packed_t* sp = context->m_stack;

		size_t varcount = (sp++)->m_u64val;
		while (varcount--)
		{
			get_string(&sp,NULL);

			sp++;
		}

		size_t heap_start = heap_top(context->m_heap);
		struct compile_context_t cc = {0};
		cc.m_heap = context->m_heap;

		if (!setjmp(cc.m_jmp))
		{			
			struct continuation_t* cont = new_continuation(&cc);
			union opcode_t* ops = append_opcodes(&cc,cont->m_tail,1);
			ops->m_opcode = OP_TRUE;

			compile_term(&cc,cont,sp);
		}
		else
		{
			/* Out of memory! */
			heap_reset(&context->m_heap,heap_start);
		}

		/* Bulk free all CFG blocks */
		heap_reset(&context->m_heap,heap_start);
	}

	/* We have just made heavy use of the heap */
	heap_compact(context->m_heap);
}

static void fmtFlags(uint64_t v, char* buf)
{
	char* s = buf;

	if (v & FLAG_FAIL)
		*buf++ = 'F';

	if (v & FLAG_CUT)
		*buf++ = 'C';

	if (v & FLAG_THROW)
		*buf++ = 'T';

	if (v & FLAG_HALT)
		*buf++ = 'H';

	if (buf == s)
		*buf++ = '0';

	*buf = '\0';
}

static void dumpCFGBlock(const struct cfg_block_t* blk, FILE* f)
{
	char buf[5] = {0};

	fprintf(f,"\tnode [shape=record];\n");
	fprintf(f,"\tN%p [label=\"{",blk);

	if (!blk->m_len)
	{
		fprintf(f,"<f0> WTF?!?!");
	}

	for (size_t i=0;i < blk->m_len; ++i)
	{
		if (i)
			fprintf(f,"|");

		fprintf(f,"<f%zu> ",i);

		switch (blk->m_ops[i].m_opcode)
		{
		case OP_TRUE:
			fprintf(f,"Success!");
			break;

		case OP_END:
			fprintf(f,"End");
			break;

		case OP_JMP:
			fprintf(f,"Jmp");
			++i;
			break;

		case OP_CALL:
			fprintf(f,"Call");
			++i;
			break;

		case OP_RET:
			fprintf(f,"Ret");
			break;

		case OP_BUILTIN:
			fprintf(f,"Builtin\\ %s|<f%zu_t> ...Call",(const char*)blk->m_ops[i+1].m_pval,i);
			i+=2;
			break;

		case OP_THROW:
			fprintf(f,"throw \\ (builtin_%s)",(const char*)blk->m_ops[i+1].m_pval);
			++i;
			break;

		case OP_SET_FLAGS:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"Set\\ Flags\\ %s",buf);
			++i;
			break;

		case OP_CLEAR_FLAGS:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"Clear\\ Flags\\ %s",buf);
			++i;
			break;

		case OP_PUSH_CUT:
			fprintf(f,"Push\\ Cut");
			break;

		case OP_POP_CUT:
			fprintf(f,"Pop\\ Cut");
			break;

		case OP_BRANCH:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"Branch|<f%zu> ...%s",i+1,buf);
			i += 2;
			break;

		default:
			fprintf(f,"WTF? %zu",(size_t)blk->m_ops[i].m_opcode);
			break;
		}
	}

	fprintf(f,"}\"];\n");

	for (size_t i=0;i < blk->m_len; ++i)
	{
		switch (blk->m_ops[i].m_opcode)
		{
		case OP_BRANCH:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [label=\"%s\"];\n",blk,i+1,blk->m_ops[i+2].m_pval,buf);
			i += 2;
			break;

		case OP_CALL:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [dir=both];\n",blk,i,blk->m_ops[i+1].m_pval);
			++i;
			break;

		case OP_JMP:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0>;\n",blk,i,blk->m_ops[i+1].m_pval);
			++i;
			break;

		case OP_BUILTIN:
			fprintf(f,"\tN%p:<f%zu_t> -> N%p:<f0> [dir=both];\n",blk,i,blk->m_ops[i+2].m_pval);
			i+=2;
			break;

		case OP_THROW:
		case OP_SET_FLAGS:
		case OP_CLEAR_FLAGS:
			++i;
			break;

		default:
			break;
		}
	}
}

struct cfg_vec_t
{
	size_t len;
	const struct cfg_block_t** blks;
};

static int addCFG(struct cfg_vec_t* blks, const struct cfg_block_t* blk)
{
	for (size_t i=0; i < blks->len; ++i)
	{
		if (blk == blks->blks[i])
			return 0;
	}

	blks->blks = realloc(blks->blks,(blks->len + 1) * sizeof(void*));
	blks->blks[blks->len++] = blk;

	return 1;
}

static void walkCFG(struct cfg_vec_t* blks, const struct cfg_block_t* blk)
{
	if (addCFG(blks,blk))
	{
		for (size_t i=0;i < blk->m_len; ++i)
		{
			switch (blk->m_ops[i].m_opcode)
			{
			case OP_JMP:
			case OP_CALL:
				walkCFG(blks,blk->m_ops[i+1].m_pval);
				++i;
				break;

			case OP_BRANCH:
			case OP_BUILTIN:
				walkCFG(blks,blk->m_ops[i+2].m_pval);
				i+=2;
				break;

			case OP_THROW:
			case OP_SET_FLAGS:
			case OP_CLEAR_FLAGS:
				++i;
				break;

			default:
				break;
			}
		}
	}
}

static void dumpCFG(const struct cfg_block_t* b, FILE* f)
{
	if (f)
	{
		fprintf(f,"digraph cfg {\n");

		if (b)
		{
			fprintf(f,"\tstart [shape=point];\n");

			struct cfg_vec_t blks = {0};
			walkCFG(&blks,b);

			for (size_t i=0; i < blks.len; ++i)
				dumpCFGBlock(blks.blks[i],f);

			fprintf(f,"\tstart -> N%p:<f0>;\n",b);
		}

		fprintf(f,"}");
	}
}
