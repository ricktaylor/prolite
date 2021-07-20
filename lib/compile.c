
#include "stream.h"

#include <stdio.h>
#include <stdarg.h>

enum OpCodes
{
	OP_END = 0,
	OP_TRUE,
	OP_JMP,
	OP_CALL,
	OP_RET,
	OP_BUILTIN,
	OP_THROW,
	OP_SET_CUT,
	OP_CLEAR_CUT,
	OP_PUSH_CUT,
	OP_POP_CUT,
	OP_BCTH, ///< Branch Cut, Throw, Halt
	OP_BTH, ///< Branch Throw, Halt
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
	unsigned                  m_always_cth : 1;
	unsigned                  m_function : 1;
};

struct compile_context_t
{
	struct heap_t* m_heap;
};

static struct cfg_block_t* new_cfg_block(struct compile_context_t* context)
{
	struct cfg_block_t* b = heap_malloc(&context->m_heap,sizeof(struct cfg_block_t));
	if (b)
	{
		b->m_len = 0;
		b->m_ops = NULL;
	}
	return b;
}

static union opcode_t* append_opcodes(struct compile_context_t* context, struct cfg_block_t* blk, size_t count)
{
	union opcode_t* ret = blk->m_ops;
	if (count)
	{
		ret = blk->m_ops = heap_realloc(&context->m_heap,blk->m_ops,blk->m_len * sizeof(union opcode_t),(blk->m_len + count) * sizeof(union opcode_t));
		if (blk->m_ops)
		{
			ret = blk->m_ops + blk->m_len;
			blk->m_len += count;
		}
	}
	return ret;
}

static struct continuation_t* new_continuation(struct compile_context_t* context)
{
	struct continuation_t* c = heap_malloc(&context->m_heap,sizeof(struct continuation_t));
	if (c)
	{
		c->m_function = 0;
		c->m_always_cth = 0;
		c->m_tail = new_cfg_block(context);
		if (!c->m_tail)
			c = NULL;
		else
			c->m_entry_point = c->m_tail;
	}
	return c;
}

#define DECLARE_BUILTIN_FUNCTION(f,n) \
const char* builtin_##f = #f;

#include "builtin_functions"

typedef const char* builtin_fn_t;

builtin_fn_t builtin_call = "call";
builtin_fn_t builtin_repeat = "repeat";

static struct continuation_t* compile_goal(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal);

static struct continuation_t* compile_true(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	return c_true;
}

static struct continuation_t* compile_false(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	struct continuation_t* c = new_continuation(context);
	if (c)
	{
		c->m_entry_point = c_fail;
		c->m_tail = c_true->m_tail;
	}	
	return c;
}

static struct continuation_t* compile_cut(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	struct continuation_t* c = new_continuation(context);
	if (c)
	{
		union opcode_t* ops = append_opcodes(context,c->m_tail,3);
		if (!ops)
			c = NULL;
		else
		{
			(ops++)->m_opcode = OP_SET_CUT;
			if (c_true->m_function)
			{
				(ops++)->m_opcode = OP_CALL;
				ops->m_pval = c_true->m_entry_point;
			}
			else
			{
				(ops++)->m_opcode = OP_JMP;
				ops->m_pval = c_true->m_entry_point;
				c->m_tail = c_true->m_tail;
			}

			c->m_always_cth = 1;
		}
	}
	return c;
}

static struct continuation_t* compile_and(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	const union packed_t* g1 = get_first_arg(goal,NULL,NULL);
	const union packed_t* g2 = get_next_arg(g1,NULL);

	struct continuation_t* c = compile_goal(context,c_true,c_fail,g2);
	if (c)
	{
		struct continuation_t* c2 = new_continuation(context);
		if (c2)
		{
			union opcode_t* ops = append_opcodes(context,c2->m_tail,4);
			if (ops)
			{
				(ops++)->m_opcode = OP_BTH;
				(ops++)->m_pval = c_fail;

				if (c->m_function)
				{
					(ops++)->m_opcode = OP_CALL;
					ops->m_pval = c->m_entry_point;
				}
				else
				{
					(ops++)->m_opcode = OP_JMP;
					ops->m_pval = c->m_entry_point;
					c2->m_tail = c->m_tail;
				}
				
				int always_cth = c->m_always_cth;
				c = compile_goal(context,c2,c_fail,g1);
				if (c)
				{
					if (always_cth)
						c->m_always_cth = 1;

					// Remove spurious entry tests
					if (c->m_entry_point->m_len == 4 &&
						c->m_entry_point->m_ops[0].m_opcode == OP_BTH &&
						c->m_entry_point->m_ops[2].m_opcode == OP_JMP)
					{
						c->m_entry_point = c->m_entry_point->m_ops[3].m_pval;
					}
				}
			}
		}
	}
	return c;
}

static struct continuation_t* compile_if_then_else(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal, const union packed_t* g_else)
{
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
			ops->m_pval = compile_goal(context,c_true,c_fail,g_then);
			if (ops->m_pval)
			{
				ops = append_opcodes(context,b2,3);
				if (ops)
				{	
					(ops++)->m_opcode = OP_POP_CUT;
					(ops++)->m_opcode = OP_JMP;
					if (!g_else)
						ops->m_pval = c_fail;
					else
						ops->m_pval = compile_goal(context,c_true,c_fail,g_else);
					
					if (ops->m_pval)
					{
						ops = append_opcodes(context,b,4);
						if (ops)
						{	
							(ops++)->m_opcode = OP_PUSH_CUT;
							(ops++)->m_opcode = OP_SET_CUT;
							(ops++)->m_opcode = OP_JMP;
							ops->m_pval = compile_goal(context,b1,b2,g_if);
							if (ops->m_pval)
								return b;
						}
					}
				}
			}
		}
	}
	return NULL;
}

static struct continuation_t* compile_or(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	const union packed_t* g1 = get_first_arg(goal,NULL,NULL);
	const union packed_t* g2 = get_next_arg(g1,NULL);
	
	if (g1->m_u64val == PACK_COMPOUND_EMBED_2(2,'-','>'))
		return compile_if_then_else(context,c_true,c_fail,g1,g2);

	if (g1->m_u64val == PACK_ATOM_EMBED_4('f','a','i','l') ||
		g1->m_u64val == PACK_ATOM_EMBED_5('f','a','l','s','e'))
	{
		return compile_goal(context,c_true,c_fail,g2);
	}

	if (g2->m_u64val == PACK_ATOM_EMBED_4('f','a','i','l') ||
		g2->m_u64val == PACK_ATOM_EMBED_5('f','a','l','s','e'))
	{
		return compile_goal(context,c_true,c_fail,g1);
	}

	// Convert c_true to a function callee
	if (!c_true->m_function)
	{
		union opcode_t* ops = append_opcodes(context,c_true->m_tail,1);
		if (!ops)
			c_true = NULL;
		else
		{
			ops->m_opcode = OP_RET;
			c_true->m_function = 1;
		}
	}

	// Compile g2
	struct continuation_t* c = NULL;
	if (c_true)
	{
		// Convert c_true to a call
		if (c_true->m_entry_point->m_len != 3 ||
			c_true->m_entry_point->m_ops[0].m_opcode != OP_CALL ||
			c_true->m_entry_point->m_ops[2].m_opcode != OP_RET)
		{
			c = new_continuation(context);
			if (c)
			{
				union opcode_t* ops = append_opcodes(context,c->m_tail,2);
				if (!ops)
					c = NULL;
				else
				{	
					(ops++)->m_opcode = OP_CALL;
					ops->m_pval = c_true->m_entry_point;
				}
			}
		}
		else
			c = c_true;

		if (c)
			c = compile_goal(context,c,c_fail,g2);
	}
			
	if (c && !c->m_always_cth)
	{
		struct continuation_t* c1 = new_continuation(context);
		if (!c1)
			c = NULL;
		else
		{
			union opcode_t* ops = append_opcodes(context,c1->m_tail,4);
			if (!ops)
				c = NULL;
			else
			{	
				(ops++)->m_opcode = OP_BCTH;
				(ops++)->m_pval = c_fail;

				if (c->m_function)
				{
					(ops++)->m_opcode = OP_CALL;
					ops->m_pval = c->m_entry_point;
				}
				else
				{
					(ops++)->m_opcode = OP_JMP;
					ops->m_pval = c->m_entry_point;
					c1->m_tail = c->m_tail;
				}
				c = c1;
			}
		}
	}
	
	// Compile g1
	if (c)
	{
		// Convert c_true to a call
		struct continuation_t* c1 = NULL;
		if (c_true->m_entry_point->m_len != 3 ||
			c_true->m_entry_point->m_ops[0].m_opcode != OP_CALL ||
			c_true->m_entry_point->m_ops[2].m_opcode != OP_RET)
		{
			c1 = new_continuation(context);
			if (c1)
			{
				union opcode_t* ops = append_opcodes(context,c1->m_tail,2);
				if (!ops)
					c = NULL;
				else
				{	
					(ops++)->m_opcode = OP_CALL;
					ops->m_pval = c_true->m_entry_point;
				}
			}
		}
		else
			c1 = c_true;

		if (c1)
		{
			c1 = compile_goal(context,c1,c->m_entry_point,g1);
			if (!c1)
				c = NULL;
			else
			{ 
				union opcode_t* ops = append_opcodes(context,c1->m_tail,2);
				if (!ops)
					c = NULL;
				else
				{	
					(ops++)->m_opcode = OP_JMP;
					(ops++)->m_pval = c->m_entry_point;
					c1->m_tail = c->m_tail;
					c = c1;
				}			
			}
		}
	}
	return c;
}

static struct continuation_t* compile_if_then(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	return compile_if_then_else(context,c_true,c_fail,goal,NULL);
}

static struct continuation_t* compile_builtin(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, builtin_fn_t fn)
{
	if (!c_true->m_function)
	{
		union opcode_t* ops = append_opcodes(context,c_true->m_tail,1);
		if (!ops)
			c_true = NULL;
		else
		{
			ops->m_opcode = OP_RET;
			c_true->m_function = 1;
		}
	}	

	while (c_true->m_entry_point->m_len == 3 &&
		c_true->m_entry_point->m_ops[0].m_opcode == OP_CALL &&
		c_true->m_entry_point->m_ops[2].m_opcode == OP_RET)
	{
		c_true->m_entry_point = c_true->m_entry_point->m_ops[1].m_pval;
	}
	
	struct continuation_t* c = NULL;
	if (c_true)
	{
		c = new_continuation(context);
		if (c)
		{
			union opcode_t* ops = append_opcodes(context,c->m_tail,3);
			if (!ops)
				c = NULL;
			else
			{
				(ops++)->m_opcode = OP_BUILTIN;
				(ops++)->m_pval = fn;
				ops->m_pval = c_true->m_entry_point;
			}
		}
	}
	return c;
}

struct continuation_t* compile_user_defined(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	return compile_builtin(context,c_true,c_fail,"user_defined");
}

struct continuation_t* compile_callN(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	return compile_builtin(context,c_true,c_fail,"Call/N");
}

static struct continuation_t* compile_call(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	const union packed_t* g1 = get_first_arg(goal,NULL,NULL);
	struct continuation_t* c;
	
	switch (UNPACK_TYPE(g1->m_u64val))
	{
	case prolite_atom:
	case prolite_compound:
		c = new_continuation(context);
		if (c)
		{
			struct cfg_block_t* b = new_cfg_block(context);
			if (!b)
				c = NULL;
			else
			{
				union opcode_t* ops = append_opcodes(context,b,3);
				if (!ops)
					c = NULL;
				else
				{	
					(ops++)->m_opcode = OP_POP_CUT;
					(ops++)->m_opcode = OP_JMP;
					ops->m_pval = c_fail;
					
					ops = append_opcodes(context,c->m_tail,3);
					if (!ops)
						c = NULL;
					else
					{
						struct continuation_t* c3 = compile_goal(context,c_true,b,g1);
						if (!c3)
							c = NULL;
						else
						{
							(ops++)->m_opcode = OP_PUSH_CUT;
							(ops++)->m_opcode = OP_JMP;
							ops->m_pval = c3->m_entry_point;
							c->m_tail = c3->m_tail;

							ops = append_opcodes(context,c->m_tail,1);
							if (!ops)
								c = NULL;
							else
								ops->m_opcode = OP_POP_CUT;
						}
					}
				}
			}
		}
		break;

	default:
		// We know this throws...
		c = new_continuation(context);
		if (c)
		{
			union opcode_t* ops = append_opcodes(context,c->m_tail,3);
			if (!ops)
				c = NULL;
			else
			{
				(ops++)->m_opcode = OP_THROW;
				(ops++)->m_pval = builtin_call;
				ops->m_pval = c_fail;
				c->m_always_cth = 1;
			}
		}
		break;
	}

	return c;
}

static struct continuation_t* compile_once(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	struct continuation_t* c = NULL;
	struct continuation_t* c2 = new_continuation(context);
	if (c2)
	{
		struct cfg_block_t* b = new_cfg_block(context);
		if (b)
		{
			union opcode_t* ops = append_opcodes(context,b,3);
			if (ops)
			{	
				(ops++)->m_opcode = OP_POP_CUT;
				(ops++)->m_opcode = OP_JMP;
				ops->m_pval = c_fail;
				c_fail = b;

				c = compile_call(context,c2,c_fail,goal);
				if (c)
				{
					union opcode_t* ops = append_opcodes(context,c2->m_tail,5);
					if (!ops)
						c = NULL;
					else
					{
						(ops++)->m_opcode = OP_BTH;
						(ops++)->m_pval = c_fail;
						(ops++)->m_opcode = OP_SET_CUT;
						(ops++)->m_opcode = OP_JMP;
						ops->m_pval = c_true->m_entry_point;
						c->m_tail = c_true->m_tail;
						c->m_always_cth = 1;

						ops = append_opcodes(context,c->m_tail,1);
						if (!ops)
							c = NULL;
						else
							ops->m_opcode = OP_POP_CUT;
					}
				}
			}
		}
	}
	
	/*if (c)
	{
		c1 = new_continuation(context);
		if (!c1)
			c = NULL;
		else
		{
			union opcode_t* ops = append_opcodes(context,c->m_tail,1);
			if (ops)
			{
				ops->m_opcode = OP_POP_CUT;
			
				struct cfg_block_t* b = new_cfg_block(context);
				if (b)
				{
					union opcode_t* ops = append_opcodes(context,b,3);
					if (ops)
					{	
						(ops++)->m_opcode = OP_POP_CUT;
						(ops++)->m_opcode = OP_JMP;
						ops->m_pval = c_fail;
						
						ops = append_opcodes(context,c1->m_tail,3);
						if (ops)
						{
							(ops++)->m_opcode = OP_PUSH_CUT;
							(ops++)->m_opcode = OP_JMP;
							ops->m_pval = c->m_entry_point;
						}
					}
				}
			}
		}

	}*/
	return c;
}

static struct continuation_t* compile_repeat(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	if (c_true->m_always_cth)
		return c_true;

	// Compile goal
	struct continuation_t* c = new_continuation(context);
	if (c)
	{
		while (c_true->m_entry_point->m_len == 3 &&
			c_true->m_entry_point->m_ops[0].m_opcode == OP_CALL)
		{
			c_true->m_entry_point = c_true->m_entry_point->m_ops[1].m_pval;
			c_true->m_function = 1;
		}

		if (!c_true->m_function)
		{
			union opcode_t* ops = append_opcodes(context,c_true->m_tail,1);
			if (!ops)
				c = NULL;
			else
			{
				ops->m_opcode = OP_RET;
				c_true->m_function = 1;

				union opcode_t* ops = append_opcodes(context,c->m_tail,2);
				if (!ops)
					c = NULL;
				else
				{	
					(ops++)->m_opcode = OP_CALL;
					ops->m_pval = c_true->m_entry_point;
				}	
			}
		}
		else
			c = c_true;

		if (c)
			c = compile_goal(context,c,c_fail,goal);
	}

	if (c)
	{
		if (!c->m_always_cth && c->m_entry_point != c_fail)
		{
			union opcode_t* ops = append_opcodes(context,c->m_tail,4);
			if (!ops)
				c = NULL;
			else
			{	
				(ops++)->m_opcode = OP_BCTH;
				(ops++)->m_pval = c_fail;

				(ops++)->m_opcode = OP_JMP;
				ops->m_pval = c->m_entry_point;
				
				c->m_entry_point = c_true->m_entry_point;
				c->m_always_cth = 1;
			}
		}
	}
	return c;
}

static struct continuation_t* compile_not_proveable(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	struct continuation_t* c = new_continuation(context);
	if (c)
	{
		c->m_entry_point = c_fail;
		c->m_tail = c_true->m_tail;
		
		c = compile_call(context,c,c_true->m_entry_point,goal);
	}
	return c;
}

static struct continuation_t* compile_goal(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	int debug = 0;

	struct continuation_t* c;
	switch (goal->m_u64val)
	{
#define DECLARE_BUILTIN_STATIC(f,n) \
	case (n): c = compile_##f(context,c_true,c_fail,goal); break;

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,n) \
	case (n): c = compile_builtin(context,c_true,c_fail,builtin_##f); break;
			
#include "builtin_functions"

	default:
		switch (UNPACK_TYPE(goal->m_u64val))
		{
		case prolite_compound:
			if ((goal->m_u64val & PACK_COMPOUND_EMBED_4(0,'c','a','l','l')) == PACK_COMPOUND_EMBED_4(0,'c','a','l','l') ||
				(goal->m_u64val & PACK_COMPOUND_BUILTIN(call,0)) == PACK_COMPOUND_BUILTIN(call,0) ||
				goal[1].m_u64val == PACK_ATOM_EMBED_4('c','a','l','l'))
			{
				c = compile_callN(context,c_true,c_fail,goal);
			}
			else
				c = compile_user_defined(context,c_true,c_fail,goal);
			break;

		case prolite_atom:
			c = compile_user_defined(context,c_true,c_fail,goal);
			break;

		default:
			c = compile_call(context,c_true,c_fail,goal);
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

static int compile_term(struct compile_context_t* context, struct continuation_t* c_true, const struct cfg_block_t* c_fail, const union packed_t* goal)
{
	struct continuation_t* c = compile_goal(context,c_true,c_fail,goal);
	if (!c)
		return -1;
	
	union opcode_t* ops = append_opcodes(context,c->m_tail,2);
	if (!ops)
		return -1;

	(ops++)->m_opcode = OP_JMP;
	ops->m_pval = c_fail;
	
	// TODO: Link!!

	dumpCFG(c->m_entry_point,stdout);
	
	return 0;
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

		struct continuation_t* c_true = new_continuation(&cc);
		if (c_true)
		{
			union opcode_t* ops = append_opcodes(&cc,c_true->m_tail,1);
			if (ops)
			{
				ops->m_opcode = OP_TRUE;
		
				struct continuation_t* c_fail = new_continuation(&cc);
				if (c_fail)
				{
					ops = append_opcodes(&cc,c_fail->m_tail,1);
					if (ops)
					{
						ops->m_opcode = OP_END;
						
						compile_term(&cc,c_true,c_fail->m_entry_point,sp);
					}
				}
			}
		}

		/* Bulk free all CFG blocks */
		heap_reset(&context->m_heap,heap_start);
	}

	/* We have just made heavy use of the heap */
	heap_compact(context->m_heap);
}





static void dumpCFGBlock(const struct cfg_block_t* blk, FILE* f)
{
	fprintf(f,"\tnode [shape=record];\n");
	fprintf(f,"\tN%p [label=\"{",blk);

	if (!blk->m_len)
	{
		fprintf(f,"<f0> End");
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
			fprintf(f,"if \\ (builtin_%s)|<f%zu_t> Call",(const char*)blk->m_ops[i+1].m_pval,i);
			i+=2;
			break;

		case OP_THROW:
			fprintf(f,"throw \\ (builtin_%s)",(const char*)blk->m_ops[i+1].m_pval);
			i+=2;
			break;

		case OP_SET_CUT:
			fprintf(f,"Cut\\ =\\ 1");
			break;

		case OP_CLEAR_CUT:
			fprintf(f,"Cut\\ =\\ 0");
			break;

		case OP_PUSH_CUT:
			fprintf(f,"Push\\ Cut");
			break;

		case OP_POP_CUT:
			fprintf(f,"Pop\\ Cut");
			break;

		case OP_BCTH:
			fprintf(f,"Branch\\ CTH|<f%zu_cth> ...cut/throw/halt",i);
			++i;
			break;

		case OP_BTH:
			fprintf(f,"Branch\\ TH|<f%zu_cth> ...throw/halt",i);
			++i;
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
		case OP_BCTH:
			fprintf(f,"\tN%p:<f%zu_cth> -> N%p:<f0> [label=\"CTH\"];\n",blk,i,blk->m_ops[i+1].m_pval);
			++i;
			break;

		case OP_BTH:
			fprintf(f,"\tN%p:<f%zu_cth> -> N%p:<f0> [label=\"TH\"];\n",blk,i,blk->m_ops[i+1].m_pval);
			++i;
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
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [label=\"T\"];\n",blk,i,blk->m_ops[i+2].m_pval);
			i+=2;
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
			case OP_BCTH:
			case OP_BTH:
			case OP_JMP:
			case OP_CALL:
				walkCFG(blks,blk->m_ops[i+1].m_pval);
				++i;
				break;

			case OP_BUILTIN:
			case OP_THROW:
				walkCFG(blks,blk->m_ops[i+2].m_pval);
				i+=2;
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
