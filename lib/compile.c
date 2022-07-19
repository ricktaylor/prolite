#include "compile.h"
#include "builtins.h"
#include "fnv1a.h"

#include <string.h>
#include <stdarg.h>
#include <math.h>

typedef struct cfg_block_info
{
	intptr_t           m_offset;
	const cfg_block_t* m_blk;
} cfg_block_info_t;

typedef struct cfg_vec
{
	size_t             m_count;
	size_t             m_total;
	cfg_block_info_t** m_blks;
	btree_t            m_index;
} cfg_vec_t;

static cfg_block_t* new_cfg_block(compile_context_t* context)
{
	cfg_block_t* b = allocator_malloc(context->m_allocator,sizeof(cfg_block_t));
	if (!b)
		longjmp(context->m_jmp,1);

	*b = (cfg_block_t){0};
	return b;
}

opcode_t* append_opcodes(compile_context_t* context, cfg_block_t* blk, size_t count)
{
	opcode_t* ret = blk->m_ops;
	if (count)
	{
		blk->m_ops = allocator_realloc(context->m_allocator,blk->m_ops,(blk->m_count + count) * sizeof(opcode_t));
		if (!blk->m_ops)
			longjmp(context->m_jmp,1);

		ret = blk->m_ops + blk->m_count;
		blk->m_count += count;
	}

	return ret;
}

cfg_t* new_cfg(compile_context_t* context)
{
	cfg_t* c = allocator_malloc(context->m_allocator,sizeof(cfg_t));
	if (!c)
		longjmp(context->m_jmp,1);

	cfg_block_t* tail = new_cfg_block(context);
	*c = (cfg_t){ .m_tail = tail, .m_entry_point = tail };

	return c;
}

cfg_t* goto_next(compile_context_t* context, cfg_t* c, cfg_t* next)
{
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode = (op_arg_t){ .m_op = OP_JMP };
	ops->m_term.m_pval = next->m_entry_point;
	c->m_tail = next->m_tail;

	return c;
}

cfg_t* add_branch(compile_context_t* context, cfg_t* c, exec_flags_t flags, cfg_t* next)
{
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	(ops++)->m_opcode = (op_arg_t){ .m_op = OP_BRANCH, .m_arg = flags};
	ops->m_term.m_pval = next->m_entry_point;

	return c;
}

void append_ret(compile_context_t* context, cfg_block_t* c)
{
	if (!c->m_count || c->m_ops[c->m_count-1].m_opcode.m_op != OP_RET)
	{
		assert(!c->m_count || c->m_ops[c->m_count-1].m_opcode.m_op != OP_JMP);

		opcode_t* ops = append_opcodes(context,c,1);
		ops->m_opcode = (op_arg_t){ .m_op = OP_RET };
	}
}

const term_t* compile_deref_var(compile_context_t* context, const term_t* t)
{
	while (unpack_term_type(t) == prolite_var)
	{
		size_t idx = unpack_var_index(t);
		assert(context->m_substs && idx < context->m_substs->m_count);
		const term_t* t1 = context->m_substs->m_vals[idx];
		if (!t1)
			break;

		t = t1;
	}
	return t;
}

size_t inc_ip(optype_t op)
{
	size_t ip = 1;
	switch (op)
	{
	case OP_JMP:
	case OP_PUSH_CONST:
	case OP_PUSH_TERM_REF:
	case OP_BRANCH:
	case OP_SET_REG:
	case OP_LOAD_REG:
	case OP_MOV_REG:
		++ip;
		break;

	case OP_BUILTIN:
	case OP_EXTERN:
	case OP_ADD_REG:
	case OP_SUB_REG:
		ip += 2;
		break;

	default:
		break;
	}

	return ip;
}

static int cfg_compare_blk(btree_t* index, const cfg_block_t* blk1, const cfg_block_t* blk2)
{
	if (blk1 == blk2)
		return 1;

	if (!blk1 || !blk2)
		return 0;

	if (blk1->m_count != blk2->m_count)
		return 0;

	if ((const cfg_block_t*)btree_lookup(index,(uintptr_t)blk1) == blk2 ||
		(const cfg_block_t*)btree_lookup(index,(uintptr_t)blk2) == blk1)
		return 1;

	for (size_t i = 0; i < blk1->m_count;)
	{
		if (memcmp(&blk1->m_ops[i],&blk2->m_ops[i],sizeof(opcode_t)))
			return 0;

		size_t inc = inc_ip(blk1->m_ops[i].m_opcode.m_op);

		switch (blk1->m_ops[i].m_opcode.m_op)
		{
		case OP_PUSH_TERM_REF:
			if (!term_compare(blk1->m_ops[i+1].m_term.m_pval,blk2->m_ops[i+1].m_term.m_pval))
				return 0;
			break;

		case OP_JMP:
			if (!cfg_compare_blk(index,blk1->m_ops[i+1].m_term.m_pval,blk2->m_ops[i+1].m_term.m_pval))
				return 0;
			break;

		case OP_BRANCH:
			if (!cfg_compare_blk(index,blk1->m_ops[i+1].m_term.m_pval,blk2->m_ops[i+1].m_term.m_pval))
				return 0;
			break;

		case OP_BUILTIN:
		case OP_EXTERN:
			if (blk1->m_ops[i+1].m_term.m_pval != blk2->m_ops[i+1].m_term.m_pval ||
				!cfg_compare_blk(index,blk1->m_ops[i+2].m_term.m_pval,blk2->m_ops[i+2].m_term.m_pval))
			{
				return 0;
			}
			break;

		default:
			if (inc > 1 && memcmp(&blk1->m_ops[i+1],&blk2->m_ops[i+1],sizeof(opcode_t) * (inc-1)))
				return 0;
			break;
		}

		i += inc;
	}

	// We got here, all matched
	btree_insert(index,(uintptr_t)blk1,(void*)blk2);
	btree_insert(index,(uintptr_t)blk2,(void*)blk1);

	return 1;
}

static int cfg_compare(compile_context_t* context, const cfg_t* c1, const cfg_t* c2)
{
	if (c1 == c2 || c1->m_entry_point == c2->m_entry_point)
		return 1;

	return cfg_compare_blk(&(btree_t){ .m_allocator = context->m_allocator },c1->m_entry_point,c2->m_entry_point);
}

static uint64_t cfg_hash_term(const term_t* t)
{
	uint64_t h = 0;

	switch (unpack_term_type(t))
	{
	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		switch (unpack_term_subtype(t))
		{
		case 0:
		case 3:
			{
				string_t s;
				unpack_string(t,&s,NULL);
				return fnv1a_64(s.m_str,s.m_len);
			}

		default:
			return t->m_u64val;
		}
		break;

	case prolite_compound:
		{
			h = t->m_u64val;
			if (unpack_term_subtype(t) == 0)
				h += cfg_hash_term(t+1);

			size_t arity;
			const term_t* p = get_first_arg(t,&arity);
			while (arity--)
			{
				h += cfg_hash_term(p);
				p = get_next_arg(p);
			}
		}
		break;

	default:
		h = t->m_u64val;
		break;
	}

	return h;
}

static uint64_t cfg_hash_blk(compile_context_t* context, btree_t* duplicates, btree_t* index, btree_t* loop_check, cfg_block_t* blk)
{
	if (blk->m_hash)
		return blk->m_hash;

	if (btree_insert(loop_check,(uintptr_t)blk,blk) != blk)
		return 0;

	uint64_t h = blk->m_count;
	for (size_t i = 0; i < blk->m_count; )
	{
		h += blk->m_ops[i].m_opcode.m_op;

		size_t inc = inc_ip(blk->m_ops[i].m_opcode.m_op);

		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_PUSH_NULL:
		case OP_POP:
		case OP_ALLOC_REGS:
		case OP_FREE_REGS:
		case OP_PUSH_REG:
			h += blk->m_ops[i].m_opcode.m_arg;
			break;

		case OP_LOAD_REG:
		case OP_SET_REG:
		case OP_MOV_REG:
			h += blk->m_ops[i].m_opcode.m_arg;
			h += blk->m_ops[i+1].m_term.m_u64val;
			break;

		case OP_ADD_REG:
		case OP_SUB_REG:
			h += blk->m_ops[i].m_opcode.m_arg;
			h += blk->m_ops[i+1].m_term.m_u64val;
			h += blk->m_ops[i+2].m_term.m_u64val;
			break;

		case OP_PUSH_CONST:
			h += blk->m_ops[i+1].m_term.m_u64val;
			break;

		case OP_PUSH_TERM_REF:
			h += cfg_hash_term(blk->m_ops[i+1].m_term.m_pval);
			break;

		case OP_JMP:
		case OP_BRANCH:
			h += cfg_hash_blk(context,duplicates,index,loop_check,(cfg_block_t*)blk->m_ops[i+1].m_term.m_pval);
			break;

		case OP_BUILTIN:
			h += blk->m_ops[i].m_opcode.m_arg;
			h += blk->m_ops[i+1].m_term.m_u64val;
			if (blk->m_ops[i+2].m_term.m_pval)
				h += cfg_hash_blk(context,duplicates,index,loop_check,(cfg_block_t*)blk->m_ops[i+2].m_term.m_pval);
			break;

		case OP_EXTERN:
			h += blk->m_ops[i+1].m_term.m_u64val;
			if (blk->m_ops[i+2].m_term.m_pval)
				h += cfg_hash_blk(context,duplicates,index,loop_check,(cfg_block_t*)blk->m_ops[i+2].m_term.m_pval);
			break;

		default:
			assert(inc == 1);
			break;
		}

		i += inc;
	}

	if (!h)
		h = 1;

	blk->m_hash = h;

	cfg_block_t* blk2 = btree_insert(index,h,blk);
	if (blk2 && blk != blk2)
	{
		if (cfg_compare(context,&(cfg_t){ .m_entry_point = blk },&(cfg_t){ .m_entry_point = blk2 }))
			btree_insert(duplicates,(uintptr_t)blk,blk2);
	}

	return h;
}

static void cfg_hash(compile_context_t* context, btree_t* duplicates, btree_t* index, const cfg_t* c)
{
	btree_t loop_check = { .m_allocator = index->m_allocator };

	cfg_hash_blk(context,duplicates,index,&loop_check,c->m_entry_point);

	btree_clear(&loop_check,NULL,NULL);
}

static void cfg_fold(void* param, uint64_t key, void* val)
{
	cfg_block_t* blk = (cfg_block_t*)key;

	// Rewrite blk to JMP to blk2
	if (blk->m_count < 2)
		append_opcodes(param,blk,2 - blk->m_count);
	else
		blk->m_count = 2;

	blk->m_ops[0].m_opcode = (op_arg_t){ .m_op = OP_JMP };
	blk->m_ops[1].m_term.m_pval = val;
}

static int cfg_simplify_blk(btree_t* loop_check, cfg_block_t* blk)
{
	if (btree_insert(loop_check,(uintptr_t)blk,blk) != blk)
		return 0;

	int simplify = 0;
	for (size_t i = 0; i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_JMP:
			{
				// Rewrite JMP -> JMP => JMP
				cfg_block_t** next = (cfg_block_t**)&blk->m_ops[i+1].m_term.m_pval;
				while ((*next)->m_count == 2 &&
					(*next)->m_ops[0].m_opcode.m_op == OP_JMP)
				{
					*next = (cfg_block_t*)(*next)->m_ops[1].m_term.m_pval;

					simplify = 1;
				}

				// Rewrite JMP -> RET => RET
				if ((*next)->m_count == 1 && (*next)->m_ops[0].m_opcode.m_op == OP_RET)
				{
					blk->m_ops[i].m_opcode = (struct op_arg){ .m_op = OP_RET };
					--blk->m_count;

					simplify = 1;
				}
				else
					simplify = cfg_simplify_blk(loop_check,*next) || simplify;
			}
			break;

		case OP_BRANCH:
			{
				// Rewrite BRANCH -> JMP => BRANCH
				cfg_block_t** next = (cfg_block_t**)&blk->m_ops[i+1].m_term.m_pval;
				while ((*next)->m_count == 2 &&
					(*next)->m_ops[0].m_opcode.m_op == OP_JMP)
				{
					*next = (cfg_block_t*)(*next)->m_ops[1].m_term.m_pval;

					simplify = 1;
				}

				simplify = cfg_simplify_blk(loop_check,*next) || simplify;
			}
			break;

		case OP_BUILTIN:
		case OP_EXTERN:
			if (blk->m_ops[i+2].m_term.m_pval)
			{
				cfg_block_t** next = (cfg_block_t**)&blk->m_ops[i+2].m_term.m_pval;

				// Rewrite GOSUB -> JMP => GOSUB
				while ((*next)->m_count == 2 &&
					(*next)->m_ops[0].m_opcode.m_op == OP_JMP)
				{
					*next = (cfg_block_t*)(*next)->m_ops[1].m_term.m_pval;

					simplify = 1;
				}

				// Rewrite GOSUB -> RET => NULL
				if ((*next)->m_count == 1 && (*next)->m_ops[0].m_opcode.m_op == OP_RET)
				{
					*next = NULL;

					simplify = 1;
				}
				else
					simplify = cfg_simplify_blk(loop_check,*next) || simplify;
			}
			break;

		default:
			break;
		}
	}

	if (simplify)
	{
		// Clear the hash if we or our children have simplified
		blk->m_hash = 0;
	}

	return simplify;
}

static void cfg_simplify(compile_context_t* context, const cfg_t* c)
{
	// Simplify the blocks
	btree_t index = { .m_allocator = context->m_allocator };
	btree_t duplicates = { .m_allocator = context->m_allocator };
	for (;;)
	{
		cfg_simplify_blk(&index,c->m_entry_point);
		btree_clear(&index,NULL,NULL);

		// See if we can fold parts of the cfg...
		cfg_hash(context,&duplicates,&index,c);
		btree_clear(&index,NULL,NULL);

		size_t f = btree_clear(&duplicates,&cfg_fold,context);
		if (!f)
			break;
	}
}

static cfg_t* compile_true(compile_context_t* context, const continuation_t* goal)
{
	return compile_subgoal(context,goal->m_next);
}

static cfg_t* compile_false(compile_context_t* context, const continuation_t* goal)
{
	return NULL;
}

static cfg_t* compile_cut(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,1);
	ops->m_opcode = (op_arg_t){ .m_op = OP_CUT};

	context->m_flags |= FLAG_CUT;

	cfg_t* c2 = compile_subgoal(context,goal->m_next);
	if (c2 && !(context->m_flags & (FLAG_HALT | FLAG_THROW)))
		c = goto_next(context,c,c2);

	return c;
}

static cfg_t* compile_and(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);

	return compile_subgoal(context,&(continuation_t){
		.m_term = g1,
		.m_next = &(continuation_t){
			.m_term = g2,
			.m_next = goal->m_next
		}
	});
}

cfg_t* compile_builtin(compile_context_t* context, builtin_fn_t fn, const term_t* arg, const continuation_t* next)
{
	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode = (op_arg_t){ .m_op = OP_BUILTIN, .m_arg = (arg ? 1 : 0) };
	(ops++)->m_term.m_pval = fn;

	cfg_t* cont = compile_subgoal(context,next);
	if (cont)
	{
		append_ret(context,cont->m_tail);
		ops->m_term.m_pval = cont->m_entry_point;
	}
	else
		ops->m_term.m_pval = NULL;

	if (arg)
	{
		cfg_t* c1 = new_cfg(context);
		ops = append_opcodes(context,c1->m_tail,2);
		(ops++)->m_opcode = (op_arg_t){ .m_op = OP_PUSH_TERM_REF };
		ops->m_term.m_pval = arg;

		c = goto_next(context,c1,c);

		ops = append_opcodes(context,c->m_tail,1);
		ops->m_opcode = (op_arg_t){ .m_op = OP_POP, .m_arg = 1 };
	}

	return c;
}

static cfg_t* compile_builtinN(compile_context_t* context, builtin_fn_t fn, size_t arity, const term_t* arg, const continuation_t* next)
{
	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode = (op_arg_t){ .m_op = OP_BUILTIN, .m_arg = arity };
	(ops++)->m_term.m_pval = fn;

	cfg_t* cont = compile_subgoal(context,next);
	if (cont)
	{
		append_ret(context,cont->m_tail);
		ops->m_term.m_pval = cont->m_entry_point;
	}
	else
		ops->m_term.m_pval = NULL;

	if (arity)
	{
		cfg_t* c1 = new_cfg(context);
		ops = append_opcodes(context,c1->m_tail,2 * arity);
		for (size_t i = 1; i <= arity; ++i)
		{
			ops[(arity - i)*2].m_opcode = (op_arg_t){ .m_op = OP_PUSH_TERM_REF };
			ops[(arity - i)*2 + 1].m_term.m_pval = compile_deref_var(context,arg);
			arg = get_next_arg(arg);
		}

		c = goto_next(context,c1,c);

		while (arity)
		{
			ops = append_opcodes(context,c->m_tail,1);
			ops->m_opcode = (op_arg_t){ .m_op = OP_POP, .m_arg = (arity > c_op_arg_max ? c_op_arg_max : arity) };
			arity -= ops->m_opcode.m_arg;
		}
	}

	return c;
}

static int compile_can_call(compile_context_t* context, const term_t* goal)
{
	switch (unpack_term_type(goal))
	{
	case prolite_var:
	case prolite_atom:
		return 1;

	case prolite_compound:
		switch (MASK_DEBUG_INFO(goal->m_u64val))
		{
		case PACK_COMPOUND_EMBED_1(2,','):
		case PACK_COMPOUND_EMBED_1(2,';'):
		case PACK_COMPOUND_EMBED_2(2,'-','>'):
			{
				size_t arity;
				for (const term_t* p = get_first_arg(goal,&arity); arity--; p = get_next_arg(p))
				{
					if (!compile_can_call(context,compile_deref_var(context,p)))
						return 0;
				}
			}
			break;

		default:
			break;
		}
		return 1;

	default:
		return 0;
	}
}

static cfg_t* compile_call_begin_shim(compile_context_t* context, const continuation_t* goal)
{
	exec_flags_t flags = *(exec_flags_t*)goal->m_term;

	cfg_t* c = compile_subgoal(context,goal->m_next);
	if (c)
	{
		cfg_t* c1 = new_cfg(context);

		opcode_t* ops = append_opcodes(context,c1->m_tail,1);
		ops->m_opcode = (op_arg_t){ .m_op = OP_PUSH_CUT };

		c = goto_next(context,c1,c);

		ops = append_opcodes(context,c->m_tail,1);
		ops->m_opcode = (op_arg_t){ .m_op = OP_POP_CUT };
	}

	context->m_flags = (context->m_flags & ~FLAG_CUT) | flags;

	return c;
}

static cfg_t* compile_call_end_shim(compile_context_t* context, const continuation_t* goal)
{
	context->m_flags = *(exec_flags_t*)goal->m_term;

	cfg_t* c = compile_subgoal(context,goal->m_next);
	if (c)
	{
		cfg_t* c1 = new_cfg(context);

		opcode_t* ops = append_opcodes(context,c1->m_tail,1);
		ops->m_opcode = (op_arg_t){ .m_op = OP_POP_CUT };

		c = goto_next(context,c1,c);

		ops = append_opcodes(context,c->m_tail,1);
		ops->m_opcode = (op_arg_t){ .m_op = OP_PUSH_CUT };
	}

	return c;
}

static cfg_t* compile_call_term(compile_context_t* context, const continuation_t* goal)
{
	const term_t* t = goal->m_term;
	while (t->m_u64val == PACK_COMPOUND_EMBED_4(1,'c','a','l','l'))
		t = compile_deref_var(context,get_first_arg(t,NULL));

	if (t->m_u64val == PACK_ATOM_EMBED_1('!'))
		return compile_subgoal(context,goal->m_next);

	exec_flags_t flags = context->m_flags;

	switch (unpack_term_type(t))
	{
	case prolite_var:
		return compile_builtin(context,&prolite_builtin_call,t,goal->m_next);

	case prolite_atom:
		return compile_subgoal(context,&(continuation_t){
			.m_term = (const term_t*)&flags,
			.m_shim = &compile_call_begin_shim,
			.m_next = &(continuation_t){
				.m_term = t,
				.m_next = &(continuation_t){
					.m_shim = &compile_call_end_shim,
					.m_term = (const term_t*)&flags,
					.m_next = goal->m_next
				}
			}
		});

	case prolite_compound:
		if (!compile_can_call(context,t))
		{
			context->m_flags |= FLAG_THROW;
			return compile_builtin(context,&prolite_builtin_throw_callable,t,NULL);
		}

		return compile_subgoal(context,&(continuation_t){
			.m_term = (const term_t*)&flags,
			.m_shim = &compile_call_begin_shim,
			.m_next = &(continuation_t){
				.m_term = t,
				.m_next = &(continuation_t){
					.m_shim = &compile_call_end_shim,
					.m_term = (const term_t*)&flags,
					.m_next = goal->m_next
				}
			}
		});

	default:
		context->m_flags |= FLAG_THROW;
		return compile_builtin(context,&prolite_builtin_throw_callable,t,NULL);
	}
}

static cfg_t* compile_call(compile_context_t* context, const continuation_t* goal)
{
	return compile_call_term(context,&(continuation_t){
		.m_term = compile_deref_var(context,get_first_arg(goal->m_term,NULL)),
		.m_next = goal->m_next
	});
}

static cfg_t* compile_once_shim(compile_context_t* context, const continuation_t* goal)
{
	return compile_subgoal(context,&(continuation_t){
		.m_term = goal->m_term,
		.m_next = &(continuation_t){
			.m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_1('!') },
			.m_next = goal->m_next
		}
	});
}

static cfg_t* compile_once_term(compile_context_t* context, const continuation_t* goal)
{
	return compile_call_term(context,&(continuation_t){
		.m_term = compile_deref_var(context,goal->m_term),
		.m_shim = &compile_once_shim,
		.m_next = goal->m_next
	});
}

static cfg_t* compile_once(compile_context_t* context, const continuation_t* goal)
{
	return compile_once_term(context,&(continuation_t){
		.m_term = get_first_arg(goal->m_term,NULL),
		.m_next = goal->m_next
	});
}

static cfg_t* compile_or(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);

	g1 = compile_deref_var(context,g1);
	g2 = compile_deref_var(context,g2);

	cfg_t* c;
	if (MASK_DEBUG_INFO(g1->m_u64val) == PACK_COMPOUND_EMBED_2(2,'-','>'))
	{
		const term_t* g_if = get_first_arg(g1,NULL);
		const term_t* g_then = get_next_arg(g_if);

		c = compile_once_term(context,&(continuation_t){
			.m_term = compile_deref_var(context,g_if),
			.m_next = &(continuation_t){
				.m_term = g_then,
				.m_next = goal->m_next
			}
		});
	}
	else
	{
		c = compile_subgoal(context,&(continuation_t){
			.m_term = g1,
			.m_next = goal->m_next
		});
	}

	if (!c)
	{
		c = compile_subgoal(context,&(continuation_t){
			.m_term = g2,
			.m_next = goal->m_next
		});
	}
	else if (!(context->m_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
	{
		cfg_t* c2 = compile_subgoal(context,&(continuation_t){
			.m_term = g2,
			.m_next = goal->m_next
		});

		if (c2)
		{
			cfg_t* c_end = new_cfg(context);
			goto_next(context,c2,c_end);

			add_branch(context,c,FLAG_CUT | FLAG_THROW | FLAG_HALT,c_end);

			goto_next(context,c,c2);
		}
	}

	return c;
}

static cfg_t* compile_if_then(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g_if = get_first_arg(goal->m_term,NULL);
	const term_t* g_then = get_next_arg(g_if);

	return compile_once_term(context,&(continuation_t){
		.m_term = g_if,
		.m_next = &(continuation_t){
			.m_term = g_then,
			.m_next = goal->m_next
		}
	});
}

static cfg_t* compile_throw(compile_context_t* context, const continuation_t* goal)
{
	context->m_flags |= FLAG_THROW;
	return compile_builtin(context,&prolite_builtin_throw,compile_deref_var(context,get_first_arg(goal->m_term,NULL)),NULL);
}

static cfg_t* compile_halt(compile_context_t* context, const continuation_t* goal)
{
	const term_t* arg = NULL;

	if (unpack_term_type(goal->m_term) != prolite_atom)
	{
		arg = compile_deref_var(context,get_first_arg(goal->m_term,NULL));
		switch (unpack_term_type(arg))
		{
		case prolite_number:
			context->m_flags |= FLAG_HALT;
			break;

		case prolite_var:
			// builtin_halt could throw not halt ;)
			break;

		default:
			context->m_flags |= FLAG_THROW;
			break;
		}
	}

	return compile_builtin(context,&prolite_builtin_halt,arg,NULL);
}

static cfg_t* compile_resume_shim(compile_context_t* context, const continuation_t* goal)
{
	return compile_call_term(context,&(continuation_t){
		.m_term = goal->m_term,
		.m_next = goal->m_next
	});
}

static cfg_t* compile_catch(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);
	const term_t* g3 = get_next_arg(g2);

	g1 = compile_deref_var(context,g1);
	g2 = compile_deref_var(context,g2);
	g3 = compile_deref_var(context,g3);

	cfg_t* c = compile_call_term(context,&(continuation_t){
		.m_term = g1,
		.m_next = goal->m_next
	});

	if (c && !(context->m_flags & FLAG_HALT))
	{
		exec_flags_t prev_flags = context->m_flags;
		context->m_flags &= ~FLAG_THROW;

		cfg_t* c_catch = compile_builtin(context,&prolite_builtin_catch,g2,&(continuation_t){
			.m_term = g3,
			.m_shim = &compile_resume_shim,
			.m_next = goal->m_next
		});

		if (prev_flags & FLAG_THROW)
			goto_next(context,c,c_catch);
		else
		{
			cfg_t* c_end = new_cfg(context);
			goto_next(context,c_catch,c_end);

			add_branch(context,c,FLAG_THROW,c_catch);
			goto_next(context,c,c_end);
		}
	}

	return c;
}

static cfg_t* compile_repeat(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = compile_subgoal(context,goal->m_next);
	if (!c)
	{
		// TODO: Warning - endless loop!
		c = new_cfg(context);
	}

	if (!(context->m_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT)))
	{
		cfg_t* c_end = new_cfg(context);

		add_branch(context,c,FLAG_CUT | FLAG_THROW | FLAG_HALT,c_end);

		goto_next(context,c,c);
		c->m_tail = c_end->m_entry_point;
	}

	return c;
}

static int compile_occurs_check(compile_context_t* context, const term_t* t1, const term_t* t2)
{
	switch (unpack_term_type(t2))
	{
	case prolite_var:
		if (t1->m_u64val == t2->m_u64val)
			return 1;
		break;

	case prolite_charcodes:
	case prolite_chars:
		assert(0);
		break;

	case prolite_compound:
		{
			size_t arity;
			t2 = get_first_arg(t2,&arity);

			for (size_t i = 0; i < arity; ++i)
			{
				if (compile_occurs_check(context,t1,compile_deref_var(context,t2)))
					return 1;

				t2 = get_next_arg(t2);
			}

		}

	default:
		break;
	}

	return 0;
}

static cfg_t* compile_unify_shim(compile_context_t* context, const continuation_t* goal)
{
	term_t** t = (term_t**)goal->m_term;

	size_t idx = unpack_var_index(t[0]);
	assert(context->m_substs && idx < context->m_substs->m_count);

	cfg_t* c = compile_subgoal(context,goal->m_next);
	if (c)
	{
		cfg_t* c1 = new_cfg(context);
		opcode_t* ops = append_opcodes(context,c1->m_tail,4);
		(ops++)->m_opcode = (op_arg_t){ .m_op = OP_PUSH_TERM_REF };
		(ops++)->m_term.m_pval = t[0];
		(ops++)->m_opcode = (op_arg_t){ .m_op = OP_PUSH_TERM_REF };
		ops->m_term.m_pval = t[1];

		c = goto_next(context,c1,c);
	}

	allocator_free(context->m_allocator,t);

	return c;
}

typedef struct unify_info
{
	size_t m_var_count;
	int    m_with_occurs_check;
} unify_info_t;

typedef struct extern_info
{
	const compile_clause_t* m_clause;
	substitutions_t*        m_next_substs;
} extern_info_t;

static const continuation_t* compile_unify_var(compile_context_t* context, const term_t* t1, const term_t* t2, unify_info_t* ui, const continuation_t* next)
{
	size_t idx = unpack_var_index(t1);
	assert(context->m_substs && idx < context->m_substs->m_count && context->m_substs->m_vals[idx] == NULL);

	context->m_substs->m_vals[idx] = t2;

	++ui->m_var_count;

	const term_t** t = allocator_malloc(context->m_allocator,2 * sizeof(term_t*));
	if (!t)
		longjmp(context->m_jmp,1);

	t[0] = t1;
	t[1] = t2;

	continuation_t* c = allocator_malloc(context->m_allocator,sizeof(continuation_t));
	if (!c)
		longjmp(context->m_jmp,1);

	*c = (continuation_t){
		.m_term = (const term_t*)t,
		.m_shim = &compile_unify_shim,
		.m_next = next
	};

	return c;
}

static const continuation_t* compile_unify_term(compile_context_t* context, const term_t* t1, const term_t* t2, unify_info_t* ui, const continuation_t* next)
{
	prolite_type_t type1 = unpack_term_type(t1);
	if (type1 == prolite_var)
	{
		if (t1->m_u64val == t2->m_u64val)
			return next;

		return compile_unify_var(context,t1,t2,ui,next);
	}

	prolite_type_t type2 = unpack_term_type(t2);
	switch (type2)
	{
	case prolite_var:
		return compile_unify_term(context,t2,t1,ui,next);

	case prolite_charcodes:
	case prolite_chars:
		assert(0);
		break;

	case prolite_compound:
		if (predicate_compare(t1,t2))
		{
			size_t arity;
			t1 = get_first_arg(t1,&arity);
			t2 = get_first_arg(t2,NULL);

			for (size_t i = 0; i < arity; ++i)
			{
				next = compile_unify_term(context,compile_deref_var(context,t1),compile_deref_var(context,t2),ui,next);
				if (!next)
					break;

				t1 = get_next_arg(t1);
				t2 = get_next_arg(t2);
			}
			return next;
		}
		break;

	default:
		if (term_compare(t1,t2))
			return next;
		break;
	}

	return NULL;
}

static cfg_t* compile_unify_end_shim(compile_context_t* context,const continuation_t* goal)
{
	unify_info_t* ui = (unify_info_t*)goal->m_term;

	cfg_t* cont = compile_subgoal(context,goal->m_next);
	if (!cont || ui->m_var_count == 0)
		return cont;

	append_ret(context,cont->m_tail);

	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode = (op_arg_t){ .m_op = OP_BUILTIN, .m_arg = ui->m_var_count * 2 };
	(ops++)->m_term.m_pval = ui->m_with_occurs_check ? &prolite_builtin_unify_with_occurs_check : &prolite_builtin_unify;
	ops->m_term.m_pval = cont->m_entry_point;

	for (size_t a = (ui->m_var_count*2); a != 0;)
	{
		opcode_t* ops = append_opcodes(context,c->m_tail,1);
		ops->m_opcode = (op_arg_t){ .m_op = OP_POP, .m_arg = (a > c_op_arg_max ? c_op_arg_max : a) };
		a -= ops->m_opcode.m_arg;
	}

	return c;
}

static cfg_t* compile_unify_inner(compile_context_t* context, const term_t* t1, const term_t* t2, cfg_t* (*fn)(compile_context_t*,const continuation_t*), const continuation_t* next, int with_occurs_check)
{
	t1 = compile_deref_var(context,t1);
	t2 = compile_deref_var(context,t2);

	if (compile_occurs_check(context,t1,t2))
		return NULL;

	substitutions_t* prev_substs = context->m_substs;
	if (context->m_substs)
	{
		substitutions_t* substs = allocator_malloc(context->m_allocator,sizeof(substitutions_t) + (sizeof(term_t) * context->m_substs->m_count));
		if (!substs)
			longjmp(context->m_jmp,1);

		substs->m_count = context->m_substs->m_count;
		memcpy(substs->m_vals,context->m_substs->m_vals,context->m_substs->m_count * sizeof(term_t*));

		context->m_substs = substs;
	}

	unify_info_t ui = {
		.m_with_occurs_check = with_occurs_check
	};

	const continuation_t* cont = &(continuation_t){
		.m_shim = &compile_unify_end_shim,
		.m_term = (const term_t*)&ui,
		.m_next = next
	};

	cfg_t* c = NULL;
	const continuation_t* cont2 = compile_unify_term(context,t1,t2,&ui,cont);
	if (cont2)
	{
		if (cont2 == cont)
			c = (*fn)(context,next);
		else
			c = (*fn)(context,cont2);
	}

	while (cont2 != cont)
	{
		const continuation_t* n = cont2->m_next;
		allocator_free(context->m_allocator,(continuation_t*)cont2);
		cont2 = n;
	}

	allocator_free(context->m_allocator,context->m_substs);
	context->m_substs = prev_substs;

	return c;
}

cfg_t* compile_unify_terms(compile_context_t* context, const term_t* t1, const term_t* t2, const continuation_t* next)
{
	return compile_unify_inner(context,t1,t2,&compile_subgoal,next,0);
}

static cfg_t* compile_unify(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);

	return compile_unify_inner(context,g1,g2,&compile_subgoal,goal->m_next,0);
}

static cfg_t* compile_unify_with_occurs_check(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);

	return compile_unify_inner(context,g1,g2,&compile_subgoal,goal->m_next,1);
}

static cfg_t* compile_not_unifiable(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = compile_subgoal(context,goal->m_next);

	const term_t* g1 = get_first_arg(goal->m_term,NULL);
	const term_t* g2 = get_next_arg(g1);

	cfg_t* c2 = compile_unify_inner(context,g1,g2,&compile_once_term,NULL,0);
	if (!c)
		c = c2;
	else if (c2)
		c = goto_next(context,c2,c);

	return c;
}

static const continuation_t* compile_unify_head_term(compile_context_t* context, const term_t* t1, const term_t* t2, substitutions_t* next_substs, unify_info_t* ui, const continuation_t* next)
{
	prolite_type_t type1 = unpack_term_type(t1);
	prolite_type_t type2 = unpack_term_type(t2);
	if (type2 == prolite_var)
	{
		// Equivalent of defer_var for t2/next_substs;
		size_t idx = unpack_var_index(t2);
		assert(next_substs && idx < next_substs->m_count);

		if (!next_substs->m_vals[idx])
		{
			next_substs->m_vals[idx] = t1;
			return next;
		}

		return compile_unify_term(context,t1,next_substs->m_vals[idx],ui,next);
	}

	if (type1 == prolite_var)
		return compile_unify_var(context,t1,t2,ui,next);

	switch (type2)
	{
	case prolite_charcodes:
	case prolite_chars:
		assert(0);
		break;

	case prolite_compound:
		if (predicate_compare(t1,t2))
		{
			size_t arity;
			t1 = get_first_arg(t1,&arity);
			t2 = get_first_arg(t2,NULL);

			for (size_t i = 0; i < arity; ++i)
			{
				next = compile_unify_head_term(context,compile_deref_var(context,t1),t2,next_substs,ui,next);
				if (!next)
					break;

				t1 = get_next_arg(t1);
				t2 = get_next_arg(t2);
			}
			return next;
		}
		break;

	default:
		if (term_compare(t1,t2))
			return next;
		break;
	}

	return NULL;
}

static cfg_t* compile_extern_shim(compile_context_t* context, const continuation_t* goal)
{
	// Push the original substs onto the stack as 'new' substs
	substitutions_t* prev_substs = context->m_substs;
	context->m_substs = (substitutions_t*)goal->m_term;

	cfg_t* c = compile_subgoal(context,goal->m_next);

	context->m_substs = prev_substs;

	return c;
}

static cfg_t* compile_head_end_shim(compile_context_t* context, const continuation_t* goal)
{
	extern_info_t* ei = (extern_info_t*)goal->m_term;

	substitutions_t* prev_substs = context->m_substs;

	cfg_t* c1;
	if (ei->m_next_substs)
	{
		substitutions_t* substs = allocator_malloc(context->m_allocator,sizeof(substitutions_t) + (sizeof(term_t) * ei->m_next_substs->m_count));
		if (!substs)
			longjmp(context->m_jmp,1);

		substs->m_count = ei->m_next_substs->m_count;

		c1 = new_cfg(context);
		opcode_t* ops = NULL;
		for (size_t i = ei->m_next_substs->m_count; i--;)
		{
			if (ei->m_next_substs->m_vals[i])
			{
				const term_t* t = compile_deref_var(context,ei->m_next_substs->m_vals[i]);
				if (unpack_term_type(t) == prolite_var)
				{
					if (ops && ops->m_opcode.m_arg < c_op_arg_max)
						++ops->m_opcode.m_arg;
					else
					{
						ops = append_opcodes(context,c1->m_tail,1);
						ops->m_opcode = (op_arg_t){ .m_op = OP_PUSH_NULL, .m_arg = 1};
					}

					substs->m_vals[i] = NULL;
				}
				else
				{
					ops = append_opcodes(context,c1->m_tail,2);
					(ops++)->m_opcode = (op_arg_t){ .m_op = OP_PUSH_TERM_REF };
					ops->m_term.m_pval = t;
					ops = NULL;

					substs->m_vals[i] = t;
				}
			}
			else
			{
				if (ops && ops->m_opcode.m_arg < c_op_arg_max)
					++ops->m_opcode.m_arg;
				else
				{
					ops = append_opcodes(context,c1->m_tail,1);
					ops->m_opcode = (op_arg_t){ .m_op = OP_PUSH_NULL, .m_arg = 1};
				}

				substs->m_vals[i] = NULL;
			}
		}

		context->m_substs = substs;
	}

	cfg_t* c = compile_subgoal(context,&(continuation_t){
		.m_term = ei->m_clause->m_body,
		.m_next = &(continuation_t){
			.m_term = (const term_t*)prev_substs,
			.m_shim = &compile_extern_shim,
			.m_next = goal->m_next
		}
	});

	if (c && ei->m_next_substs)
	{
		c = goto_next(context,c1,c);

		for (size_t a = ei->m_next_substs->m_count; a != 0;)
		{
			opcode_t* ops = append_opcodes(context,c->m_tail,1);
			ops->m_opcode = (op_arg_t){ .m_op = OP_POP, .m_arg = (a > c_op_arg_max ? c_op_arg_max : a) };
			a -= ops->m_opcode.m_arg;
		}

		allocator_free(context->m_allocator,context->m_substs);
		context->m_substs = prev_substs;
	}

	return c;
}

static cfg_t* compile_head(compile_context_t* context, const term_t* goal, const compile_clause_t* clause, const continuation_t* next)
{
	substitutions_t* prev_substs = context->m_substs;
	if (context->m_substs)
	{
		substitutions_t* substs = allocator_malloc(context->m_allocator,sizeof(substitutions_t) + (sizeof(term_t) * context->m_substs->m_count));
		if (!substs)
			longjmp(context->m_jmp,1);

		substs->m_count = context->m_substs->m_count;
		memcpy(substs->m_vals,context->m_substs->m_vals,context->m_substs->m_count * sizeof(term_t*));

		context->m_substs = substs;
	}

	extern_info_t ei = { .m_clause = clause };
	if (clause->m_var_count)
	{
		ei.m_next_substs = allocator_malloc(context->m_allocator,sizeof(substitutions_t) + (sizeof(term_t) * clause->m_var_count));
		if (!ei.m_next_substs)
			longjmp(context->m_jmp,1);

		ei.m_next_substs->m_count = clause->m_var_count;
		memset(ei.m_next_substs->m_vals,0,clause->m_var_count * sizeof(term_t*));
	}

	unify_info_t ui = {0};
	continuation_t* cont = &(continuation_t){
		.m_shim = &compile_unify_end_shim,
		.m_term = (const term_t*)&ui,
		.m_next = &(continuation_t){
			.m_shim = &compile_head_end_shim,
			.m_term = (const term_t*)&ei,
			.m_next = next
		}
	};

	if (!clause->m_body)
		cont->m_next = next;

	cfg_t* c = NULL;
	const continuation_t* cont2 = compile_unify_head_term(context,goal,clause->m_head,ei.m_next_substs,&ui,cont);
	if (cont2)
	{
		if (cont2 == cont)
			cont2 = cont->m_next;

		c = compile_subgoal(context,cont2);
	}

	allocator_free(context->m_allocator,ei.m_next_substs);
	allocator_free(context->m_allocator,context->m_substs);
	context->m_substs = prev_substs;

	return c;
}

static int substs_compare(const substitutions_t* s1, const substitutions_t* s2)
{
	int r = (s1 == s2);
	if (!r && s1 && s2)
	{
		if (s1->m_count == s2->m_count)
		{
			for (size_t i = 0; i < s1->m_count; ++i)
			{
				if (s1->m_vals[i] != s2->m_vals[i])
				{
					if (!s1->m_vals[i] || !s2->m_vals[i] || !term_compare(s1->m_vals[i],s2->m_vals[i]))
						return 0;
				}
			}
			r = 1;
		}
	}

	return r;
}

cfg_t* compile_predicate_call(compile_context_t* context, const compile_predicate_t* pred, const term_t* goal, const continuation_t* next)
{
	if (!pred || !pred->m_clauses || pred->m_dynamic)
		return compile_builtin(context,&prolite_builtin_user_defined,goal,next);

	// Check for recursive call
	for (compile_recursion_t* r = context->m_recursion; r; r = r->m_prev)
	{
		if (r->m_pred == pred && substs_compare(context->m_substs,r->m_substs))
		{
			/*if (r == context->m_recursion)
			{
				// TODO: Tail call!
			}*/

			return goto_next(context,new_cfg(context),r->m_cfg);
		}
	}

	context->m_recursion = &(compile_recursion_t){
		.m_prev = context->m_recursion,
		.m_pred = pred,
		.m_substs = context->m_substs,
		.m_cfg = new_cfg(context)
	};

	cfg_t* c_end = NULL;
	for (const compile_clause_t* clause = pred->m_clauses; clause; clause = clause->m_next)
	{
		cfg_t* c1 = compile_head(context,goal,clause,next);
		if (c1)
			goto_next(context,context->m_recursion->m_cfg,c1);

		if (context->m_flags & (FLAG_CUT | FLAG_THROW | FLAG_HALT))
			break;

		if (c1 && clause->m_next)
		{
			if (!c_end)
				c_end = new_cfg(context);

			add_branch(context,context->m_recursion->m_cfg,FLAG_CUT | FLAG_THROW | FLAG_HALT,c_end);
		}
	}

	if (c_end)
		goto_next(context,context->m_recursion->m_cfg,c_end);

	if (pred->m_clauses->m_next)
		cfg_simplify(context,context->m_recursion->m_cfg);

	if (context->m_recursion->m_cfg->m_entry_point->m_count)
		c_end = context->m_recursion->m_cfg;

	context->m_recursion = context->m_recursion->m_prev;

	return c_end;
}

static cfg_t* compile_user_defined(compile_context_t* context, const continuation_t* goal)
{
	return (*context->m_link_fn)(context,context->m_link_param,goal->m_term,goal->m_next);
}

static cfg_t* compile_not_proveable(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));
	if (g1->m_u64val == PACK_COMPOUND_EMBED_2(1,'\\','+'))
	{
		return compile_call_term(context,&(continuation_t){
			.m_term = compile_deref_var(context,get_first_arg(g1,NULL)),
			.m_next = goal->m_next
		});
	}

	cfg_t* c1 = compile_once_term(context,&(continuation_t){
		.m_term = g1,
		.m_next = NULL
	});

	if (context->m_flags & (FLAG_THROW | FLAG_HALT))
		return c1;

	cfg_t* c = compile_subgoal(context,goal->m_next);
	if (!c)
		c = c1;
	else if (c1)
	{
		cfg_t* c_end = new_cfg(context);
		goto_next(context,c,c_end);

		add_branch(context,c1,FLAG_THROW | FLAG_HALT,c_end);

		c = goto_next(context,c1,c);
	}

	return c;
}

static int compile_is_callable(compile_context_t* context, const term_t* goal)
{
	switch (unpack_term_type(goal))
	{
	case prolite_atom:
		return 1;

	case prolite_compound:
		switch (MASK_DEBUG_INFO(goal->m_u64val))
		{
		case PACK_COMPOUND_EMBED_1(2,','):
		case PACK_COMPOUND_EMBED_1(2,';'):
		case PACK_COMPOUND_EMBED_2(2,'-','>'):
			{
				size_t arity;
				for (const term_t* p = get_first_arg(goal,&arity); arity--; p = get_next_arg(p))
				{
					if (!compile_is_callable(context,compile_deref_var(context,p)))
						return 0;
				}
			}
			break;

		default:
			break;
		}
		return 1;

	default:
		return 0;
	}
}

static cfg_t* compile_callable(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (compile_is_callable(context,g1))
	{
	case 1:
		return compile_subgoal(context,goal->m_next);

	case 0:
		return NULL;

	default:
		return compile_builtin(context,&prolite_builtin_callable,g1,goal->m_next);
	}
}

static cfg_t* compile_type_test(compile_context_t* context, prolite_type_flags_t types, int negate, const term_t* t, const continuation_t* next)
{
	cfg_t* cont = compile_subgoal(context,next);
	if (!cont)
		return NULL;

	append_ret(context,cont->m_tail);

	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,8);
	(ops++)->m_opcode = (op_arg_t){ .m_op = OP_PUSH_CONST };
	(ops++)->m_term.m_dval = types;
	(ops++)->m_opcode = (op_arg_t){ .m_op = OP_PUSH_TERM_REF };
	(ops++)->m_term.m_pval = t;
	(ops++)->m_opcode = (op_arg_t){ .m_op = OP_BUILTIN, .m_arg = 2 };
	(ops++)->m_term.m_pval = &prolite_builtin_type_test;
	(ops++)->m_term.m_pval = cont->m_entry_point;
	ops->m_opcode = (op_arg_t){ .m_op = OP_POP, .m_arg = 2 };

	return c;
}

static cfg_t* compile_var(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));

	if (unpack_term_type(g1) != prolite_var)
		return NULL;

	return compile_type_test(context,type_flag_var,0,g1,goal->m_next);
}

static cfg_t* compile_atom(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (unpack_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_atom,0,g1,goal->m_next);

	case prolite_atom:
		return compile_subgoal(context,goal->m_next);

	default:
		return NULL;
	}
}

static cfg_t* compile_integer(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (unpack_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_integer,0,g1,goal->m_next);

	case prolite_number:
		if (nearbyint(g1->m_dval) != g1->m_dval)
			return NULL;

		return compile_subgoal(context,goal->m_next);

	default:
		return NULL;
	}
}

static cfg_t* compile_float(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (unpack_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_double,0,g1,goal->m_next);

	case prolite_number:
		if (nearbyint(g1->m_dval) == g1->m_dval)
			return NULL;

		return compile_subgoal(context,goal->m_next);

	default:
		return NULL;
	}
}

static cfg_t* compile_atomic(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (unpack_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_var | type_flag_chars | type_flag_charcodes | type_flag_compound,1,g1,goal->m_next);

	case prolite_chars:
	case prolite_charcodes:
	case prolite_compound:
		return NULL;

	default:
		return compile_subgoal(context,goal->m_next);
	}
}

static cfg_t* compile_compound(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (unpack_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_chars | type_flag_charcodes | type_flag_compound,0,g1,goal->m_next);

	case prolite_chars:
	case prolite_charcodes:
	case prolite_compound:
		return compile_subgoal(context,goal->m_next);

	default:
		return NULL;
	}
}

static cfg_t* compile_nonvar(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));
	if (unpack_term_type(g1) == prolite_var)
		return compile_type_test(context,type_flag_var,1,g1,goal->m_next);

	return compile_subgoal(context,goal->m_next);
}

static cfg_t* compile_number(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));
	switch (unpack_term_type(g1))
	{
	case prolite_var:
		return compile_type_test(context,type_flag_integer | type_flag_double,0,g1,goal->m_next);

	case prolite_number:
		return compile_subgoal(context,goal->m_next);

	default:
		return NULL;
	}
}

static int compile_is_ground(compile_context_t* context, const term_t* goal)
{
	goal = compile_deref_var(context,goal);
	prolite_type_t t = unpack_term_type(goal);
	if (t == prolite_var)
		return 0;

	if (t == prolite_compound)
	{
		size_t arity;
		for (const term_t* p = get_first_arg(goal,&arity); arity--; p = get_next_arg(p))
		{
			if (!compile_is_ground(context,p))
				return 0;
		}
	}
	return 1;
}

static cfg_t* compile_ground(compile_context_t* context, const continuation_t* goal)
{
	const term_t* g1 = compile_deref_var(context,get_first_arg(goal->m_term,NULL));

	if (compile_is_ground(context,g1))
		return compile_subgoal(context,goal->m_next);

	return compile_builtin(context,&prolite_builtin_ground,g1,goal->m_next);
}

cfg_t* compile_is(compile_context_t* context, const continuation_t* goal);

static cfg_t* compile_subterm(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = NULL;

	const debug_info_t* debug_info = unpack_debug_info(goal->m_term);

	switch (MASK_DEBUG_INFO(goal->m_term->m_u64val))
	{
#define DECLARE_BUILTIN_INTRINSIC(f,p) \
	case (p): c = compile_##f(context,goal); break;

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,a,p) \
	case (p): c = compile_builtinN(context,&prolite_builtin_##f,a,a ? get_first_arg(goal->m_term,NULL) : NULL,goal->m_next); break;

#include "builtin_functions.h"

	default:
		switch (unpack_term_type(goal->m_term))
		{
		case prolite_compound:
			if ((MASK_DEBUG_INFO(goal->m_term->m_u64val) & PACK_COMPOUND_EMBED_MASK) == PACK_COMPOUND_EMBED_4(0,'c','a','l','l') ||
				MASK_DEBUG_INFO(goal->m_term[1].m_u64val) == PACK_ATOM_EMBED_4('c','a','l','l'))
			{
				size_t arity = 0;
				const term_t* t = get_first_arg(goal->m_term,&arity);
				c = compile_builtinN(context,&prolite_builtin_callN,arity,t,goal->m_next);
			}
			else
				c = compile_user_defined(context,goal);
			break;

		case prolite_atom:
			c = compile_user_defined(context,goal);
			break;

		default:
			c = compile_call_term(context,&(continuation_t){
				.m_term = compile_deref_var(context,goal->m_term),
				.m_next = goal->m_next
			});
			break;
		}
	}

	if (debug_info)
	{
		// TODO: Emit tracepoints
	}

	return c;
}

cfg_t* compile_subgoal(compile_context_t* context, const continuation_t* goal)
{
	cfg_t* c = NULL;
	if (goal)
	{
		if (goal->m_shim)
			c = (*goal->m_shim)(context,goal);
		else
			c = compile_subterm(context,goal);
	}
	return c;
}

static void walk_cfgs(compile_context_t* context, cfg_vec_t* blks, cfg_block_t* blk)
{
	cfg_block_info_t* bi = allocator_malloc(context->m_allocator,sizeof(cfg_block_info_t));
	if (!bi)
		longjmp(context->m_jmp,1);

	*bi = (cfg_block_info_t){ .m_blk = blk };
	if (btree_insert(&blks->m_index,(uintptr_t)blk,bi) != bi)
	{
		allocator_free(context->m_allocator,bi);
		return;
	}

	blks->m_blks = allocator_realloc(context->m_allocator,blks->m_blks,(blks->m_count + 1) * sizeof(cfg_block_info_t*));
	if (!blks->m_blks)
		longjmp(context->m_jmp,1);

	blks->m_blks[blks->m_count++] = bi;
	blks->m_total += blk->m_count;

	int do_again = 0;

	for (size_t i = 0; i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_NOP:
			--blks->m_total;
			break;

		case OP_JMP:
			walk_cfgs(context,blks,(cfg_block_t*)blk->m_ops[i+1].m_term.m_pval);
			break;

		case OP_BRANCH:
			do_again = 1;
			break;

		case OP_BUILTIN:
		case OP_EXTERN:
			if (blk->m_ops[i+2].m_term.m_pval)
				do_again = 1;
			break;

		default:
			break;
		}
	}

	if (do_again)
	{
		for (size_t i = 0; i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
		{
			switch (blk->m_ops[i].m_opcode.m_op)
			{
			case OP_BRANCH:
				walk_cfgs(context,blks,(cfg_block_t*)blk->m_ops[i+1].m_term.m_pval);
				break;

			case OP_BUILTIN:
			case OP_EXTERN:
				if (blk->m_ops[i+2].m_term.m_pval)
					walk_cfgs(context,blks,(cfg_block_t*)blk->m_ops[i+2].m_term.m_pval);
				break;

			default:
				break;
			}
		}
	}
}

static size_t emit_ops(opcode_t* code, const cfg_vec_t* blks)
{
	opcode_t* start = code;
	for (size_t j = 0; j < blks->m_count; ++j)
	{
		const cfg_block_t* blk = blks->m_blks[j]->m_blk;
		blks->m_blks[j]->m_offset = (code - start);

		for (size_t i = 0; i < blk->m_count; )
		{
			size_t inc = inc_ip(blk->m_ops[i].m_opcode.m_op);

			switch (blk->m_ops[i].m_opcode.m_op)
			{
			case OP_NOP:
				break;

			case OP_RET:
				if (j == blks->m_count-1 ||
					blks->m_blks[j+1]->m_blk->m_count != 1 ||
					blks->m_blks[j+1]->m_blk->m_ops->m_opcode.m_op != OP_RET)
				{
					(code++)->m_opcode = (struct op_arg){ .m_op = OP_RET };
				}
				break;

			case OP_JMP:
				if (j == blks->m_count-1 || blk->m_ops[i+1].m_term.m_pval != blks->m_blks[j+1]->m_blk)
				{
					memcpy(code,blk->m_ops + i,inc * sizeof(*code));
					code += inc;
				}
				break;

			default:
				memcpy(code,blk->m_ops + i,inc * sizeof(*code));
				code += inc;
				break;
			}

			i += inc;
		}
	}

	opcode_t* end = code;
	for (code = start; code < end; code += inc_ip(code->m_opcode.m_op))
	{
		switch (code->m_opcode.m_op)
		{
		case OP_JMP:
		case OP_BRANCH:
			{
				const cfg_block_info_t* blk = btree_lookup(&blks->m_index,(uintptr_t)code[1].m_term.m_pval);
				assert(blk);
				code[1].m_term.m_u64val = blk->m_offset - (code + 1 - start);
			}
			break;

		case OP_BUILTIN:
		case OP_EXTERN:
			if (code[2].m_term.m_pval)
			{
				const cfg_block_info_t* blk = btree_lookup(&blks->m_index,(uintptr_t)code[2].m_term.m_pval);
				assert(blk);
				code[2].m_term.m_u64val = blk->m_offset - (code + 2 - start);
			}
			break;

		default:
			break;
		}
	}

	return (end - start);
}

#if ENABLE_TESTS

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#undef DECLARE_BUILTIN_INTRINSIC
#define DECLARE_BUILTIN_INTRINSIC(f,p)

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,a,p) \
	{ &prolite_builtin_##f, #f },

static const char* builtinName(const builtin_fn_t fn)
{
	static const struct builtin_names
	{
		builtin_fn_t fn;
		const char* name;
	} bns[] = {
		#include "builtin_functions.h"

		{ &prolite_builtin_continue, "continue" },
		{ &prolite_builtin_call, "call" },
		{ &prolite_builtin_callN, "callN" },
		{ &prolite_builtin_catch, "catch" },
		{ &prolite_builtin_throw, "throw", },
		{ &prolite_builtin_throw_evaluable, "throw_evaluable", },
		{ &prolite_builtin_throw_zero_div, "throw_zero_div", },
		{ &prolite_builtin_throw_underflow, "throw_underflow", },
		{ &prolite_builtin_throw_integer_overflow, "throw_integer_overflow", },
		{ &prolite_builtin_throw_float_overflow, "throw_float_overflow", },
		{ &prolite_builtin_throw_callable, "throw_callable", },
		{ &prolite_builtin_halt, "halt" },
		{ &prolite_builtin_user_defined, "user_defined" },
		{ &prolite_builtin_callable, "callable" },
		{ &prolite_builtin_unify, "unify" },
		{ &prolite_builtin_unify_with_occurs_check, "unify_with_occurs_check" },
		{ &prolite_builtin_unify_is, "unify_is" },
		{ &prolite_builtin_ground, "ground" },
		{ &prolite_builtin_type_test, "type_test" },
		{ &prolite_builtin_expression, "expression" }
	};

	for (size_t i=0; i < sizeof(bns)/sizeof(bns[0]); ++i)
	{
		if (fn == bns[i].fn)
			return bns[i].name;
	}

	return "unknown";
}

static void fmtFlags(exec_flags_t v, char* buf)
{
	char* s = buf;

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

#include "write_term.h"

#include <inttypes.h>

static void dumpPI(const term_t* t, FILE* f)
{
	string_t s;
	size_t arity = unpack_predicate(t,&s,NULL);
	fprintf(f,"%.*s/%zu",(int)s.m_len,s.m_str,arity);
}

static void dumpCFGBlock(context_t* context, const cfg_block_t* blk, FILE* f)
{
	char buf[10] = {0};

	fprintf(f,"\tnode [shape=record];\n");
	fprintf(f,"\tN%p [label=\"{",blk);

	if (!blk->m_count)
	{
		fprintf(f,"<f0> WTF?!?!");
	}

	for (size_t i=0;i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		if (i)
			fprintf(f,"|");
		fprintf(f,"<f%zu> ",i);

		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_NOP:
			fprintf(f,"(NOP)");
			break;

		case OP_CONTINUE:
			fprintf(f,"Continue");
			break;

		case OP_JMP:
			fprintf(f,"Jmp");
			break;

		case OP_RET:
			fprintf(f,"Ret");
			break;

		case OP_BUILTIN:
			fprintf(f,"Builtin\\ %s/%"PRIu64,builtinName(blk->m_ops[i+1].m_term.m_pval),(uint64_t)blk->m_ops[i].m_opcode.m_arg);
			if (blk->m_ops[i+2].m_term.m_pval)
				fprintf(f,"|<f%zu> Gosub",i+1);
			break;

		case OP_EXTERN:
			fprintf(f,"Call\\ ");
			dumpPI(((const compile_clause_t*)blk->m_ops[i+1].m_term.m_pval)->m_head,f);
			if (blk->m_ops[i+2].m_term.m_pval)
				fprintf(f,"\\ %p|<f%zu> Gosub",blk->m_ops[i+1].m_term.m_pval,i+1);
			break;

		case OP_CUT:
			fprintf(f,"Cut");
			break;

		case OP_PUSH_CUT:
			fprintf(f,"Push\\ Cut");
			break;

		case OP_POP_CUT:
			fprintf(f,"Pop\\ Cut");
			break;

		case OP_BRANCH:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"Branch \\%s",buf);
			break;

		case OP_PUSH_CONST:
			fprintf(f,"Push\\ Const\\ %g",blk->m_ops[i+1].m_term.m_dval);
			break;

		case OP_PUSH_NULL:
			fprintf(f,"Push\\ NULL\\ *\\ %"PRIu64,(uint64_t)blk->m_ops[i].m_opcode.m_arg);
			break;

		case OP_PUSH_TERM_REF:
			fprintf(f,"Push\\ ");
			dumpTerm(context,blk->m_ops[i+1].m_term.m_pval,f,1);
			break;

		case OP_POP:
			fprintf(f,"Pop\\ (%"PRIu64")",(uint64_t)blk->m_ops[i].m_opcode.m_arg);
			break;

		case OP_ALLOC_REGS:
			fprintf(f,"Alloc\\ %zu\\ registers",(size_t)blk->m_ops[i].m_opcode.m_arg);
			break;

		case OP_FREE_REGS:
			fprintf(f,"Free\\ %zu\\ registers",(size_t)blk->m_ops[i].m_opcode.m_arg);
			break;

		case OP_PUSH_REG:
			fprintf(f,"Push\\ $%zu",(size_t)blk->m_ops[i].m_opcode.m_arg);
			break;

		case OP_SET_REG:
			fprintf(f,"$%zu\\ =\\ %g",(size_t)blk->m_ops[i].m_opcode.m_arg,blk->m_ops[i+1].m_term.m_dval);
			break;

		case OP_LOAD_REG:
			fprintf(f,"$%zu\\ =\\ SP[%zu]",(size_t)blk->m_ops[i].m_opcode.m_arg,(size_t)blk->m_ops[i+1].m_term.m_u64val);
			break;

		case OP_MOV_REG:
			fprintf(f,"$%zu\\ =\\ $%zu",(size_t)blk->m_ops[i].m_opcode.m_arg,(size_t)blk->m_ops[i+1].m_term.m_u64val);
			break;

		case OP_ADD_REG:
			fprintf(f,"$%zu\\ =\\ $%zu\\ +\\ $%zu",(size_t)blk->m_ops[i].m_opcode.m_arg,(size_t)blk->m_ops[i+1].m_term.m_u64val,(size_t)blk->m_ops[i+2].m_term.m_u64val);
			break;

		case OP_SUB_REG:
			fprintf(f,"$%zu\\ =\\ $%zu\\ -\\ $%zu",(size_t)blk->m_ops[i].m_opcode.m_arg,(size_t)blk->m_ops[i+1].m_term.m_u64val,(size_t)blk->m_ops[i+2].m_term.m_u64val);
			break;

		default:
			fprintf(f,"OP %zu",(size_t)blk->m_ops[i].m_opcode.m_op);
			break;
		}
	}

	fprintf(f,"}\"];\n");

	for (size_t i=0;i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_BRANCH:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [label=\"%s\"];\n",blk,i,blk->m_ops[i+1].m_term.m_pval,buf);
			break;

		case OP_JMP:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0>;\n",blk,i,blk->m_ops[i+1].m_term.m_pval);
			break;

		case OP_BUILTIN:
		case OP_EXTERN:
			if (blk->m_ops[i+2].m_term.m_pval)
				fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [dir=both];\n",blk,i+1,blk->m_ops[i+2].m_term.m_pval);
			break;

		default:
			break;
		}
	}
}

void dumpCFG(context_t* context, const cfg_vec_t* blks, const char* filename)
{
	FILE* f = fopen(filename,"w");

	fprintf(f,"digraph cfg {\n\tstart [shape=point];\n");

	for (size_t i=0; i < blks->m_count; ++i)
		dumpCFGBlock(context,blks->m_blks[i]->m_blk,f);

	fprintf(f,"\tstart -> N%p:<f0>;\n}",blks->m_blks[0]->m_blk);

	fclose(f);
}

void dumpTrace(context_t* context, const opcode_t* code, size_t count, const char* filename)
{
	FILE* f = fopen(filename,"w");

	char buf[10] = {0};

	int spaces = 0;
	for (size_t c = count; c; c /= 10)
		++spaces;

	const opcode_t* start = code;
	for (const opcode_t* end = code + count; code < end; code += inc_ip(code->m_opcode.m_op))
	{
		fprintf(f,"%*zu: ",spaces,code - start);

		switch (code->m_opcode.m_op)
		{
		case OP_NOP:
			fprintf(f,"NOP;\n");
			break;

		case OP_CONTINUE:
			fprintf(f,"continue;\n");
			break;

		case OP_JMP:
			fprintf(f,"goto %+d (%zu);\n",(int)code[1].m_term.m_u64val,(size_t)((code + 1 - start) + (int64_t)code[1].m_term.m_u64val));
			break;

		case OP_RET:
			fprintf(f,"return;\n");
			break;

		case OP_BUILTIN:
			fprintf(f,"builtin(%s/%"PRIu64")",builtinName(code[1].m_term.m_pval),(uint64_t)code[0].m_opcode.m_arg);
			if (code[2].m_term.m_u64val)
				fprintf(f," gosub %+d (%zu)",(int)code[2].m_term.m_u64val,(size_t)((code + 2 - start) + (int64_t)code[2].m_term.m_u64val));
			fprintf(f,";\n");
			break;

		case OP_EXTERN:
			fprintf(f,"call(");
			dumpPI(((const compile_clause_t*)code[1].m_term.m_pval)->m_head,f);
			fprintf(f,")");
			if (code[2].m_term.m_u64val)
				fprintf(f," %p gosub %+d (%zu)",code[1].m_term.m_pval,(int)code[2].m_term.m_u64val,(size_t)((code + 2 - start) + (int64_t)code[2].m_term.m_u64val));
			fprintf(f,";\n");
			break;

		case OP_CUT:
			fprintf(f,"flags |= C;\n");
			break;

		case OP_PUSH_CUT:
			fprintf(f,"push cut;\n");
			break;

		case OP_POP_CUT:
			fprintf(f,"pop cut;\n");
			break;

		case OP_BRANCH:
			fmtFlags(code->m_opcode.m_arg,buf);
			fprintf(f,"if (flags & %s) goto %+d (%zu);\n",buf,(int)code[1].m_term.m_u64val,(size_t)((code + 1 - start) + (int64_t)code[1].m_term.m_u64val));
			break;

		case OP_PUSH_CONST:
			fprintf(f,"push const %g;\n",code[1].m_term.m_dval);
			break;

		case OP_PUSH_NULL:
			fprintf(f,"push NULL * %"PRIu64";\n",(uint64_t)code->m_opcode.m_arg);
			break;

		case OP_PUSH_TERM_REF:
			fprintf(f,"push ");
			dumpTerm(context,code[1].m_term.m_pval,f,0);
			fprintf(f,";\n");
			break;

		case OP_POP:
			fprintf(f,"pop (%"PRIu64");\n",(uint64_t)code->m_opcode.m_arg);
			break;

		case OP_ALLOC_REGS:
			fprintf(f,"alloc %zu registers;\n",(size_t)code->m_opcode.m_arg);
			break;

		case OP_FREE_REGS:
			fprintf(f,"free %zu registers;\n",(size_t)code->m_opcode.m_arg);
			break;

		case OP_PUSH_REG:
			fprintf(f,"push $%zu;\n",(size_t)code->m_opcode.m_arg);
			break;

		case OP_SET_REG:
			fprintf(f,"$%zu = %g;\n",(size_t)code->m_opcode.m_arg,code[1].m_term.m_dval);
			break;

		case OP_LOAD_REG:
			fprintf(f,"$%zu = SP[%zu];\n",(size_t)code->m_opcode.m_arg,(size_t)code[1].m_term.m_u64val);
			break;

		case OP_MOV_REG:
			fprintf(f,"$%zu = $%zu;\n",(size_t)code->m_opcode.m_arg,(size_t)code[1].m_term.m_u64val);
			break;

		case OP_ADD_REG:
			fprintf(f,"$%zu = $%zu + $%zu;\n",(size_t)code->m_opcode.m_arg,(size_t)code[1].m_term.m_u64val,(size_t)code[2].m_term.m_u64val);
			break;

		case OP_SUB_REG:
			fprintf(f,"$%zu = $%zu - $%zu;\n",(size_t)code->m_opcode.m_arg,(size_t)code[1].m_term.m_u64val,(size_t)code[2].m_term.m_u64val);
			break;

		default:
			fprintf(f,"OP %zu;\n",(size_t)code->m_opcode.m_op);
			assert(0);
			break;
		}
	}

	fclose(f);
}
#endif // ENABLE_TESTS

void compile_goal(context_t* context, link_fn_t link_fn, void* link_param, const term_t* goal, size_t var_count, const continuation_t* next)
{
	size_t heap_start = heap_top(&context->m_heap);
	size_t trail_start = heap_top(&context->m_trail);
	compile_context_t cc = {
		.m_allocator = &heap_allocator(&context->m_heap),
		.m_link_fn = link_fn,
		.m_link_param = link_param
	};
	if (!setjmp(cc.m_jmp))
	{
		if (var_count)
		{
			cc.m_substs = allocator_malloc(cc.m_allocator,sizeof(substitutions_t) + (sizeof(term_t) * var_count));
			if (!cc.m_substs)
				longjmp(cc.m_jmp,1);

			cc.m_substs->m_count = var_count;
			memset(cc.m_substs->m_vals,0,var_count * sizeof(term_t*));
		}

		continuation_t cont = {
			.m_term = goal,
			.m_next = next
		};
		cfg_t* c = compile_subgoal(&cc,goal ? &cont : cont.m_next);
		if (!c)
			c = new_cfg(&cc);

		append_ret(&cc,c->m_tail);

		cfg_simplify(&cc,c);

		cfg_vec_t blks = { .m_index.m_allocator = cc.m_allocator };
		walk_cfgs(&cc,&blks,c->m_entry_point);

#if ENABLE_TESTS
		dumpCFG(context,&blks,"./cfg.dot");
#endif
		if (blks.m_total)
		{
			opcode_t* code = heap_malloc(&context->m_trail,blks.m_total * sizeof(opcode_t));
			if (!code)
				longjmp(cc.m_jmp,1);

			blks.m_total = emit_ops(code,&blks);

#if ENABLE_TESTS
			dumpTrace(context,code,blks.m_total,"./pcode.txt");
#endif
			// TODO - JIT time!
		}
	}
	else
	{
		/* Bulk free all trail allocs */
		heap_reset(&context->m_trail,trail_start);
	}

#if ENABLE_TESTS
	fprintf(stdout,"Heap: %zu\n",heap_top(&context->m_heap) - heap_start);
	fprintf(stdout,"Trail: %zu\n",heap_top(&context->m_trail) - trail_start);
#endif

	/* Bulk free all heap allocs */
	heap_reset(&context->m_heap,heap_start);
}

static void jit_goal(context_t* context, const continuation_t* goal)
{
	// TODO


}

PROLITE_EXPORT void prolite_builtin_continue(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(!gosub);
	assert(argc == 1);

	builtin_gosub(context,argv[0]);
}

static cfg_t* jit_gosub(compile_context_t* context, const continuation_t* goal)
{
	assert(!goal->m_next);

	return compile_builtin(context,&prolite_builtin_continue,goal->m_term,NULL);
}

static int jit_can_call(context_t* context, const term_t* t)
{
	switch (unpack_term_type(t))
	{
	case prolite_var:
	case prolite_atom:
		return 1;

	case prolite_compound:
		switch (MASK_DEBUG_INFO(t->m_u64val))
		{
		case PACK_COMPOUND_EMBED_1(2,','):
		case PACK_COMPOUND_EMBED_1(2,';'):
		case PACK_COMPOUND_EMBED_2(2,'-','>'):
			{
				size_t arity;
				for (const term_t* p = get_first_arg(t,&arity); arity--; p = get_next_arg(p))
				{
					if (!jit_can_call(context,deref_local_var(context,p)))
						return 0;
				}
			}
			break;

		default:
			break;
		}
		return 1;

	default:
		return 0;
	}
}

PROLITE_EXPORT void prolite_builtin_call(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(argc == 1);

	const term_t* t = deref_local_var(context,argv[0]);
	while (t->m_u64val == PACK_COMPOUND_EMBED_4(1,'c','a','l','l'))
		t = deref_local_var(context,get_first_arg(t,NULL));

	if (t->m_u64val == PACK_ATOM_EMBED_1('!'))
		return builtin_gosub(context,gosub);

	exec_flags_t flags = context->m_flags;

	switch (unpack_term_type(t))
	{
	case prolite_var:
		return throw_instantiation_error(context,t);

	case prolite_atom:
		return jit_goal(context,&(continuation_t){
			.m_term = (const term_t*)&flags,
			.m_shim = &compile_call_begin_shim,
			.m_next = &(continuation_t){
				.m_term = t,
				.m_next = &(continuation_t){
					.m_shim = &compile_call_end_shim,
					.m_term = (const term_t*)&flags,
					.m_next = &(continuation_t){
						.m_term = gosub,
						.m_shim = &jit_gosub
					}
				}
			}
		});

	case prolite_compound:
		if (!jit_can_call(context,t))
			return prolite_builtin_throw_callable(context,NULL,1,(const term_t*[]){ t });

		return jit_goal(context,&(continuation_t){
			.m_term = (const term_t*)&flags,
			.m_shim = &compile_call_begin_shim,
			.m_next = &(continuation_t){
				.m_term = t,
				.m_next = &(continuation_t){
					.m_shim = &compile_call_end_shim,
					.m_term = (const term_t*)&flags,
					.m_next = &(continuation_t){
						.m_term = gosub,
						.m_shim = &jit_gosub
					}
				}
			}
		});
		break;

	default:
		return prolite_builtin_throw_callable(context,NULL,1,(const term_t*[]){ t });
	}
}

PROLITE_EXPORT void prolite_builtin_callN(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(argc > 1);

	// TODO - Compose a term!
	assert(0);

	const term_t* t = NULL;

	return prolite_builtin_call(context,gosub,1,(const term_t*[]){ t });
}
