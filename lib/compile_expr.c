#include "compile.h"
#include "builtins.h"

#include <math.h>
#include <fenv.h>
#include <string.h>

#if !defined(__GNUC__)
#pragma STDC FENV_ACCESS ON
#endif

typedef enum expr_node_type
{
	EXPR_TYPE_CONST,
	EXPR_TYPE_REG,
	EXPR_TYPE_THROW

} expr_node_type_t;

typedef struct expr_subst
{
	const term_t* m_expr;
	size_t        m_stack_idx;

} expr_subst_t;

typedef struct walk_info
{
	expr_subst_t* m_substs;
	const term_t* m_expr;

} walk_info_t;

typedef struct expr_node
{
	expr_node_type_t m_type;
	union
	{
		double        m_dval;
		size_t        m_idx;
		const term_t* m_pval;
	};
	cfg_t* (*m_shim)(compile_context_t* context, const struct expr_node* node, size_t* regs);
	struct expr_node* m_child[2];

} expr_node_t;

static expr_node_t* walk_expr(compile_context_t* context, const term_t* expr, expr_subst_t* substs);

static expr_node_t* new_expr_node(compile_context_t* context, expr_node_type_t type)
{
	expr_node_t* e = allocator_malloc(context->m_allocator,sizeof(expr_node_t));
	if (!e)
		longjmp(context->m_jmp,1);

	*e = (expr_node_t){ .m_type = type };
	return e;
}

static void free_expr_node(compile_context_t* context, expr_node_t* e)
{
	if (e)
	{
		free_expr_node(context,e->m_child[0]);
		free_expr_node(context,e->m_child[1]);

		allocator_free(context->m_allocator,e);
	}
}

/*static expr_node_t* walk_div(compile_context_t* context, expr_node_t* p1, expr_node_t* p2, const debug_info_t* di)
{
	if (p1->m_type != EXPR_TYPE_SHIM && p2->m_type != EXPR_TYPE_SHIM)
	{
		if (p1->m_type == EXPR_TYPE_INT && p2->m_type == EXPR_TYPE_INT)
		{
			if (p2->m_nval != 0)
				p1->m_nval /= p2->m_nval;
			else
			{
				p1->m_type = EXPR_TYPE_SHIM;
				p1->m_fn = &emit_throw_zero_div;
				p1->m_args[0] = (void*)di;
			}
		}
		else
		{
			if (p1->m_type == EXPR_TYPE_INT)
				p1->m_dval = p1->m_nval / p2->m_dval;
			else if (p2->m_type == EXPR_TYPE_INT)
				p1->m_dval /= p2->m_nval;
			else
				p1->m_dval /= p2->m_dval;

			test_fpexcept(p1,di,FE_DIVBYZERO | FE_UNDERFLOW | FE_OVERFLOW);
		}

		heap_free(context->m_heap,p2,sizeof(expr_node_t));
		return p1;
	}

	expr_node_t* n = allocator_malloc(context->m_allocator,sizeof(expr_node_t));
	if (!n)
		longjmp(context->m_jmp,1);

	n->m_type = EXPR_TYPE_SHIM;
	n->m_fn = &emit_div;
	n->m_args[0] = p1;
	n->m_args[1] = p2;
	reg_count_binary_expr(n,EXPR_TYPE_DOUBLE);

	return n;
}*/

static cfg_t* compile_throw_zero_div(compile_context_t* context, const expr_node_t* e, size_t* regs)
{
	return compile_builtin(context,&prolite_builtin_throw_zero_div,1,e->m_pval,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
}

static cfg_t* compile_throw_underflow(compile_context_t* context, const expr_node_t* e, size_t* regs)
{
	return compile_builtin(context,&prolite_builtin_throw_underflow,1,e->m_pval,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
}

/*static cfg_t* compile_throw_integer_overflow(compile_context_t* context, const expr_node_t* e, size_t* regs)
{
	return compile_builtin(context,&prolite_builtin_throw_integer_overflow,1,e->m_pval,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
}*/

static cfg_t* compile_throw_float_overflow(compile_context_t* context, const expr_node_t* e, size_t* regs)
{
	return compile_builtin(context,&prolite_builtin_throw_float_overflow,1,e->m_pval,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
}

static cfg_t* compile_throw_evaluable(compile_context_t* context, const expr_node_t* e, size_t* regs)
{
	return compile_builtin(context,&prolite_builtin_throw_evaluable,1,e->m_pval,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
}

static expr_node_t* test_fpexcept(compile_context_t* context, const term_t* expr, int flags)
{
	flags = fetestexcept(flags);
	if (!flags)
		return NULL;

	feclearexcept(flags);

	if (flags & FE_DIVBYZERO)
	{
		expr_node_t* e = new_expr_node(context,EXPR_TYPE_THROW);
		e->m_shim = &compile_throw_zero_div;
		e->m_pval = expr;
		return e;
	}

	if (flags & FE_UNDERFLOW)
	{
		expr_node_t* e = new_expr_node(context,EXPR_TYPE_THROW);
		e->m_shim = &compile_throw_underflow;
		e->m_pval = expr;
		return e;
	}

	//if (flags & FE_OVERFLOW)
	{
		expr_node_t* e = new_expr_node(context,EXPR_TYPE_THROW);
		e->m_shim = &compile_throw_float_overflow;
		e->m_pval = expr;
		return e;
	}
}

static cfg_t* compile_set_reg(compile_context_t* context, const expr_node_t* e, size_t* regs)
{
	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode.m_op = OP_SET_REG;
	(ops++)->m_term.m_u64val = (*regs)++;
	ops->m_term.m_dval = e->m_dval;
	return c;
}

static cfg_t* combine_binary_op(compile_context_t* context, cfg_t* c1, size_t* regs1, cfg_t* c2, size_t regs2, size_t* result)
{
	if (*regs1 >= regs2)
	{
		if (*regs1 == regs2)
		{
			int adjusted = 0;
			if (c1->m_tail->m_count >= 3)
			{
				switch (c1->m_tail->m_ops[c1->m_tail->m_count-3].m_opcode.m_op)
				{
				case OP_SET_REG:
				case OP_LOAD_REG:
					adjusted = 1;
					++c1->m_tail->m_ops[c1->m_tail->m_count-2].m_term.m_u64val;
					break;

				default:
					break;
				}
			}

			if (!adjusted)
			{
				opcode_t* ops = append_opcodes(context,c1->m_tail,3);
				(ops++)->m_opcode.m_op = OP_MOV_REG;
				(ops++)->m_term.m_u64val = *regs1;
				(ops++)->m_term.m_u64val = *regs1 - 1;
			}
			++(*regs1);
		}
		*result = *regs1;
		return goto_next(context,c1,c2);
	}

	*result = regs2;
	return goto_next(context,c2,c1);
}

static cfg_t* compile_binop(compile_context_t* context, optype_t op, const expr_node_t* e, size_t* regs)
{
	size_t regs1 = *regs;
	cfg_t* c1 = (*e->m_child[0]->m_shim)(context,e->m_child[0],&regs1);

	size_t regs2 = *regs;
	cfg_t* c2 = (*e->m_child[1]->m_shim)(context,e->m_child[1],&regs2);

	cfg_t* c = combine_binary_op(context,c1,&regs1,c2,regs2,regs);

	opcode_t* ops = append_opcodes(context,c->m_tail,4);
	(ops++)->m_opcode.m_op = op;
	(ops++)->m_term.m_u64val = regs1 - 1;
	(ops++)->m_term.m_u64val = regs2 - 1;
	ops->m_term.m_u64val = *regs - 1;
	return c;
}

static cfg_t* compile_add(compile_context_t* context, const expr_node_t* e, size_t* regs)
{
	return compile_binop(context,OP_ADD_REG,e,regs);
}

static expr_node_t* walk_expr_add(compile_context_t* context, const term_t* expr, const term_t* t1, const term_t* t2, expr_subst_t* substs)
{
	expr_node_t* e1 = walk_expr(context,t1,substs);
	if (e1->m_type == EXPR_TYPE_THROW)
		return e1;

	expr_node_t* e2 = walk_expr(context,t2,substs);
	if (e2->m_type == EXPR_TYPE_THROW)
	{
		free_expr_node(context,e1);
		return e2;
	}

	if (e1->m_type == EXPR_TYPE_CONST)
	{
		if (e2->m_type == EXPR_TYPE_CONST)
		{
			double r = e1->m_dval + e2->m_dval;
			free_expr_node(context,e2);

			expr_node_t* e = test_fpexcept(context,expr,FE_UNDERFLOW | FE_OVERFLOW);
			if (e)
			{
				free_expr_node(context,e1);
				return e;
			}

			e1->m_dval = r;
			return e1;
		}

		// Swap e1 and e2 to aid optimizer
		expr_node_t* e3 = e2;
		e2 = e1;
		e1 = e3;
	}

	expr_node_t* e = new_expr_node(context,EXPR_TYPE_REG);
	e->m_shim = &compile_add;
	e->m_child[0] = e1;
	e->m_child[1] = e2;
	return e;
}

static cfg_t* compile_sub(compile_context_t* context, const expr_node_t* e, size_t* regs)
{
	return compile_binop(context,OP_SUB_REG,e,regs);
}

static expr_node_t* walk_expr_sub(compile_context_t* context, const term_t* expr, const term_t* t1, const term_t* t2, expr_subst_t* substs)
{
	expr_node_t* e1 = walk_expr(context,t1,substs);
	if (e1->m_type == EXPR_TYPE_THROW)
		return e1;

	expr_node_t* e2 = walk_expr(context,t2,substs);
	if (e2->m_type == EXPR_TYPE_THROW)
	{
		free_expr_node(context,e1);
		return e2;
	}

	if (e1->m_type == EXPR_TYPE_CONST && e2->m_type == EXPR_TYPE_CONST)
	{
		double r = e1->m_dval - e2->m_dval;
		free_expr_node(context,e2);
		expr_node_t* e = test_fpexcept(context,expr,FE_UNDERFLOW | FE_OVERFLOW);
		if (e)
		{
			free_expr_node(context,e1);
			return e;
		}
		e1->m_dval = r;
		return e1;
	}

	expr_node_t* e = new_expr_node(context,EXPR_TYPE_REG);
	e->m_shim = &compile_sub;
	e->m_child[0] = e1;
	e->m_child[1] = e2;
	return e;
}

static cfg_t* compile_load_reg(compile_context_t* context, const expr_node_t* e, size_t* regs)
{
	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode.m_op = OP_LOAD_REG;
	(ops++)->m_term.m_u64val = (*regs)++;
	ops->m_term.m_u64val = e->m_idx;
	return c;
}

static expr_node_t* optimize_expr(compile_context_t* context, expr_node_t* e)
{
	// TODO - Optimization point

	return e;
}

static expr_node_t* walk_expr(compile_context_t* context, const term_t* expr, expr_subst_t* substs)
{
	expr_node_t* e;
	switch (unpack_term_type(expr))
	{
	case prolite_var:
		{
			size_t idx = unpack_var_index(expr);
			assert(substs && idx < context->m_substs->m_count && substs[idx].m_expr);

			e = new_expr_node(context,EXPR_TYPE_REG);
			e->m_shim = &compile_load_reg;
			e->m_idx = substs[idx].m_stack_idx;
		}
		return e;

	case prolite_number:
		e = new_expr_node(context,EXPR_TYPE_CONST);
		e->m_shim = &compile_set_reg;
		e->m_dval = expr->m_dval;
		return e;

	case prolite_atom:
		switch (expr->m_u64val)
		{
#undef DECLARE_EXPR_1
#undef DECLARE_EXPR_2
#undef DECLARE_EXPR_CONSTANT
#define DECLARE_EXPR_CONSTANT(a,d) case a: { \
				e = new_expr_node(context,EXPR_TYPE_CONST); \
				e->m_shim = &compile_set_reg; \
				e->m_dval = M_PI; \
				return e; \
			} break;

#include "builtin_exprs.h"

		default:
			break;
		}
		break;

	case prolite_compound:
		switch (expr->m_u64val)
		{
#undef DECLARE_EXPR_CONSTANT
#define DECLARE_EXPR_1(fn,t) case t: \
			return optimize_expr(context,walk_expr_##fn(context,expr,compile_deref_var(context,get_first_arg(expr,NULL)),substs));

#define DECLARE_EXPR_2(fn,t) case t: { \
			const term_t* t1 = get_first_arg(expr,NULL); \
			return optimize_expr(context,walk_expr_##fn(context,expr,compile_deref_var(context,t1),compile_deref_var(context,get_next_arg(t1)),substs)); }

#include "builtin_exprs.h"

		default:
			break;
		}
		break;

	default:
		break;
	}

	e = new_expr_node(context,EXPR_TYPE_THROW);
	e->m_shim = &compile_throw_evaluable;
	e->m_pval = expr;
	return e;
}

static size_t extract_vars(compile_context_t* context, const term_t* expr, expr_subst_t* substs)
{
	switch (unpack_term_type(expr))
	{
	case prolite_var:
		{
			size_t idx = unpack_var_index(expr);
			assert(substs && idx < context->m_substs->m_count);
			if (substs[idx].m_expr)
				return 0;

			substs[idx].m_expr = expr;
			return 1;
		}

	case prolite_compound:
		switch (expr->m_u64val)
		{
#undef DECLARE_EXPR_1
#undef DECLARE_EXPR_CONSTANT
#undef DECLARE_EXPR_2

#define DECLARE_EXPR_1(fn,t) case t: \
			return extract_vars(context,compile_deref_var(context,get_first_arg(expr,NULL)),substs);

#define DECLARE_EXPR_2(fn,t) case t: { \
			const term_t* t1 = get_first_arg(expr,NULL); \
			size_t var_count = extract_vars(context,compile_deref_var(context,t1),substs); \
			var_count += extract_vars(context,compile_deref_var(context,get_next_arg(t1)),substs); \
			return var_count; }

#include "builtin_exprs.h"

		default:
			break;
		}
		break;

	default:
		break;
	}

	return 0;
}

static cfg_t* compile_unify_is(compile_context_t* context, const term_t* term, const continuation_t* next)
{
	cfg_t* cont = compile_subgoal(context,next);
	if (!cont)
		return NULL;

	append_ret(context,cont->m_tail);

	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,6);
	(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
	(ops++)->m_term.m_pval = term;
	(ops++)->m_opcode = (op_arg_t){ .m_op = OP_BUILTIN, .m_arg = 2 };
	(ops++)->m_term.m_pval = &prolite_builtin_unify_is;
	(ops++)->m_term.m_pval = cont->m_entry_point;
	ops->m_opcode = (op_arg_t){ .m_op = OP_POP, .m_arg = 2 };
	return c;
}

static cfg_t* compile_expr_inner(compile_context_t* context, const term_t* term, const continuation_t* next)
{
	walk_info_t* wi = (walk_info_t*)term;

	expr_node_t* e = walk_expr(context,wi->m_expr,wi->m_substs);

	size_t regs = 0;
	cfg_t* c = (*e->m_shim)(context,e,&regs);
	opcode_t* ops;

	switch (e->m_type)
	{
	case EXPR_TYPE_CONST:
		if (next->m_shim == &compile_unify_is)
		{
			term_t t = { .m_dval = e->m_dval };
			return compile_unify_terms(context,next->m_term,&t,next->m_next);
		}

		c = new_cfg(context);
		ops = append_opcodes(context,c->m_tail,2);
		(ops++)->m_opcode.m_op = OP_PUSH_CONST;
		ops->m_term.m_dval = e->m_dval;
		break;

	case EXPR_TYPE_REG:
		ops = append_opcodes(context,c->m_tail,2);
		(ops++)->m_opcode.m_op = OP_PUSH_REG;
		ops->m_term.m_u64val = regs - 1;
		break;

	default:
		return c;
	}

	if (regs)
	{
		cfg_t* c1 = new_cfg(context);
		ops = append_opcodes(context,c1->m_tail,2);
		(ops++)->m_opcode.m_op = OP_ALLOC_REGS;
		ops->m_term.m_u64val = regs;

		ops = append_opcodes(context,c->m_tail,2);
		(ops++)->m_opcode.m_op = OP_FREE_REGS;
		ops->m_term.m_u64val = regs;

		c = goto_next(context,c1,c);
	}

	return goto_next(context,c,compile_subgoal(context,next));
}

static cfg_t* compile_expr_var(compile_context_t* context, const term_t* term, const continuation_t* next)
{
	expr_subst_t* subst = (expr_subst_t*)term;

	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,5);
	(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
	(ops++)->m_term.m_pval = subst->m_expr;
	(ops++)->m_opcode = (op_arg_t){ .m_op = OP_BUILTIN, .m_arg = 1 };
	(ops++)->m_term.m_pval = &prolite_builtin_expression;
	//ops->m_term.m_pval = NULL;

	cfg_t* c1 = compile_subgoal(context,next);
	if (c1->m_tail->m_count &&
		c1->m_tail->m_ops[c1->m_tail->m_count-1].m_opcode.m_op == OP_POP &&
		c1->m_tail->m_ops[c1->m_tail->m_count-1].m_opcode.m_arg < c_op_arg_max)
	{
		++c1->m_tail->m_ops[c1->m_tail->m_count-1].m_opcode.m_arg;
	}
	else
	{
		ops = append_opcodes(context,c1->m_tail,1);
		ops->m_opcode = (op_arg_t){ .m_op = OP_POP, .m_arg = 1 };
	}

	cfg_t* c_end = new_cfg(context);
	add_branch(context,c,FLAG_THROW,c_end);
	goto_next(context,c1,c_end);
	return goto_next(context,c,c1);
}

static cfg_t* free_wi(compile_context_t* context, const term_t* term, const continuation_t* next)
{
	cfg_t* c = compile_subgoal(context,next);

	allocator_free(context->m_allocator,(void*)term);

	return c;
}

static const continuation_t* walk_expr_vars(compile_context_t* context, const term_t* expr, expr_subst_t* substs, size_t start, size_t idx, const continuation_t* next)
{
	while (start < context->m_substs->m_count && !substs[start].m_expr)
		++start;

	continuation_t* c = allocator_malloc(context->m_allocator,sizeof(continuation_t));
	if (!c)
		longjmp(context->m_jmp,1);

	if (start == context->m_substs->m_count)
	{
		walk_info_t* wi = allocator_malloc(context->m_allocator,sizeof(walk_info_t));
		if (!wi)
			longjmp(context->m_jmp,1);

		*wi = (walk_info_t){
			.m_substs = substs,
			.m_expr = expr
		};

		*c = (continuation_t){
			.m_shim = &compile_expr_inner,
			.m_term = (const term_t*)wi,
			.m_next = next
		};

		continuation_t* c1 = allocator_malloc(context->m_allocator,sizeof(continuation_t));
		if (!c1)
			longjmp(context->m_jmp,1);

		*c1 = (continuation_t){
			.m_shim = &free_wi,
			.m_term = (const term_t*)wi,
			.m_next = c
		};
		c = c1;
	}
	else
	{
		substs[start].m_stack_idx = --idx;

		*c = (continuation_t){
			.m_shim = &compile_expr_var,
			.m_term = (const term_t*)&substs[start],
			.m_next = walk_expr_vars(context,expr,substs,start+1,idx,next)
		};
	}

	return c;
}

static cfg_t* compile_expr(compile_context_t* context, const term_t* expr, const continuation_t* next)
{
	if (!context->m_substs || !context->m_substs->m_count)
	{
		return compile_subgoal(context,&(continuation_t){
			.m_shim = &compile_expr_inner,
			.m_term = (const term_t*)&(walk_info_t){
				.m_expr = expr
			},
			.m_next = next
		});
	}

	expr_subst_t* substs = allocator_malloc(context->m_allocator,sizeof(expr_subst_t) * context->m_substs->m_count);
	if (!substs)
		longjmp(context->m_jmp,1);

	memset(substs,0,sizeof(expr_subst_t) * context->m_substs->m_count);

	size_t var_count = extract_vars(context,expr,substs);
	cfg_t* c = compile_subgoal(context,walk_expr_vars(context,expr,substs,0,var_count,next));

	allocator_free(context->m_allocator,substs);

	return c;
}

cfg_t* compile_is(compile_context_t* context, const continuation_t* goal)
{
	const term_t* result = get_first_arg(goal->m_term,NULL);
	const term_t* expr = get_next_arg(result);

	result = compile_deref_var(context,result);
	expr = compile_deref_var(context,expr);

	switch (unpack_term_type(result))
	{
	case prolite_var:
		switch (unpack_term_type(expr))
		{
		case prolite_var:
			return compile_builtin(context,&prolite_builtin_throw,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });

		case prolite_compound:
			break;

		case prolite_number:
			return compile_unify_terms(context,result,expr,goal->m_next);

		case prolite_atom:
			switch (expr->m_u64val)
			{
#undef DECLARE_EXPR_1
#undef DECLARE_EXPR_2
#undef DECLARE_EXPR_CONSTANT
#define DECLARE_EXPR_CONSTANT(a,d) case a: break;

#include "builtin_exprs.h"

			default:
				return compile_builtin(context,&prolite_builtin_throw_evaluable,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
			}
			break;

		default:
			return compile_builtin(context,&prolite_builtin_throw_evaluable,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
		}
		break;

	case prolite_number:
		switch (unpack_term_type(expr))
		{
		case prolite_var:
			return compile_builtin(context,&prolite_builtin_throw,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });

		case prolite_number:
			if (result->m_dval != expr->m_dval)
				return NULL;
			return compile_subgoal(context,goal->m_next);

		case prolite_atom:
			switch (expr->m_u64val)
			{
#undef DECLARE_EXPR_CONSTANT
#define DECLARE_EXPR_CONSTANT(a,d) case a: return (result->m_dval != d) ? NULL : compile_subgoal(context,goal->m_next);

#include "builtin_exprs.h"

			default:
				return compile_builtin(context,&prolite_builtin_throw_evaluable,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
			}

		case prolite_compound:
			break;

		default:
			return compile_builtin(context,&prolite_builtin_throw_evaluable,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
		}
		break;

	default:
		switch (unpack_term_type(expr))
		{
		case prolite_var:
			return compile_builtin(context,&prolite_builtin_throw,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });

		case prolite_compound:
			// Need to check for evaluable by compiling although it will never unify
			break;

		case prolite_number:
			// Never unifiable
			return NULL;

		case prolite_atom:
			switch (expr->m_u64val)
			{
#undef DECLARE_EXPR_CONSTANT
#define DECLARE_EXPR_CONSTANT(a,d) case a: return NULL;

#include "builtin_exprs.h"

			default:
				return compile_builtin(context,&prolite_builtin_throw_evaluable,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
			}

		default:
			return compile_builtin(context,&prolite_builtin_throw_evaluable,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
		}
		break;
	}

	return compile_expr(context,expr,&(continuation_t){
		.m_shim = &compile_unify_is,
		.m_term = result,
		.m_next = goal->m_next
	});
}

static void expr_jit(context_t* context, const term_t* expr)
{
	// TODO!!
	assert(0);
}

PROLITE_EXPORT void prolite_builtin_expression(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
	assert(argc == 1);

	const term_t* expr = deref_local_var(context,argv[0]);

	double result = 0.0;

	switch (unpack_term_type(expr))
	{
	case prolite_var:
		return throw_instantiation_error(context,expr);

	case prolite_number:
		result = expr->m_dval;
		break;

	case prolite_atom:
		switch (expr->m_u64val)
		{
#undef DECLARE_EXPR_CONSTANT
#define DECLARE_EXPR_CONSTANT(a,d) case a: result = d; context->m_flags |= FLAG_CUT; break;

#include "builtin_exprs.h"

		default:
			return throw_evaluable_error(context,expr);
		}
		break;

	case prolite_compound:
		switch (expr->m_u64val)
		{
#undef DECLARE_EXPR_CONSTANT
#define DECLARE_EXPR_1(fn,t) case t: return expr_jit(context,expr);
#define DECLARE_EXPR_2(fn,t) case t: return expr_jit(context,expr);

#include "builtin_exprs.h"

		default:
			return throw_evaluable_error(context,expr);
		}
		break;

	default:
		return throw_evaluable_error(context,expr);
	}

	// TODO - Something with result!!
	(void)result;
	assert(0);
}
