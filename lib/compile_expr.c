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
	EXPR_TYPE_INT,
	EXPR_TYPE_DOUBLE,
	EXPR_TYPE_IREG,
	EXPR_TYPE_DREG,
	EXPR_TYPE_THROW

} expr_node_type_t;

typedef struct expr_subst
{
	const term_t* m_expr;
	size_t        m_stack_idx;
	int           m_is_double;

} expr_subst_t;

typedef struct expr_node
{
	expr_node_type_t m_type;
	union
	{
		double        m_dval;
		int64_t       m_n64val;
		const term_t* m_pval;
	};
	cfg_t* (*m_shim)(compile_context_t* context, const struct expr_node* node, size_t* int_regs, size_t* double_regs);
	struct expr_node* m_child[2];

} expr_node_t;

typedef struct expr_result
{
	size_t             m_int_regs;
	size_t             m_double_regs;
} expr_result_t;

static expr_node_t* walk_expr(compile_context_t* context, const term_t* expr, expr_subst_t* substs);

static expr_node_t* new_expr_node(compile_context_t* context, expr_node_type_t type)
{
	expr_node_t* e = stack_malloc(&context->m_stack,sizeof(expr_node_t));
	e->m_type = type;
	return e;
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

	expr_node_t* n = heap_malloc(context->m_heap,sizeof(expr_node_t));
	if (!n)
		longjmp(context->m_jmp,1);

	n->m_type = EXPR_TYPE_SHIM;
	n->m_fn = &emit_div;
	n->m_args[0] = p1;
	n->m_args[1] = p2;
	reg_count_binary_expr(n,EXPR_TYPE_DOUBLE);

	return n;
}*/

static cfg_t* compile_throw_zero_div(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	return compile_builtin(context,&prolite_builtin_throw_zero_div,1,e->m_pval,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
}

static cfg_t* compile_throw_underflow(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	return compile_builtin(context,&prolite_builtin_throw_underflow,1,e->m_pval,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
}

static cfg_t* compile_throw_integer_overflow(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	return compile_builtin(context,&prolite_builtin_throw_integer_overflow,1,e->m_pval,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
}

static cfg_t* compile_throw_float_overflow(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	return compile_builtin(context,&prolite_builtin_throw_float_overflow,1,e->m_pval,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
}

static cfg_t* compile_throw_evaluable(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	return compile_builtin(context,&prolite_builtin_throw_evaluable,1,e->m_pval,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });
}

static expr_node_t* test_fpexcept(compile_context_t* context, const term_t* expr, int flags)
{
	flags = fetestexcept(flags);
	if (!flags)
		return NULL;

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

static cfg_t* compile_set_ireg(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode.m_op = OP_SET_IREG;
	(ops++)->m_term.m_u64val = (*int_regs)++;
	ops->m_term.m_u64val = e->m_n64val;
	return c;
}

static cfg_t* compile_set_dreg(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode.m_op = OP_SET_DREG;
	(ops++)->m_term.m_u64val = (*double_regs)++;
	ops->m_term.m_dval = e->m_dval;
	return c;
}

static cfg_t* compile_cvt_i2d(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	cfg_t* c = (*e->m_child[0]->m_shim)(context,e->m_child[0],int_regs,double_regs);

	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode.m_op = OP_CVT_I2D;
	if (*double_regs == 0)
		++(*double_regs);

	(ops++)->m_term.m_u64val = *double_regs - 1;
	ops->m_term.m_u64val = *int_regs - 1;

	return c;
}

static cfg_t* combine_binary_op(compile_context_t* context, optype_t op, cfg_t* c1, size_t* regs1, cfg_t* c2, size_t regs2, size_t* result)
{
	if (*regs1 >= regs2)
	{
		if (*regs1 == regs2)
		{
			opcode_t* ops = append_opcodes(context,c1->m_tail,3);
			(ops++)->m_opcode.m_op = op;
			(ops++)->m_term.m_u64val = *regs1;
			(ops++)->m_term.m_u64val = *regs1 - 1;
			++(*regs1);
		}
		*result = *regs1;
		return goto_next(context,c1,c2);
	}

	*result = regs2;
	return goto_next(context,c2,c1);
}

static cfg_t* compile_binop_i(compile_context_t* context, optype_t op, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	size_t int_regs1 = *int_regs;
	size_t double_regs1 = *double_regs;
	cfg_t* c1 = (*e->m_child[0]->m_shim)(context,e->m_child[0],&int_regs1,&double_regs1);

	size_t int_regs2 = *int_regs;
	size_t double_regs2 = *double_regs;
	cfg_t* c2 = (*e->m_child[1]->m_shim)(context,e->m_child[1],&int_regs2,&double_regs2);

	*double_regs = (double_regs1 > double_regs2 ? double_regs1 : double_regs2);

	cfg_t* c = combine_binary_op(context,OP_MOV_I,c1,&int_regs1,c2,int_regs2,int_regs);
	opcode_t* ops = append_opcodes(context,c->m_tail,4);
	(ops++)->m_opcode.m_op = op;
	(ops++)->m_term.m_u64val = int_regs1 - 1;
	(ops++)->m_term.m_u64val = int_regs2 - 1;
	ops->m_term.m_u64val = *int_regs - 1;
	return c;
}

static cfg_t* compile_binop_d(compile_context_t* context, optype_t op, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	size_t int_regs1 = *int_regs;
	size_t double_regs1 = *double_regs;
	cfg_t* c1 = (*e->m_child[0]->m_shim)(context,e->m_child[0],&int_regs1,&double_regs1);

	size_t int_regs2 = *int_regs;
	size_t double_regs2 = *double_regs;
	cfg_t* c2 = (*e->m_child[1]->m_shim)(context,e->m_child[1],&int_regs2,&double_regs2);

	*int_regs = (int_regs1 > int_regs2 ? int_regs1 : int_regs2);

	cfg_t* c = combine_binary_op(context,OP_MOV_D,c1,&double_regs1,c2,double_regs2,double_regs);
	opcode_t* ops = append_opcodes(context,c->m_tail,4);
	(ops++)->m_opcode.m_op = op;
	(ops++)->m_term.m_u64val = double_regs1 - 1;
	(ops++)->m_term.m_u64val = double_regs2 - 1;
	ops->m_term.m_u64val = *double_regs - 1;
	return c;
}

static cfg_t* compile_add_i(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	return compile_binop_i(context,OP_ADD_I,e,int_regs,double_regs);
}

static cfg_t* compile_add_d(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	return compile_binop_d(context,OP_ADD_D,e,int_regs,double_regs);
}

static expr_node_t* walk_expr_add(compile_context_t* context, const term_t* expr, const term_t* t1, const term_t* t2, expr_subst_t* substs)
{
	expr_node_t* e1 = walk_expr(context,t1,substs);
	if (e1->m_type == EXPR_TYPE_THROW)
		return e1;

	expr_node_t* e2 = walk_expr(context,t2,substs);
	if (e2->m_type == EXPR_TYPE_THROW)
		return e2;

	switch (e1->m_type)
	{
	case EXPR_TYPE_INT:
		if (e2->m_type == EXPR_TYPE_INT)
		{
			int64_t r;
			if (!__builtin_add_overflow(e1->m_n64val,e2->m_n64val,&r))
				e1->m_n64val = r;
			else
			{
				e1->m_type = EXPR_TYPE_THROW;
				e1->m_shim = &compile_throw_integer_overflow;
				e1->m_pval = expr;
			}
			return e1;
		}

		e1->m_shim = &compile_set_ireg;
		e1->m_type = EXPR_TYPE_IREG;
		// Intentional fall-through

	case EXPR_TYPE_IREG:
		if (e2->m_type == EXPR_TYPE_IREG)
		{
			expr_node_t* e = new_expr_node(context,EXPR_TYPE_IREG);
			e->m_shim = &compile_add_i;
			e->m_child[0] = e1;
			e->m_child[1] = e2;
			return e;
		}

		expr_node_t* e = new_expr_node(context,EXPR_TYPE_DREG);
		e->m_shim = &compile_cvt_i2d;
		e->m_child[0] = e1;
		e1 = e;
		break;

	case EXPR_TYPE_DOUBLE:
		switch (e2->m_type)
		{
		case EXPR_TYPE_INT:
			{
				double r = e1->m_dval + e2->m_n64val;
				expr_node_t* e = test_fpexcept(context,expr,FE_UNDERFLOW | FE_OVERFLOW);
				if (e)
					return e;
				e1->m_dval = r;
				return e1;
			}

		case EXPR_TYPE_DOUBLE:
			{
				double r = e1->m_dval + e2->m_dval;
				expr_node_t* e = test_fpexcept(context,expr,FE_UNDERFLOW | FE_OVERFLOW);
				if (e)
					return e;
				e1->m_dval = r;
				return e1;
			}

		default:
			e1->m_shim = &compile_set_dreg;
			e1->m_type = EXPR_TYPE_DREG;
			break;
		}

	default:
		break;
	}

	assert(e1->m_type == EXPR_TYPE_DREG);

	switch (e2->m_type)
	{
	case EXPR_TYPE_INT:
		{
			double r = e2->m_n64val;
			expr_node_t* e = test_fpexcept(context,expr,FE_UNDERFLOW | FE_OVERFLOW);
			if (e)
				return e;
			e2->m_dval = r;
			e2->m_type = EXPR_TYPE_DOUBLE;
		}
		// Intentional fall-through

	case EXPR_TYPE_DOUBLE:
		e2->m_shim = &compile_set_dreg;
		e2->m_type = EXPR_TYPE_DREG;
		break;

	case EXPR_TYPE_IREG:
		{
			expr_node_t* e = new_expr_node(context,EXPR_TYPE_DREG);
			e->m_shim = &compile_cvt_i2d;
			e->m_child[0] = e2;
			e2 = e;
		}
		break;

	default:
		break;
	}

	assert(e2->m_type == EXPR_TYPE_DREG);

	expr_node_t* e = new_expr_node(context,EXPR_TYPE_DREG);
	e->m_shim = &compile_add_d;
	e->m_child[0] = e1;
	e->m_child[1] = e2;
	return e;
}

static cfg_t* compile_sub_i(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	return compile_binop_i(context,OP_SUB_I,e,int_regs,double_regs);
}

static cfg_t* compile_sub_d(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	return compile_binop_d(context,OP_SUB_D,e,int_regs,double_regs);
}

static expr_node_t* walk_expr_sub(compile_context_t* context, const term_t* expr, const term_t* t1, const term_t* t2, expr_subst_t* substs)
{
	expr_node_t* e1 = walk_expr(context,t1,substs);
	if (e1->m_type == EXPR_TYPE_THROW)
		return e1;

	expr_node_t* e2 = walk_expr(context,t2,substs);
	if (e2->m_type == EXPR_TYPE_THROW)
		return e2;

	switch (e1->m_type)
	{
	case EXPR_TYPE_INT:
		if (e2->m_type == EXPR_TYPE_INT)
		{
			int64_t r;
			if (!__builtin_sub_overflow(e1->m_n64val,e2->m_n64val,&r))
				e1->m_n64val = r;
			else
			{
				e1->m_type = EXPR_TYPE_THROW;
				e1->m_shim = &compile_throw_integer_overflow;
				e1->m_pval = expr;
			}
			return e1;
		}

		e1->m_shim = &compile_set_ireg;
		e1->m_type = EXPR_TYPE_IREG;
		// Intentional fall-through

	case EXPR_TYPE_IREG:
		if (e2->m_type == EXPR_TYPE_IREG)
		{
			expr_node_t* e = new_expr_node(context,EXPR_TYPE_IREG);
			e->m_shim = &compile_sub_i;
			e->m_child[0] = e1;
			e->m_child[1] = e2;
			return e;
		}

		expr_node_t* e = new_expr_node(context,EXPR_TYPE_DREG);
		e->m_shim = &compile_cvt_i2d;
		e->m_child[0] = e1;
		e1 = e;
		break;

	case EXPR_TYPE_DOUBLE:
		switch (e2->m_type)
		{
		case EXPR_TYPE_INT:
			{
				double r = e1->m_dval - e2->m_n64val;
				expr_node_t* e = test_fpexcept(context,expr,FE_UNDERFLOW | FE_OVERFLOW);
				if (e)
					return e;
				e1->m_dval = r;
				return e1;
			}

		case EXPR_TYPE_DOUBLE:
			{
				double r = e1->m_dval - e2->m_dval;
				expr_node_t* e = test_fpexcept(context,expr,FE_UNDERFLOW | FE_OVERFLOW);
				if (e)
					return e;
				e1->m_dval = r;
				return e1;
			}

		default:
			e1->m_shim = &compile_set_dreg;
			e1->m_type = EXPR_TYPE_DREG;
			break;
		}

	default:
		break;
	}

	assert(e1->m_type == EXPR_TYPE_DREG);

	switch (e2->m_type)
	{
	case EXPR_TYPE_INT:
		{
			double r = e2->m_n64val;
			expr_node_t* e = test_fpexcept(context,expr,FE_UNDERFLOW | FE_OVERFLOW);
			if (e)
				return e;
			e2->m_dval = r;
			e2->m_type = EXPR_TYPE_DOUBLE;
		}
		// Intentional fall-through

	case EXPR_TYPE_DOUBLE:
		e2->m_shim = &compile_set_dreg;
		e2->m_type = EXPR_TYPE_DREG;
		break;

	case EXPR_TYPE_IREG:
		{
			expr_node_t* e = new_expr_node(context,EXPR_TYPE_DREG);
			e->m_shim = &compile_cvt_i2d;
			e->m_child[0] = e2;
			e2 = e;
		}
		break;

	default:
		break;
	}

	assert(e2->m_type == EXPR_TYPE_DREG);

	expr_node_t* e = new_expr_node(context,EXPR_TYPE_DREG);
	e->m_shim = &compile_sub_d;
	e->m_child[0] = e1;
	e->m_child[1] = e2;
	return e;
}

static cfg_t* compile_load_ireg(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode.m_op = OP_LOAD_IREG;
	(ops++)->m_term.m_u64val = (*int_regs)++;
	ops->m_term.m_u64val = e->m_n64val;
	return c;
}

static cfg_t* compile_load_dreg(compile_context_t* context, const expr_node_t* e, size_t* int_regs, size_t* double_regs)
{
	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,3);
	(ops++)->m_opcode.m_op = OP_LOAD_DREG;
	(ops++)->m_term.m_u64val = (*double_regs)++;
	ops->m_term.m_u64val = e->m_n64val;
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
	switch (get_term_type(expr))
	{
	case prolite_var:
		{
			size_t idx = get_var_index(expr);
			assert(substs && idx < context->m_substs->m_count && substs[idx].m_expr);

			if (substs[idx].m_is_double)
			{
				e = new_expr_node(context,EXPR_TYPE_DREG);
				e->m_shim = &compile_load_dreg;
			}
			else
			{
				e = new_expr_node(context,EXPR_TYPE_IREG);
				e->m_shim = &compile_load_ireg;
			}
			e->m_n64val = substs[idx].m_stack_idx;
		}
		return e;

	case prolite_double:
		e = new_expr_node(context,EXPR_TYPE_DOUBLE);
		e->m_shim = NULL;
		e->m_dval = expr->m_dval;
		return e;

	case prolite_integer:
		e = new_expr_node(context,EXPR_TYPE_INT);
		e->m_shim = NULL;
		e->m_n64val = get_integer(expr);
		return e;

	case prolite_atom:
		switch (expr->m_u64val)
		{
#undef DECLARE_EXPR_1
#undef DECLARE_EXPR_2
#undef DECLARE_EXPR_CONSTANT
#define DECLARE_EXPR_CONSTANT(a,d) case a: { \
				e = new_expr_node(context,EXPR_TYPE_DOUBLE); \
				e->m_shim = NULL; \
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
	switch (get_term_type(expr))
	{
	case prolite_var:
		{
			size_t idx = get_var_index(expr);
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


typedef struct walk_info
{
	expr_subst_t* m_substs;
	const term_t* m_expr;
} walk_info_t;

static cfg_t* compile_expr_inner(compile_context_t* context, const term_t* term, const continuation_t* next)
{
	walk_info_t* wi = (walk_info_t*)term;

	expr_node_t* e = walk_expr(context,wi->m_expr,wi->m_substs);

	size_t int_regs = 0;
	size_t double_regs = 0;
	cfg_t* c = NULL;
	if (!e->m_shim)
		c = new_cfg(context);
	else
	{
		c = (*e->m_shim)(context,e,&int_regs,&double_regs);
		if (e->m_type == EXPR_TYPE_THROW)
			return c;
	}
		
	opcode_t* ops = append_opcodes(context,c->m_tail,2);
	switch (e->m_type)
	{
	case EXPR_TYPE_INT:
		(ops++)->m_opcode.m_op = OP_PUSH_CONST;

		// Check for overflow when converting to prolite_integer and use prolite_double instead
		if (e->m_n64val > INT64_C(0x7FFFFFFFFFFF) || e->m_n64val < -INT64_C(0x7FFFFFFFFFFF) - 1)
			(ops++)->m_term.m_dval = e->m_n64val;			
		else
			(ops++)->m_term.m_u64val = PACK_TYPE(prolite_integer) | PACK_MANT_48(e->m_n64val);
		break;

	case EXPR_TYPE_DOUBLE:
		(ops++)->m_opcode.m_op = OP_PUSH_CONST;
		(ops++)->m_term.m_dval = e->m_dval;
		break;

	case EXPR_TYPE_IREG:
		(ops++)->m_opcode.m_op = OP_PUSH_IREG;
		(ops++)->m_term.m_u64val = int_regs - 1;
		break;

	case EXPR_TYPE_DREG:
		(ops++)->m_opcode.m_op = OP_PUSH_DREG;
		(ops++)->m_term.m_u64val = double_regs - 1;
		break;

	default:
		assert(0);
		break;
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
	(ops++)->m_opcode.m_op = OP_BUILTIN;
	(ops++)->m_term.m_pval = &prolite_builtin_expression;
	//ops->m_term.m_pval = NULL;

	cfg_t* c_int = compile_subgoal(context,next);
	
	subst->m_is_double = 1;
	cfg_t* c_double = compile_subgoal(context,next);
	
	cfg_t* c1 = new_cfg(context);
	ops = append_opcodes(context,c1->m_tail,1);
	ops->m_opcode = (op_arg_t){ .m_op = OP_CLEAR_FLAGS, .m_arg = FLAG_CUT };
	c_double = goto_next(context,c1,c_double);

	cfg_t* c_end = new_cfg(context);
	add_branch(context,c,FLAG_THROW,c_end);
	add_branch(context,c,FLAG_CUT,c_double);
	goto_next(context,c_double,c_end);
	goto_next(context,c,c_int);
	return goto_next(context,c,c_end);
}

static const continuation_t* walk_expr_vars(compile_context_t* context, const term_t* expr, expr_subst_t* substs, size_t start, size_t idx, const continuation_t* next)
{
	while (start < context->m_substs->m_count && !substs[start].m_expr)
		++start;

	continuation_t* c = stack_malloc(&context->m_stack,sizeof(continuation_t));
	if (start == context->m_substs->m_count)
	{
		walk_info_t* wi = stack_malloc(&context->m_stack,sizeof(walk_info_t));
		*wi = (walk_info_t){
			.m_substs = substs,
			.m_expr = expr
		};

		*c = (continuation_t){
			.m_shim = &compile_expr_inner,
			.m_term = (const term_t*)wi,
			.m_next = next
		};
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
	term_t* sp = context->m_stack;

	cfg_t* c;
	if (!context->m_substs || !context->m_substs->m_count)
	{
		c = compile_subgoal(context,&(continuation_t){
			.m_shim = &compile_expr_inner,
			.m_term = (const term_t*)&(walk_info_t){
				.m_expr = expr
			},
			.m_next = next
		});
	}
	else
	{
		expr_subst_t* substs = stack_malloc(&context->m_stack,sizeof(expr_subst_t) * context->m_substs->m_count);
		memset(substs,0,sizeof(expr_subst_t) * context->m_substs->m_count);

		size_t var_count = extract_vars(context,expr,substs);
		c = compile_subgoal(context,walk_expr_vars(context,expr,substs,0,var_count,next));
	}

	context->m_stack = sp;

	return c;
}

static cfg_t* compile_unify2(compile_context_t* context, const term_t* term, const continuation_t* next)
{
	cfg_t* cont = compile_subgoal(context,next);
	if (!cont)
		return NULL;

	append_ret(context,cont->m_tail);

	cfg_t* c = new_cfg(context);
	opcode_t* ops = append_opcodes(context,c->m_tail,6);
	(ops++)->m_opcode.m_op = OP_PUSH_TERM_REF;
	(ops++)->m_term.m_pval = term;
	(ops++)->m_opcode.m_op = OP_BUILTIN;
	(ops++)->m_term.m_pval = &prolite_builtin_unify2;
	(ops++)->m_term.m_pval = cont->m_entry_point;
	ops->m_opcode = (op_arg_t){ .m_op = OP_POP, .m_arg = 2 };
	return c;
}

cfg_t* compile_is(compile_context_t* context, const continuation_t* goal)
{
	const term_t* result = get_first_arg(goal->m_term,NULL);
	const term_t* expr = get_next_arg(result);

	result = compile_deref_var(context,result);
	expr = compile_deref_var(context,expr);

	switch (get_term_type(result))
	{
	case prolite_var:
		switch (get_term_type(expr))
		{
		case prolite_var:
			return compile_builtin(context,&prolite_builtin_throw,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });

		case prolite_compound:
			break;

		case prolite_double:
		case prolite_integer:
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

	case prolite_integer:
		switch (get_term_type(expr))
		{
		case prolite_var:
			return compile_builtin(context,&prolite_builtin_throw,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });

		case prolite_integer:
			if (get_integer(result) != get_integer(expr))
				return NULL;
			return compile_subgoal(context,goal->m_next);

		case prolite_double:
			if (get_integer(result) != expr->m_dval)
				return NULL;
			return compile_subgoal(context,goal->m_next);

		case prolite_atom:
			switch (expr->m_u64val)
			{
#undef DECLARE_EXPR_CONSTANT
#define DECLARE_EXPR_CONSTANT(a,d) case a: return NULL;

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

	case prolite_double:
		switch (get_term_type(expr))
		{
		case prolite_var:
			return compile_builtin(context,&prolite_builtin_throw,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });

		case prolite_integer:
			if (result->m_dval != get_integer(expr))
				return NULL;
			return compile_subgoal(context,goal->m_next);

		case prolite_double:
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
		switch (get_term_type(expr))
		{
		case prolite_var:
			return compile_builtin(context,&prolite_builtin_throw,1,expr,&(continuation_t){ .m_term = &(term_t){ .m_u64val = PACK_ATOM_EMBED_4('f','a','i','l')} });

		case prolite_compound:
			// Need to check for evaluable by compiling although it will never unify
			break;

		case prolite_double:
		case prolite_integer:
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
		.m_shim = &compile_unify2,
		.m_term = result,
		.m_next = goal->m_next
	});
}

static void expr_jit(context_t* context, const term_t* expr)
{
	// TODO!!
	assert(0);
}

PROLITE_EXPORT void prolite_builtin_expression(context_t* context, const term_t* gosub)
{
	term_t* sp = context->m_stack;
	const term_t* expr = deref_local_var(context,context->m_stack->m_pval);

	// Result is pushed to stack
	// FLAG_CUT set if prolite_double

	switch (get_term_type(expr))
	{
	case prolite_var:
		return push_instantiation_error(context,expr);

	case prolite_integer:
		context->m_stack->m_u64val = get_integer(expr);
		return;

	case prolite_double:
		context->m_stack->m_dval = expr->m_dval;
		context->m_flags |= FLAG_CUT;
		return;

	case prolite_atom:
		switch (expr->m_u64val)
		{
#undef DECLARE_EXPR_CONSTANT
#define DECLARE_EXPR_CONSTANT(a,d) case a: context->m_stack->m_dval = d; context->m_flags |= FLAG_CUT; return;

#include "builtin_exprs.h"

		default:
			return push_evaluable_error(context,expr);
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
			return push_evaluable_error(context,expr);
		}
		break;

	default:
		return push_evaluable_error(context,expr);
	}

	if (context->m_flags & FLAG_THROW)
	{
		builtin_throw(context);

		// Pop the arguments
		context->m_stack = sp + 1;
	}
}
