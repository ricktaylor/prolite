
#include "stack.h"
#include "clause.h"

union instruction_t
{
	enum eOpCode
	{
		OP_CHOICEPOINT,
		OP_CUTPOINT,
		OP_SET_CATCH,
		OP_POP_CP,

		OP_REDO,
		OP_CUT,
		OP_THROW,
		OP_JMP_REL,

		OP_PUSH_GOAL,
		OP_POP_GOAL,
		OP_SWAP,

		OP_COPY_GOAL,
		OP_SPLIT_1,
		OP_SPLIT_2,

	}           m_op;
	ptrdiff_t   m_offset;
};

struct exec_state_t
{
	union instruction_t* m_ip;

	uint64_t m_catch_point;
	uint64_t m_cut_point;
	uint64_t m_choice_point;

	struct term_t* m_goal;
};

static inline uint64_t exec_push(struct exec_state_t* exec, struct context_t* context)
{
	uint64_t r = stack_push(&context->m_exec_stack,(uintptr_t)(exec->m_ip+2));
	if (r != -1)
		r = stack_push(&context->m_exec_stack,exec->m_catch_point);
	if (r != -1)
		r = stack_push(&context->m_exec_stack,exec->m_cut_point);
	if (r != -1)
		r = stack_push(&context->m_exec_stack,exec->m_choice_point);
	return r;
}

static inline void pop_cp(struct exec_state_t* exec, struct context_t* context)
{
	exec->m_choice_point = stack_pop(&context->m_exec_stack);
	exec->m_cut_point = stack_pop(&context->m_exec_stack);
	exec->m_catch_point = stack_pop(&context->m_exec_stack);
}

static inline void exec_pop(struct exec_state_t* exec, struct context_t* context, uint64_t base)
{
	stack_reset(&context->m_exec_stack,base);
	pop_cp(exec,context);
	exec->m_ip = (void*)(uintptr_t)stack_pop(&context->m_exec_stack);
}

static struct term_t* next_arg(struct term_t* v)
{
	if ((v->m_value->m_uval & BOX_TAG_MASK) == BOX_TAG_COMPOUND)
	{
		uint64_t arity = compound_arity(v->m_value);
		if ((v->m_value->m_uval & BOX_TAG_COMPOUND_EMBED) != BOX_TAG_COMPOUND_EMBED)
			++v->m_value;

		while (arity--)
			v = next_arg(v);
	}
	else
		++v->m_value;
	return v;
}

static void dispatch(struct exec_state_t* exec, struct context_t* context)
{
	for (;;)
	{
		switch ((exec->m_ip++)->m_op)
		{
		case OP_CHOICEPOINT:
			exec->m_choice_point = exec_push(exec,context);
			break;

		case OP_CUTPOINT:
			exec->m_cut_point = exec->m_choice_point = exec_push(exec,context);
			break;

		case OP_SET_CATCH:
			exec->m_catch_point = exec_push(exec,context);
			break;

		case OP_POP_CP:
			pop_cp(exec,context);
			break;

		case OP_REDO:
			exec_pop(exec,context,exec->m_choice_point);
			break;

		case OP_CUT:
			exec_pop(exec,context,exec->m_cut_point);
			break;

		case OP_THROW:
			exec_pop(exec,context,exec->m_catch_point);
			break;

		case OP_JMP_REL:
			exec->m_ip += exec->m_ip->m_offset;
			break;

		case OP_PUSH_GOAL:
			stack_push(&context->m_exec_stack,(uintptr_t)(exec->m_goal));
			break;

		case OP_POP_GOAL:
			exec->m_goal = (void*)(uintptr_t)stack_pop(&context->m_exec_stack);
			break;

		case OP_SWAP:
			{
				uint64_t t1 = stack_pop(&context->m_exec_stack);
				uint64_t t2 = stack_pop(&context->m_exec_stack);
				stack_push(&context->m_exec_stack,t1);
				stack_push(&context->m_exec_stack,t2);
			}
			break;
		}
	}
}

static int compile_goal(struct context_t* context, struct term_t* goal);

static int compile_and(struct context_t* context, struct term_t* goal)
{
	union instruction_t* op_codes = stack_malloc(&context->m_scratch_stack,sizeof(union instruction_t) * 2);
	if (!op_codes)
		return -1;

	/* Goal = ','(First,Second) */
	op_codes[0].m_op = OP_COPY_GOAL; /* Copy Goal */
	op_codes[1].m_op = OP_SPLIT_2;

	/* Solve(First) */
	compile_goal(context,goal);

	/* Pop Second */
	stack_push(&context->m_scratch_stack,OP_POP_GOAL);

	/* Solve Second */
	return compile_goal(context,next_arg(goal));
}

static int compile_or(struct context_t* context, struct term_t* goal)
{
	uint64_t bp,sp;
	union instruction_t* op_codes = stack_malloc(&context->m_scratch_stack,sizeof(union instruction_t) * 10);
	if (!op_codes)
		return -1;

	/* Goal = ';'(Either,Or) */
	op_codes[1].m_op = OP_SPLIT_2;
	op_codes[2].m_op = OP_COPY_GOAL; /* Copy Either */
	op_codes[3].m_op = OP_CHOICEPOINT;
	op_codes[4].m_op = OP_JMP_REL;   /* Jump to Solve(Either) */
	op_codes[5].m_offset = 5;
	op_codes[6].m_op = OP_POP_GOAL;  /* Pop Or */
	op_codes[7].m_op = OP_COPY_GOAL; /* Copy Or */
	op_codes[8].m_op = OP_JMP_REL;   /* Jump to Solve(Or) */
	op_codes[9].m_offset = 0;        /* ...Fix up after emitting */

	bp = stack_top(context->m_scratch_stack);

	/* Solve(Either) */
	if (compile_goal(context,goal) != 0)
		return -1;

	sp = stack_push(&context->m_scratch_stack,OP_REDO);
	if (sp == -1)
		return -1;


	op_codes[13].m_offset = sp - bp;

	/* Solve(Or) */
	return compile_goal(context,next_arg(goal));
}

static int compile_if_then(struct context_t* context, struct term_t* goal)
{
	union instruction_t* op_codes = stack_malloc(&context->m_scratch_stack,sizeof(union instruction_t) * 6);
	if (!op_codes)
		return -1;

	/* Goal = '->'(If,Then) */
	op_codes[0].m_op = OP_COPY_GOAL; /* Copy Goal */
	op_codes[1].m_op = OP_SPLIT_2; /* Goal = If */

	/* Set a cut */
	op_codes[2].m_op = OP_CUTPOINT;
	op_codes[3].m_op = OP_JMP_REL;
	op_codes[4].m_offset = 1;
	op_codes[5].m_op = OP_REDO;

	/* Solve(If) */
	if (compile_goal(context,goal) != 0)
		return -1;

	op_codes = stack_malloc(&context->m_scratch_stack,sizeof(union instruction_t) * 2);
	if (!op_codes)
		return -1;

	op_codes[0].m_op = OP_POP_CP;    /* Restore CP */
	op_codes[1].m_op = OP_POP_GOAL;  /* Pop Then */

	/* Solve(Then) */
	return compile_goal(context,next_arg(goal));
}

static int compile_cut(struct context_t* context)
{
	union instruction_t* op_codes = stack_malloc(&context->m_scratch_stack,sizeof(union instruction_t) * 4);
	if (!op_codes)
		return -1;

	op_codes[0].m_op = OP_CHOICEPOINT;
	op_codes[1].m_op = OP_JMP_REL;
	op_codes[2].m_offset = 1;
	op_codes[3].m_op = OP_CUT;

	return 0;
}

static int compile_call(struct context_t* context, struct term_t* goal)
{
	union instruction_t* op_codes = stack_malloc(&context->m_scratch_stack,sizeof(union instruction_t) * 5);
	if (!op_codes)
		return -1;

	/* Goal = call(G) */
	op_codes[0].m_op = OP_CUTPOINT;
	op_codes[1].m_op = OP_JMP_REL;
	op_codes[2].m_offset = 1;
	op_codes[3].m_op = OP_REDO;
	op_codes[4].m_op = OP_SPLIT_1; /* Goal = G */

	/* Solve(G) */
	return compile_goal(context,goal);
}

/* Emit the opcodes for a goal on the scratch stack */
static int compile_goal(struct context_t* context, struct term_t* goal)
{
	switch (goal->m_value->m_uval & BOX_TAG_MASK)
	{
	case BOX_COMPOUND_EMBED_1(2,','):
		goal->m_value++;
		return compile_and(context,goal);

	case BOX_COMPOUND_EMBED_1(2,';'):
		goal->m_value++;
		if ((goal->m_value->m_uval & BOX_TAG_MASK) == BOX_COMPOUND_EMBED_2(2,'-','>'))
		{
			goal->m_value++;
			return compile_if_then_else(context,goal);
		}
		return compile_or(context,goal);

	case BOX_COMPOUND_EMBED_2(2,'-','>'):
		goal->m_value++;
		return compile_if_then(context,goal);

	case BOX_ATOM_EMBED_1('!'):
		return compile_cut(context);

	case BOX_COMPOUND_EMBED_4(1,'c','a','l','l'):
		goal->m_value++;
		return compile_call(context,goal);

	case BOX_ATOM_EMBED_4('t','r','u','e'):
		return 0;

	case BOX_ATOM_EMBED_4('f','a','i','l'):
		return (stack_push(&context->m_scratch_stack,OP_REDO) == -1 ? -1 : 0);

	case BOX_COMPOUND_EMBED_5(3,'c','a','t','c','h'):
		goal->m_value++;
		return compile_catch(context,goal);

	case BOX_COMPOUND_EMBED_5(1,'t','h','r','o','w'):
		goal->m_value++;
		return compile_throw(context,goal);

	default:
		break;
	}

	/* TODO: Check for builtins */

	/* Emit user defined */
	return compile_dynamic(context,goal);
}

/* Convert a term into an initializer */
int compile_initializer(struct context_t* context, struct term_t* term, uint64_t stack_base)
{
	uint64_t scratch_base = stack_top(context->m_scratch_stack);
	int err = check_callable_term(term->m_value);
	switch (err)
	{
	case -1:
		return throw_instantiation_error(context);

	case 1:
		return throw_type_error(context,BUILTIN_ATOM(callable),term->m_value);
	}

	/* Emit goal */
	err = compile_goal(context,term);
	if (!err)
	{
		/* Reset the exec stack */
		stack_reset(&context->m_exec_stack,stack_base);

		/* Copy into exec stack from scratch stack */
		err = stack_copy(&context->m_exec_stack,&context->m_scratch_stack,scratch_base);
	}

	return err;
}
