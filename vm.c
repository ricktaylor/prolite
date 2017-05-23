
#include "stack.h"
#include "clause.h"

union instruction_t
{
	enum eOpCode
	{
		OP_SET_CP,
		OP_SET_CUT,
		OP_SET_CATCH,

		OP_REDO,
		OP_CUT,
		OP_THROW,
		OP_JMP_REL,

	}           m_op;
	void*       m_ptr;
	uint64_t    m_uval;
	double      m_dval;
};

struct exec_state_t
{
	union instruction_t* m_ip;

	uint64_t m_catch_point;
	uint64_t m_cut_point;
	uint64_t m_choice_point;
};

static void dispatch(struct exec_state_t* exec, struct context_t* context)
{
	for (;;)
	{
		switch ((exec->m_ip++)->m_op)
		{
		case OP_SET_CP:
			stack_push(&context->m_exec_stack,(uintptr_t)(exec->m_ip+1));
			stack_push(&context->m_exec_stack,exec->m_catch_point);
			stack_push(&context->m_exec_stack,exec->m_cut_point);
			stack_push(&context->m_exec_stack,exec->m_choice_point);
			exec->m_choice_point = stack_top(context->m_exec_stack);
			break;

		case OP_SET_CUT:
			stack_push(&context->m_exec_stack,(uintptr_t)(exec->m_ip+1));
			stack_push(&context->m_exec_stack,exec->m_catch_point);
			stack_push(&context->m_exec_stack,exec->m_cut_point);
			stack_push(&context->m_exec_stack,exec->m_choice_point);
			exec->m_cut_point = exec->m_choice_point = stack_top(context->m_exec_stack);
			break;

		case OP_SET_CATCH:
			stack_push(&context->m_exec_stack,(uintptr_t)(exec->m_ip+1));
			stack_push(&context->m_exec_stack,exec->m_catch_point);
			stack_push(&context->m_exec_stack,exec->m_cut_point);
			stack_push(&context->m_exec_stack,exec->m_choice_point);
			exec->m_catch_point = stack_top(context->m_exec_stack);
			break;

		case OP_REDO:
			stack_reset(&context->m_exec_stack,exec->m_choice_point);
			exec->m_choice_point = stack_pop(&context->m_exec_stack);
			exec->m_cut_point = stack_pop(&context->m_exec_stack);
			exec->m_catch_point = stack_pop(&context->m_exec_stack);
			exec->m_ip = (void*)(uintptr_t)stack_pop(&context->m_exec_stack);
			break;

		case OP_CUT:
			stack_reset(&context->m_exec_stack,exec->m_cut_point);
			exec->m_choice_point = stack_pop(&context->m_exec_stack);
			exec->m_cut_point = stack_pop(&context->m_exec_stack);
			exec->m_catch_point = stack_pop(&context->m_exec_stack);
			exec->m_ip = (void*)(uintptr_t)stack_pop(&context->m_exec_stack);
			break;

		case OP_THROW:
			stack_reset(&context->m_exec_stack,exec->m_catch_point);
			exec->m_choice_point = stack_pop(&context->m_exec_stack);
			exec->m_cut_point = stack_pop(&context->m_exec_stack);
			exec->m_catch_point = stack_pop(&context->m_exec_stack);
			exec->m_ip = (void*)(uintptr_t)stack_pop(&context->m_exec_stack);
			break;

		case OP_JMP_REL:
			exec->m_ip += (exec->m_ip++)->m_uval;
			break;
		}
	}
}

static int compile_call(struct context_t* context, struct term_t* goal)
{

}

static int compile_and(struct context_t* context, struct term_t* goal)
{
	uint64_t stack_base = stack_top(&context->m_exec_stack);

	struct term_t* local = copy_term(goal);

label_1:
	if (!solve(context,local))
	{
		stack_reset(&context->m_exec_stack,stack_base);
		return 0;
	}

	local->m_value = next_value(local->m_value);

	solve(context,local);

	goto label_1;



	union instruction_t* op_codes = stack_alloc();

	op_codes[0].m_op = OP_PUSH_CP;
	op_codes[1].m_op = OP_SET_CP;
	op_codes[2].m_uval = 0; // New CP value;


}

/* Emit the opcode for a goal on the scratch stack */
int compile_goal(struct context_t* context, struct term_t* goal)
{
	switch (goal->m_value->m_uval & BOX_TAG_MASK)
	{
	case BOX_COMPOUND_EMBED_1(2,','):
		goal->m_value++;
		return compile_and(context,goal);

	case BOX_COMPOUND_EMBED_1(2,';'):
		goal->m_value++;
		return compile_or(context,goal);

	case BOX_COMPOUND_EMBED_2(2,'-','>'):
		goal->m_value++;
		return compile_if(context,goal);

	case BOX_ATOM_EMBED_1('!'):
		return compile_cut(context);

	case BOX_COMPOUND_EMBED_4(1,'c','a','l','l'):
		goal->m_value++;
		return compile_call(context,goal);

	case BOX_ATOM_EMBED_4('t','r','u','e'):
		return compile_true(context);

	case BOX_ATOM_EMBED_4('f','a','i','l'):
		return compile_fail(context);

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
	return compile_dynamic_user_defined(context,goal);
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
