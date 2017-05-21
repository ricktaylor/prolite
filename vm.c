
#include "clause.h"

union instruction_t
{
	enum eOpCode
	{
		OP_PUSH_FALSE_HANDLER,
		OP_POP_FALSE_HANDLER,
		OP_PUSH_CUT_HANDLER,
		OP_POP_CUT_HANDLER,
		OP_PUSH_THROW_HANDLER,
		OP_POP_THROW_HANDLER,



		OP_PUSH_STACK,
		OP_POP_STACK,
	}           m_op;
	void*       m_ptr;
	uint64_t    m_uval;
};

static void dispatch(struct context_t* context)
{
	union instruction_t* ip;
	union instruction_t* false_handler;
	union instruction_t* cut_handler;
	union instruction_t* throw_handler;
	struct term_t* args;

	uint64_t stack_base = 0;

	for (;;)
	{
		switch ((ip++)->m_op)
		{
		case OP_PUSH_FALSE_HANDLER:
			stack_push(&context->m_exec_stack,false_handler->m_uval);
			false_handler->m_ptr = ip + (ip++)->m_uval;
			break;

		case OP_POP_FALSE_HANDLER:
			false_handler->m_uval = stack_pop(&context->m_exec_stack);
			break;

		case OP_PUSH_CUT_HANDLER:
			stack_push(&context->m_exec_stack,cut_handler->m_uval);
			cut_handler->m_ptr = ip + (ip++)->m_uval;
			break;

		case OP_POP_CUT_HANDLER:
			cut_handler->m_uval = stack_pop(&context->m_exec_stack);
			break;

		case OP_PUSH_THROW_HANDLER:
			stack_push(&context->m_exec_stack,throw_handler->m_uval);
			throw_handler->m_ptr = ip + (ip++)->m_uval;
			break;

		case OP_POP_THROW_HANDLER:
			throw_handler->m_uval = stack_pop(&context->m_exec_stack);
			break;

		case OP_PUSH_STACK:
			stack_push(&context->m_exec_stack,stack_base);
			stack_base = stack_top(context->m_exec_stack);
			break;

		case OP_POP_STACK:
			stack_reset(&context->m_exec_stack,stack_base);
			stack_base = stack_pop(&context->m_exec_stack);
			break;
		}
	}
}

static int compile_call(struct context_t* context, struct term_t* goal)
{

}

static int compile_user_defined(struct context_t* context, struct term_t* goal, struct user_procedure_t* procedure)
{
	size_t i;
	union instruction_t fn_prolog[20];

	fn_prolog[0].m_op = OP_PUSH_STACK;
	emit_instructions(context,fn_prolog,1);

	for (i = 0; i < procedure->m_clause_count; ++i)
	{
		struct clause_t* clause = procedure->m_clauses[i];

		if (could_unify(goal,clause->m_term))
		{
			fn_prolog[0].m_op = OP_PUSH_STACK;
			fn_prolog[0].m_op = OP_PUSH_STACK;
		}
	}

	fn_prolog[0].m_op = OP_POP_STACK;
	emit_instructions(context,fn_prolog,1);
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
