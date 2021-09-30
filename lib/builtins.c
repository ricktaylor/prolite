#include "context.h"

#define BUILTIN_FUNCTION_DEFN(f) \
	void builtin_##f(context_t* context) {

#define BUILTIN_FUNCTION_POP \
	term_t* sp = (term_t*)get_next_arg(arg1); \
	const builtin_fn_t gosub = (sp++)->m_pval; \
	context->m_stack = sp;

#define BUILTIN_FUNCTION_RET \
	if (context->m_flags & FLAG_THROW) \
		throw_exception(context); \
	else if (!(context->m_flags & FLAG_FAIL))\
		(*gosub)(context); \
	}

#define DECLARE_BUILTIN_FUNCTION_0(f,p) \
	void prolite_builtin_##f(context_t* context); \
	BUILTIN_FUNCTION_DEFN(f) \
	term_t* sp = (term_t*)context->m_stack; \
	const builtin_fn_t gosub = (sp++)->m_pval; \
	context->m_stack = sp; \
	prolite_builtin_##f(context); \
	BUILTIN_FUNCTION_RET

#define DECLARE_BUILTIN_FUNCTION_1(f,p) \
	void prolite_builtin_##f(context_t* context, const term_t* arg1); \
	BUILTIN_FUNCTION_DEFN(f) \
	const term_t* arg1 = context->m_stack; \
	BUILTIN_FUNCTION_POP \
	prolite_builtin_##f(context,arg1); \
	BUILTIN_FUNCTION_RET

#define DECLARE_BUILTIN_FUNCTION_2(f,p) \
	void prolite_builtin_##f(context_t* context, const term_t* arg1, const term_t* arg2); \
	BUILTIN_FUNCTION_DEFN(f) \
	const term_t* arg2 = context->m_stack; \
	const term_t* arg1 = get_next_arg(arg2); \
	BUILTIN_FUNCTION_POP \
	prolite_builtin_##f(context,arg1,arg2); \
	BUILTIN_FUNCTION_RET

#define DECLARE_BUILTIN_FUNCTION_3(f,p) \
	void prolite_builtin_##f(context_t* context, const term_t* arg1, const term_t* arg2, const term_t* arg3); \
	BUILTIN_FUNCTION_DEFN(f) \
	const term_t* arg3 = context->m_stack; \
	const term_t* arg2 = get_next_arg(arg3); \
	const term_t* arg1 = get_next_arg(arg2); \
	BUILTIN_FUNCTION_POP \
	prolite_builtin_##f(context,arg1,arg2,arg3); \
	BUILTIN_FUNCTION_RET

#include "builtin_functions.h"

// TEMP

void builtin_call(context_t* context) {  }
void builtin_callN(context_t* context) {  }
void builtin_catch(context_t* context) {  }
void builtin_throw(context_t* context) {  }
void builtin_halt(context_t* context) {  }
void builtin_occurs_check(context_t* context) {  }
void builtin_callable(context_t* context) {  }
void builtin_ground(context_t* context) {  }
void builtin_term_compare(context_t* context) {  }

// END TEMP

