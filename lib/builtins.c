#include "context.h"

void prolite_builtin_throw(context_t* context);

#define BUILTIN_FUNCTION_DEFN(f) \
	PROLITE_EXPORT void prolite_builtin_##f(context_t* context) {

#define BUILTIN_FUNCTION_POP \
	term_t* sp = (term_t*)get_next_arg(arg1); \
	const builtin_fn_t gosub = (sp++)->m_pval;
	
#define BUILTIN_FUNCTION_RET \
	if (context->m_flags & FLAG_THROW) \
		prolite_builtin_throw(context); \
	else \
	{ \
		if (!(context->m_flags & FLAG_FAIL))\
			(*gosub)(context); \
		context->m_stack = sp; \
	} }

#define BUILTIN_THUNK_0(f) \
	void builtin_##f(context_t* context); \
	BUILTIN_FUNCTION_DEFN(f) \
	term_t* sp = (term_t*)context->m_stack; \
	const builtin_fn_t gosub = (sp++)->m_pval; \
	context->m_stack = sp; \
	builtin_##f(context); \
	BUILTIN_FUNCTION_RET

#define BUILTIN_THUNK_1(f) \
	void builtin_##f(context_t* context, const term_t* arg1); \
	BUILTIN_FUNCTION_DEFN(f) \
	const term_t* arg1 = context->m_stack; \
	BUILTIN_FUNCTION_POP \
	builtin_##f(context,arg1); \
	BUILTIN_FUNCTION_RET

#define BUILTIN_THUNK_2(f) \
	void builtin_##f(context_t* context, const term_t* arg1, const term_t* arg2); \
	BUILTIN_FUNCTION_DEFN(f) \
	const term_t* arg2 = context->m_stack; \
	const term_t* arg1 = get_next_arg(arg2); \
	BUILTIN_FUNCTION_POP \
	builtin_##f(context,arg1,arg2); \
	BUILTIN_FUNCTION_RET

#define BUILTIN_THUNK_3(f) \
	void builtin_##f(context_t* context, const term_t* arg1, const term_t* arg2, const term_t* arg3); \
	BUILTIN_FUNCTION_DEFN(f) \
	const term_t* arg3 = context->m_stack; \
	const term_t* arg2 = get_next_arg(arg3); \
	const term_t* arg1 = get_next_arg(arg2); \
	BUILTIN_FUNCTION_POP \
	builtin_##f(context,arg1,arg2,arg3); \
	BUILTIN_FUNCTION_RET

#undef DECLARE_BUILTIN_FUNCTION
#undef DECLARE_BUILTIN_FUNCTION_0
#undef DECLARE_BUILTIN_FUNCTION_1
#undef DECLARE_BUILTIN_FUNCTION_2
#undef DECLARE_BUILTIN_FUNCTION_3

#define DECLARE_BUILTIN_FUNCTION_0(f,p) BUILTIN_THUNK_0(f)
#define DECLARE_BUILTIN_FUNCTION_1(f,p) BUILTIN_THUNK_1(f)
#define DECLARE_BUILTIN_FUNCTION_2(f,p) BUILTIN_THUNK_2(f)
#define DECLARE_BUILTIN_FUNCTION_3(f,p) BUILTIN_THUNK_3(f)

#include "builtin_functions.h"

// These are the supporting builtin thunks for some intrinsics
BUILTIN_THUNK_1(call)
BUILTIN_THUNK_1(callN)
BUILTIN_THUNK_1(catch)
BUILTIN_THUNK_1(throw)
BUILTIN_THUNK_1(halt)
BUILTIN_THUNK_2(occurs_check)
BUILTIN_THUNK_1(callable)
BUILTIN_THUNK_1(ground)
BUILTIN_THUNK_2(term_compare)
BUILTIN_THUNK_1(user_defined)


// TEMP

void builtin_call(context_t* context, const term_t* arg1) {  }
void builtin_callN(context_t* context, const term_t* arg1) {  }
void builtin_halt(context_t* context, const term_t* arg1) {  }
void builtin_occurs_check(context_t* context, const term_t* arg1, const term_t* arg2) {  }
void builtin_callable(context_t* context, const term_t* arg1) {  }
void builtin_ground(context_t* context, const term_t* arg1) {  }
void builtin_term_compare(context_t* context, const term_t* arg1, const term_t* arg2) {  }

// END TEMP

