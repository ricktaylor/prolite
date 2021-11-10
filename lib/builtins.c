#include "context.h"

#define ARG_EXPAND_0
#define ARG_EXPAND_1 deref_local_var(context,context->m_stack[0].m_pval)
#define ARG_EXPAND_2 ARG_EXPAND_1, deref_local_var(context,context->m_stack[1].m_pval)
#define ARG_EXPAND_3 ARG_EXPAND_2, deref_local_var(context,context->m_stack[2].m_pval)

#undef DECLARE_BUILTIN_FUNCTION
#define BUILTIN_THUNK(f,n) \
	void builtin_##f(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[]); \
	PROLITE_EXPORT void prolite_builtin_##f(context_t* context, const term_t* gosub) { \
		const term_t* args[] = { ARG_EXPAND_##n }; \
		term_t* sp = context->m_stack; \
		builtin_##f(context,gosub,n,args); \
		if (context->m_flags & FLAG_THROW) { \
			builtin_throw(context); \
			context->m_stack = sp; \
		} \
	}

#define DECLARE_BUILTIN_FUNCTION(f,a,p) BUILTIN_THUNK(f,a)

#include "builtin_functions.h"

// These are the supporting builtin thunks for some intrinsics
BUILTIN_THUNK(call,1)
BUILTIN_THUNK(callN,1)
BUILTIN_THUNK(catch,1)
BUILTIN_THUNK(halt,1)
BUILTIN_THUNK(callable,1)
BUILTIN_THUNK(ground,1)
BUILTIN_THUNK(term_compare,2)
BUILTIN_THUNK(type_test,2)
BUILTIN_THUNK(user_defined,1)
BUILTIN_THUNK(throw_evaluable,1);
BUILTIN_THUNK(throw_zero_div,1);
BUILTIN_THUNK(throw_underflow,1);
BUILTIN_THUNK(throw_integer_overflow,1);
BUILTIN_THUNK(throw_float_overflow,1);

// TEMP

void builtin_call(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[]) {  }
void builtin_callN(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[]) {  }
void builtin_callable(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[]) {  }
void builtin_ground(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[]) {  }
void builtin_term_compare(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[]) {  }

// END TEMP

