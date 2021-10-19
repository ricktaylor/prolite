#include "context.h"

void prolite_builtin_throw(context_t* context);

#undef DECLARE_BUILTIN_FUNCTION
#define BUILTIN_THUNK(f,n) \
	void builtin_##f(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[]); \
	PROLITE_EXPORT void prolite_builtin_##f(context_t* context) { \
		term_t* sp = context->m_stack; \
		builtin_fn_t gosub = (sp++)->m_pval; \
		const term_t* args = sp; \
		context->m_stack = sp + n; \
		builtin_##f(context,gosub,n,n ? &args : NULL); \
		if (context->m_flags & FLAG_THROW) \
			prolite_builtin_throw(context); \
		context->m_stack = sp; \
	}	

#define DECLARE_BUILTIN_FUNCTION_0(f,p) BUILTIN_THUNK(f,0)
#define DECLARE_BUILTIN_FUNCTION_1(f,p) BUILTIN_THUNK(f,1)
#define DECLARE_BUILTIN_FUNCTION_2(f,p) BUILTIN_THUNK(f,2)
#define DECLARE_BUILTIN_FUNCTION_3(f,p) BUILTIN_THUNK(f,3)

#include "builtin_functions.h"

// These are the supporting builtin thunks for some intrinsics
BUILTIN_THUNK(call,1)
BUILTIN_THUNK(callN,1)
BUILTIN_THUNK(catch,1)
BUILTIN_THUNK(throw,1)
BUILTIN_THUNK(halt,1)
BUILTIN_THUNK(callable,1)
BUILTIN_THUNK(ground,1)
BUILTIN_THUNK(term_compare,2)
BUILTIN_THUNK(type_test,2)
BUILTIN_THUNK(user_defined,1)


// TEMP

void builtin_call(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[]) {  }
void builtin_callN(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[]) {  }
void builtin_halt(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[]) {  }
void builtin_callable(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[]) {  }
void builtin_ground(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[]) {  }
void builtin_term_compare(context_t* context, builtin_fn_t gosub, size_t argc, const term_t* argv[]) {  }

// END TEMP

