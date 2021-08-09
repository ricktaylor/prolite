#ifndef BUILTINS_H_
#define BUILTINS_H_

#define DECLARE_BUILTIN_FUNCTION(f,n) \
void builtin_##f(context_t* context);

#include "builtin_functions.h"

void builtin_call(context_t* context);
void builtin_callN(context_t* context);
void builtin_catch(context_t* context);
void builtin_throw(context_t* context);
void builtin_halt(context_t* context);
void builtin_user_defined(context_t* context);
void builtin_callable(context_t* context);
void builtin_occurs_check(context_t* context);
void builtin_term_compare(context_t* context);

#endif // BUILTINS_H_
