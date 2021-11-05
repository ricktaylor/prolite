#ifndef BUILTINS_H_
#define BUILTINS_H_

#define DECLARE_BUILTIN_FUNCTION(f,a,p) \
void prolite_builtin_##f(context_t* context, const void* gosub);

#include "builtin_functions.h"

void prolite_builtin_call(context_t* context, const void* gosub);
void prolite_builtin_callN(context_t* context, const void* gosub);
void prolite_builtin_catch(context_t* context, const void* gosub);
void prolite_builtin_throw(context_t* context, const void* gosub);
void prolite_builtin_throw_evaluable(context_t* context, const void* gosub);
void prolite_builtin_throw_zero_div(context_t* context, const void* gosub);
void prolite_builtin_throw_underflow(context_t* context, const void* gosub);
void prolite_builtin_throw_integer_overflow(context_t* context, const void* gosub);
void prolite_builtin_throw_float_overflow(context_t* context, const void* gosub);
void prolite_builtin_halt(context_t* context, const void* gosub);
void prolite_builtin_user_defined(context_t* context, const void* gosub);
void prolite_builtin_callable(context_t* context, const void* gosub);
void prolite_builtin_unify(context_t* context, const void* gosub);
void prolite_builtin_unify_with_occurs_check(context_t* context, const void* gosub);
void prolite_builtin_ground(context_t* context, const void* gosub);
void prolite_builtin_type_test(context_t* context, const void* gosub);

#endif // BUILTINS_H_
