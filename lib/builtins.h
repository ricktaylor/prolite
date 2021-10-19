#ifndef BUILTINS_H_
#define BUILTINS_H_

#define DECLARE_BUILTIN_FUNCTION(f,p,a) \
void prolite_builtin_##f(context_t* context);

#include "builtin_functions.h"

void prolite_builtin_call(context_t* context);
void prolite_builtin_callN(context_t* context);
void prolite_builtin_catch(context_t* context);
void prolite_builtin_throw(context_t* context);
void prolite_builtin_halt(context_t* context);
void prolite_builtin_user_defined(context_t* context);
void prolite_builtin_callable(context_t* context);
void prolite_builtin_unify(context_t* context);
void prolite_builtin_ground(context_t* context);
void prolite_builtin_type_test(context_t* context);
void prolite_builtin_term_compare(context_t* context);

#endif // BUILTINS_H_
