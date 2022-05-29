#include "context.h"

// TEMP

PROLITE_EXPORT void prolite_builtin_call(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
    exec_flags_t flags = context->m_flags;

    builtin_gosub(context,gosub);

    context->m_flags = (context->m_flags & ~FLAG_CUT) | flags;
}

PROLITE_EXPORT void prolite_builtin_callN(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[]) {  }

// END TEMP

