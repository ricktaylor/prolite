#include "context.h"

// TEMP

PROLITE_EXPORT void prolite_builtin_call(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[])
{
    if (argc == 1)
    {
        assert(!gosub);

        if (unpack_term_type(argv[0]) == prolite_var)
            throw_instantiation_error(context,argv[0]);
        else
        {
            // Callable error
            assert(0);
        }
    }
    else
    {
        // Do the deed
    }
}

PROLITE_EXPORT void prolite_builtin_callN(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[]) {  }

// END TEMP

