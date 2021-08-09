
#include "context.h"


void builtin_user_defined(context_t* context)
{
    // Find predicates
    size_t count = 0;//find_predicates(context);
    if (!count)
    {
        // Check if we should throw
    }
    else
    {

    }
}


static void builtin_assert(context_t* context, int front)
{

}

void builtin_asserta(context_t* context)
{
    return builtin_assert(context,1);
}

void builtin_assertz(context_t* context)
{
    return builtin_assert(context,0);
}
