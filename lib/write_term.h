#ifndef WRITE_TERM_H_INCLUDED
#define WRITE_TERM_H_INCLUDED

#include "context.h"

typedef struct write_options
{
    unsigned quoted : 1;
    unsigned ignore_ops : 1;
    unsigned numbervars : 1;
    const term_t* variable_names;

} write_options_t;

prolite_stream_error_t write_term(context_t* context, prolite_stream_t* s, const term_t* term, const write_options_t* options, const operator_table_t* ops);

#if ENABLE_TESTS
prolite_stream_error_t dump_term(prolite_stream_t* s, const term_t* term);
#endif

#endif // WRITE_TERM_H_INCLUDED
