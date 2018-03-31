
#ifndef CONTEXT_H_INCLUDED_
#define CONTEXT_H_INCLUDED_

#include "types.h"

uint32_t convert_char(struct context_t* context, uint32_t in_char);

/* Try to find a infix/suffix op, otherwise find prefix */
struct operator_t* lookup_op(struct context_t* context, const union box_t* b);

/* Try to find a prefix op, otherwise find infix/suffix */
struct operator_t* lookup_prefix_op(struct context_t* context, const union box_t* b);

int op_3(struct context_t* context, struct term_t* term);
int char_conversion_2(struct context_t* context, struct term_t* term);

void context_reset(struct context_t* context, size_t pos);

#endif /* CONTEXT_H_INCLUDED_ */
