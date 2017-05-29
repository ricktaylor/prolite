
#ifndef CLAUSE_H_INCLUDED_
#define CLAUSE_H_INCLUDED_

#include "types.h"

/* Convert a term into a query */
int add_query(struct context_t* context, struct term_t* term, uint64_t stack_base);

/* Assert a clause */
int assert_clause(struct context_t* context, struct term_t* term, int z);

int check_callable_term(union box_t* v);

#endif /* CLAUSE_H_INCLUDED_ */
