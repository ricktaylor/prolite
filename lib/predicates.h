#ifndef PREDICATES_H_
#define PREDICATES_H_

#include "context.h"

typedef struct clause
{
	size_t              m_locals_count;
	const term_t*       m_head;
	const term_t*       m_body;

} clause_t;

void term_to_clause(context_t* context, const term_t* t, clause_t* clause);

typedef struct predicate
{
	unsigned  m_dynamic : 1;
	size_t    m_clause_count;
	clause_t* m_clauses;
} predicate_t;



#endif // PREDICATES_H_
