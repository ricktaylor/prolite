#ifndef PREDICATES_H_
#define PREDICATES_H_

#include "context.h"
#include "btree.h"

typedef btree_t predicate_map_t;

typedef struct predicate_base
{
	const term_t* m_functor;

} predicate_base_t;

int predicate_is_builtin(const term_t* functor);

predicate_base_t* predicate_map_lookup(predicate_map_t* pm, const term_t* functor);
predicate_base_t* predicate_map_insert(predicate_map_t* pm, predicate_base_t* pred);
predicate_base_t* predicate_map_remove(predicate_map_t* pm, const term_t* functor);
predicate_base_t predicate_map_clear(predicate_map_t* pm);

typedef struct clause
{
	size_t              m_locals_count;
	const term_t*       m_head;
	const term_t*       m_body;

} clause_t;

void term_to_clause(context_t* context, const term_t* t, clause_t* clause);

typedef struct predicate
{
	predicate_base_t m_base;
	unsigned         m_dynamic : 1;
	size_t           m_clause_count;
	clause_t*        m_clauses;
} predicate_t;

#endif // PREDICATES_H_
