#ifndef PREDICATES_H_
#define PREDICATES_H_

#include "export.h"
#include "btree.h"
#include "types.h"

typedef btree_t predicate_map_t;

typedef struct predicate_base
{
	const term_t* m_functor;

} predicate_base_t;

int predicate_is_builtin(const term_t* pred);

predicate_base_t* predicate_map_lookup(predicate_map_t* pm, const term_t* pred);
predicate_base_t* predicate_map_insert(predicate_map_t* pm, predicate_base_t* pred);
//predicate_base_t* predicate_map_remove(predicate_map_t* pm, const term_t* pred);
void predicate_map_enum(predicate_map_t* pm, void (*callback)(void* param, predicate_base_t* pred), void* param);
void predicate_map_clear(predicate_map_t* pm, void (*callback)(void* param, predicate_base_t* pred), void* param);

#endif // PREDICATES_H_
