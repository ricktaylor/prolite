
#ifndef CLAUSE_H_INCLUDED_
#define CLAUSE_H_INCLUDED_

#include "box_types.h"

struct term_t;
struct procedure_t;

struct procedure_prototype_t
{
	int (*m_get_static)(const struct procedure_t* proc);
	int (*m_set_static)(struct procedure_t* proc, int s);
	int (*m_get_public)(const struct procedure_t* proc);
	int (*m_set_public)(struct procedure_t* proc, int p);
	int (*m_get_multifile)(const struct procedure_t* proc);
	int (*m_set_multifile)(struct procedure_t* proc, int m);
	int (*m_get_discontiguous)(const struct procedure_t* proc);
	int (*m_set_discontiguous)(struct procedure_t* proc, int d);
};

struct clause_t
{

};

struct procedure_t
{
	struct procedure_prototype_t const* m_proto;
};

struct user_procedure_t
{
	struct procedure_t m_base;

	// Housekeeping
	struct clause_t m_clauses[];
};

struct procedure_table_t
{
	// Housekeeping
	int blah;

	struct procedure_t* m_procedures[];
};

/* Convert a term into a query */
int add_query(struct context_t* context, struct term_t* term, uint64_t stack_base);

/* Assert a clause */
int assert_clause(struct context_t* context, struct term_t* term, int z);

#endif /* CLAUSE_H_INCLUDED_ */
