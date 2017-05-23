
#ifndef CLAUSE_H_INCLUDED_
#define CLAUSE_H_INCLUDED_

#include "box_types.h"

struct term_t;
struct procedure_t;

struct clause_t
{
	struct term_t m_term;

	uint64_t* m_opcodes;
};

struct procedure_t
{
	struct procedure_flags_t
	{
		unsigned dynamic : 1;
		unsigned multifile : 1;
		unsigned discontiguous : 1;
		unsigned public : 1;
	} m_flags;

	uint64_t* m_opcodes;

	size_t m_clause_count;
	struct clause_t m_clauses[];
};

struct procedure_table_t
{
	int fast_hash_table;

	size_t m_procedure_count;
	struct procedure_t* m_procedures[];
};

/* Convert a term into a query */
int add_query(struct context_t* context, struct term_t* term, uint64_t stack_base);

/* Assert a clause */
int assert_clause(struct context_t* context, struct term_t* term, int z);

#endif /* CLAUSE_H_INCLUDED_ */
