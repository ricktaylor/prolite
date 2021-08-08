#ifndef CONTEXT_H_
#define CONTEXT_H_

#include "types.h"
#include "heap.h"

typedef enum operator_specifier
{
	eFX,
	eFY,
	eXFX,
	eXFY,
	eYFX,
	eXF,
	eYF
} operator_specifier_t;

typedef struct operator_defn
{
	struct operator_defn* m_prev;
	operator_specifier_t  m_specifier;
	unsigned int          m_precedence;
} operator_t;

typedef struct module
{
	struct module_flags
	{
		unsigned char_conversion : 1;
		unsigned double_quotes : 2;
		unsigned back_quotes : 2;
		unsigned debug : 1;
		unsigned unknown : 2;
		unsigned colon_sets_calling_context : 1;
	} m_flags;

	operator_t* m_operators;

} module_t;

typedef enum exec_flags
{
	FLAG_FAIL = 1,
	FLAG_CUT = 2,
	FLAG_THROW = 4,
	FLAG_HALT = 8
} exec_flags_t;

typedef struct context
{
	heap_t*      m_heap;
	term_t*      m_stack;
	exec_flags_t m_flags;

	module_t*    m_module;
} context_t;

context_t* context_new(heap_t* heap);
void context_delete(context_t* c);

#endif // CONTEXT_H_


/* OLD GUFF

struct predicate;

struct clause
{
	struct predicate*   m_pred;
	struct clause*      m_next;
	struct clause*      m_prev;
	size_t              m_var_count;
	const term_t*       m_head;
	size_t              m_entry_point;
};

*/
