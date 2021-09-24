#ifndef CONTEXT_H_
#define CONTEXT_H_

#include "heap.h"
#include "types.h"
#include "setjmp.h"

#include <assert.h>

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

	operator_t*   m_operators;
	const term_t* m_name;

} module_t;

typedef enum exec_flags
{
	FLAG_FAIL = 1,
	FLAG_CUT = 2,
	FLAG_THROW = 4,
	FLAG_HALT = 8
} exec_flags_t;

typedef struct substitutions
{
	size_t        m_count;
	const term_t* m_vals[];
} substitutions_t;

typedef struct context
{
	void*                           m_user_data;
	heap_t                          m_heap;
	term_t*                         m_stack;
	prolite_exception_handler_fn_t  m_eh;
	prolite_stream_resolver_t*      m_resolver;
	exec_flags_t                    m_flags;
	substitutions_t*                m_locals;
	substitutions_t*                m_params;
	module_t*                       m_module;
	
} context_t;

static_assert(offsetof(context_t,m_user_data) == 0,"structure members reorganised");

extern const prolite_environment_t g_default_env;

typedef void (*builtin_fn_t)(context_t* context);

const term_t* deref_local_var(context_t* context, const term_t* t);

const term_t* copy_term_to_heap(context_t* context, const term_t* t, size_t* var_count);

context_t* context_new(void* user_data, const prolite_environment_t* env);
void context_delete(context_t* c);

#endif // CONTEXT_H_
