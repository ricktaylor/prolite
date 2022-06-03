#ifndef CONTEXT_H_
#define CONTEXT_H_

#include "export.h"
#include "heap.h"
#include "types.h"

#include <setjmp.h>
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
	operator_specifier_t  m_specifier;
	unsigned int          m_precedence;
} operator_t;

typedef btree_t operator_table_t;
typedef btree_t char_conv_table_t;

const operator_t* lookup_op(const operator_table_t* ops, const unsigned char* name, size_t name_len);
const operator_t* lookup_prefix_op(const operator_table_t* ops, const unsigned char* name, size_t name_len);

typedef struct prolog_flags
{
	unsigned char_conversion : 1;
	unsigned double_quotes : 2;
	unsigned back_quotes : 2;
	unsigned debug : 1;
	unsigned unknown : 2;
	unsigned colon_sets_calling_context : 1;
} prolog_flags_t;

extern const prolog_flags_t g_default_prolog_flags;

typedef struct module
{
	prolog_flags_t    m_flags;
	operator_table_t  m_operators;
	char_conv_table_t m_char_conversion;

	const term_t*  m_name;

} module_t;

enum exec_flags
{
	FLAG_CUT = 1,
	FLAG_THROW = 2,
	FLAG_HALT = 4
};
typedef unsigned int exec_flags_t;

typedef struct substitutions
{
	size_t        m_count;
	const term_t* m_vals[];
} substitutions_t;

typedef struct exception
{
	heap_t  m_heap;
	term_t* m_term;
} exception_t;

typedef struct context
{
	void*                           m_user_data;
	heap_t                          m_heap;
	heap_t                          m_trail;
	exception_t*                    m_exception;
	prolite_exception_handler_fn_t  m_eh;
	prolite_stream_resolver_t*      m_resolver;
	exec_flags_t                    m_flags;
	substitutions_t*                m_substs;
	module_t*                       m_module;

} context_t;

extern const prolite_environment_t g_default_env;

typedef void (*builtin_fn_t)(context_t* context, const term_t* gosub, size_t argc, const term_t* argv[]);
void builtin_gosub(context_t* context, const term_t* gosub);

const term_t* deref_local_var(context_t* context, const term_t* t);
int unify_terms(context_t* context, const term_t* t1, const term_t* t2, int with_occurs_check);

term_t* copy_string(context_t* context, prolite_allocator_t* a, const term_t* src, int shallow);
term_t* copy_term(context_t* context, prolite_allocator_t* a, prolite_allocator_t* tmp_a, const term_t* src, int shallow, int deref, size_t* var_count);

context_t* context_new(void* user_data, const prolite_environment_t* env);
void context_delete(context_t* c);

void throw_term(context_t* context, const term_t* ball);
void throw_out_of_memory_error(context_t* context, const term_t* t);
void throw_instantiation_error(context_t* context, const term_t* t);
void throw_permission_error(context_t* context, uint64_t p1, uint64_t p2, const term_t* t);
void throw_type_error(context_t* context, uint64_t p1, const term_t* t);
void throw_domain_error(context_t* context, uint64_t p1, const term_t* t);
void throw_representation_error(context_t* context, uint64_t p1, const term_t* t);

void unhandled_exception(context_t* context, prolite_allocator_t* a, const operator_table_t* ops);
int is_out_of_memory_exception(context_t* context);

#endif // CONTEXT_H_
