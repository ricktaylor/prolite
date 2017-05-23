
#ifndef CONTEXT_H_INCLUDED_
#define CONTEXT_H_INCLUDED_

#include "stack.h"
#include "box_types.h"
#include "clause.h"

struct var_info_t
{
	union box_t* m_value;
	union box_t  m_name;
};

struct term_t
{
	struct var_info_t* m_vars;
	union box_t*       m_value;
};

enum eOpSpec
{
	eFX,
	eFY,
	eXFX,
	eXFY,
	eYFX,
	eXF,
	eYF
};

struct operator_t
{
	struct operator_t* m_prev;
	enum eOpSpec       m_specifier;
	unsigned int       m_precedence;
};

struct module_t
{
	struct module_flags_t
	{
		unsigned char_conversion : 1;
		unsigned double_quotes : 2;
		unsigned back_quotes : 2;
		unsigned debug : 1;
		unsigned unknown : 2;
		unsigned colon_sets_calling_context : 1;
	} m_flags;

	struct operator_t*        m_operators;
	struct procedure_table_t* m_procedures;
};

struct string_ptr_t
{
	struct string_ptr_t* m_prev;
	size_t               m_len;
	unsigned char        m_str[];
};

struct context_t
{
	struct stack_t* m_scratch_stack;
	struct stack_t* m_exec_stack;

	struct string_ptr_t* m_strings;
	struct module_t*     m_module;
};

uint32_t convert_char(struct context_t* context, uint32_t in_char);

/* Try to find a infix/suffix op, otherwise find prefix */
struct operator_t* lookup_op(struct context_t* context, const union box_t* b);

/* Try to find a prefix op, otherwise find infix/suffix */
struct operator_t* lookup_prefix_op(struct context_t* context, const union box_t* b);

int op_3(struct context_t* context, struct term_t* term);
int char_conversion_2(struct context_t* context, struct term_t* term);

#endif /* CONTEXT_H_INCLUDED_ */
