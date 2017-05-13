
#include "stack.h"
#include "box_types.h"

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

	struct context_flags_t
	{
		unsigned char_conversion : 1;
		unsigned double_quotes : 2;
		unsigned back_quotes : 2;
		unsigned debug : 1;
		unsigned unknown : 2;
	} m_flags;
};

uint32_t context_convert_char(struct context_t* context, uint32_t in_char);

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
	enum eOpSpec m_specifier;
	unsigned int m_precedence;
};

/* Try to find a infix/suffix op, otherwise find prefix */
struct operator_t* lookup_op(struct context_t* context, const union box_t* b);

/* Try to find a prefix op, otherwise find infix/suffix */
struct operator_t* lookup_prefix_op(struct context_t* context, const union box_t* b);
