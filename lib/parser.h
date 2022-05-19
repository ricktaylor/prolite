#ifndef PARSER_H_
#define PARSER_H_

#include "context.h"

typedef struct token
{
	prolite_allocator_t* m_allocator;
	size_t               m_alloc;
	size_t               m_len;
	unsigned char*       m_str;  /* Need not be zero-terminated! */
} token_t;

typedef struct line_info
{
	size_t         m_start_line;
	size_t         m_start_col;
	size_t         m_end_line;
	size_t         m_end_col;
} line_info_t;

typedef struct parser
{
	prolite_allocator_t* m_allocator;
	context_t*           m_context;
	prolite_stream_t*    m_s;
	token_t*             m_buffer;
	line_info_t          m_line_info;
	jmp_buf              m_jmp;
	unsigned             m_eof : 1;

	const prolog_flags_t*    m_flags;
	const operator_table_t*  m_operators;
	const char_conv_table_t* m_char_conversion;
} parser_t;

typedef struct var_info
{
	size_t               m_use_count;
	const unsigned char* m_name;
	size_t               m_name_len;
	const debug_info_t*  m_debug_info;
} var_info_t;

typedef void (*pfn_parse_t)(context_t* context, void* param, const term_t* term, size_t var_count, const var_info_t* var_info);
void read_term(parser_t* parser, void* param, pfn_parse_t callback, int multiterm);

#endif // PARSER_H_
