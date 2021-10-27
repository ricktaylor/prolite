#ifndef PARSER_H_
#define PARSER_H_

#include "context.h"

typedef struct token
{
	size_t         m_alloc;
	size_t         m_len;
	unsigned char* m_str;  /* Need not be zero-terminated! */
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
	context_t*        m_context;
	prolite_stream_t* m_s;
	token_t*          m_buffer;
	line_info_t       m_line_info;
	jmp_buf           m_jmp;
	unsigned          m_eof : 1;
	unsigned          m_multiterm : 1;

	const prolog_flags_t*    m_flags;
	const operator_table_t*  m_operators;
	const char_conv_table_t* m_char_conversion;
} parser_t;

const term_t* read_term(parser_t* parser);

#endif // PARSER_H_
