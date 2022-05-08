#include "parser.h"

#include <string.h>
#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <fenv.h>

#if !defined(__GNUC__)
#pragma STDC FENV_ACCESS ON
#endif

#ifdef _MSC_VER
#include <float.h>
#define isnan _isnan
#endif

uint32_t convert_char(const char_conv_table_t* cc, uint32_t in_char);

typedef enum token_type
{
	tokNeedMore = 0,
	tokEOF,
	tokMissingSQ,
	tokMissingDQ,
	tokMissingBQ,
	tokName,
	tokVar,
	tokInt,
	tokBinaryInt,
	tokOctalInt,
	tokHexInt,
	tokCharCode,
	tokFloat,
	tokDQL,
	tokBackQuote,
	tokOpen,
	tokOpenCt,
	tokClose,
	tokOpenL,
	tokCloseL,
	tokOpenC,
	tokCloseC,
	tokBar,
	tokComma,
	tokEnd
} token_type_t;

typedef struct ast_node
{
	union
	{
		const unsigned char* m_str;
		double               m_dval;
		uint64_t             m_u64val;
	};
	size_t           m_str_len;
	size_t           m_arity;
	prolite_type_t   m_type;
	debug_info_t*    m_debug_info;
	struct ast_node* m_params[];
} ast_node_t;

typedef enum ast_error
{
	AST_ERR_NONE = 0,
	AST_ERR_OUTOFMEMORY,
	AST_ERR_FLOAT_OVERFLOW,
	AST_ERR_FLOAT_UNDERFLOW,
	AST_ERR_MAX_INTEGER,
	AST_ERR_MIN_INTEGER,
	AST_ERR_MAX_ARITY,
	AST_SYNTAX_ERR_MISSING_DOT,
	AST_SYNTAX_ERR_MISSING_CLOSE,
	AST_SYNTAX_ERR_MISSING_CLOSE_L,
	AST_SYNTAX_ERR_MISSING_CLOSE_C,
	AST_SYNTAX_ERR_MISSING_SQ,
	AST_SYNTAX_ERR_MISSING_DQ,
	AST_SYNTAX_ERR_MISSING_BQ,
	AST_SYNTAX_ERR_INVALID_ARG,
	AST_SYNTAX_ERR_UNEXPECTED_TOKEN,
	AST_SYNTAX_ERR_UNEXPECTED_EOF,
	AST_SYNTAX_ERR_INVALID_CHAR,
	AST_SYNTAX_ERR_INVALID_ESCAPE,
	AST_SYNTAX_ERR_INVALID_UTF8
} ast_error_t;

enum char_max
{
	CHAR_MAX_VALID = 0x1FFFF, /* Greatest valid unicode char */
	CHAR_NEED_MORE,
	CHAR_EOF,
	CHAR_ILLEGAL_SEQ
};

static void syntax_error(parser_t* parser, ast_error_t error)
{
	longjmp(parser->m_jmp,error);
}

static void token_reset(token_t* token)
{
	token->m_alloc = 0;
	token->m_len = 0;
	token->m_str = NULL;
}

static void token_append_char(parser_t* parser, token_t* token, unsigned char c)
{
	if (token->m_alloc == token->m_len)
	{
		size_t new_size = (token->m_alloc == 0 ? 16 : token->m_alloc * 2);
		unsigned char* new_str = heap_realloc(&parser->m_context->m_heap,token->m_str,token->m_alloc,new_size);
		if (!new_str)
			syntax_error(parser,AST_ERR_OUTOFMEMORY);

		token->m_alloc = new_size;
		token->m_str = new_str;
	}

	token->m_str[token->m_len++] = c;
}

static void token_append_unicode_char(parser_t* parser, token_t* token, uint32_t unicode_char)
{
	if (unicode_char <= 0x7F)
		token_append_char(parser,token,(unsigned char)unicode_char);
	else
	{
		unsigned char chars[4] = {0};
		unsigned int count;

		if (unicode_char <= 0x7FF)
		{
			chars[0] = (unsigned char)(0xC0 | ((unicode_char & 0x7C0) >> 6));
			chars[1] = (unsigned char)(0x80 | (unicode_char & 0x3F));
			count = 2;
		}
		else if (unicode_char <= 0xFFFF)
		{
			chars[0] = (unsigned char)(0xE0 | ((unicode_char & 0xF000) >> 12));
			chars[1] = (unsigned char)(0x80 | ((unicode_char & 0xFC0) >> 6));
			chars[2] = (unsigned char)(0x80 | (unicode_char & 0x3F));
			count = 3;
		}
		else
		{
			chars[0] = (unsigned char)(0xF0 | ((unicode_char & 0x1C0000) >> 18));
			chars[1] = (unsigned char)(0x80 | ((unicode_char & 0x3F000) >> 12));
			chars[2] = (unsigned char)(0x80 | ((unicode_char & 0xFC0) >> 6));
			chars[3] = (unsigned char)(0x80 | (unicode_char & 0x3F));
			count = 3;
		}

		for (unsigned int i = 0; i < count; ++i)
			token_append_char(parser,token,chars[i]);
	}
}

static uint32_t token_skip_ilseq(const unsigned char** p, const unsigned char* pe, int eof, size_t* col, unsigned int i)
{
	while (i < 4)
	{
		if ((*p)+i == pe)
		{
			if (eof)
			{
				*p += i;
				*col += i;
				break;
			}

			return CHAR_NEED_MORE;
		}

		if ((*p)[i] <= 0x7f)
		{
			*p += i;
			*col += i;
			break;
		}

		++i;
	}
	return CHAR_ILLEGAL_SEQ;
}

static uint32_t token_get_char(const unsigned char** p, const unsigned char* pe, int eof, size_t* line, size_t* col)
{
	const unsigned char* c = *p;
	if (c == pe)
	{
		if (eof)
			return CHAR_EOF;

		return CHAR_NEED_MORE;
	}

	unsigned int count = 0;
	uint32_t val;

	if (c[0] <= 0x7f)
	{
		val = c[0];
		count = 1;
	}
	else if (c[0] >= 0xC2 && c[0] <= 0xF4)
	{
		if ((c[0] & 0xE0) == 0xC0)
		{
			count = 2;
			val = (c[0] & 0x1F);
		}
		else if ((c[0] & 0xF0) == 0xE0)
		{
			if (c+1 == pe)
			{
				if (eof)
					return token_skip_ilseq(p,pe,eof,col,1);

				return CHAR_NEED_MORE;
			}

			if ((c[0] == 0xE0 && c[1] >= 0x80 && c[1] <= 0x9F) ||
				(c[0] == 0xED && c[1] >= 0xA0 && c[1] <= 0xBF))
			{
				return token_skip_ilseq(p,pe,eof,col,2);
			}

			count = 3;
			val = (c[0] & 0x0F);
		}
		else if ((c[0] & 0xF8) == 0xF0)
		{
			if (c+1 == pe)
			{
				if (eof)
					return token_skip_ilseq(p,pe,eof,col,1);

				return CHAR_NEED_MORE;
			}

			if ((c[0] == 0xF0 && c[1] >= 0x80 && c[1] <= 0x8F) ||
				(c[0] == 0xF4 && c[1] >= 0x90 && c[1] <= 0xBF))
			{
				return token_skip_ilseq(p,pe,eof,col,2);
			}

			count = 4;
			val = (c[0] & 0x7);
		}
		else
			return token_skip_ilseq(p,pe,eof,col,1);

		if (c > pe - count)
		{
			if (eof)
				return token_skip_ilseq(p,pe,eof,col,1);

			return CHAR_NEED_MORE;
		}
		else
		{
			for (unsigned int i = 1; i < count; ++i)
			{
				if ((c[i] & 0xC0) != 0x80)
				{
					*p += i;
					*col += i;
					return CHAR_ILLEGAL_SEQ;
				}

				val = (val << 6) | (c[i] & 0x3F);
			}
		}
	}
	else
		return token_skip_ilseq(p,pe,eof,col,1);

	if (val == '\n')
	{
		++(*line);
		*col = 1;
	}
	else
		++(*col);

	*p += count;
	return val;
}

static uint32_t token_get_char_conv(parser_t* parser, const unsigned char** p, const unsigned char* pe, int eof, size_t* line, size_t* col)
{
	uint32_t c = token_get_char(p,pe,eof,line,col);
	if (parser->m_flags->char_conversion && c <= CHAR_MAX_VALID)
		c = convert_char(parser->m_char_conversion,c);
	return c;
}

enum eState
{
	eStart = 0,
	eLayout,
	eSingleComment,
	eMultiComment1,
	eMultiComment2,
	eMultiComment3,
	eDot,
	eName,
	eGraphicName,
	eVar,
	eZero,
	eBinaryInt,
	eOctalInt,
	eOctalCharCode,
	eHexInt,
	eHexCharCode,
	eInteger,
	eDecimal,
	eFraction,
	eExponent,
	eMantissa,
	eSingleQuote,
	eSingleQuoteOct,
	eSingleQuoteHex,
	eDoubleQuote,
	eDoubleQuoteOct,
	eDoubleQuoteHex,
	eBackQuote,
	eBackQuoteOct,
	eBackQuoteHex
};

enum eAction
{
	eaErr = 0,
	eaWhitespace,
	eaShortName,
	eaSingleComment,
	eaFwSlash,
	eaDot,
	eaOpen,
	eaClose,
	eaOpenL,
	eaCloseL,
	eaOpenC,
	eaCloseC,
	eaComma,
	eaBar,
	eaGraphic,
	eaName,
	eaVar,
	eaZero,
	eaNumber,
	eaSingleQuote,
	eaDoubleQuote,
	eaBackQuote
};

static const enum eAction c_actions[128] = {
	/* \x0 */ eaErr, /* \x1 */ eaErr, /* \x2 */ eaErr, /* \x3 */ eaErr,
	/* \x4 */ eaErr, /* \x5 */ eaErr, /* \x6 */ eaErr, /* \x7 */ eaErr,
	/* \x8 */ eaErr, /* \t */ eaWhitespace,/* \n */ eaWhitespace,/* \xb */ eaErr,
	/* \xc */ eaErr, /* \xd */ eaErr, /* \xe */ eaErr, /* \xf */ eaErr,
	/* \x10 */ eaErr,/* \x11 */ eaErr,/* \x12 */ eaErr,/* \x13 */ eaErr,
	/* \x14 */ eaErr,/* \x15 */ eaErr,/* \x16 */ eaErr,/* \x17 */ eaErr,
	/* \x18 */ eaErr,/* \x19 */ eaErr,/* \x1a */ eaErr,/* \x1b */ eaErr,
	/* \x1c */ eaErr,/* \x1d */ eaErr,/* \x1e */ eaErr,/* \x1f */ eaErr,
	/* space */ eaWhitespace, /* ! */ eaShortName, /* " */ eaDoubleQuote, /* # */ eaGraphic,
	/* $ */ eaGraphic, /* % */ eaSingleComment, /* & */ eaGraphic, /* ' */ eaSingleQuote,
	/* ( */ eaOpen, /* ) */ eaClose, /* * */ eaGraphic, /* + */ eaGraphic,
	/* , */ eaComma, /* - */ eaGraphic, /* . */ eaDot, /* / */ eaFwSlash,
	/* 0 */ eaZero, /* 1 */ eaNumber, /* 2 */ eaNumber, /* 3 */ eaNumber,
	/* 4 */ eaNumber, /* 5 */ eaNumber, /* 6 */ eaNumber, /* 7 */ eaNumber,
	/* 8 */ eaNumber, /* 9 */ eaNumber, /* : */ eaGraphic, /* ; */ eaShortName,
	/* < */ eaGraphic, /* = */ eaGraphic, /* > */ eaGraphic, /* ? */ eaGraphic,
	/* @ */ eaGraphic, /* A */ eaVar, /* B */ eaVar, /* C */ eaVar,
	/* D */ eaVar, /* E */ eaVar, /* F */ eaVar, /* G */ eaVar,
	/* H */ eaVar, /* I */ eaVar, /* J */ eaVar, /* K */ eaVar,
	/* L */ eaVar, /* M */ eaVar, /* N */ eaVar, /* O */ eaVar,
	/* P */ eaVar, /* Q */ eaVar, /* R */ eaVar, /* S */ eaVar,
	/* T */ eaVar, /* U */ eaVar, /* V */ eaVar, /* W */ eaVar,
	/* X */ eaVar, /* Y */ eaVar, /* Z */ eaVar, /* [ */ eaOpenL,
	/* \ */ eaGraphic, /* ] */ eaCloseL, /* ^ */ eaGraphic, /* _ */ eaVar,
	/* ` */ eaBackQuote, /* a */ eaName, /* b */ eaName, /* c */ eaName,
	/* d */ eaName, /* e */ eaName, /* f */ eaName, /* g */ eaName,
	/* h */ eaName, /* i */ eaName, /* j */ eaName, /* k */ eaName,
	/* l */ eaName, /* m */ eaName, /* n */ eaName, /* o */ eaName,
	/* p */ eaName, /* q */ eaName, /* r */ eaName, /* s */ eaName,
	/* t */ eaName, /* u */ eaName, /* v */ eaName, /* w */ eaName,
	/* x */ eaName, /* y */ eaName, /* z */ eaName, /* { */ eaOpenC,
	/* | */ eaBar, /* } */ eaCloseC, /* ~ */ eaGraphic, /*0x7f */ eaErr
};

static int token_meta_char(parser_t* parser, uint32_t meta, token_t* token)
{
	unsigned char c = 0;
	switch (meta)
	{
	case '\'':
	case '"':
	case '\\':
	case '`':
		c = (unsigned char)meta;
		break;

	case 'a':
		c = '\a';
		break;

	case 'b':
		c = '\b';
		break;

	case 'f':
		c = '\f';
		break;

	case 'n':
		c = '\n';
		break;

	case 'r':
		c = '\r';
		break;

	case 't':
		c = '\t';
		break;

	case 'v':
		c = '\v';
		break;

	default:
		return 0;
	}

	token_append_char(parser,token,c);
	return 1;
}

static token_type_t parse_token(parser_t* parser, enum eState* state, const unsigned char** p, const unsigned char* pe, token_t* token)
{
	const unsigned char* peek = *p;
	size_t peek_line = parser->m_line_info.m_end_line;
	size_t peek_col = parser->m_line_info.m_end_col;
	uint32_t c;
	uint32_t meta;

	switch (*state)
	{
	case eStart:
		/* Clear the current token */
		token->m_len = 0;

		c = token_get_char_conv(parser,p,pe,parser->m_eof,&parser->m_line_info.m_end_line,&parser->m_line_info.m_end_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		/* Check for ( first*/
		if (c == '(')
			return tokOpenCt;

		*state = eLayout;
		goto layout;

	case eLayout:
	case eSingleComment:
	case eMultiComment2:
	case eMultiComment3:
		c = token_get_char_conv(parser,p,pe,parser->m_eof,&parser->m_line_info.m_end_line,&parser->m_line_info.m_end_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

	layout:
		for (;;)
		{
			if (c == CHAR_EOF)
				return tokEOF;

			if (*state == eLayout)
			{
				if (c == CHAR_ILLEGAL_SEQ)
					syntax_error(parser,AST_SYNTAX_ERR_INVALID_UTF8);

				if (c > 0x7f)
					syntax_error(parser,AST_SYNTAX_ERR_INVALID_CHAR);

				switch (c_actions[c & 0x7F])
				{
				case eaWhitespace:
					parser->m_line_info.m_start_line = parser->m_line_info.m_end_line;
					parser->m_line_info.m_start_col = parser->m_line_info.m_end_col;
					break;

				case eaShortName:
					token_append_char(parser,token,(unsigned char)c);
					*state = eStart;
					return tokName;

				case eaSingleComment:
					*state = eSingleComment;
					break;

				case eaFwSlash:
					*state = eMultiComment1;
					goto multi_comment;

				case eaDot:
					*state = eDot;
					goto dot;

				case eaOpen:
					*state = eStart;
					return tokOpen;

				case eaClose:
					*state = eStart;
					return tokClose;

				case eaOpenL:
					*state = eStart;
					return tokOpenL;

				case eaCloseL:
					*state = eStart;
					return tokCloseL;

				case eaOpenC:
					*state = eStart;
					return tokOpenC;

				case eaCloseC:
					*state = eStart;
					return tokCloseC;

				case eaComma:
					*state = eStart;
					return tokComma;

				case eaBar:
					*state = eStart;
					return tokBar;

				case eaGraphic:
					*state = eGraphicName;
					goto append_chars;

				case eaName:
					*state = eName;
					goto append_chars;

				case eaVar:
					*state = eVar;
					goto append_chars;

				case eaZero:
					*state = eZero;
					goto zero;

				case eaNumber:
					*state = eInteger;
					goto append_chars;

				case eaSingleQuote:
					*state = eSingleQuote;
					goto quote;

				case eaDoubleQuote:
					*state = eDoubleQuote;
					goto quote;

				case eaBackQuote:
					*state = eBackQuote;
					goto quote;

				case eaErr:
				default:
					syntax_error(parser,AST_SYNTAX_ERR_INVALID_CHAR);
				}
			}
			else if (*state == eSingleComment)
			{
				if (c == '\n')
				{
					parser->m_line_info.m_start_line = parser->m_line_info.m_end_line;
					parser->m_line_info.m_start_col = parser->m_line_info.m_end_col;
					*state = eLayout;
				}
			}
			else if (*state == eMultiComment2)
			{
				if (c == '*')
					*state = eMultiComment3;
			}
			else if (*state == eMultiComment3)
			{
				if (c == '/')
				{
					parser->m_line_info.m_start_line = parser->m_line_info.m_end_line;
					parser->m_line_info.m_start_col = parser->m_line_info.m_end_col;
					*state = eLayout;
				}
				else if (c != '*')
					*state = eMultiComment2;
			}

			/* Get the next character */
			c = token_get_char_conv(parser,p,pe,parser->m_eof,&parser->m_line_info.m_end_line,&parser->m_line_info.m_end_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;
		}

	multi_comment:
		peek = *p;
		peek_line = parser->m_line_info.m_end_line;
		peek_col = parser->m_line_info.m_end_col;

	case eMultiComment1:
		c = token_get_char_conv(parser,&peek,pe,parser->m_eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c != '*')
		{
			token_append_char(parser,token,'/');
			*state = eGraphicName;
			goto graphic_name;
		}

		*p = peek;
		parser->m_line_info.m_end_line = peek_line;
		parser->m_line_info.m_end_col = peek_col;
		*state = eMultiComment2;
		goto layout;

	dot:
		peek = *p;
		peek_col = parser->m_line_info.m_end_col;

	case eDot:
		c = token_get_char_conv(parser,&peek,pe,parser->m_eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c == CHAR_EOF || c == ' ' || c == '\n' || c == '\t')
		{
			*state = eLayout;
			return tokEnd;
		}

		if (c == '%')
		{
			*state = eSingleComment;
			return tokEnd;
		}

		token_append_char(parser,token,'.');
		*state = eGraphicName;
		goto graphic_name;

	quote:
		peek = *p;
		peek_col = parser->m_line_info.m_end_col;

	case eSingleQuote:
	case eDoubleQuote:
	case eBackQuote:
		for (;;)
		{
			c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
			switch (c)
			{
			case CHAR_NEED_MORE:
				return tokNeedMore;

			case CHAR_EOF:
				if (*state == eSingleQuote)
				{
					*state = eStart;
					return tokMissingSQ;
				}
				if (*state == eDoubleQuote)
				{
					*state = eStart;
					return tokMissingDQ;
				}
				*state = eStart;
				return tokMissingBQ;

			case CHAR_ILLEGAL_SEQ:
				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				syntax_error(parser,AST_SYNTAX_ERR_INVALID_UTF8);

			case '\\':
				c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
				if (c == CHAR_NEED_MORE)
					return tokNeedMore;

				if (!token_meta_char(parser,c,token))
				{
					if (c == 'o')
					{
						c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
						if (c == CHAR_NEED_MORE)
							return tokNeedMore;

						if (c >= '0' && c <= '7')
						{
							if (*state == eSingleQuote)
								*state = eSingleQuoteOct;
							else if (*state == eDoubleQuote)
								*state = eDoubleQuoteOct;
							else
								*state = eBackQuoteOct;
							meta = 0;
							goto octal_char;
						}
						c = 0;
					}
					else if (c == 'x')
					{
						c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
						if (c == CHAR_NEED_MORE)
							return tokNeedMore;

						if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
						{
							if (*state == eSingleQuote)
								*state = eSingleQuoteHex;
							else if (*state == eDoubleQuote)
								*state = eDoubleQuoteHex;
							else
								*state = eBackQuoteHex;
							meta = 0;
							goto hex_char;
						}
						c = 0;
					}

					if (c != '\n')
					{
						parser->m_line_info.m_end_line = peek_line;
						parser->m_line_info.m_end_col = peek_col;
						if (c == CHAR_ILLEGAL_SEQ)
							syntax_error(parser,AST_SYNTAX_ERR_INVALID_UTF8);

						syntax_error(parser,AST_SYNTAX_ERR_INVALID_ESCAPE);
					}
				}
				break;

			case '\'':
				if (*state == eSingleQuote)
				{
					size_t peek_line2 = peek_line;
					size_t peek_col2 = peek_col;
					const unsigned char* peek2 = peek;
					c = token_get_char(&peek2,pe,parser->m_eof,&peek_line2,&peek_col2);
					if (c == CHAR_NEED_MORE)
						return tokNeedMore;

					if (c != '\'')
					{
						parser->m_line_info.m_end_line = peek_line;
						parser->m_line_info.m_end_col = peek_col;
						*p = peek;
						*state = eStart;
						return tokName;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				token_append_char(parser,token,(unsigned char)c);
				break;

			case '"':
				if (*state == eDoubleQuote)
				{
					size_t peek_line2 = peek_line;
					size_t peek_col2 = peek_col;
					const unsigned char* peek2 = peek;
					c = token_get_char(&peek2,pe,parser->m_eof,&peek_line2,&peek_col2);
					if (c == CHAR_NEED_MORE)
						return tokNeedMore;

					if (c != '"')
					{
						parser->m_line_info.m_end_line = peek_line;
						parser->m_line_info.m_end_col = peek_col;
						*p = peek;
						*state = eStart;
						return tokDQL;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				token_append_char(parser,token,(unsigned char)c);
				break;

			case '`':
				if (*state == eBackQuote)
				{
					size_t peek_line2 = peek_line;
					size_t peek_col2 = peek_col;
					const unsigned char* peek2 = peek;
					c = token_get_char(&peek2,pe,parser->m_eof,&peek_line2,&peek_col2);
					if (c == CHAR_NEED_MORE)
						return tokNeedMore;

					if (c != '`')
					{
						parser->m_line_info.m_end_line = peek_line;
						parser->m_line_info.m_end_col = peek_col;
						*p = peek;
						*state = eStart;
						return tokBackQuote;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				token_append_char(parser,token,(unsigned char)c);
				break;

			default:
				if (c < 32)
				{
					parser->m_line_info.m_end_line = peek_line;
					parser->m_line_info.m_end_col = peek_col;
					syntax_error(parser,AST_SYNTAX_ERR_INVALID_CHAR);
				}
				token_append_unicode_char(parser,token,c);
				break;
			}

			parser->m_line_info.m_end_line = peek_line;
			parser->m_line_info.m_end_col = peek_col;
			*p = peek;
		}

	case eSingleQuoteOct:
	case eDoubleQuoteOct:
	case eBackQuoteOct:
		/* We know *p == "\o" */
		peek += 2;
		peek_col += 2;
		meta = 0;
		for (;;)
		{
			c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c < '0' || c > '7')
			{
				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				*p = peek;

				if (c == '\\')
				{
					token_append_unicode_char(parser,token,meta);

					if (*state == eSingleQuoteOct)
						*state = eSingleQuote;
					else if (*state == eDoubleQuoteOct)
						*state = eDoubleQuote;
					else
						*state = eBackQuote;
					goto quote;
				}

				if (c == CHAR_ILLEGAL_SEQ)
					syntax_error(parser,AST_SYNTAX_ERR_INVALID_UTF8);

				syntax_error(parser,AST_SYNTAX_ERR_INVALID_ESCAPE);
			}

		octal_char:
			meta = (meta << 3) | (c - '0');
			if (meta > CHAR_MAX_VALID)
			{
				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				syntax_error(parser,AST_SYNTAX_ERR_INVALID_ESCAPE);
			}
		}

	case eSingleQuoteHex:
	case eDoubleQuoteHex:
	case eBackQuoteHex:
		/* We know *p == "\x" */
		peek += 2;
		peek_col += 2;
		meta = 0;
		for (;;)
		{
			c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')))
			{
				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				*p = peek;

				if (c == '\\')
				{
					token_append_unicode_char(parser,token,meta);

					if (*state == eSingleQuoteHex)
						*state = eSingleQuote;
					else if (*state == eDoubleQuoteHex)
						*state = eDoubleQuote;
					else
						*state = eBackQuote;
					goto quote;
				}

				if (c == CHAR_ILLEGAL_SEQ)
					syntax_error(parser,AST_SYNTAX_ERR_INVALID_UTF8);

				syntax_error(parser,AST_SYNTAX_ERR_INVALID_ESCAPE);
			}

		hex_char:
			meta = (meta << 4);
			if (c >= '0' && c <= '9')
				meta |= (c - '0');
			else if (c >= 'A' && c <= 'F')
				meta |= (c - 'A') + 10;
			else
				meta |= (c - 'a') + 10;

			if (meta > CHAR_MAX_VALID)
			{
				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				syntax_error(parser,AST_SYNTAX_ERR_INVALID_ESCAPE);
			}
		}

	zero:
		peek = *p;
		peek_col = parser->m_line_info.m_end_col;

	case eZero:
		c = token_get_char_conv(parser,&peek,pe,parser->m_eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c == '\'')
		{
			/* No char_conversion for single_quoted_character */
			c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c == '\'')
			{
				c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
				if (c == CHAR_NEED_MORE)
					return tokNeedMore;

				if (c == '\'')
				{
					token_append_char(parser,token,'\'');
					parser->m_line_info.m_end_line = peek_line;
					parser->m_line_info.m_end_col = peek_col;
					*p = peek;
					*state = eStart;
					return tokCharCode;
				}
			}
			else if (c == '\\')
			{
				c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
				if (c == CHAR_NEED_MORE)
					return tokNeedMore;

				if (token_meta_char(parser,c,token))
				{
					parser->m_line_info.m_end_line = peek_line;
					parser->m_line_info.m_end_col = peek_col;
					*p = peek;
					*state = eStart;
					return tokCharCode;
				}
				else if (c == 'o')
				{
					c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
					if (c == CHAR_NEED_MORE)
						return tokNeedMore;

					if (c >= '0' && c <= '7')
					{
						meta = 0;
						*state = eOctalCharCode;
						goto octal_char_code;
					}
				}
				else if (c == 'x')
				{
					c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
					if (c == CHAR_NEED_MORE)
						return tokNeedMore;

					if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
					{
						meta = 0;
						*state = eHexCharCode;
						goto hex_char_code;
					}
				}
			}
			else if (c >= 32 && c <= CHAR_MAX_VALID)
			{
				token_append_unicode_char(parser,token,c);

				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokCharCode;
			}
		}
		else if (c == 'b')
		{
			c = token_get_char_conv(parser,&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c == '0' || c == '1')
			{
				*state = eBinaryInt;
				goto next_char;
			}
		}
		else if (c == 'o')
		{
			c = token_get_char_conv(parser,&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c >= '0' && c <= '7')
			{
				*state = eOctalInt;
				goto next_char;
			}
		}
		else if (c == 'x')
		{
			c = token_get_char_conv(parser,&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
			{
				*state = eHexInt;
				goto next_char;
			}
		}

		/* Reset the state and treat as integer */
		c = '0';
		*state = eInteger;
		goto append_chars;

	case eOctalCharCode:
		/* We know *p == "0'\o" */
		peek = *p + 4;
		peek_col = parser->m_line_info.m_end_col + 4;

		meta = 0;
		for (;;)
		{
			c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c == '\\')
			{
				token_append_unicode_char(parser,token,meta);

				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokCharCode;
			}

			if (c < '0' || c > '7')
			{
				/* Reset the state and treat as integer */
				c = '0';
				*state = eInteger;
				peek = *p + 1;
				peek_line = parser->m_line_info.m_end_line;
				peek_col = parser->m_line_info.m_end_col + 1;
				goto next_char;
			}

		octal_char_code:
			meta = (meta << 3) | (c - '0');
			if (meta > CHAR_MAX_VALID)
			{
				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				syntax_error(parser,AST_SYNTAX_ERR_INVALID_ESCAPE);
			}
		}

	case eHexCharCode:
		/* We know *p == "0'\x" */
		peek = *p + 4;
		peek_col = parser->m_line_info.m_end_col + 4;
		meta = 0;
		for (;;)
		{
			c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c == '\\')
			{
				token_append_unicode_char(parser,token,meta);

				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokCharCode;
			}

			if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')))
			{
				/* Reset the state and treat as integer */
				c = '0';
				*state = eInteger;
				peek = *p + 1;
				peek_line = parser->m_line_info.m_end_line;
				peek_col = parser->m_line_info.m_end_col + 1;
				goto next_char;
			}

		hex_char_code:
			meta = (meta << 4);
			if (c >= '0' && c <= '9')
				meta |= (c - '0');
			else if (c >= 'A' && c <= 'F')
				meta |= (c - 'A') + 10;
			else
				meta |= (c - 'a') + 10;

			if (meta > CHAR_MAX_VALID)
			{
				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				syntax_error(parser,AST_SYNTAX_ERR_INVALID_ESCAPE);
			}
		}

	case eDecimal:
		/* We know *p == '.' */
		++peek;
		++peek_col;

	decimal:
		c = token_get_char_conv(parser,&peek,pe,parser->m_eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c < '0' || c > '9')
		{
			*state = eStart;
			return tokInt;
		}

		token_append_char(parser,token,'.');
		*state = eFraction;
		goto next_char;

	case eExponent:
		/* We know *p == 'E' or 'e' */
		++peek;
		++peek_col;

	exponent:
		c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c >= '0' && c <= '9')
		{
			token_append_char(parser,token,'e');
			*state = eMantissa;
			goto next_char;
		}

		if (c == '-' || c == '+')
		{
			/* Check the next char */
			uint32_t c2 = token_get_char_conv(parser,&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c2 == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c2 >= '0' && c2 <= '9')
			{
				token_append_char(parser,token,'e');
				token_append_char(parser,token,(unsigned char)c);
				*state = eMantissa;
				c = c2;
				goto next_char;
			}
		}

		*state = eStart;
		return tokFloat;

	append_chars:
		peek = *p;
		peek_line = parser->m_line_info.m_end_line;
		peek_col = parser->m_line_info.m_end_col;
		token_append_unicode_char(parser,token,c);

	default:
		for (;;)
		{
			c = token_get_char_conv(parser,&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			switch (*state)
			{
			case eName:
				if (!((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_'))
				{
					*state = eStart;
					return tokName;
				}
				break;

			case eGraphicName:
			graphic_name:
				if (!strchr("#$&*+-./:<=>?@^~\\",c))
				{
					*state = eStart;
					return tokName;
				}
				break;

			case eVar:
				if (!((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_'))
				{
					*state = eStart;
					return tokVar;
				}
				break;

			case eBinaryInt:
				if (c < '0' || c > '1')
				{
					*state = eStart;
					return tokBinaryInt;
				}
				break;

			case eOctalInt:
				if (c < '0' || c > '7')
				{
					*state = eStart;
					return tokOctalInt;
				}
				break;

			case eHexInt:
				if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')))
				{
					*state = eStart;
					return tokHexInt;
				}
				break;

			case eInteger:
				if (c == '.')
				{
					*state = eDecimal;
					goto decimal;
				}

				if (c < '0' || c > '9')
				{
					*state = eStart;
					return tokInt;
				}
				break;

			case eFraction:
				if (c == 'e' || c == 'E')
				{
					*state = eExponent;
					goto exponent;
				}

				if (c < '0' || c > '9')
				{
					*state = eStart;
					return tokFloat;
				}
				break;

			case eMantissa:
				if (c < '0' || c > '9')
				{
					*state = eStart;
					return tokFloat;
				}
				break;

			default:
				/* Never going to happen */
				assert(0);
				break;
			}

		next_char:
			token_append_unicode_char(parser,token,c);
			parser->m_line_info.m_end_line = peek_line;
			parser->m_line_info.m_end_col = peek_col;
			*p = peek;
		}
	}
}

static token_type_t token_next(parser_t* parser, token_t* token)
{
	token_type_t tok;
	enum eState state = eStart;

	parser->m_line_info.m_start_line = parser->m_line_info.m_end_line;
	parser->m_line_info.m_start_col = parser->m_line_info.m_end_col;

	do
	{
		if (!parser->m_eof)
		{
			unsigned char c;
			prolite_stream_error_t err = prolite_stream_error_none;
			int64_t i = (*parser->m_s->m_fn_read)(parser->m_s,&c,1,&err);
			if (i <= 0)
			{
				switch (err)
				{
				case prolite_stream_error_eof:
					parser->m_eof = 1;
					break;

				default:
					// TODO: File I/O error?
					break;
				}
			}
			else
			{
				if (parser->m_buffer->m_alloc == parser->m_buffer->m_len)
				{
					size_t new_size = (parser->m_buffer->m_alloc == 0 ? 16 : parser->m_buffer->m_alloc * 2);
					unsigned char* new_str = allocator_realloc(parser->m_context->m_heap.m_allocator,parser->m_buffer->m_str,new_size);
					if (!new_str)
					{
						parser->m_buffer->m_str = allocator_free(parser->m_context->m_heap.m_allocator,parser->m_buffer->m_str);
						parser->m_buffer->m_len = parser->m_buffer->m_alloc = 0;
						syntax_error(parser,AST_ERR_OUTOFMEMORY);
					}

					parser->m_buffer->m_alloc = new_size;
					parser->m_buffer->m_str = new_str;
				}

				parser->m_buffer->m_str[parser->m_buffer->m_len++] = c;
			}
		}

		const unsigned char* p = parser->m_buffer->m_str;
		const unsigned char* pe = p + parser->m_buffer->m_len;

		tok = parse_token(parser,&state,&p,pe,token);

		if (p == pe)
			parser->m_buffer->m_len = 0;
		else if (p > parser->m_buffer->m_str)
		{
			memmove(parser->m_buffer->m_str,p,pe - p);
			parser->m_buffer->m_len = pe - p;
		}
	}
	while (tok == tokNeedMore);

	return tok;
}

static ast_node_t* parse_number(parser_t* parser, ast_node_t* node, token_type_t* next_type, token_t* next, int neg)
{
	if (!node)
	{
		node = heap_malloc(&parser->m_context->m_heap,sizeof(ast_node_t));
		if (!node)
			syntax_error(parser,AST_ERR_OUTOFMEMORY);

		*node = (ast_node_t){ .m_type = prolite_number	};
	}

	if (*next_type == tokFloat)
	{
		// Ensure we are NUL terminated
		token_append_char(parser,next,'\0');

		errno = 0;
		double v = strtod((const char*)next->m_str,NULL);
		if (v == HUGE_VAL)
			syntax_error(parser,AST_ERR_FLOAT_OVERFLOW);

		if (v == 0.0 && errno == ERANGE)
			syntax_error(parser,AST_ERR_FLOAT_UNDERFLOW);

		node->m_type = prolite_number;
		if (isnan(v))
			node->m_u64val = PACK_EXP_16(0x7FF8);
		else
			node->m_dval = v;
	}
	else if (*next_type == tokCharCode)
	{
		uint32_t v = next->m_str[0];
		if ((next->m_str[0] & 0xE0) == 0xC0)
			v = (next->m_str[0] & 0x1F);
		else if ((next->m_str[0] & 0xF0) == 0xE0)
			v = (next->m_str[0] & 0x0F);
		else
			v = (next->m_str[0] & 0x7);

		for (size_t i=1; i<next->m_len; ++i)
		{
			v = (v << 6) | (next->m_str[i] & 0x3F);
		}

		node->m_type = prolite_number;
		node->m_dval = v;
	}
	else
	{
		double v = 0;

		feclearexcept(FE_INEXACT);

		for (size_t i=0; i<next->m_len; ++i)
		{
			if (*next_type == tokInt)
				v = (v * 10) + (next->m_str[i] - '0');
			else if (*next_type == tokBinaryInt)
				v = (v * 2) + (next->m_str[i] - '0');
			else if (*next_type == tokOctalInt)
				v = (v * 8) + (next->m_str[i] - '0');
			else /* tokHexInt */
			{
				v *= 16;
				if (next->m_str[i] >= 'a')
					v += (next->m_str[i] - 'a');
				else if (next->m_str[i] >= 'A')
					v += (next->m_str[i] - 'A');
				else
					v += (next->m_str[i] - '0');
			}

			if (fetestexcept(FE_INEXACT))
			{
				feclearexcept(FE_INEXACT);
				syntax_error(parser,neg ? AST_ERR_MIN_INTEGER : AST_ERR_MAX_INTEGER);
			}
		}

		node->m_type = prolite_number;
		node->m_dval = v;
	}

	*next_type = token_next(parser,next);
	return node;
}

static ast_node_t* parse_negative(parser_t* parser, ast_node_t* node, token_type_t* next_type, token_t* next)
{
	node = parse_number(parser,node,next_type,next,1);

	node->m_dval = -node->m_dval;

	return node;
}

static ast_node_t* parse_term(parser_t* parser, unsigned int max_prec, token_type_t* next_type, token_t* next);

static ast_node_t* parse_arg(parser_t* parser, token_type_t* next_type, token_t* next)
{
	if (*next_type == tokName)
	{
		const operator_t* op = lookup_op(parser->m_operators,next->m_str,next->m_len);
		if (op && op->m_precedence > 999)
		{
			ast_node_t* node = heap_malloc(&parser->m_context->m_heap,sizeof(ast_node_t));
			if (!node)
				syntax_error(parser,AST_ERR_OUTOFMEMORY);

			*node = (ast_node_t){
				.m_type = prolite_atom,
				.m_str = next->m_str,
				.m_str_len = next->m_len
			};
			token_reset(next);

			if (node->m_str_len == 1 && node->m_str[0] == '-')
			{
				if (*next_type >= tokInt && *next_type <= tokFloat)
					return parse_negative(parser,node,next_type,next);
			}

			return node;
		}
	}

	ast_node_t* node = parse_term(parser,999,next_type,next);
	if (!node)
		syntax_error(parser,AST_SYNTAX_ERR_UNEXPECTED_EOF);

	return node;
}

static ast_node_t* parse_compound_term(parser_t* parser, ast_node_t* node, token_type_t* next_type, token_t* next)
{
	size_t alloc_arity = 4;
	node = heap_realloc(&parser->m_context->m_heap,node,sizeof(ast_node_t),sizeof(ast_node_t) + (alloc_arity * sizeof(ast_node_t*)));
	if (!node)
		syntax_error(parser,AST_ERR_OUTOFMEMORY);

	node->m_type = prolite_compound;

	do
	{
		if (node->m_arity == MAX_ARITY)
			syntax_error(parser,AST_ERR_MAX_ARITY);

		*next_type = token_next(parser,next);

		if (node->m_arity == alloc_arity)
		{
			size_t new_arity = alloc_arity * 2;
			ast_node_t* new_node = heap_realloc(&parser->m_context->m_heap,node,sizeof(ast_node_t) + (alloc_arity * sizeof(ast_node_t*)),sizeof(ast_node_t) + (new_arity * sizeof(ast_node_t*)));
			if (!new_node)
				syntax_error(parser,AST_ERR_OUTOFMEMORY);

			alloc_arity = new_arity;
			node = new_node;
		}

		node->m_params[node->m_arity++] = parse_arg(parser,next_type,next);
	}
	while (*next_type == tokComma);

	if (*next_type != tokClose)
		syntax_error(parser,AST_SYNTAX_ERR_MISSING_CLOSE);

	*next_type = token_next(parser,next);
	return node;
}

static ast_node_t* parse_list_term(parser_t* parser, token_type_t* next_type, token_t* next)
{
	ast_node_t* node = NULL;
	ast_node_t** tail = &node;

	if (*next_type != tokCloseL)
	{
		for (;;)
		{
			*tail = heap_malloc(&parser->m_context->m_heap,sizeof(ast_node_t) + (2*sizeof(ast_node_t*)));
			if (!(*tail))
				syntax_error(parser,AST_ERR_OUTOFMEMORY);

			**tail = (ast_node_t){
				.m_type = prolite_compound,
				.m_str = (const unsigned char*)".",
				.m_str_len = 1,
				.m_arity = 2
			};

			(*tail)->m_params[0] = parse_arg(parser,next_type,next);
			tail = &((*tail)->m_params[1]);

			if (*next_type != tokComma)
				break;

			*next_type = token_next(parser,next);
		}
	}

	if (*next_type == tokBar)
	{
		*next_type = token_next(parser,next);

		*tail = parse_arg(parser,next_type,next);
	}
	else if (*next_type == tokCloseL)
	{
		(*tail) = heap_malloc(&parser->m_context->m_heap,sizeof(ast_node_t));
		if (!(*tail))
			syntax_error(parser,AST_ERR_OUTOFMEMORY);

		**tail = (ast_node_t){
			.m_type = prolite_atom,
			.m_str = (const unsigned char*)"[]",
			.m_str_len = 2
		};
	}

	if (*next_type != tokCloseL)
		syntax_error(parser,AST_SYNTAX_ERR_MISSING_CLOSE_L);

	*next_type = token_next(parser,next);
	return node;
}

static ast_node_t* parse_name(parser_t* parser, unsigned int* max_prec, token_type_t* next_type, token_t* next)
{
	ast_node_t* node;

	const operator_t* op = lookup_prefix_op(parser->m_operators,next->m_str,next->m_len);
	if (op && op->m_precedence <= *max_prec && (op->m_specifier == eFX || op->m_specifier == eFY))
		node = heap_malloc(&parser->m_context->m_heap,sizeof(ast_node_t) + sizeof(ast_node_t*));
	else
	{
		op = NULL;
		node = heap_malloc(&parser->m_context->m_heap,sizeof(ast_node_t));
	}

	if (!node)
		syntax_error(parser,AST_ERR_OUTOFMEMORY);

	*node = (ast_node_t){
		.m_type = prolite_atom,
		.m_str = next->m_str,
		.m_str_len = next->m_len
	};
	token_reset(next);

	*max_prec = 0;

	*next_type = token_next(parser,next);
	if (*next_type == tokOpenCt)
		return parse_compound_term(parser,node,next_type,next);

	if (node->m_str_len == 1 && node->m_str[0] == '-' && *next_type >= tokInt && *next_type <= tokFloat)
		return parse_negative(parser,node,next_type,next);

	if (op && *next_type != tokClose && *next_type != tokComma)
	{
		node->m_type = prolite_compound;
		node->m_arity = 1;
		node->m_params[0] = parse_term(parser,op->m_specifier == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next);
		if (!node->m_params[0])
			syntax_error(parser,AST_SYNTAX_ERR_UNEXPECTED_EOF);

		*max_prec = op->m_precedence;
	}

	return node;
}

static ast_node_t* parse_chars_and_codes(parser_t* parser, int chars, token_t* token)
{
	/* TODO: Check for utf8 chars token and split into multiple lists */

	ast_node_t* node = heap_malloc(&parser->m_context->m_heap,sizeof(ast_node_t));
	if (!node)
		syntax_error(parser,AST_ERR_OUTOFMEMORY);

	*node = (ast_node_t){
		.m_type = chars ? prolite_chars : prolite_charcodes,
		.m_str = token->m_str,
		.m_str_len = token->m_len
	};
	token_reset(token);

	return node;
}

static ast_node_t* parse_term_base(parser_t* parser, unsigned int* max_prec, token_type_t* next_type, token_t* next)
{
	ast_node_t* node = NULL;

	switch (*next_type)
	{
	case tokName:
		return parse_name(parser,max_prec,next_type,next);

	case tokVar:
		node = heap_malloc(&parser->m_context->m_heap,sizeof(ast_node_t));
		if (!node)
			syntax_error(parser,AST_ERR_OUTOFMEMORY);

		*node = (ast_node_t){
			.m_type = prolite_var,
			.m_arity = UINT64_C(-1),
			.m_str = next->m_str,
			.m_str_len = next->m_len
		};
		token_reset(next);
		break;

	case tokInt:
	case tokBinaryInt:
	case tokOctalInt:
	case tokHexInt:
	case tokCharCode:
	case tokFloat:
		*max_prec = 0;
		return parse_number(parser,node,next_type,next,0);

	case tokDQL:
		if (parser->m_flags->double_quotes == 2 /* atom */)
		{
			/* ISO/IEC 13211-1:1995/Cor.1:2007 */
			return parse_name(parser,max_prec,next_type,next);
		}

		node = parse_chars_and_codes(parser,parser->m_flags->double_quotes,next);
		break;

	case tokBackQuote:
		if (parser->m_flags->back_quotes == 2 /* atom */)
			return parse_name(parser,max_prec,next_type,next);

		node = parse_chars_and_codes(parser,parser->m_flags->back_quotes,next);
		break;

	case tokOpen:
	case tokOpenCt:
		*next_type = token_next(parser,next);
		node = parse_term(parser,1201,next_type,next);
		if (!node)
			syntax_error(parser,AST_SYNTAX_ERR_UNEXPECTED_EOF);
		if (*next_type != tokClose)
			syntax_error(parser,AST_SYNTAX_ERR_MISSING_CLOSE);
		break;

	case tokOpenL:
		*next_type = token_next(parser,next);
		*max_prec = 0;
		return parse_list_term(parser,next_type,next);

	case tokOpenC:
		node = heap_malloc(&parser->m_context->m_heap,sizeof(ast_node_t) + sizeof(ast_node_t*));
		if (!node)
			syntax_error(parser,AST_ERR_OUTOFMEMORY);

		*node = (ast_node_t){
			.m_type = prolite_compound,
			.m_str = (const unsigned char*)"{}",
			.m_str_len = 2,
			.m_arity = 1
		};
		*next_type = token_next(parser,next);

		node->m_params[0] = parse_term(parser,1201,next_type,next);
		if (!node->m_params[0])
			syntax_error(parser,AST_SYNTAX_ERR_UNEXPECTED_EOF);

		if (*next_type != tokCloseC)
			syntax_error(parser,AST_SYNTAX_ERR_MISSING_CLOSE_C);
		break;

	case tokNeedMore:
		/* Shouldn't happen... */
		assert(0);

	case tokComma:
	case tokClose:
	case tokCloseL:
	case tokCloseC:
	case tokBar:
	case tokEnd:
		syntax_error(parser,AST_SYNTAX_ERR_UNEXPECTED_TOKEN);

	case tokMissingSQ:
		syntax_error(parser,AST_SYNTAX_ERR_MISSING_SQ);

	case tokMissingDQ:
		syntax_error(parser,AST_SYNTAX_ERR_MISSING_DQ);

	case tokMissingBQ:
		syntax_error(parser,AST_SYNTAX_ERR_MISSING_BQ);

	case tokEOF:
		return NULL;
	}

	*max_prec = 0;
	*next_type = token_next(parser,next);
	return node;
}

static ast_node_t* parse_term(parser_t* parser, unsigned int max_prec, token_type_t* next_type, token_t* next)
{
	unsigned int prev_prec = max_prec;
	ast_node_t* node = parse_term_base(parser,&prev_prec,next_type,next);
	if (!node)
		return NULL;

	/* This is precedence climbing, if you're interested */
	for (;;)
	{
		unsigned int right_prec = 0;
		unsigned int left_prec = 0;
		const unsigned char* name = next->m_str;
		size_t name_len = next->m_len;

		if (*next_type == tokName)
		{
			const operator_t* op = lookup_op(parser->m_operators,name,name_len);
			if (!op || op->m_precedence > max_prec)
				break;

			switch (op->m_specifier)
			{
			case eFX:
			case eFY:
				return node;

			case eXFX:
				left_prec = right_prec = op->m_precedence - 1;
				break;

			case eXFY:
				left_prec = op->m_precedence - 1;
				right_prec = op->m_precedence;
				break;

			case eYFX:
				left_prec = op->m_precedence;
				right_prec = op->m_precedence - 1;
				break;

			case eXF:
				left_prec = op->m_precedence - 1;
				break;

			case eYF:
				left_prec = op->m_precedence;
				break;
			}
		}
		else if (*next_type == tokComma)
		{
			if (1000 > max_prec)
				break;

			left_prec = 999;
			right_prec = 1000;

			name = (const unsigned char*)",";
			name_len = 1;
		}
		else if (*next_type == tokBar)
		{
			/* ISO/IEC 13211-1:1995/Cor.2:2012 */
			name = (const unsigned char*)"|";
			name_len = 1;

			const operator_t* op = lookup_op(parser->m_operators,name,name_len);
			if (!op || op->m_precedence > max_prec)
				break;

			switch (op->m_specifier)
			{
			case eFX:
			case eFY:
			case eXF:
			case eYF:
				return node;

			case eXFX:
				left_prec = right_prec = op->m_precedence - 1;
				break;

			case eXFY:
				left_prec = op->m_precedence - 1;
				right_prec = op->m_precedence;
				break;

			case eYFX:
				left_prec = op->m_precedence;
				right_prec = op->m_precedence - 1;
				break;
			}
		}
		else
			break;

		if (prev_prec > left_prec)
			break;
		else
		{
			size_t arity = (left_prec != 0 && right_prec != 0) ? 2 : 1;

			ast_node_t* next_node = heap_malloc(&parser->m_context->m_heap,sizeof(ast_node_t) + (arity * sizeof(ast_node_t*)));
			if (!next_node)
				syntax_error(parser,AST_ERR_OUTOFMEMORY);

			*next_node = (ast_node_t){
				.m_type = prolite_compound,
				.m_str = name,
				.m_str_len = name_len,
				.m_arity = arity,
			};
			next_node->m_params[0] = node;

			if (*next_type == tokName)
				token_reset(next);

			*next_type = token_next(parser,next);

			if (arity == 2)
			{
				next_node->m_params[1] = parse_term(parser,right_prec,next_type,next);
				if (!next_node->m_params[1])
					syntax_error(parser,AST_SYNTAX_ERR_UNEXPECTED_EOF);
			}

			node = next_node;
		}
	}

	return node;
}

static const term_t* emit_ast_node(parser_t* parser, emit_buffer_t* out, ast_node_t* node)
{
	switch (node->m_type)
	{
	case prolite_compound:
		if (!emit_predicate(out,node->m_arity,node->m_str,node->m_str_len,0,node->m_debug_info))
			syntax_error(parser,AST_ERR_OUTOFMEMORY);

		for (size_t i = 0; i < node->m_arity; ++i)
			emit_ast_node(parser,out,node->m_params[i]);
		break;

	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		if (!emit_string(out,node->m_type,node->m_str,node->m_str_len,0,node->m_debug_info))
			syntax_error(parser,AST_ERR_OUTOFMEMORY);
		break;

	case prolite_var:
		if (!emit_var(out,node->m_arity,node->m_debug_info))
			syntax_error(parser,AST_ERR_OUTOFMEMORY);
		break;

	case prolite_number:
	default:
		if (!emit_number(out,node->m_dval,node->m_debug_info))
			syntax_error(parser,AST_ERR_OUTOFMEMORY);
		break;
	}

	return out->m_buf;
}

static const term_t* emit_error_line_info(parser_t* parser, emit_buffer_t* out)
{
	// TODO: Line info
	term_t* r = emit_buffer_append(out,1);
	if (r)
		r->m_u64val = PACK_ATOM_EMBED_4('t','o','d','o');

	return r;
}

static const term_t* emit_syntax_error_missing(parser_t* parser, emit_buffer_t* out, uint64_t missing_atom)
{
	term_t* r = emit_buffer_append(out,4);
	if (!r)
		return NULL;

	r[0].m_u64val = PACK_COMPOUND_EMBED_5(2,'e','r','r','o','r');
	r[1].m_u64val = PACK_COMPOUND_BUILTIN(syntax_error,1);
	r[2].m_u64val = PACK_COMPOUND_BUILTIN(missing,1);
	r[3].m_u64val = missing_atom;

	if (!emit_error_line_info(parser,out))
		return NULL;

	return out->m_buf;
}

static const term_t* emit_simple_error(parser_t* parser, emit_buffer_t* out, uint64_t f, uint64_t arg)
{
	term_t* r = emit_buffer_append(out,3);
	if (!r)
		return NULL;

	r[0].m_u64val = PACK_COMPOUND_EMBED_5(2,'e','r','r','o','r');
	r[1].m_u64val = f;
	r[2].m_u64val = arg;

	if (!emit_error_line_info(parser,out))
		return NULL;

	return out->m_buf;
}

static const term_t* emit_ast_error(parser_t* parser, emit_buffer_t* out, ast_error_t ast_err)
{
	switch (ast_err)
	{
	case AST_ERR_NONE:
	default:
		assert(0);
		return NULL;

	case AST_ERR_OUTOFMEMORY:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(resource_error,1),PACK_ATOM_EMBED_4('h','e','a','p'));

	case AST_ERR_FLOAT_OVERFLOW:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(evaluation_error,1),PACK_ATOM_BUILTIN(float_overflow));

	case AST_ERR_FLOAT_UNDERFLOW:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(evaluation_error,1),PACK_ATOM_BUILTIN(underflow));

	case AST_ERR_MAX_INTEGER:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(representation_error,1),PACK_ATOM_BUILTIN(max_integer));

	case AST_ERR_MIN_INTEGER:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(representation_error,1),PACK_ATOM_BUILTIN(min_integer));

	case AST_ERR_MAX_ARITY:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(representation_error,1),PACK_ATOM_BUILTIN(max_arity));

	case AST_SYNTAX_ERR_MISSING_DOT:
		return emit_syntax_error_missing(parser,out,PACK_ATOM_EMBED_1('.'));

	case AST_SYNTAX_ERR_MISSING_CLOSE:
		return emit_syntax_error_missing(parser,out,PACK_ATOM_EMBED_1(')'));

	case AST_SYNTAX_ERR_MISSING_CLOSE_L:
		return emit_syntax_error_missing(parser,out,PACK_ATOM_EMBED_1(']'));

	case AST_SYNTAX_ERR_MISSING_CLOSE_C:
		return emit_syntax_error_missing(parser,out,PACK_ATOM_EMBED_1('}'));

	case AST_SYNTAX_ERR_MISSING_SQ:
		return emit_syntax_error_missing(parser,out,PACK_ATOM_EMBED_1('\''));

	case AST_SYNTAX_ERR_MISSING_DQ:
		return emit_syntax_error_missing(parser,out,PACK_ATOM_EMBED_1('"'));

	case AST_SYNTAX_ERR_MISSING_BQ:
		return emit_syntax_error_missing(parser,out,PACK_ATOM_EMBED_1('`'));

	case AST_SYNTAX_ERR_INVALID_ARG:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(invalid_argument));

	case AST_SYNTAX_ERR_UNEXPECTED_TOKEN:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(unexpected_token));

	case AST_SYNTAX_ERR_UNEXPECTED_EOF:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(past_end_of_stream));

	case AST_SYNTAX_ERR_INVALID_CHAR:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(invalid_character));

	case AST_SYNTAX_ERR_INVALID_ESCAPE:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(invalid_escape));

	case AST_SYNTAX_ERR_INVALID_UTF8:
		return emit_simple_error(parser,out,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(invalid_utf8));
	}
}

static void collate_var_info(parser_t* parser, var_info_t** varinfo, size_t* var_count, ast_node_t* node)
{
	if (node->m_type == prolite_var)
	{
		size_t i = *var_count;
		if (node->m_str_len != 1 || node->m_str[0] != '_')
		{
			for (i = 0; i < *var_count; ++i)
			{
				if (node->m_str_len == (*varinfo)[i].m_name_len && memcmp(node->m_str,(*varinfo)[i].m_name,node->m_str_len) == 0)
				{
					++(*varinfo)[i].m_use_count;
					break;
				}
			}
		}

		if (i == *var_count)
		{
			var_info_t* new_varinfo;

			// Check for variable index overflow
			if (i == MAX_VAR_INDEX)
				syntax_error(parser,AST_ERR_OUTOFMEMORY);

			new_varinfo = heap_realloc(&parser->m_context->m_trail,*varinfo,sizeof(var_info_t) * (*var_count),sizeof(var_info_t) * ((*var_count)+1));
			if (!new_varinfo)
				syntax_error(parser,AST_ERR_OUTOFMEMORY);

			*varinfo = new_varinfo;
			new_varinfo = *varinfo + (*var_count)++;

			*new_varinfo = (var_info_t){
				.m_name = node->m_str,
				.m_name_len = node->m_str_len,
				.m_use_count = 1,
				.m_debug_info = node->m_debug_info
			};
		}

		node->m_arity = i;
	}
	else if (node->m_type == prolite_compound)
	{
		for (size_t i = 0; i < node->m_arity; ++i)
			collate_var_info(parser,varinfo,var_count,node->m_params[i]);
	}
}

void read_term(parser_t* parser, void* param, pfn_parse_t callback, int multiterm)
{
	size_t heap_start = heap_top(&parser->m_context->m_heap);

	ast_node_t* node = NULL;
	ast_error_t ast_err = setjmp(parser->m_jmp);
	if (!ast_err)
	{
		token_t next = {0};
		token_type_t next_type = token_next(parser,&next);
		if (!(node = parse_term(parser,1201,&next_type,&next)))
			return;

		if (next_type != tokEnd)
			syntax_error(parser,AST_SYNTAX_ERR_MISSING_DOT);
	}
	else if (multiterm)
	{
		// Skip to '.'
		token_t next = {0};
		token_type_t next_type = tokNeedMore;
		do
		{
			heap_reset(&parser->m_context->m_heap,heap_start);

			if (!setjmp(parser->m_jmp))
				next_type = token_next(parser,&next);

			token_reset(&next);
		}
		while (next_type != tokEnd && next_type != tokEOF);
	}

	size_t trail_start = heap_top(&parser->m_context->m_trail);

	if (!ast_err)
	{
		ast_err = setjmp(parser->m_jmp);
		if (!ast_err)
		{
			var_info_t* varinfo = NULL;
			size_t var_count = 0;
			collate_var_info(parser,&varinfo,&var_count,node);

			const term_t* t = emit_ast_node(parser,&(emit_buffer_t){ .m_a = &bump_allocator(&parser->m_context->m_trail) },node);

			heap_reset(&parser->m_context->m_heap,heap_start);

			(*callback)(parser->m_context,param,t,var_count,varinfo);
		}
	}

	if (ast_err)
	{
		heap_reset(&parser->m_context->m_heap,heap_start);
		heap_reset(&parser->m_context->m_trail,trail_start);

		parser->m_context->m_flags |= FLAG_THROW;

		const term_t* t = emit_ast_error(parser,&(emit_buffer_t){ .m_a = &bump_allocator(&parser->m_context->m_trail) },ast_err);

		(*callback)(parser->m_context,param,t,0,NULL);
	}

	heap_reset(&parser->m_context->m_trail,trail_start);
}

/*void read_term_todo(context_t* context, prolite_stream_t* s)
{
	parser_t parser = {
		.m_context = context,
		.m_flags = &context->m_module->m_flags,
		.m_s = s
	};

	const term_t* ret = read_term(&parser);
	if (!ret && !(context->m_flags & FLAG_THROW))
		emit_eof_error(&parser);
}
*/