#include "parser.h"

#include <string.h>
#include <errno.h>
#include <math.h>
#include <assert.h>
#include <stdlib.h>

#ifdef _MSC_VER
#include <float.h>
#define isnan _isnan
#endif

uint32_t convert_char(context_t* context, uint32_t in_char);

/* Try to find a infix/suffix op, otherwise find prefix */
operator_t* lookup_op(context_t* context, const unsigned char* name, size_t name_len);

/* Try to find a prefix op, otherwise find infix/suffix */
operator_t* lookup_prefix_op(context_t* context, const unsigned char* name, size_t name_len);

typedef enum token_type
{
	tokNeedMore = 0,
	tokEOF,
	tokInvalidSeq,
	tokInvalidEscape,
	tokInvalidChar,
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

enum ast_type
{
	AST_TYPE_VAR,
	AST_TYPE_COMPOUND,
	AST_TYPE_DOUBLE,
	AST_TYPE_INTEGER,
	AST_TYPE_ATOM,
	AST_TYPE_CHARS,
	AST_TYPE_CODES,
};

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
	enum ast_type    m_type;
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
	AST_SYNTAX_ERR_MISSING_CLOSE,
	AST_SYNTAX_ERR_MISSING_CLOSE_L,
	AST_SYNTAX_ERR_MISSING_CLOSE_C,
	AST_SYNTAX_ERR_MISSING_SQ,
	AST_SYNTAX_ERR_MISSING_DQ,
	AST_SYNTAX_ERR_MISSING_BQ,
	AST_SYNTAX_ERR_INVALID_ARG,
	AST_SYNTAX_ERR_UNEXPECTED_TOKEN,
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

static void token_append_char(context_t* context, parser_t* parser, token_t* token, unsigned char c)
{
	if (token->m_alloc == token->m_len)
	{
		size_t new_size = (token->m_alloc == 0 ? 16 : token->m_alloc * 2);
		unsigned char* new_str = heap_realloc(context->m_heap,token->m_str,token->m_alloc,new_size);
		if (!new_str)
			longjmp(parser->m_jmp,1);

		token->m_alloc = new_size;
		token->m_str = new_str;
	}

	token->m_str[token->m_len++] = c;
}

static void token_append_unicode_char(context_t* context, parser_t* parser, token_t* token, uint32_t unicode_char)
{
	if (unicode_char <= 0x7F)
		token_append_char(context,parser,token,(unsigned char)unicode_char);
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
			token_append_char(context,parser,token,chars[i]);
	}
}

static uint32_t token_skip_ilseq(const unsigned char** p, const unsigned char* pe, int eof, size_t* col, unsigned int i)
{
	for (;;)
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
		*col = 0;
	}
	else
		++(*col);

	*p += count;
	return val;
}

static uint32_t token_get_char_conv(context_t* context, const unsigned char** p, const unsigned char* pe, int eof, size_t* line, size_t* col)
{
	uint32_t c = token_get_char(p,pe,eof,line,col);
	if (context->m_module->m_flags.char_conversion && c <= CHAR_MAX_VALID)
		c = convert_char(context,c);
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

static const enum eAction actions[128] =
{
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

static int token_meta_char(context_t* context, parser_t* parser, uint32_t meta, token_t* token)
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
		c = 7;
		break;

	case 'b':
		c = 127;
		break;

	case 'f':
		c = 12;
		break;

	case 'n':
		c = 10;
		break;

	case 'r':
		c = 13;
		break;

	case 't':
		c = 9;
		break;

	case 'v':
		c = 11;
		break;

	default:
		return 0;
	}

	token_append_char(context,parser,token,c);
	return 1;
}

static token_type_t parse_token(context_t* context, parser_t* parser, enum eState* state, const unsigned char** p, const unsigned char* pe, token_t* token)
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

		c = token_get_char_conv(context,p,pe,parser->m_eof,&parser->m_line_info.m_end_line,&parser->m_line_info.m_end_col);
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
		c = token_get_char_conv(context,p,pe,parser->m_eof,&parser->m_line_info.m_end_line,&parser->m_line_info.m_end_col);
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
				{
					*state = eStart;
					return tokInvalidSeq;
				}

				if (c > 0x7f)
				{
					*state = eStart;
					return tokInvalidChar;
				}

				switch (actions[c & 0x7F])
				{
				case eaWhitespace:
					parser->m_line_info.m_start_line = parser->m_line_info.m_end_line;
					parser->m_line_info.m_start_col = parser->m_line_info.m_end_col;
					break;

				case eaShortName:
					*state = eStart;
					token_append_char(context,parser,token,(unsigned char)c);
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
					*state = eStart;
					return tokInvalidChar;
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
			c = token_get_char_conv(context,p,pe,parser->m_eof,&parser->m_line_info.m_end_line,&parser->m_line_info.m_end_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;
		}

	multi_comment:
		peek = *p;
		peek_line = parser->m_line_info.m_end_line;
		peek_col = parser->m_line_info.m_end_col;

	case eMultiComment1:
		c = token_get_char_conv(context,&peek,pe,parser->m_eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c != '*')
		{
			token_append_char(context,parser,token,'/');
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
		c = token_get_char_conv(context,&peek,pe,parser->m_eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c == CHAR_EOF)
			return tokEnd;

		if (c == ' ' || c == '\n' || c == '\t')
		{
			*state = eLayout;
			return tokEnd;
		}

		if (c == '%')
		{
			*state = eSingleComment;
			return tokEnd;
		}

		token_append_char(context,parser,token,'.');
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
				*state = eStart;
				if (*state == eSingleQuote)
					return tokMissingSQ;
				else if (*state == eDoubleQuote)
					return tokMissingDQ;
				return tokMissingBQ;

			case CHAR_ILLEGAL_SEQ:
				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidSeq;

			case '\\':
				c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
				if (c == CHAR_NEED_MORE)
					return tokNeedMore;

				if (!token_meta_char(context,parser,c,token))
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
						*p = peek;
						*state = eStart;
						if (c == CHAR_ILLEGAL_SEQ)
							return tokInvalidSeq;

						return tokInvalidEscape;
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
						return tokName;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				token_append_char(context,parser,token,(unsigned char)c);
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
						return tokDQL;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				token_append_char(context,parser,token,(unsigned char)c);
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
						return tokBackQuote;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				token_append_char(context,parser,token,(unsigned char)c);
				break;

			default:
				if (c < 32)
				{
					parser->m_line_info.m_end_line = peek_line;
					parser->m_line_info.m_end_col = peek_col;
					*p = peek;
					*state = eStart;
					return tokInvalidChar;
				}
				token_append_unicode_char(context,parser,token,c);
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
					token_append_unicode_char(context,parser,token,meta);

					if (*state == eSingleQuoteOct)
						*state = eSingleQuote;
					else if (*state == eDoubleQuoteOct)
						*state = eDoubleQuote;
					else
						*state = eBackQuote;
					goto quote;
				}

				*state = eStart;
				if (c == CHAR_ILLEGAL_SEQ)
					return tokInvalidSeq;

				return tokInvalidEscape;
			}

		octal_char:
			meta = (meta << 3) | (c - '0');
			if (meta > CHAR_MAX_VALID)
			{
				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidEscape;
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
					token_append_unicode_char(context,parser,token,meta);

					if (*state == eSingleQuoteHex)
						*state = eSingleQuote;
					else if (*state == eDoubleQuoteHex)
						*state = eDoubleQuote;
					else
						*state = eBackQuote;
					goto quote;
				}

				*state = eStart;
				if (c == CHAR_ILLEGAL_SEQ)
					return tokInvalidSeq;

				return tokInvalidEscape;
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
				*p = peek;
				*state = eStart;
				return tokInvalidEscape;
			}
		}

	case eZero:
	zero:
		c = token_get_char_conv(context,&peek,pe,parser->m_eof,&peek_line,&peek_col);
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
					token_append_char(context,parser,token,'\'');
					parser->m_line_info.m_end_line = peek_line;
					parser->m_line_info.m_end_col = peek_col;
					*p = peek;
					return tokCharCode;
				}
			}
			else if (c == '\\')
			{
				c = token_get_char(&peek,pe,parser->m_eof,&peek_line,&peek_col);
				if (c == CHAR_NEED_MORE)
					return tokNeedMore;

				if (token_meta_char(context,parser,c,token))
				{
					parser->m_line_info.m_end_line = peek_line;
					parser->m_line_info.m_end_col = peek_col;
					*p = peek;
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
				token_append_unicode_char(context,parser,token,c);

				parser->m_line_info.m_end_line = peek_line;
				parser->m_line_info.m_end_col = peek_col;
				*p = peek;
				return tokCharCode;
			}
		}
		else if (c == 'b')
		{
			c = token_get_char_conv(context,&peek,pe,parser->m_eof,&peek_line,&peek_col);
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
			c = token_get_char_conv(context,&peek,pe,parser->m_eof,&peek_line,&peek_col);
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
			c = token_get_char_conv(context,&peek,pe,parser->m_eof,&peek_line,&peek_col);
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
				token_append_unicode_char(context,parser,token,meta);

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
				*p = peek;
				*state = eStart;
				return tokInvalidEscape;
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
				token_append_unicode_char(context,parser,token,meta);

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
				*p = peek;
				*state = eStart;
				return tokInvalidEscape;
			}
		}

	case eDecimal:
		/* We know *p == '.' */
		++peek;
		++peek_col;

	decimal:
		c = token_get_char_conv(context,&peek,pe,parser->m_eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c < '0' || c > '9')
		{
			*state = eStart;
			return tokInt;
		}

		token_append_char(context,parser,token,'.');
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
			token_append_char(context,parser,token,'e');
			*state = eMantissa;
			goto next_char;
		}

		if (c == '-' || c == '+')
		{
			/* Check the next char */
			uint32_t c2 = token_get_char_conv(context,&peek,pe,parser->m_eof,&peek_line,&peek_col);
			if (c2 == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c2 >= '0' && c2 <= '9')
			{
				token_append_char(context,parser,token,'e');
				token_append_char(context,parser,token,(unsigned char)c);
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
		token_append_unicode_char(context,parser,token,c);

	default:
		for (;;)
		{
			c = token_get_char_conv(context,&peek,pe,parser->m_eof,&peek_line,&peek_col);
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
			token_append_unicode_char(context,parser,token,c);
			parser->m_line_info.m_end_line = peek_line;
			parser->m_line_info.m_end_col = peek_col;
			*p = peek;
		}
	}
}

static token_type_t token_next(context_t* context, parser_t* parser, token_t* token)
{
	token_type_t tok;
	enum eState state = eStart;

	parser->m_line_info.m_start_line = parser->m_line_info.m_end_line;
	parser->m_line_info.m_start_col = parser->m_line_info.m_end_col;

	do
	{
		const unsigned char* start;
		const unsigned char* p;
		const unsigned char* pe;

		if (!parser->m_eof)
		{
			unsigned char c;
			int64_t i = stream_read(parser->m_s,&c,1);
			if (i == 0)
				parser->m_eof = 1;
			else
				token_append_char(context,parser,&parser->m_buffer,c);
		}

		p = start = parser->m_buffer.m_str;
		pe = start + parser->m_buffer.m_len;

		tok = parse_token(context,parser,&state,&p,pe,token);

		if (p == pe)
			parser->m_buffer.m_len = 0;
		else if (p > start)
		{
			memmove(parser->m_buffer.m_str,parser->m_buffer.m_str + (p - start),pe - p);
			parser->m_buffer.m_len = pe - p;
		}
	}
	while (tok == tokNeedMore);

	return tok;
}

static ast_node_t* syntax_error(ast_error_t error, ast_error_t* ast_err)
{
	*ast_err = error;
	return NULL;
}

static ast_node_t* atom_to_compound(context_t* context, parser_t* parser, ast_node_t* node, ast_error_t* ast_err)
{
	ast_node_t* new_node = heap_realloc(context->m_heap,node,sizeof(ast_node_t),sizeof(ast_node_t) + sizeof(ast_node_t*));
	if (!new_node)
		longjmp(parser->m_jmp,1);

	new_node->m_type = AST_TYPE_COMPOUND;
	new_node->m_arity = 0;

	return new_node;
}

static ast_node_t* parse_number(context_t* context, parser_t* parser, ast_node_t* node, token_type_t* next_type, token_t* next, ast_error_t* ast_err, int neg)
{
	if (!node)
	{
		node = heap_malloc(context->m_heap,sizeof(ast_node_t));
		if (!node)
			longjmp(parser->m_jmp,1);

		node->m_type = AST_TYPE_DOUBLE;
		node->m_arity = 0;
	}

	if (*next_type == tokFloat)
	{
		double dval;
		errno = 0;
		dval = strtod((const char*)next->m_str,NULL);
		if (dval == HUGE_VAL)
			return syntax_error(AST_ERR_FLOAT_OVERFLOW,ast_err);

		if (dval == 0.0 && errno == ERANGE)
			return syntax_error(AST_ERR_FLOAT_UNDERFLOW,ast_err);

		node->m_type = AST_TYPE_DOUBLE;
		if (isnan(dval))
			node->m_u64val = PACK_EXP_16(0x7FF8);
		else
			node->m_dval = dval;
	}
	else if (*next_type == tokCharCode)
	{
		size_t i;
		uint32_t v = next->m_str[0];

		if ((next->m_str[0] & 0xE0) == 0xC0)
			v = (next->m_str[0] & 0x1F);
		else if ((next->m_str[0] & 0xF0) == 0xE0)
			v = (next->m_str[0] & 0x0F);
		else
			v = (next->m_str[0] & 0x7);

		for (i=1;i<next->m_len;++i)
		{
			v = (v << 6) | (next->m_str[i] & 0x3F);
		}

		node->m_type = AST_TYPE_INTEGER;
		node->m_u64val = v;
	}
	else
	{
		uint64_t v = 0;
		size_t i;
		for (i=0;i<next->m_len;++i)
		{
			if (*next_type == tokInt)
				v = (v * 10) + (next->m_str[i] - '0');
			else if (*next_type == tokBinaryInt)
				v = (v << 1) | (next->m_str[i] - '0');
			else if (*next_type == tokOctalInt)
				v = (v << 3) | (next->m_str[i] - '0');
			else /* tokHexInt */
			{
				v <<= 4;
				if (next->m_str[i] >= 'a')
					v |= (next->m_str[i] - 'a');
				else if (next->m_str[i] >= 'A')
					v |= (next->m_str[i] - 'A');
				else
					v |= (next->m_str[i] - '0');
			}

			if (v > (UINT64_C(0x7FFFFFFFFFFF) + neg))
				return syntax_error(neg ? AST_ERR_MIN_INTEGER : AST_ERR_MAX_INTEGER,ast_err);
		}

		node->m_type = AST_TYPE_INTEGER;
		node->m_u64val = v;
	}

	*next_type = token_next(context,parser,next);
	return node;
}

static ast_node_t* parse_negative(context_t* context, parser_t* parser, ast_node_t* node, token_type_t* next_type, token_t* next, ast_error_t* ast_err)
{
	node = parse_number(context,parser,node,next_type,next,ast_err,1);
	if (node)
	{
		if (node->m_type == AST_TYPE_DOUBLE)
			node->m_dval = -node->m_dval;
		else
		{
			int32_t v = node->m_u64val;
			if (v > 0)
				node->m_u64val = (uint32_t)(-v);
		}
	}
	return node;
}

static ast_node_t* parse_term(context_t* context, parser_t* parser, unsigned int max_prec, token_type_t* next_type, token_t* next, ast_error_t* ast_err);
static ast_node_t* parse_compound_term(context_t* context, parser_t* parser, ast_node_t* node, token_type_t* next_type, token_t* next, ast_error_t* ast_err);

static ast_node_t* parse_arg(context_t* context, parser_t* parser, token_type_t* next_type, token_t* next, ast_error_t* ast_err)
{
	ast_node_t* node;
	operator_t* op;

	if (*next_type != tokName)
		return parse_term(context,parser,999,next_type,next,ast_err);

	node = heap_malloc(context->m_heap,sizeof(ast_node_t));
	if (!node)
		longjmp(parser->m_jmp,1);

	node->m_arity = 0;
	node->m_type = AST_TYPE_ATOM;
	node->m_str = next->m_str;
	node->m_str_len = next->m_len;
	next->m_alloc = next->m_len = 0;

	if (node->m_str_len > MAX_ATOM_LEN)
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	*next_type = token_next(context,parser,next);

	if (*next_type == tokOpenCt)
		return parse_compound_term(context,parser,node,next_type,next,ast_err);

	if (node->m_str_len == 1 && node->m_str[0] == '-')
	{
		if (*next_type >= tokInt && *next_type <= tokFloat)
			return parse_negative(context,parser,node,next_type,next,ast_err);
	}

	op = lookup_prefix_op(context,node->m_str,node->m_str_len);
	if (op && op->m_precedence <= 999 && (op->m_specifier == eFX || op->m_specifier == eFY))
	{
		node = atom_to_compound(context,parser,node,ast_err);
		node->m_arity = 1;
		node->m_params[0] = parse_term(context,parser,op->m_specifier == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,ast_err);
		if (!node->m_params[0])
			node = NULL;
	}

	return node;
}

static ast_node_t* parse_compound_term(context_t* context, parser_t* parser, ast_node_t* node, token_type_t* next_type, token_t* next, ast_error_t* ast_err)
{
	size_t alloc_arity = 1;
	node = atom_to_compound(context,parser,node,ast_err);

	do
	{
		*next_type = token_next(context,parser,next);

		if (node->m_arity == alloc_arity)
		{
			size_t new_arity = alloc_arity * 2;
			ast_node_t* new_node = heap_realloc(context->m_heap,node,sizeof(ast_node_t) + (alloc_arity * sizeof(ast_node_t*)),sizeof(ast_node_t) + (new_arity * sizeof(ast_node_t*)));
			if (!new_node)
				longjmp(parser->m_jmp,1);

			alloc_arity = new_arity;
			node = new_node;
		}

		node->m_params[node->m_arity] = parse_arg(context,parser,next_type,next,ast_err);
		if (!node->m_params[node->m_arity])
			return NULL;

		++node->m_arity;
		if (node->m_arity > MAX_ARITY)
			return syntax_error(AST_ERR_MAX_ARITY,ast_err);
	}
	while (*next_type == tokComma);

	if (*next_type != tokClose)
		return syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE,ast_err);

	*next_type = token_next(context,parser,next);
	return node;
}

static ast_node_t* parse_list_term(context_t* context, parser_t* parser, token_type_t* next_type, token_t* next, ast_error_t* ast_err)
{
	ast_node_t* node = NULL;
	ast_node_t** tail = &node;

	do
	{
		*tail = heap_malloc(context->m_heap,sizeof(ast_node_t) + (2*sizeof(ast_node_t*)));
		if (!(*tail))
			longjmp(parser->m_jmp,1);

		(*tail)->m_type = AST_TYPE_COMPOUND;
		(*tail)->m_str = (const unsigned char*)".";
		(*tail)->m_str_len = 1;
		(*tail)->m_arity = 2;
		(*tail)->m_params[1] = NULL;

		*next_type = token_next(context,parser,next);

		(*tail)->m_params[0] = parse_arg(context,parser,next_type,next,ast_err);
		if (!(*tail)->m_params[0])
			return NULL;

		tail = &((*tail)->m_params[1]);
	}
	while (*next_type == tokComma);

	if (*next_type == tokBar)
	{
		*next_type = token_next(context,parser,next);

		*tail = parse_arg(context,parser,next_type,next,ast_err);
		if (!(*tail))
			return NULL;

		*next_type = token_next(context,parser,next);
	}
	else if (*next_type == tokCloseL)
	{
		/* Append [] */
		*tail = heap_malloc(context->m_heap,sizeof(ast_node_t));
		if (!(*tail))
			longjmp(parser->m_jmp,1);

		(*tail)->m_type = AST_TYPE_ATOM;
		(*tail)->m_str = (const unsigned char*)"[]";
		(*tail)->m_str_len = 2;
		(*tail)->m_arity = 0;
	}

	if (*next_type != tokCloseL)
		return syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE_L,ast_err);

	return node;
}

static ast_node_t* parse_name(context_t* context, parser_t* parser, unsigned int* max_prec, token_type_t* next_type, token_t* next, ast_error_t* ast_err)
{
	operator_t* op;
	ast_node_t* node = heap_malloc(context->m_heap,sizeof(ast_node_t));
	if (!node)
		longjmp(parser->m_jmp,1);

	node->m_arity = 0;
	node->m_type = AST_TYPE_ATOM;
	node->m_str = next->m_str;
	node->m_str_len = next->m_len;
	next->m_alloc = next->m_len = 0;

	*next_type = token_next(context,parser,next);
	*max_prec = 0;

	if (*next_type == tokOpenCt)
		return parse_compound_term(context,parser,node,next_type,next,ast_err);

	if (node->m_str_len == 1 && node->m_str[0] == '-')
	{
		if (*next_type >= tokInt && *next_type <= tokFloat)
			return parse_negative(context,parser,node,next_type,next,ast_err);
	}

	op = lookup_prefix_op(context,node->m_str,node->m_str_len);
	if (op)
	{
		if (op->m_precedence > *max_prec && (op->m_specifier == eFX || op->m_specifier == eFY))
		{
			node = atom_to_compound(context,parser,node,ast_err);
			node->m_params[0] = parse_term(context,parser,op->m_specifier == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,ast_err);
			if (!node->m_params[0])
				node = NULL;

			*max_prec = op->m_precedence;
			return node;
		}

		if (*max_prec < 1201)
			return syntax_error(AST_SYNTAX_ERR_INVALID_ARG,ast_err);

		*max_prec = 1201;
	}

	return node;
}

static ast_node_t* parse_chars_and_codes(context_t* context, parser_t* parser, int chars, token_t* token, ast_error_t* ast_err)
{
	/* TODO: Check for utf8 chars token and split into multiple lists */

	ast_node_t* node = heap_malloc(context->m_heap,sizeof(ast_node_t));
	if (!node)
		longjmp(parser->m_jmp,1);

	node->m_type = chars ? AST_TYPE_CHARS : AST_TYPE_CODES;
	node->m_arity = 0;
	node->m_str = token->m_str;
	node->m_str_len = token->m_len;
	token->m_alloc = token->m_len = 0;

	return node;
}

static ast_node_t* parse_term_base(context_t* context, parser_t* parser, unsigned int* max_prec, token_type_t* next_type, token_t* next, ast_error_t* ast_err)
{
	ast_node_t* node = NULL;

	switch (*next_type)
	{
	case tokName:
		return parse_name(context,parser,max_prec,next_type,next,ast_err);

	case tokVar:
		node = heap_malloc(context->m_heap,sizeof(ast_node_t));
		if (!node)
			longjmp(parser->m_jmp,1);

		node->m_type = AST_TYPE_VAR;
		node->m_arity = UINT64_C(-1);
		node->m_str = next->m_str;
		node->m_str_len = next->m_len;
		next->m_alloc = next->m_len = 0;
		break;

	case tokInt:
	case tokBinaryInt:
	case tokOctalInt:
	case tokHexInt:
	case tokCharCode:
	case tokFloat:
		*max_prec = 0;
		return parse_number(context,parser,node,next_type,next,ast_err,0);

	case tokDQL:
		if (context->m_module->m_flags.double_quotes == 2 /* atom */)
		{
			/* ISO/IEC 13211-1:1995/Cor.1:2007 */
			return parse_name(context,parser,max_prec,next_type,next,ast_err);
		}

		node = parse_chars_and_codes(context,parser,context->m_module->m_flags.double_quotes,next,ast_err);
		break;

	case tokBackQuote:
		if (context->m_module->m_flags.back_quotes == 2 /* atom */)
			return parse_name(context,parser,max_prec,next_type,next,ast_err);

		node = parse_chars_and_codes(context,parser,context->m_module->m_flags.back_quotes,next,ast_err);
		break;

	case tokOpen:
	case tokOpenCt:
		*next_type = token_next(context,parser,next);
		node = parse_term(context,parser,1201,next_type,next,ast_err);
		if (node && *next_type != tokClose)
			return syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE,ast_err);
		break;

	case tokOpenL:
		*next_type = token_next(context,parser,next);
		if (*next_type != tokCloseL)
		{
			*max_prec = 0;
			return parse_list_term(context,parser,next_type,next,ast_err);
		}

		node = heap_malloc(context->m_heap,sizeof(ast_node_t));
		if (!node)
			longjmp(parser->m_jmp,1);

		node->m_type = AST_TYPE_ATOM;
		node->m_str = (const unsigned char*)"[]";
		node->m_str_len = 2;
		node->m_arity = 0;
		break;

	case tokOpenC:
		node = heap_malloc(context->m_heap,sizeof(ast_node_t) + sizeof(ast_node_t*));
		if (!node)
			longjmp(parser->m_jmp,1);

		node->m_type = AST_TYPE_COMPOUND;
		node->m_str = (const unsigned char*)"{}";
		node->m_str_len = 2;
		node->m_arity = 1;

		*next_type = token_next(context,parser,next);

		node->m_params[0] = parse_term(context,parser,1201,next_type,next,ast_err);
		if (!node->m_params[0])
			return NULL;

		if (*next_type != tokCloseC)
			return syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE_C,ast_err);
		break;

	case tokNeedMore:
		/* Shouldn't happen... */

	case tokComma:
	case tokClose:
	case tokCloseL:
	case tokCloseC:
	case tokBar:
	case tokEnd:
		return syntax_error(AST_SYNTAX_ERR_UNEXPECTED_TOKEN,ast_err);

	case tokInvalidChar:
		return syntax_error(AST_SYNTAX_ERR_INVALID_CHAR,ast_err);

	case tokMissingSQ:
		return syntax_error(AST_SYNTAX_ERR_MISSING_SQ,ast_err);

	case tokMissingDQ:
		return syntax_error(AST_SYNTAX_ERR_MISSING_DQ,ast_err);

	case tokMissingBQ:
		return syntax_error(AST_SYNTAX_ERR_MISSING_BQ,ast_err);

	case tokInvalidSeq:
		return syntax_error(AST_SYNTAX_ERR_INVALID_UTF8,ast_err);

	case tokInvalidEscape:
		return syntax_error(AST_SYNTAX_ERR_INVALID_ESCAPE,ast_err);

	case tokEOF:
		return NULL;
	}

	*max_prec = 0;
	*next_type = token_next(context,parser,next);
	return node;
}

static ast_node_t* parse_term(context_t* context, parser_t* parser, unsigned int max_prec, token_type_t* next_type, token_t* next, ast_error_t* ast_err)
{
	unsigned int prev_prec = max_prec;
	ast_node_t* node = parse_term_base(context,parser,&prev_prec,next_type,next,ast_err);
	if (!node)
		return NULL;

	/* This is precedence climbing, if you're interested */
	for (;;)
	{
		unsigned int right_prec = 0;
		unsigned int left_prec = 0;
		int binary = 0;
		const unsigned char* name = next->m_str;
		size_t name_len = next->m_len;

		if (*next_type == tokName)
		{
			operator_t* op = lookup_op(context,name,name_len);
			if (!op || op->m_precedence > max_prec)
				break;

			switch (op->m_specifier)
			{
			case eFX:
			case eFY:
				return node;

			case eXFX:
				left_prec = right_prec = op->m_precedence - 1;
				binary = 1;
				break;

			case eXFY:
				left_prec = op->m_precedence - 1;
				right_prec = op->m_precedence;
				binary = 1;
				break;

			case eYFX:
				left_prec = op->m_precedence;
				right_prec = op->m_precedence - 1;
				binary = 1;
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

			binary = 1;
			left_prec = 999;
			right_prec = 1000;

			name = (const unsigned char*)",";
			name_len = 1;
		}
		else if (*next_type == tokBar)
		{
			/* ISO/IEC 13211-1:1995/Cor.2:2012 */
			operator_t* op;

			name = (const unsigned char*)"|";
			name_len = 1;

			op = lookup_op(context,name,name_len);
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
				binary = 1;
				break;

			case eXFY:
				left_prec = op->m_precedence - 1;
				right_prec = op->m_precedence;
				binary = 1;
				break;

			case eYFX:
				left_prec = op->m_precedence;
				right_prec = op->m_precedence - 1;
				binary = 1;
				break;
			}
		}
		else
			break;

		if (prev_prec > left_prec)
			break;
		else
		{
			ast_node_t* next_node = heap_malloc(context->m_heap,sizeof(ast_node_t) + ((1 + binary) * sizeof(ast_node_t*)));
			if (!next_node)
				longjmp(parser->m_jmp,1);

			next_node->m_type = AST_TYPE_COMPOUND;
			next_node->m_str = name;
			next_node->m_str_len = name_len;
			next_node->m_arity = 1 + binary;
			next_node->m_params[0] = node;

			if (*next_type == tokName)
				next->m_alloc = next->m_len = 0;

			*next_type = token_next(context,parser,next);

			if (binary)
			{
				next_node->m_params[1] = parse_term(context,parser,right_prec,next_type,next,ast_err);
				if (!next_node->m_params[1])
					return NULL;
			}

			node = next_node;
		}
	}

	return node;
}

static term_t* emit_ast_string(term_t* stack, prolite_type_t type, ast_node_t* node)
{
	// TODO: We could de-duplicate here...
	return push_string(stack,type,node->m_str,node->m_str_len,0);
}

static term_t* emit_ast_node(term_t* stack, ast_node_t* node)
{
	size_t i;
	switch (node->m_type)
	{
	case AST_TYPE_COMPOUND:
		for (i = node->m_arity; i--;)
			stack = emit_ast_node(stack,node->m_params[i]);

		// TODO: We could de-duplicate here...
		stack = push_predicate(stack,node->m_arity,node->m_str,node->m_str_len,0);
		break;

	case AST_TYPE_ATOM:
		stack = emit_ast_string(stack,prolite_atom,node);
		break;

	case AST_TYPE_CHARS:
		stack = emit_ast_string(stack,prolite_chars,node);
		break;

	case AST_TYPE_CODES:
		stack = emit_ast_string(stack,prolite_charcodes,node);
		break;

	case AST_TYPE_VAR:
	case AST_TYPE_DOUBLE:
	case AST_TYPE_INTEGER:
		if (node->m_type == AST_TYPE_VAR)
			stack = push_var(stack,node->m_arity);
		else if (node->m_type == AST_TYPE_DOUBLE)
			stack = push_double(stack,node->m_dval);
		else if (node->m_type == AST_TYPE_INTEGER)
			stack = push_integer(stack,node->m_u64val);
		break;
	}

	// TODO: Debug info

	return stack;
}

static term_t* emit_error_line_info(term_t* stack, line_info_t* info)
{
	if (!info)
		(--stack)->m_u64val = PACK_ATOM_EMBED_5('f','a','l','s','e');
	else
	{
		// TODO: Line if
		(--stack)->m_u64val = PACK_ATOM_EMBED_4('T','o','d','o');
	}

	return stack;
}

static parse_status_t emit_syntax_error_missing(term_t** stack, uint64_t missing_atom, line_info_t* info)
{
	*stack = emit_error_line_info(*stack,info);

	(--*stack)->m_u64val = missing_atom;
	(--*stack)->m_u64val = PACK_COMPOUND_BUILTIN(missing,1);
	(--*stack)->m_u64val = PACK_COMPOUND_BUILTIN(syntax_error,1);
	(--*stack)->m_u64val = PACK_COMPOUND_EMBED_5(2,'e','r','r','o','r');

	return PARSE_THROW;
}

static parse_status_t emit_simple_error(term_t** stack, uint64_t f, uint64_t arg, line_info_t* info)
{
	*stack = emit_error_line_info(*stack,info);

	(--*stack)->m_u64val = arg;
	(--*stack)->m_u64val = f;
	(--*stack)->m_u64val = PACK_COMPOUND_EMBED_5(2,'e','r','r','o','r');

	return PARSE_THROW;
}

static parse_status_t emit_out_of_heap_error(term_t** stack, line_info_t* info)
{
	return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(resource_error,1),PACK_ATOM_EMBED_4('h','e','a','p'),info);
}

static parse_status_t emit_eof_error(term_t** stack, line_info_t* info)
{
	return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(past_end_of_stream),info);
}

static parse_status_t emit_ast_error(term_t** stack, ast_error_t ast_err, line_info_t* info)
{
	switch (ast_err)
	{
	case AST_ERR_NONE:
	default:
		assert(0);
		return PARSE_OK;

	case AST_ERR_OUTOFMEMORY:
		return emit_out_of_heap_error(stack,info);

	case AST_ERR_FLOAT_OVERFLOW:
		return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(evaluation_error,1),PACK_ATOM_BUILTIN(float_overflow),info);

	case AST_ERR_FLOAT_UNDERFLOW:
		return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(evaluation_error,1),PACK_ATOM_BUILTIN(underflow),info);

	case AST_ERR_MAX_INTEGER:
		return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(representation_error,1),PACK_ATOM_BUILTIN(max_integer),info);

	case AST_ERR_MIN_INTEGER:
		return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(representation_error,1),PACK_ATOM_BUILTIN(min_integer),info);

	case AST_ERR_MAX_ARITY:
		return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(representation_error,1),PACK_ATOM_BUILTIN(max_arity),info);

	case AST_SYNTAX_ERR_MISSING_CLOSE:
		return emit_syntax_error_missing(stack,PACK_ATOM_EMBED_1(')'),info);

	case AST_SYNTAX_ERR_MISSING_CLOSE_L:
		return emit_syntax_error_missing(stack,PACK_ATOM_EMBED_1(']'),info);

	case AST_SYNTAX_ERR_MISSING_CLOSE_C:
		return emit_syntax_error_missing(stack,PACK_ATOM_EMBED_1('}'),info);

	case AST_SYNTAX_ERR_MISSING_SQ:
		return emit_syntax_error_missing(stack,PACK_ATOM_EMBED_1('\''),info);

	case AST_SYNTAX_ERR_MISSING_DQ:
		return emit_syntax_error_missing(stack,PACK_ATOM_EMBED_1('"'),info);

	case AST_SYNTAX_ERR_MISSING_BQ:
		return emit_syntax_error_missing(stack,PACK_ATOM_EMBED_1('`'),info);

	case AST_SYNTAX_ERR_INVALID_ARG:
		return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(invalid_argument),info);

	case AST_SYNTAX_ERR_UNEXPECTED_TOKEN:
		return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(unexpected_token),info);

	case AST_SYNTAX_ERR_INVALID_CHAR:
		return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(invalid_character),info);

	case AST_SYNTAX_ERR_INVALID_ESCAPE:
		return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(invalid_escape),info);

	case AST_SYNTAX_ERR_INVALID_UTF8:
		return emit_simple_error(stack,PACK_COMPOUND_BUILTIN(syntax_error,1),PACK_ATOM_BUILTIN(invalid_utf8),info);
	}
}

typedef struct var_info
{
	size_t               m_use_count;
	const unsigned char* m_name;
	size_t               m_name_len;
} var_info_t;

static parse_status_t collate_var_info(context_t* context, parser_t* parser, var_info_t** varinfo, size_t* var_count, ast_node_t* node)
{
	parse_status_t status = PARSE_OK;

	if (node->m_type == AST_TYPE_VAR)
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
				return emit_out_of_heap_error(&context->m_stack,NULL);

			new_varinfo = heap_realloc(context->m_heap,*varinfo,sizeof(var_info_t) * (*var_count),sizeof(var_info_t) * ((*var_count)+1));
			if (!new_varinfo)
				longjmp(parser->m_jmp,1);

			*varinfo = new_varinfo;
			new_varinfo = *varinfo + (*var_count)++;

			new_varinfo->m_name = node->m_str;
			new_varinfo->m_name_len = node->m_str_len;
			new_varinfo->m_use_count = 1;
		}

		node->m_arity = i;
	}
	else if (node->m_type == AST_TYPE_COMPOUND)
	{
		size_t i;
		for (i = 0; !status && i < node->m_arity; ++i)
			status = collate_var_info(context,parser,varinfo,var_count,node->m_params[i]);
	}

	return status;
}

parse_status_t consult_term(context_t* context, parser_t* parser)
{
	parse_status_t status = PARSE_OK;
	size_t heap_start = heap_top(context->m_heap);

	if (!setjmp(parser->m_jmp))
	{
		ast_error_t ast_err = AST_ERR_NONE;
		token_t next = {0};
		token_type_t next_type;
		ast_node_t* node;

		next_type = token_next(context,parser,&next);
		node = parse_term(context,parser,1201,&next_type,&next,&ast_err);
		if (!node)
		{
			if (next_type == tokEOF)
				status = PARSE_EOF;
			else
				status = emit_ast_error(&context->m_stack,ast_err,&parser->m_line_info);	
		}
		else if (next_type != tokEnd)
		{
			status = emit_syntax_error_missing(&context->m_stack,PACK_ATOM_EMBED_1('.'),&parser->m_line_info);
		}
		else
		{
			var_info_t* varinfo = NULL;
			size_t varcount = 0;
			status = collate_var_info(context,parser,&varinfo,&varcount,node);
			if (status == PARSE_OK)
			{
				size_t i = varcount;

				context->m_stack = emit_ast_node(context->m_stack,node);

				/* Write out var count */
				while (i--)
				{
					/* use count */
					(--context->m_stack)->m_u64val = varinfo[i].m_use_count;

					/* variable name */
					context->m_stack = push_string(context->m_stack,prolite_atom,varinfo[i].m_name,varinfo[i].m_name_len,0);
				}
				(--context->m_stack)->m_u64val = varcount;
			}
		}
	}
	else
	{
		status = emit_out_of_heap_error(&context->m_stack,&parser->m_line_info);
	}

	/* Reset the heap */
	heap_reset(context->m_heap,heap_start);

	return status;
}

parse_status_t read_term(context_t* context, stream_t* s)
{
	parser_t parser = 
	{
		.m_s = s,
		.m_line_info.m_end_line = 1
	};

	parse_status_t status = consult_term(context,&parser);
	if (status == PARSE_EOF)
		status = emit_eof_error(&context->m_stack,&parser.m_line_info);
		
	return status;
}
