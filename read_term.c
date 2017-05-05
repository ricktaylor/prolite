
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <malloc.h>
#include <errno.h>
#include <limits.h>

#include <stdint.h>

#include "context.h"

/* These need to go elsewhere */
struct stream_t;

int64_t read(struct stream_t* s, void* dest, size_t len);

int prolog_flag(const char*);

uint32_t char_conversion(uint32_t in_char);

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

struct Operator
{
	enum eOpSpec m_specifier;
	unsigned int m_precedence;
};

struct Operator* lookup_op(const unsigned char* name, size_t len)
{
	struct Operator* op = NULL;

	/* Try to find a infix/suffix op, otherwise find prefix */

	/* Lookup and return */

	return op;
}

struct Operator* lookup_prefix_op(struct ast_node_t* node)
{
	struct Operator* op = NULL;

	/* Try to find a prefix op, otherwise find infix/suffix */

	/* Lookup and return */

	return op;
}






enum eTokenType
{
	tokMore = 0,
	tokNoMem,
	tokEOF,
	tokInvalidSeq,
	tokInvalidEscape,
	tokInvalidChar,
	tokHalfQuote,
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
};

struct token_t
{
	size_t         m_alloc;
	size_t         m_len;
	unsigned char* m_str;  /* Need not be zero-terminated! */
};

struct token_info_t
{
	size_t         m_start_line;
	size_t         m_start_col;
	size_t         m_line;
	size_t         m_col;
};

struct tokenizer_t
{
	struct stream_t*    m_s;
	struct token_t      m_buffer;
	struct token_info_t m_info;
	int                 m_eof;
};

enum eASTNodeType
{
	AST_TYPE_VAR,
	AST_TYPE_COMPOUND,
	AST_TYPE_DOUBLE,
	AST_TYPE_INTEGER,
	AST_TYPE_ATOM,
	AST_TYPE_DQL,
	AST_TYPE_BQL,
	AST_TYPE_DOT,
	AST_TYPE_EMPTY_LIST,
};

struct ast_node_t
{
	unsigned char*     m_name;
	union
	{
		size_t         m_name_len;
		double         m_double;
		int32_t        m_integer;
	};
	size_t             m_arity;
	enum eASTNodeType  m_type;
	unsigned char      m_buf[5];  //< TODO: Match however we NaN-box a short string
	struct ast_node_t* m_params[];
};

enum eASTError
{
	AST_ERR_NONE = 0,
	AST_ERR_OUTOFMEMORY,
	AST_ERR_EOF,
	AST_SYNTAX_ERR_FLOAT_OVERFLOW,
	AST_SYNTAX_ERR_FLOAT_UNDERFLOW,
	AST_SYNTAX_ERR_INTEGER_OVERFLOW,
	AST_SYNTAX_ERR_MAX_ARITY,
	AST_SYNTAX_ERR_MISSING_CLOSE,
	AST_SYNTAX_ERR_MISSING_CLOSE_L,
	AST_SYNTAX_ERR_MISSING_CLOSE_C,
	AST_SYNTAX_ERR_INVALID_ARG,
	AST_SYNTAX_ERR_UNEXPECTED_TOKEN,
	AST_SYNTAX_ERR_INVALID_CHAR,
	AST_SYNTAX_ERR_INVALID_ESCAPE,
	AST_SYNTAX_ERR_INVALID_UTF8,
	AST_SYNTAX_ERR_MISSING_QUOTE,
};

static const uint32_t char_max = 0x1FFFF; /* Greatest valid unicode char */
static const uint32_t char_more = char_max+1;
static const uint32_t char_eof = char_max+2;
static const uint32_t char_ilseq = char_max+3;

static int token_append_char(struct token_t* token, unsigned char c)
{
	if (token->m_alloc == token->m_len)
	{
		size_t new_size = (token->m_alloc == 0 ? 32 : token->m_alloc * 2);
		unsigned char* new_str = realloc(token->m_str,new_size);
		if (!new_str)
			return -1;

		token->m_alloc = new_size;
		token->m_str = new_str;
	}

	token->m_str[token->m_len++] = c;
	return 0;
}

static int token_append_unicode_char(struct token_t* token, uint32_t unicode_char)
{
	if (unicode_char <= 0x7F)
	{
		return token_append_char(token,(unsigned char)unicode_char);
	}
	else
	{
		unsigned char chars[4] = {0};
		unsigned int i,count;

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

		for (i=0; i<count; ++i)
		{
			if (token_append_char(token,chars[i]) == -1)
				return -1;
		}

		return 0;
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

			return char_more;
		}

		if ((*p)[i] <= 0x7f)
		{
			*p += i;
			*col += i;
			break;
		}

		++i;
	}
	return char_ilseq;
}

static uint32_t token_get_char(const unsigned char** p, const unsigned char* pe, int eof, size_t* line, size_t* col)
{
	unsigned int count = 0;
	uint32_t val;
	const unsigned char* c = *p;

	if (c == pe)
	{
		if (eof)
			return char_eof;

		return char_more;
	}

	if (c[0] <= 0x7f)
	{
		val = c[0];
		count = 1;
	}
	else if (c[0] >= 0xC2 && c[0] <= 0xF4)
	{
		unsigned int i;

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

				return char_more;
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

				return char_more;
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

			return char_more;
		}

		for (i=1;i<count;++i)
		{
			if ((c[i] & 0xC0) != 0x80)
			{
				*p += i;
				*col += i;
				return char_ilseq;
			}

			val = (val << 6) | (c[i] & 0x3F);
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

static uint32_t token_get_char_conv(const unsigned char** p, const unsigned char* pe, struct token_info_t* info)
{
	uint32_t c = token_get_char(p,pe,info);
	if (c <= char_max)
		c = char_conversion(c);
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
	/* space */ eaWhitespace, /* ! */ eaShortName, /* " */ eaSingleQuote, /* # */ eaGraphic,
	/* $ */ eaGraphic, /* % */ eaSingleComment, /* & */ eaGraphic, /* ' */ eaDoubleQuote,
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

static int token_meta_char(uint32_t meta, struct token_t* token)
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

	if (token_append_char(token,c) == -1)
		return -1;

	return 1;
}

static enum eTokenType read_token(enum eState* state, const unsigned char** p, const unsigned char* pe, int eof, struct token_t* token, struct token_info_t* info)
{
	const unsigned char* peek = *p;
	size_t peek_line = info->m_line;
	size_t peek_col = info->m_col;
	uint32_t c;
	uint32_t meta;

	switch (*state)
	{
	case eStart:
		/* Clear the current token */
		token->m_len = 0;

		c = token_get_char_conv(p,pe,info);
		if (c == char_more)
			return tokMore;

		/* Check for ( first*/
		if (c == '(')
			return tokOpenCt;

		*state = eLayout;
		goto layout;

	case eLayout:
	case eSingleComment:
	case eMultiComment2:
	case eMultiComment3:
		c = token_get_char_conv(p,pe,eof,&info->m_line,&info->m_col);
		if (c == char_more)
			return tokMore;

	layout:
		for (;;)
		{
			if (c == char_eof)
				return tokEOF;

			if (*state == eLayout)
			{
				if (c == char_ilseq)
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
					info->m_start_line = info->m_line;
					info->m_start_col = info->m_col;
					break;

				case eaShortName:
					*state = eStart;
					if (token_append_char(token,(unsigned char)c) != 0)
						return tokNoMem;
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
					goto next_char;

				case eaName:
					*state = eName;
					goto next_char;

				case eaVar:
					*state = eVar;
					goto next_char;

				case eaZero:
					*state = eZero;
					goto zero;

				case eaNumber:
					*state = eInteger;
					goto next_char;

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
					info->m_start_line = info->m_line;
					info->m_start_col = info->m_col;
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
					info->m_start_line = info->m_line;
					info->m_start_col = info->m_col;
					*state = eLayout;
				}
				else if (c != '*')
					*state = eMultiComment2;
			}

			/* Get the next character */
			c = token_get_char_conv(p,pe,eof,&info->m_line,&info->m_col);
			if (c == char_more)
				return tokMore;
		}

	multi_comment:
		peek = *p;
		peek_line = info->m_line;
		peek_col = info->m_col;

	case eMultiComment1:
		c = token_get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c != '*')
		{
			if (token_append_char(token,'/') != 0)
			{
				*state = eStart;
				return tokNoMem;
			}
			*state = eGraphicName;
			goto graphic_name;
		}

		*p = peek;
		info->m_line = peek_line;
		info->m_col = peek_col;
		*state = eMultiComment2;
		goto layout;

	dot:
		peek = *p;
		peek_col = info->m_col;

	case eDot:
		c = token_get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c == char_eof)
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

		if (token_append_char(token,'.') != 0)
		{
			*state = eStart;
			return tokNoMem;
		}
		*state = eGraphicName;
		goto graphic_name;

	quote:
		peek = *p;
		peek_col = info->m_col;

	case eSingleQuote:
	case eDoubleQuote:
	case eBackQuote:
		for (;;)
		{
			c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
			switch (c)
			{
			case char_more:
				return tokMore;

			case char_eof:
				*state = eStart;
				return tokHalfQuote;

			case char_ilseq:
				info->m_line = peek_line;
				info->m_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidSeq;

			case '\\':
				c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
				if (c == char_more)
					return tokMore;

				if (!token_meta_char(c,token))
				{
					if (c == 'o')
					{
						c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
						if (c == char_more)
							return tokMore;

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
						c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
						if (c == char_more)
							return tokMore;

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
						info->m_line = peek_line;
						info->m_col = peek_col;
						*p = peek;
						*state = eStart;
						if (c == char_ilseq)
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
					c = token_get_char(&peek2,pe,eof,&peek_line2,&peek_col2);
					if (c == char_more)
						return tokMore;

					if (c != '\'')
					{
						info->m_line = peek_line;
						info->m_col = peek_col;
						*p = peek;
						return tokName;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				if (token_append_char(token,(unsigned char)c) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}
				break;

			case '"':
				if (*state == eDoubleQuote)
				{
					size_t peek_line2 = peek_line;
					size_t peek_col2 = peek_col;
					const unsigned char* peek2 = peek;
					c = token_get_char(&peek2,pe,eof,&peek_line2,&peek_col2);
					if (c == char_more)
						return tokMore;

					if (c != '"')
					{
						info->m_line = peek_line;
						info->m_col = peek_col;
						*p = peek;
						return tokDQL;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				if (token_append_char(token,(unsigned char)c) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}
				break;

			case '`':
				if (*state == eBackQuote)
				{
					size_t peek_line2 = peek_line;
					size_t peek_col2 = peek_col;
					const unsigned char* peek2 = peek;
					c = token_get_char(&peek2,pe,eof,&peek_line2,&peek_col2);
					if (c == char_more)
						return tokMore;

					if (c != '`')
					{
						info->m_line = peek_line;
						info->m_col = peek_col;
						*p = peek;
						return tokBackQuote;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				if (token_append_char(token,(unsigned char)c) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}
				break;

			default:
				if (c < 32)
				{
					info->m_line = peek_line;
					info->m_col = peek_col;
					*p = peek;
					*state = eStart;
					return tokInvalidChar;
				}
				if (token_append_unicode_char(token,c) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}
				break;
			}

			info->m_line = peek_line;
			info->m_col = peek_col;
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
			c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c < '0' || c > '7')
			{
				info->m_line = peek_line;
				info->m_col = peek_col;
				*p = peek;

				if (c == '\\')
				{
					if (token_append_unicode_char(token,meta) != 0)
					{
						*state = eStart;
						return tokNoMem;
					}
					else if (*state == eSingleQuoteOct)
						*state = eSingleQuote;
					else if (*state == eDoubleQuoteOct)
						*state = eDoubleQuote;
					else
						*state = eBackQuote;
					goto quote;
				}

				*state = eStart;
				if (c == char_ilseq)
					return tokInvalidSeq;

				return tokInvalidEscape;
			}

		octal_char:
			meta = (meta << 3) | (c - '0');
			if (meta > char_max)
			{
				info->m_line = peek_line;
				info->m_col = peek_col;
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
			c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')))
			{
				info->m_line = peek_line;
				info->m_col = peek_col;
				*p = peek;

				if (c == '\\')
				{
					if (token_append_unicode_char(token,meta) != 0)
					{
						*state = eStart;
						return tokNoMem;
					}
					else if (*state == eSingleQuoteHex)
						*state = eSingleQuote;
					else if (*state == eDoubleQuoteHex)
						*state = eDoubleQuote;
					else
						*state = eBackQuote;
					goto quote;
				}

				*state = eStart;
				if (c == char_ilseq)
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

			if (meta > char_max)
			{
				info->m_line = peek_line;
				info->m_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidEscape;
			}
		}

	case eZero:
	zero:
		c = token_get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c == '\'')
		{
			/* No char_conversion for single_quoted_character */
			c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c == '\'')
			{
				c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
				if (c == char_more)
					return tokMore;

				if (c == '\'')
				{
					if (token_append_char(token,'\'') != 0)
					{
						*state = eStart;
						return tokNoMem;
					}

					info->m_line = peek_line;
					info->m_col = peek_col;
					*p = peek;
					return tokCharCode;
				}
			}
			else if (c == '\\')
			{
				c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
				if (c == char_more)
					return tokMore;

				if (token_meta_char(c,token))
				{
					info->m_line = peek_line;
					info->m_col = peek_col;
					*p = peek;
					return tokCharCode;
				}
				else if (c == 'o')
				{
					c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
					if (c == char_more)
						return tokMore;

					if (c >= '0' && c <= '7')
					{
						meta = 0;
						*state = eOctalCharCode;
						goto octal_char_code;
					}
				}
				else if (c == 'x')
				{
					c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
					if (c == char_more)
						return tokMore;

					if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
					{
						meta = 0;
						*state = eHexCharCode;
						goto hex_char_code;
					}
				}
			}
			else if (c >= 32 && c <= char_max)
			{
				if (token_append_unicode_char(token,c) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}

				info->m_line = peek_line;
				info->m_col = peek_col;
				*p = peek;
				return tokCharCode;
			}
		}
		else if (c == 'b')
		{
			c = token_get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c == '0' || c == '1')
			{
				*state = eBinaryInt;
				goto next_char;
			}
		}
		else if (c == 'o')
		{
			c = token_get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c >= '0' && c <= '7')
			{
				*state = eOctalInt;
				goto next_char;
			}
		}
		else if (c == 'x')
		{
			c = token_get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
			{
				*state = eHexInt;
				goto next_char;
			}
		}

		/* Reset the state and treat as integer */
		c = '0';
		*state = eInteger;
		peek = *p;
		peek_line = info->m_line;
		peek_col = info->m_col;
		goto next_char;

	case eOctalCharCode:
		/* We know *p == "0'\o" */
		peek = *p + 4;
		peek_col = info->m_col + 4;

		meta = 0;
		for (;;)
		{
			c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c == '\\')
			{
				if (token_append_unicode_char(token,meta) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}

				info->m_line = peek_line;
				info->m_col = peek_col;
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
				peek_line = info->m_line;
				peek_col = info->m_col + 1;
				goto next_char;
			}

		octal_char_code:
			meta = (meta << 3) | (c - '0');
			if (meta > char_max)
			{
				info->m_line = peek_line;
				info->m_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidEscape;
			}
		}

	case eHexCharCode:
		/* We know *p == "0'\x" */
		peek = *p + 4;
		peek_col = info->m_col + 4;
		meta = 0;
		for (;;)
		{
			c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c == '\\')
			{
				if (token_append_unicode_char(token,meta) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}

				info->m_line = peek_line;
				info->m_col = peek_col;
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
				peek_line = info->m_line;
				peek_col = info->m_col + 1;
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

			if (meta > char_max)
			{
				info->m_line = peek_line;
				info->m_col = peek_col;
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
		c = token_get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c < '0' || c > '9')
		{
			*state = eStart;
			return tokInt;
		}

		if (token_append_char(token,'.') != 0)
		{
			*state = eStart;
			return tokNoMem;
		}

		*state = eFraction;
		goto next_char;

	case eExponent:
		/* We know *p == 'E' or 'e' */
		++peek;
		++peek_col;

	exponent:
		c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c >= '0' && c <= '9')
		{
			if (token_append_char(token,'e') != 0)
			{
				*state = eStart;
				return tokNoMem;
			}

			*state = eMantissa;
			goto next_char;
		}

		if (c == '-' || c == '+')
		{
			/* Check the next char */
			uint32_t c2 = token_get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
			if (c2 == char_more)
				return tokMore;

			if (c2 >= '0' && c2 <= '9')
			{
				if (token_append_char(token,'e') != 0 || token_append_char(token,(unsigned char)c) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}

				*state = eMantissa;
				c = c2;
				goto next_char;
			}
		}

		*state = eStart;
		return tokFloat;

	default:
		for (;;)
		{
			c = token_get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

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
				// Never going to happen
				break;
			}

		next_char:
			if (token_append_unicode_char(token,c) != 0)
			{
				*state = eStart;
				return tokNoMem;
			}

			info->m_line = peek_line;
			info->m_col = peek_col;
			*p = peek;
		}
	}
}

static enum eTokenType token_next(struct tokenizer_t* t, struct token_t* token)
{
	enum eTokenType tok;
	enum eState state = eStart;

	t->m_info.m_start_line = t->m_info.m_line;
	t->m_info.m_start_col = t->m_info.m_col;

	do
	{
		const unsigned char* start;
		const unsigned char* p;
		const unsigned char* pe;

		if (!t->m_eof)
		{
			unsigned char c;
			int64_t i = read(t->m_s,&c,1);
			if (i == 0)
				t->m_eof = 1;
			else if (token_append_char(&t->m_buffer,c) != 0)
				return tokNoMem;
		}

		p = start = t->m_buffer.m_str;
		pe = start + t->m_buffer.m_len;

		tok = read_token(&state,&p,pe,t->m_eof,token,&t->m_info);

		if (p == pe)
			t->m_buffer.m_len = 0;
		else if (p > start)
		{
			memmove(t->m_buffer.m_str,t->m_buffer.m_str + (p - start),pe - p);
			t->m_buffer.m_len = pe - p;
		}
	}
	while (tok == tokMore);

	return tok;
}

static struct ast_node_t* ast_free_node(struct ast_node_t* node)
{
	if (node)
	{
		while (node->m_arity--)
			ast_free_node(node->m_params[node->m_arity]);

		free(node);

	}
	return NULL;
}

static struct ast_node_t* ast_syntax_error(enum eASTError error, struct ast_node_t* node, enum eASTError* ast_err)
{
	*ast_err = error;
	return ast_free_node(node);
}

static struct ast_node_t* ast_alloc_node(struct context_t* context, enum eASTNodeType type, struct token_t* token, uint32_t arity, enum eASTError* ast_err)
{
	/* Allocate name first if needed */
	unsigned char* buf = NULL;
	if (token && token->m_len > sizeof(((struct ast_node_t*)0)->m_buf))
	{
		/* Allocate from context stack... */
		buf = context_alloc_string(context,token->m_len);
		if (!buf)
			return ast_syntax_error(AST_ERR_OUTOFMEMORY,NULL,ast_err);
	}

	struct ast_node_t* node = malloc(sizeof(struct ast_node_t) + (arity * sizeof(struct ast_node_t*)));
	if (!node)
		return ast_syntax_error(AST_ERR_OUTOFMEMORY,NULL,ast_err);

	node->m_type = type;
	if (!token || token->m_len == 0)
		node->m_name = NULL;
	else
	{
		if (token->m_len <= sizeof(node->m_buf))
			node->m_name = node->m_buf;
		else
			node->m_name = buf;

		memcpy(node->m_name,token->m_str,token->m_len);
		node->m_name_len = token->m_len;
	}

	node->m_arity = arity;
	if (arity)
		memset(node->m_params,0,arity * sizeof(struct ast_node_t*));

	return node;
}

static struct ast_node_t* ast_atom_to_compound(struct ast_node_t* node, enum eASTError* ast_err)
{
	struct ast_node_t* new_node = realloc(node,sizeof(struct ast_node_t) + sizeof(struct ast_node_t*));
	if (!new_node)
		return ast_syntax_error(AST_ERR_OUTOFMEMORY,node,ast_err);

	new_node->m_type = AST_TYPE_COMPOUND;
	new_node->m_arity = 1;
	new_node->m_params[0] = NULL;

	return new_node;
}

static struct ast_node_t* ast_read_number(struct context_t* context, struct tokenizer_t* t, struct ast_node_t* node, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	if (!node)
	{
		node = ast_alloc_node(context,AST_TYPE_DOUBLE,NULL,0,ast_err);
		if (!node)
			return node;
	}

	if (*next_type == tokFloat)
	{
		errno = 0;
		node->m_double = strtod((const char*)next->m_str,NULL);
		if (node->m_double == HUGE_VAL)
			return ast_syntax_error(AST_SYNTAX_ERR_FLOAT_OVERFLOW,node,ast_err);

		if (node->m_double == 0.0 && errno == ERANGE)
			return ast_syntax_error(AST_SYNTAX_ERR_FLOAT_UNDERFLOW,node,ast_err);
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
		node->m_integer = (int32_t)v;
	}
	else
	{
		size_t i;
		uint32_t v = 0;

		if (*next_type == tokInt)
		{
			for (i=0;i<next->m_len;++i)
			{
				if (v > 0x0CCCCCCC)
					return ast_syntax_error(AST_SYNTAX_ERR_INTEGER_OVERFLOW,node,ast_err);

				v = (v * 10) + (next->m_str[i] - '0');
			}
		}
		else if (*next_type == tokBinaryInt)
		{
			for (i=0;i<next->m_len;++i)
			{
				if (v & 0xC0000000)
					return ast_syntax_error(AST_SYNTAX_ERR_INTEGER_OVERFLOW,node,ast_err);

				v = (v << 1) | (next->m_str[i] - '0');
			}
		}
		else if (*next_type == tokOctalInt)
		{
			for (i=0;i<next->m_len;++i)
			{
				if (v & 0xF0000000)
					return ast_syntax_error(AST_SYNTAX_ERR_INTEGER_OVERFLOW,node,ast_err);

				v = (v << 3) | (next->m_str[i] - '0');
			}
		}
		else /* tokHexInt */
		{
			for (i=0;i<next->m_len;++i)
			{
				if (v & 0xF8000000)
					return ast_syntax_error(AST_SYNTAX_ERR_INTEGER_OVERFLOW,node,ast_err);

				v <<= 4;

				if (next->m_str[i] >= 'a')
					v |= (next->m_str[i] - 'a');
				else if (next->m_str[i] >= 'A')
					v |= (next->m_str[i] - 'A');
				else
					v |= (next->m_str[i] - '0');
			}
		}

		node->m_type = AST_TYPE_INTEGER;
		node->m_integer = (int32_t)v;
	}

	*next_type = token_next(t,next);
	return node;
}

static struct ast_node_t* ast_read_negative(struct context_t* context, struct tokenizer_t* t, struct ast_node_t* node, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	node = ast_read_number(context,t,node,next_type,next,ast_err);
	if (node)
	{
		if (node->m_type == AST_TYPE_DOUBLE)
			node->m_double = -node->m_double;
		else
			node->m_integer = -node->m_integer;
	}
	return node;
}

static struct ast_node_t* ast_read_term(struct context_t* context, struct tokenizer_t* t, unsigned int max_prec, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err);
static struct ast_node_t* ast_read_compound_term(struct context_t* context, struct tokenizer_t* t, struct ast_node_t* node, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err);

static struct ast_node_t* ast_read_arg(struct context_t* context, struct tokenizer_t* t, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	if (*next_type != tokName)
		return ast_read_term(context,t,999,next_type,next,ast_err);

	struct Operator* op;
	struct ast_node_t* node = ast_alloc_node(context,AST_TYPE_ATOM,t,next,0,ast_err);
	if (!node)
		return node;

	*next_type = token_next(t,next);

	if (*next_type == tokOpenCt)
		return ast_read_compound_term(context,t,node,next_type,next,ast_err);

	if (node->m_type == AST_TYPE_ATOM && node->m_name_len == 1 && node->m_name[0] == '-')
	{
		if (*next_type >= tokInt && *next_type <= tokFloat)
			return ast_read_negative(context,t,node,next_type,next,ast_err);
	}

	op = lookup_prefix_op(node);
	if (op && op->m_precedence <= 999 && (op->m_specifier == eFX || op->m_specifier == eFY))
	{
		node = ast_atom_to_compound(node,ast_err);
		if (node)
		{
			node->m_params[0] = ast_read_term(context,t,op->m_specifier == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,ast_err);
			if (!node->m_params[0])
				node = ast_free_node(node);
		}
	}

	return node;
}

static struct ast_node_t* ast_read_compound_term(struct context_t* context, struct tokenizer_t* t, struct ast_node_t* node, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	uint32_t arity = 0;
	uint32_t alloc_arity = 1;
	node = ast_atom_to_compound(node,ast_err);
	if (!node)
		return NULL;

	do
	{
		*next_type = token_next(t,next);

		if (node->m_arity == alloc_arity)
		{
			struct ast_node_t* new_node;
			uint32_t new_arity = 4;
			if (alloc_arity > 2)
				new_arity = alloc_arity * 2;
							
			new_node = realloc(node,sizeof(struct ast_node_t) + (new_arity * sizeof(struct ast_node_t*)));
			if (!new_node)
				return ast_syntax_error(AST_ERR_OUTOFMEMORY,node,ast_err);

			alloc_arity = new_arity;
			node = new_node;
		}

		/*if (arity == ~TAG_MASK)
		{
			TODO: MAX_ARITY
			return ast_syntax_error(AST_SYNTAX_ERR_MAX_ARITY,node,ast_err);
		}*/

		node->m_params[arity] = ast_read_arg(context,t,next_type,next,ast_err);
		if (!node->m_params[arity])
			return ast_free_node(node);

		++arity;
	}
	while (*next_type == tokComma);

	if (*next_type == tokNoMem)
		return ast_syntax_error(AST_ERR_OUTOFMEMORY,node,ast_err);

	if (*next_type != tokClose)
		return ast_syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE,node,ast_err);
	
	*next_type = token_next(t,next);
	return node;
}

static struct ast_node_t* ast_read_list_term(struct context_t* context, struct tokenizer_t* t, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	struct ast_node_t* node = NULL;
	struct ast_node_t** tail = &node;
	
	do
	{
		*tail = ast_alloc_node(context,AST_TYPE_DOT,NULL,2,ast_err);
		if (!(*tail))
			return ast_free_node(node);
		
		*next_type = token_next(t,next);
		
		(*tail)->m_params[0] = ast_read_arg(context,t,next_type,next,ast_err);
		if (!(*tail)->m_params[0])
			return ast_free_node(node);
			
		tail = &((*tail)->m_params[1]);
	}
	while (*next_type == tokComma);

	if (*next_type == tokBar)
	{
		*next_type = token_next(t,next);

		*tail = ast_read_arg(context,t,next_type,next,ast_err);
		if (!(*tail))
			return ast_free_node(node);

		*next_type = token_next(t,next);
	}
	else if (*next_type == tokCloseL)
	{
		/* Append [] */
		*tail = ast_alloc_node(context,AST_TYPE_EMPTY_LIST,NULL,0,ast_err);
		if (!(*tail))
			return ast_free_node(node);
	}
	
	if (*next_type == tokNoMem)
		return ast_syntax_error(AST_ERR_OUTOFMEMORY,node,ast_err);

	if (*next_type != tokCloseL)
		return ast_syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE_L,node,ast_err);
	
	return node;
}

static struct ast_node_t* ast_read_name(struct context_t* context, struct tokenizer_t* t, unsigned int* max_prec, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	struct Operator* op;
	struct ast_node_t* node = ast_alloc_node(context,AST_TYPE_ATOM,t,next,0,ast_err);
	if (!node)
		return node;

	*next_type = token_next(t,next);

	if (*next_type == tokOpenCt)
	{
		*max_prec = 0;
		return ast_read_compound_term(context,t,node,next_type,next,ast_err);
	}

	if (node->m_type == AST_TYPE_ATOM && node->m_name_len == 1 && node->m_name[0] == '-')
	{
		if (*next_type >= tokInt && *next_type <= tokFloat)
		{
			*max_prec = 0;
			return ast_read_negative(context,t,node,next_type,next,ast_err);
		}
	}

	op = lookup_prefix_op(node);
	if (op)
	{
		if (op->m_precedence > *max_prec && (op->m_specifier == eFX || op->m_specifier == eFY))
		{
			node = ast_atom_to_compound(node,ast_err);
			if (node)
			{
				node->m_params[0] = ast_read_term(context,t,op->m_specifier == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,ast_err);
				if (!node->m_params[0])
					node = ast_free_node(node);
				
				*max_prec = op->m_precedence;
			}
			return node;
		}

		if (*max_prec < 1201)
			return ast_syntax_error(AST_SYNTAX_ERR_INVALID_ARG,node,ast_err);

		*max_prec = 1201;
	}
	else
		*max_prec = 0;

	return node;
}

static struct ast_node_t* ast_read_term_base(struct context_t* context, struct tokenizer_t* t, unsigned int* max_prec, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	struct ast_node_t* node = NULL;

	switch (*next_type)
	{
	case tokName:
		return ast_read_name(context,t,max_prec,next_type,next,ast_err);

	case tokVar:
		return ast_alloc_node(context,AST_TYPE_VAR,next,0,ast_err);

	case tokInt:
	case tokBinaryInt:
	case tokOctalInt:
	case tokHexInt:
	case tokCharCode:
	case tokFloat:
		*max_prec = 0;
		return ast_read_number(context,t,node,next_type,next,ast_err);

	case tokDQL:
		if (prolog_flag("double_quotes") == 0 /* atom */)
		{
			/* ISO/IEC 13211-1:1995/Cor.1:2007 */
			return ast_read_name(context,t,max_prec,next_type,next,ast_err);
		}

		*max_prec = 0;
		return ast_alloc_node(context,AST_TYPE_DQL,next,0,ast_err);

	case tokBackQuote:
		if (prolog_flag("back_quotes") == 0 /* atom */)
			return ast_read_name(context,t,max_prec,next_type,next,ast_err);

		*max_prec = 0;
		return ast_alloc_node(context,AST_TYPE_BQL,next,0,ast_err);

	case tokOpen:
	case tokOpenCt:
		*next_type = token_next(t,next);
		node = ast_read_term(context,t,1201,next_type,next,ast_err);
		if (node)
		{
			if (*next_type != tokClose)
				return ast_syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE,node,ast_err);

			*next_type = token_next(t,next);
			*max_prec = 0;
		}
		break;

	case tokOpenL:
		*next_type = token_next(t,next);
		*max_prec = 0;

		if (*next_type != tokCloseL)
			return ast_read_list_term(context,t,next_type,next,ast_err);

		node = ast_alloc_node(context,AST_TYPE_EMPTY_LIST,NULL,0,ast_err);
		if (!node)
			return node;

		*next_type = token_next(t,next);
		return node;
		
	case tokOpenC:
		if (token_append_char(next,'}') != 0)
			return ast_syntax_error(AST_ERR_OUTOFMEMORY,node,ast_err);

		node = ast_alloc_node(context,AST_TYPE_COMPOUND,next,1,ast_err);
		if (!node)
			return node;

		*next_type = token_next(t,next);

		node->m_params[0] = ast_read_term(context,t,1201,next_type,next,ast_err);
		if (!node->m_params[0])
			return ast_free_node(node);

		if (*next_type != tokCloseC)
			return ast_syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE_C,node,ast_err);

		*next_type = token_next(t,next);
		*max_prec = 0;
		return node;

	case tokMore: 
		/* Shouldn't happen... */

	case tokComma:
	case tokClose:
	case tokCloseL:
	case tokCloseC:
	case tokBar:
	case tokEnd:
		return ast_syntax_error(AST_SYNTAX_ERR_UNEXPECTED_TOKEN,node,ast_err);

	case tokInvalidChar:
		return ast_syntax_error(AST_SYNTAX_ERR_INVALID_CHAR,node,ast_err);

	case tokEOF:
		return ast_syntax_error(AST_ERR_EOF,node,ast_err);

	case tokInvalidSeq:
		return ast_syntax_error(AST_SYNTAX_ERR_INVALID_UTF8,node,ast_err);

	case tokInvalidEscape:
		return ast_syntax_error(AST_SYNTAX_ERR_INVALID_ESCAPE,node,ast_err);

	case tokHalfQuote:
		return ast_syntax_error(AST_SYNTAX_ERR_MISSING_QUOTE,node,ast_err);

	case tokNoMem:
		return ast_syntax_error(AST_ERR_OUTOFMEMORY,node,ast_err);
	}

	return node;
}

static struct ast_node_t* ast_read_term(struct context_t* context, struct tokenizer_t* t, unsigned int max_prec, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	unsigned int prev_prec = max_prec;
	struct ast_node_t* node = ast_read_term_base(context,t,&prev_prec,next_type,next,ast_err);

	/* This is precedence climbing, if you're interested */
	for (;;)
	{
		unsigned int right_prec = 0;
		unsigned int left_prec = 0;
		int binary = 0;

		if (*next_type == tokName)
		{
			struct Operator* op = lookup_op(next->m_str,next->m_len);
			if (!op || op->m_precedence > max_prec)
				return node;

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

			if (token_append_char(next,',') != 0)
				return ast_syntax_error(AST_ERR_OUTOFMEMORY,node,ast_err);
		}
		else if (*next_type == tokBar)
		{
			/* ISO/IEC 13211-1:1995/Cor.2:2012 */

			struct Operator* op = lookup_op((const unsigned char*)"|",1);
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

			if (token_append_char(next,'|') != 0)
				return ast_syntax_error(AST_ERR_OUTOFMEMORY,node,ast_err);
		}
		else
			break;

		if (prev_prec > left_prec)
			break;

		struct ast_node_t* next_node = ast_alloc_node(context,AST_TYPE_COMPOUND,next,1 + binary,ast_err);
		if (!next_node)
			return ast_free_node(node);

		next_node->m_params[0] = node;
		node = next_node;
		*next_type = token_next(t,next);

		if (binary)
		{
			next_node->m_params[1] = ast_read_term(context,t,right_prec,next_type,next,ast_err);
			if (!next_node->m_params[1])
				return ast_free_node(node);
		}
	}

	return node;
}

static int emit_term(struct context_t* context, struct stream_t* s, struct tokenizer_t* t, struct token_t* next)
{
	int err = 0;
	enum eASTError ast_err = AST_ERR_NONE;
	enum eTokenType next_type = token_next(t,next);
	struct ast_node_t* node = ast_read_term(context,t,1201,&next_type,next,&ast_err);
	if (node)
	{
		if (next_type == tokNoMem)
			node = ast_syntax_error(AST_ERR_OUTOFMEMORY,node,&ast_err);
		else if (next_type != tokEnd)
			node = ast_syntax_error(AST_SYNTAX_ERR_UNEXPECTED_TOKEN,node,&ast_err);
	}

	if (!node)
	{
		err = -1;
	}
	else
	{
		/* Emit the node */
	}

	return err;
}

int read_term(struct context_t* context, struct stream_t* s)
{
	struct tokenizer_t t = { .m_s = s, .m_info = { .m_line = 1 } };
	struct token_t next = {0};
	int err = emit_term(context,s,&t,&next);
	if (err)
	{
		/* Throw the error */
	}

	free(next.m_str);

	return 0;
}

int consult(struct context_t* context, struct stream_t* s)
{
	int err = 0;
	struct tokenizer_t t = { .m_s = s, .m_info = { .m_line = 1 } };
	struct token_t next = {0};

	for (;;)
	{
		err = emit_term(context,s,&t,&next);
		if (err)
		{
			/* TODO: Throw the error */

			break;
		}

		/* Assert the node */
	}

	free(next.m_str);

	return err;
}

int consult_bulk(struct context_t* context, struct stream_t* s)
{
	int err = 0;
	struct tokenizer_t t = { .m_s = s, .m_info = { .m_line = 1 } };
	struct token_t next = {0};

	for (;;)
	{
		err = emit_term(context,s,&t,&next);
		if (err)
		{
			/* TODO: Report the error */

			/* Skip to tokEnd or tokEOF */
			enum eTokenType next_type;
			do
			{
				next_type = token_next(&t,&next);
			}
			while (next_type != tokEnd && next_type != tokEOF);
		}

		if (!err)
		{
			/* Assert the node */
		}
	}

	free(next.m_str);

	return err;
}
