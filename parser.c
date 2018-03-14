
#include "context.h"
#include "throw.h"
#include "stream.h"

#include <string.h>
#include <errno.h>
#include <assert.h>

enum eTokenType
{
	tokMore = 0,
	tokNoMem,
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

struct parser_t
{
	struct stream_t*    m_s;
	struct token_t      m_buffer;
	struct token_info_t m_info;
	int                 m_eof;
};

enum eAstType
{
	AST_TYPE_VAR,
	AST_TYPE_COMPOUND,
	AST_TYPE_DOUBLE,
	AST_TYPE_INTEGER,
	AST_TYPE_ATOM,
	AST_TYPE_CHARS,
	AST_TYPE_CODES,
};

struct ast_node_t
{
	union box_t        m_boxed;
	uint64_t           m_arity;
	enum eAstType      m_type;
	struct ast_node_t* m_params[];
};

enum eASTError
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
};

enum eCharMax
{
	char_max = 0x1FFFF, /* Greatest valid unicode char */
	char_more = char_max+1,
	char_eof = char_max+2,
	char_ilseq = char_max+3
};

static int token_append_char(struct context_t* context, struct token_t* token, unsigned char c)
{
	if (token->m_alloc == token->m_len)
	{
		size_t new_size = (token->m_alloc == 0 ? 64 : token->m_alloc * 2);
		unsigned char* new_str = stack_realloc(&context->m_scratch_stack,token->m_str,token->m_alloc,new_size);
		if (!new_str)
			return -1;

		token->m_alloc = new_size;
		token->m_str = new_str;
	}

	token->m_str[token->m_len++] = c;
	return 0;
}

static int token_append_unicode_char(struct context_t* context, struct token_t* token, uint32_t unicode_char)
{
	if (unicode_char <= 0x7F)
	{
		return token_append_char(context,token,(unsigned char)unicode_char);
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
			if (token_append_char(context,token,chars[i]) == -1)
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
		else
		{
			unsigned int i;
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

static uint32_t token_get_char_conv(struct context_t* context, const unsigned char** p, const unsigned char* pe, int eof, size_t* line, size_t* col)
{
	uint32_t c = token_get_char(p,pe,eof,line,col);
	if (context->m_module->m_flags.char_conversion && c <= char_max)
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

static int token_meta_char(struct context_t* context, uint32_t meta, struct token_t* token)
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

	if (token_append_char(context,token,c) == -1)
		return -1;

	return 1;
}

static enum eTokenType parse_token(struct context_t* context, enum eState* state, const unsigned char** p, const unsigned char* pe, int eof, struct token_t* token, struct token_info_t* info)
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

		c = token_get_char_conv(context,p,pe,eof,&info->m_line,&info->m_col);
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
		c = token_get_char_conv(context,p,pe,eof,&info->m_line,&info->m_col);
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
					if (token_append_char(context,token,(unsigned char)c) != 0)
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
			c = token_get_char_conv(context,p,pe,eof,&info->m_line,&info->m_col);
			if (c == char_more)
				return tokMore;
		}

	multi_comment:
		peek = *p;
		peek_line = info->m_line;
		peek_col = info->m_col;

	case eMultiComment1:
		c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c != '*')
		{
			if (token_append_char(context,token,'/') != 0)
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
		c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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

		if (token_append_char(context,token,'.') != 0)
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
				if (*state == eSingleQuote)
					return tokMissingSQ;
				else if (*state == eDoubleQuote)
					return tokMissingDQ;
				return tokMissingBQ;

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

				if (!token_meta_char(context,c,token))
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
				if (token_append_char(context,token,(unsigned char)c) != 0)
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
				if (token_append_char(context,token,(unsigned char)c) != 0)
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
				if (token_append_char(context,token,(unsigned char)c) != 0)
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
				if (token_append_unicode_char(context,token,c) != 0)
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
					if (token_append_unicode_char(context,token,meta) != 0)
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
					if (token_append_unicode_char(context,token,meta) != 0)
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
		c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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
					if (token_append_char(context,token,'\'') != 0)
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

				if (token_meta_char(context,c,token))
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
				if (token_append_unicode_char(context,token,c) != 0)
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
			c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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
			c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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
			c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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
				if (token_append_unicode_char(context,token,meta) != 0)
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
				if (token_append_unicode_char(context,token,meta) != 0)
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
		c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c < '0' || c > '9')
		{
			*state = eStart;
			return tokInt;
		}

		if (token_append_char(context,token,'.') != 0)
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
			if (token_append_char(context,token,'e') != 0)
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
			uint32_t c2 = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
			if (c2 == char_more)
				return tokMore;

			if (c2 >= '0' && c2 <= '9')
			{
				if (token_append_char(context,token,'e') != 0 || token_append_char(context,token,(unsigned char)c) != 0)
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
			c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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
				/* Never going to happen */
				break;
			}

		next_char:
			if (token_append_unicode_char(context,token,c) != 0)
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

static enum eTokenType token_next(struct context_t* context, struct parser_t* parser, struct token_t* token)
{
	enum eTokenType tok;
	enum eState state = eStart;

	parser->m_info.m_start_line = parser->m_info.m_line;
	parser->m_info.m_start_col = parser->m_info.m_col;

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
			else if (token_append_char(context,&parser->m_buffer,c) != 0)
				return tokNoMem;
		}

		p = start = parser->m_buffer.m_str;
		pe = start + parser->m_buffer.m_len;

		tok = parse_token(context,&state,&p,pe,parser->m_eof,token,&parser->m_info);

		if (p == pe)
			parser->m_buffer.m_len = 0;
		else if (p > start)
		{
			memmove(parser->m_buffer.m_str,parser->m_buffer.m_str + (p - start),pe - p);
			parser->m_buffer.m_len = pe - p;
		}
	}
	while (tok == tokMore);

	return tok;
}

static struct ast_node_t* syntax_error(enum eASTError error, enum eASTError* ast_err)
{
	*ast_err = error;
	return NULL;
}

static struct ast_node_t* atom_to_compound(struct context_t* context, struct ast_node_t* node, enum eASTError* ast_err)
{
	struct ast_node_t* new_node = stack_realloc(&context->m_scratch_stack,node,sizeof(struct ast_node_t),sizeof(struct ast_node_t) + sizeof(struct ast_node_t*));
	if (!new_node)
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	new_node->m_type = AST_TYPE_COMPOUND;
	new_node->m_arity = 1;
	new_node->m_params[0] = NULL;

	return new_node;
}

static struct ast_node_t* parse_number(struct context_t* context, struct parser_t* parser, struct ast_node_t* node, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err, int neg)
{
	if (!node)
	{
		node = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t));
		if (!node)
			return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

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
		node->m_boxed = box_double(dval);
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
		node->m_boxed = box_int32(v);
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

			if (v > (UINT64_C(0x7FFFFFFF) + neg))
				return syntax_error(neg ? AST_ERR_MIN_INTEGER : AST_ERR_MAX_INTEGER,ast_err);
		}

		node->m_type = AST_TYPE_INTEGER;

		if (v == UINT64_C(0x80000000))
			node->m_boxed = box_int32(INT32_MIN);
		else
			node->m_boxed = box_int32(v);
	}

	*next_type = token_next(context,parser,next);
	return node;
}

static struct ast_node_t* parse_negative(struct context_t* context, struct parser_t* parser, struct ast_node_t* node, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	node = parse_number(context,parser,node,next_type,next,ast_err,1);
	if (node)
	{
		if (node->m_type == AST_TYPE_DOUBLE)
			node->m_boxed = box_double(-unbox_double(&node->m_boxed));
		else
		{
			int32_t v = unbox_int32(&node->m_boxed);
			if (v > 0)
				node->m_boxed = box_int32(-v);
		}
	}
	return node;
}

static struct ast_node_t* parse_term(struct context_t* context, struct parser_t* parser, unsigned int max_prec, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err);
static struct ast_node_t* parse_compound_term(struct context_t* context, struct parser_t* parser, struct ast_node_t* node, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err);

static struct ast_node_t* parse_arg(struct context_t* context, struct parser_t* parser, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	struct ast_node_t* node;
	struct operator_t* op;

	if (*next_type != tokName)
		return parse_term(context,parser,999,next_type,next,ast_err);

	node = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t));
	if (!node)
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	node->m_arity = 0;
	node->m_type = AST_TYPE_ATOM;

	node->m_boxed.m_uval = BOX_TAG_ATOM;
	if (!box_string(context,&node->m_boxed,next->m_str,next->m_len))
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	*next_type = token_next(context,parser,next);

	if (*next_type == tokOpenCt)
		return parse_compound_term(context,parser,node,next_type,next,ast_err);

	if (node->m_boxed.m_uval == (BOX_ATOM_EMBED_1('-')))
	{
		if (*next_type >= tokInt && *next_type <= tokFloat)
			return parse_negative(context,parser,node,next_type,next,ast_err);
	}

	op = lookup_prefix_op(context,&node->m_boxed);
	if (op && op->m_precedence <= 999 && (op->m_specifier == eFX || op->m_specifier == eFY))
	{
		node = atom_to_compound(context,node,ast_err);
		if (node)
		{
			node->m_params[0] = parse_term(context,parser,op->m_specifier == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,ast_err);
			if (!node->m_params[0])
				node = NULL;
		}
	}

	return node;
}

static struct ast_node_t* parse_compound_term(struct context_t* context, struct parser_t* parser, struct ast_node_t* node, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	uint64_t arity = 0;
	uint64_t alloc_arity = 1;
	node = atom_to_compound(context,node,ast_err);
	if (!node)
		return NULL;

	do
	{
		*next_type = token_next(context,parser,next);

		if (node->m_arity == alloc_arity)
		{
			struct ast_node_t* new_node;
			uint32_t new_arity = 4;
			if (alloc_arity > 2)
				new_arity = alloc_arity * 2;
							
			new_node = stack_realloc(&context->m_scratch_stack,node,sizeof(struct ast_node_t) + (alloc_arity * sizeof(struct ast_node_t*)),sizeof(struct ast_node_t) + (new_arity * sizeof(struct ast_node_t*)));
			if (!new_node)
				return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

			alloc_arity = new_arity;
			node = new_node;
		}

		if (arity > MAX_ARITY)
			return syntax_error(AST_ERR_MAX_ARITY,ast_err);

		node->m_params[arity] = parse_arg(context,parser,next_type,next,ast_err);
		if (!node->m_params[arity])
			return NULL;

		++arity;
	}
	while (*next_type == tokComma);

	if (*next_type == tokNoMem)
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	if (*next_type != tokClose)
		return syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE,ast_err);
	
	*next_type = token_next(context,parser,next);
	return node;
}

static struct ast_node_t* parse_list_term(struct context_t* context, struct parser_t* parser, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	struct ast_node_t* node = NULL;
	struct ast_node_t** tail = &node;
	
	do
	{
		*tail = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t) + (2*sizeof(struct ast_node_t*)));
		if (!(*tail))
			return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

		(*tail)->m_type = AST_TYPE_COMPOUND;
		(*tail)->m_boxed.m_uval = BOX_ATOM_EMBED_1('.');
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
		*tail = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t));
		if (!(*tail))
			return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

		(*tail)->m_type = AST_TYPE_ATOM;
		(*tail)->m_boxed.m_uval = BOX_ATOM_EMBED_2('[',']');
		(*tail)->m_arity = 0;
	}
	
	if (*next_type == tokNoMem)
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	if (*next_type != tokCloseL)
		return syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE_L,ast_err);
	
	return node;
}

static struct ast_node_t* parse_name(struct context_t* context, struct parser_t* parser, unsigned int* max_prec, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	struct operator_t* op;
	struct ast_node_t* node = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t));
	if (!node)
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	node->m_arity = 0;
	node->m_type = AST_TYPE_ATOM;

	node->m_boxed.m_uval = BOX_TAG_ATOM;
	if (!box_string(context,&node->m_boxed,next->m_str,next->m_len))
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	*next_type = token_next(context,parser,next);
	*max_prec = 0;

	if (*next_type == tokOpenCt)
		return parse_compound_term(context,parser,node,next_type,next,ast_err);

	if (node->m_boxed.m_uval == (BOX_ATOM_EMBED_1('-')))
	{
		if (*next_type >= tokInt && *next_type <= tokFloat)
			return parse_negative(context,parser,node,next_type,next,ast_err);
	}

	op = lookup_prefix_op(context,&node->m_boxed);
	if (op)
	{
		if (op->m_precedence > *max_prec && (op->m_specifier == eFX || op->m_specifier == eFY))
		{
			node = atom_to_compound(context,node,ast_err);
			if (node)
			{
				node->m_params[0] = parse_term(context,parser,op->m_specifier == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,ast_err);
				if (!node->m_params[0])
					node = NULL;

				*max_prec = op->m_precedence;
			}
			return node;
		}

		if (*max_prec < 1201)
			return syntax_error(AST_SYNTAX_ERR_INVALID_ARG,ast_err);

		*max_prec = 1201;
	}

	return node;
}

static struct ast_node_t* parse_chars_and_codes(struct context_t* context, int chars, struct token_t* token, enum eASTError* ast_err)
{
	/* TODO: Check for utf8 chars token and split into multiple lists */

	struct ast_node_t* node = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t));
	if (!node)
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	node->m_type = chars ? AST_TYPE_CHARS : AST_TYPE_CODES;
	node->m_arity = 0;

	node->m_boxed.m_uval = (node->m_type == AST_TYPE_CHARS ? BOX_TAG_CHARS : BOX_TAG_CODES);
	if (!box_string(context,&node->m_boxed,token->m_str,token->m_len))
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	return node;
}

static struct ast_node_t* parse_term_base(struct context_t* context, struct parser_t* parser, unsigned int* max_prec, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	struct ast_node_t* node = NULL;

	switch (*next_type)
	{
	case tokName:
		return parse_name(context,parser,max_prec,next_type,next,ast_err);

	case tokVar:
		node = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t));
		if (!node)
			return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

		node->m_type = AST_TYPE_VAR;
		node->m_arity = -1;

		node->m_boxed.m_uval = BOX_TAG_ATOM;
		if (!box_string(context,&node->m_boxed,next->m_str,next->m_len))
			return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);
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
		if (context->m_module->m_flags.double_quotes == 0 /* atom */)
		{
			/* ISO/IEC 13211-1:1995/Cor.1:2007 */
			return parse_name(context,parser,max_prec,next_type,next,ast_err);
		}

		node = parse_chars_and_codes(context,context->m_module->m_flags.double_quotes == 1 ? 1 : 0,next,ast_err);
		break;

	case tokBackQuote:
		if (context->m_module->m_flags.back_quotes == 0 /* atom */)
			return parse_name(context,parser,max_prec,next_type,next,ast_err);

		node = parse_chars_and_codes(context,context->m_module->m_flags.back_quotes == 1 ? 1 : 0,next,ast_err);
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

		node = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t));
		if (!node)
			return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

		node->m_type = AST_TYPE_ATOM;
		node->m_boxed.m_uval = BOX_ATOM_EMBED_2('[',']');
		node->m_arity = 0;
		break;
		
	case tokOpenC:
		node = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t) + sizeof(struct ast_node_t*));
		if (!node)
			return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

		node->m_type = AST_TYPE_COMPOUND;
		node->m_boxed.m_uval = BOX_ATOM_EMBED_2('{','}');
		node->m_arity = 1;

		*next_type = token_next(context,parser,next);

		node->m_params[0] = parse_term(context,parser,1201,next_type,next,ast_err);
		if (!node->m_params[0])
			return NULL;

		if (*next_type != tokCloseC)
			return syntax_error(AST_SYNTAX_ERR_MISSING_CLOSE_C,ast_err);
		break;

	case tokMore: 
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

	case tokNoMem:
		return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

	case tokEOF:
		return NULL;
	}

	*max_prec = 0;
	*next_type = token_next(context,parser,next);
	return node;
}

static struct ast_node_t* parse_term(struct context_t* context, struct parser_t* parser, unsigned int max_prec, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	unsigned int prev_prec = max_prec;
	struct ast_node_t* node = parse_term_base(context,parser,&prev_prec,next_type,next,ast_err);
	if (!node)
		return NULL;

	/* This is precedence climbing, if you're interested */
	for (;;)
	{
		unsigned int right_prec = 0;
		unsigned int left_prec = 0;
		int binary = 0;
		union box_t name;

		if (*next_type == tokName)
		{
			struct operator_t* op;

			name.m_uval = BOX_TAG_ATOM;
			if (!box_string(context,&name,next->m_str,next->m_len))
				return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

			op = lookup_op(context,&name);
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

			name.m_uval = BOX_ATOM_EMBED_1(',');
		}
		else if (*next_type == tokBar)
		{
			/* ISO/IEC 13211-1:1995/Cor.2:2012 */
			struct operator_t* op;
			name.m_uval = BOX_ATOM_EMBED_1('|');

			op = lookup_op(context,&name);
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
			struct ast_node_t* next_node = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t) + ((1 + binary) * sizeof(struct ast_node_t*)));
			if (!next_node)
				return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

			node->m_type = AST_TYPE_COMPOUND;
			node->m_boxed = name;
			node->m_arity = 1 + binary;
			next_node->m_params[0] = node;
			node = next_node;
		
			*next_type = token_next(context,parser,next);

			if (binary)
			{
				next_node->m_params[1] = parse_term(context,parser,right_prec,next_type,next,ast_err);
				if (!next_node->m_params[1])
					return NULL;
			}
		}
	}

	return node;
}

enum eEmitStatus
{
	EMIT_OK = 0,
	EMIT_EOF,
	EMIT_THROW,
	EMIT_OUT_OF_MEM
};

static enum eEmitStatus emit_node_vars(struct context_t* context, struct var_info_t** varinfo, struct ast_node_t* node)
{
	enum eEmitStatus status = EMIT_OK;
	if (node->m_type == AST_TYPE_VAR)
	{
		size_t var_count = (*varinfo) ? (*varinfo)->m_count : 0;
		size_t i;
		for (i = 0; i < var_count; ++i)
		{
			if (node->m_boxed.m_uval == (*varinfo)->m_vars[i].m_name.m_uval)
			{
				node->m_arity = i;
				break;
			}
		}

		if (i == var_count)
		{
			if (var_count >= ~BOX_TAG_MASK)
			{
				/* There is not an ISO standard error for 'too many vars'
				 * but we will be out of memory soon anyway, so that will do */
				return EMIT_OUT_OF_MEM;
			}

			if (*varinfo)
			{
				struct var_info_t* new_varinfo = stack_realloc(&context->m_exec_stack,*varinfo,sizeof(struct var_info_t) + (var_count * sizeof(struct var_t)),sizeof(struct var_info_t) + ((var_count+1) * sizeof(struct var_t)));
				if (!new_varinfo)
					return EMIT_OUT_OF_MEM;

				*varinfo = new_varinfo;
			}
			else
			{
				*varinfo = stack_malloc(&context->m_exec_stack,sizeof(struct var_info_t) + sizeof(struct var_t));
				if (!(*varinfo))
					return EMIT_OUT_OF_MEM;

				(*varinfo)->m_count = 0;
			}

			(*varinfo)->m_vars[var_count].m_name = node->m_boxed;
			(*varinfo)->m_vars[var_count].m_value = NULL;
			++(*varinfo)->m_count;
		}
	}
	else if (node->m_type == AST_TYPE_COMPOUND)
	{
		uint64_t i;
		for (i = 0; !status && i < node->m_arity; ++i)
			status = emit_node_vars(context,varinfo,node->m_params[i]);
	}

	return status;
}

static enum eEmitStatus emit_compound(struct stack_t** stack, uint64_t functor, uint64_t arity)
{
	enum eEmitStatus status;
	if (arity < 16 && (functor & BOX_TAG_ATOM_EMBED) == BOX_TAG_ATOM_EMBED)
	{
		uint64_t len = (functor & ((uint64_t)7 << 40)) << 4;
		uint64_t val = BOX_TAG_COMPOUND_EMBED | (functor & ~(UINT64_C(0xFFFFFF) << 40)) | len | (arity << 40);
		status = (stack_push(stack,val) == -1 ? EMIT_OUT_OF_MEM : EMIT_OK);
	}
	else
	{
		status = (stack_push(stack,BOX_TAG_COMPOUND | arity) == -1 ? EMIT_OUT_OF_MEM : EMIT_OK);
		if (!status)
			status = (stack_push(stack,functor) == -1 ? EMIT_OUT_OF_MEM : EMIT_OK);
	}
	return status;
}

static enum eEmitStatus emit_node_value(struct context_t* context, struct ast_node_t* node)
{
	enum eEmitStatus status = EMIT_OK;
	if (node->m_type == AST_TYPE_VAR)
		status = (stack_push(&context->m_exec_stack,BOX_TAG_VAR | node->m_arity) == -1 ? EMIT_OUT_OF_MEM : EMIT_OK);
	else if (node->m_type == AST_TYPE_COMPOUND)
	{
		status = emit_compound(&context->m_exec_stack,node->m_boxed.m_uval,node->m_arity);
		if (!status)
		{
			uint64_t i;
			for (i = 0; !status && i < node->m_arity; ++i)
				status = emit_node_value(context,node->m_params[i]);
		}
	}
	else
		status = (stack_push(&context->m_exec_stack,node->m_boxed.m_uval) == -1 ? EMIT_OUT_OF_MEM : EMIT_OK);

	return status;
}

static enum eEmitStatus emit_error(struct context_t* context, enum eASTError ast_err, struct parser_t* parser, struct term_t* term)
{
	if (ast_err >= AST_SYNTAX_ERR_MISSING_CLOSE)
	{
		enum eEmitStatus status = emit_compound(&context->m_scratch_stack,BOX_ATOM_EMBED_5('e','r','r','o','r'),2);
		if (!status)
			status = emit_compound(&context->m_scratch_stack,BUILTIN_ATOM(syntax_error),1);
		if (!status && ast_err < AST_SYNTAX_ERR_INVALID_ARG)
			status = emit_compound(&context->m_scratch_stack,BUILTIN_ATOM(missing),1);
		if (!status)
		{
			/* TODO: line info */
			status = (stack_push(&context->m_scratch_stack,BOX_ATOM_EMBED_4('i','n','f','o')) == -1 ? EMIT_OUT_OF_MEM : EMIT_OK);
		}

		if (status != EMIT_OK)
			return status;
	}

	switch (ast_err)
	{
	case AST_ERR_FLOAT_OVERFLOW:
		return (throw_evaluation_error(context,BUILTIN_ATOM(float_overflow)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_ERR_FLOAT_UNDERFLOW:
		return (throw_evaluation_error(context,BUILTIN_ATOM(underflow)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_ERR_MAX_INTEGER:
		return (throw_representation_error(context,BUILTIN_ATOM(max_integer)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_ERR_MIN_INTEGER:
		return (throw_representation_error(context,BUILTIN_ATOM(min_integer)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_ERR_MAX_ARITY:
		return (throw_representation_error(context,BUILTIN_ATOM(max_arity)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_MISSING_CLOSE:
		return (stack_push(&context->m_scratch_stack,BOX_ATOM_EMBED_1(')')) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_MISSING_CLOSE_L:
		return (stack_push(&context->m_scratch_stack,BOX_ATOM_EMBED_1(']')) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_MISSING_CLOSE_C:
		return (stack_push(&context->m_scratch_stack,BOX_ATOM_EMBED_1('}')) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_MISSING_SQ:
		return (stack_push(&context->m_scratch_stack,BOX_ATOM_EMBED_1('\'')) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_MISSING_DQ:
		return (stack_push(&context->m_scratch_stack,BOX_ATOM_EMBED_1('"')) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_MISSING_BQ:
		return (stack_push(&context->m_scratch_stack,BOX_ATOM_EMBED_1('`')) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_INVALID_ARG:
		return (stack_push(&context->m_scratch_stack,BUILTIN_ATOM(invalid_argument)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_UNEXPECTED_TOKEN:
		return (stack_push(&context->m_scratch_stack,BUILTIN_ATOM(unexpected_token)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_INVALID_CHAR:
		return (stack_push(&context->m_scratch_stack,BUILTIN_ATOM(invalid_character)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_INVALID_ESCAPE:
		return (stack_push(&context->m_scratch_stack,BUILTIN_ATOM(invalid_escape)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	case AST_SYNTAX_ERR_INVALID_UTF8:
		return (stack_push(&context->m_scratch_stack,BUILTIN_ATOM(invalid_utf8)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);

	default:
		return EMIT_OUT_OF_MEM;
	}
}

static enum eEmitStatus emit_term(struct context_t* context, struct term_t* term, struct parser_t* parser)
{
	enum eEmitStatus status = EMIT_OK;
	enum eASTError ast_err = AST_ERR_NONE;
	uint64_t stack_base = stack_top(context->m_exec_stack);
	uint64_t scratch_base = stack_top(context->m_scratch_stack);
	struct string_ptr_t* prev_strings = context->m_strings;

	struct token_t next = {0};
	enum eTokenType next_type = token_next(context,parser,&next);
	struct ast_node_t* node = parse_term(context,parser,1201,&next_type,&next,&ast_err);
	if (!node || next_type != tokEnd)
	{
		/* Reset stacks */
		context->m_strings = prev_strings;
		stack_reset(&context->m_exec_stack,stack_base);
		stack_reset(&context->m_scratch_stack,scratch_base);

		/* Reset token because we just trashed it */
		next.m_alloc = 0;
		next.m_len = 0;
		next.m_str = NULL;

		if (node)
		{
			/* Missing . */
			status = emit_compound(&context->m_scratch_stack,BUILTIN_ATOM(syntax_error),1);
			if (!status)
				status = emit_compound(&context->m_scratch_stack,BUILTIN_ATOM(missing),1);
			if (!status)
				status = (stack_push(&context->m_scratch_stack,BOX_ATOM_EMBED_1('.')) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);
		}
		else if (next_type != tokEOF)
			status = emit_error(context,ast_err,parser,term);
		else
			status = EMIT_EOF;

		/* Skip to what looks like a complete term */
		while (next_type != tokEnd && next_type != tokEOF)
			next_type = token_next(context,parser,&next);
	}
	else
	{
		/* Emit the node */
		term->m_vars = NULL;
		status = emit_node_vars(context,&term->m_vars,node);
		if (!status)
		{
			term->m_value = stack_top_ptr(context->m_exec_stack);
			status = emit_node_value(context,node);
		}
	}

	/* Reset scratch stack */
	stack_reset(&context->m_scratch_stack,scratch_base);

	/* Emit out of memory last */
	if (status == EMIT_OUT_OF_MEM)
	{
		/* Reset exec stack */
		context->m_strings = prev_strings;
		stack_reset(&context->m_exec_stack,stack_base);

		status = (throw_oom_error(context) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);
	}

	return status;
}

static int include(struct context_t* context, struct term_t* term);

/* 'Do' a directive */
static int directive(struct context_t* context, struct term_t* term)
{
	switch (term->m_value->m_uval & BOX_TAG_MASK)
	{
	case BOX_TAG_VAR:
		return throw_instantiation_error(context);

	case BOX_TAG_COMPOUND:
	case BOX_TAG_ATOM:
		break;

	default:
		return throw_type_error(context,BUILTIN_ATOM(callable),term->m_value);
	}

	if (term->m_value->m_uval == BOX_COMPOUND_EMBED_2(3,'o','p'))
	{
		++term->m_value;
		return op_3(context,term);
	}

	if (term->m_value->m_uval == (UINT64_C(0xFFF6) << 48 | 1))
	{
		switch (term->m_value[1].m_uval)
		{
		case BUILTIN_ATOM(dynamic):
		case BUILTIN_ATOM(multifile):
		case BUILTIN_ATOM(discontiguous):
			++term->m_value;
			return clause_directive(context,term);

		case BUILTIN_ATOM(include):
			term->m_value += 2;
			return include(context,term);

		case BUILTIN_ATOM(ensure_loaded):
			term->m_value += 2;
			return ensure_loaded(context,term);

		default:
			break;
		}
	}
	else if (term->m_value->m_uval == (UINT64_C(0xFFF6) << 48 | 2))
	{
		switch (term->m_value[1].m_uval)
		{
		case BUILTIN_ATOM(char_conversion):
			term->m_value += 2;
			return char_conversion_2(context,term);

		case BUILTIN_ATOM(set_prolog_flag):
			term->m_value += 2;
			return set_prolog_flag_2(context,term);

		default:
			break;
		}
	}

	if (context->m_module->m_flags.unknown == 2)
		return throw_existence_error(context,BUILTIN_ATOM(procedure),term->m_value);

	/* TODO: Warn? */

	return 0;
}

static int load_file(struct context_t* context, struct stream_t* s)
{
	uint64_t original_stack_base = stack_top(context->m_exec_stack);
	uint64_t stack_base = original_stack_base;
	struct string_ptr_t* original_prev_strings = context->m_strings;
	struct string_ptr_t* prev_strings = original_prev_strings;
	int failed = 0;
	struct parser_t parser = {0};
	parser.m_s = s;
	parser.m_info.m_line = 1;

	while (failed != -1)
	{
		struct term_t term = {0};
		enum eEmitStatus status = emit_term(context,&term,&parser);
		if (status == EMIT_EOF)
			break;

		if (status == EMIT_OK)
		{
			if (term.m_value[0].m_uval == BOX_COMPOUND_EMBED_2(1,':','-'))
			{
				/* Check now for :- initialization/1 */
				if (term.m_value[1].m_uval == (UINT64_C(0xFFF6) << 48 | 1) &&
						term.m_value[2].m_uval == BUILTIN_ATOM(initialization))
				{
					term.m_value += 3;
					int err = compile_initializer(context,&term,stack_base);
					if (err == -1)
						status = EMIT_OUT_OF_MEM;
					else if (err)
						status = EMIT_THROW;
					else
					{
						/* compile_initializer will write op codes to the exec stack, so don't pop them */
						stack_base = original_stack_base = stack_top(context->m_exec_stack);
					}
				}
				else
				{
					/* Directive, skip to first arg */
					++term.m_value;
					int err = directive(context,&term);
					if (err == -1)
						status = EMIT_OUT_OF_MEM;
					else if (err)
						status = EMIT_THROW;
				}
			}
			else if (!failed)
			{
				/* Assert the term */
				int err = assert_clause(context,&term,1);
				if (err == -1)
					status = EMIT_OUT_OF_MEM;
				else if (err)
					status = EMIT_THROW;
			}
		}

		if (status == EMIT_THROW)
		{
			/* TODO: Report the error term, or throw it? */
			failed = 1;
		}

		if (status == EMIT_OUT_OF_MEM)
			failed = -1;

		/* Reset exec and scratch stack */
		context->m_strings = prev_strings;
		stack_reset(&context->m_exec_stack,stack_base);
	}

	/* Hard reset the stacks because we may have allocated a lot */
	context->m_strings = original_prev_strings;
	stack_reset(&context->m_exec_stack,original_stack_base);
	stack_compact(context->m_exec_stack);
	stack_compact(context->m_scratch_stack);

	return failed ? -1 : 0;
}

static int include(struct context_t* context, struct term_t* term)
{
	int err = 0;
	struct stream_t* s = NULL;

	/* TODO: Open stream term->m_value[0] */

	/* TODO: Twiddle with context */

	err = load_file(context,s);
	if (!err)
	{
		/* TODO: Untwiddle context */
	}

	return err;
}

int consult(struct context_t* context, struct stream_t* s)
{
	return load_file(context,s);
}

int read_term(struct context_t* context, struct stream_t* s, struct term_t* term)
{
	enum eEmitStatus status = EMIT_OK;
	struct parser_t parser = {0};
	parser.m_s = s;
	parser.m_info.m_line = 1;

	/* TODO: Check for permission errors first */

	status = emit_term(context,term,&parser);
	if (status == EMIT_EOF)
	{
		status = emit_compound(&context->m_scratch_stack,BUILTIN_ATOM(syntax_error),1);
		if (!status)
			status = (stack_push(&context->m_scratch_stack,BUILTIN_ATOM(past_end_of_stream)) == -1 ? EMIT_OUT_OF_MEM : EMIT_THROW);
	}

	if (status == EMIT_THROW)
	{
		/* TODO: Throw the error term */

		return 1;
	}

	if (status == EMIT_OUT_OF_MEM)
		return -1;

	return 0;
}
