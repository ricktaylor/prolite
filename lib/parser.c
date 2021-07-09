
#include "types.h"
#include "stream.h"

#include <string.h>
#include <errno.h>
#include <math.h>
#include <assert.h>
#include <stdarg.h>

#ifdef _MSC_VER
#include <float.h>
#define isnan _isnan
#endif

uint32_t convert_char(struct context_t* context, uint32_t in_char);

/* Try to find a infix/suffix op, otherwise find prefix */
struct operator_t* lookup_op(struct context_t* context, const unsigned char* name, size_t name_len);

/* Try to find a prefix op, otherwise find infix/suffix */
struct operator_t* lookup_prefix_op(struct context_t* context, const unsigned char* name, size_t name_len);

enum eTokenType
{
	tokNeedMore = 0,
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

struct parser_t
{
	struct stream_t*    m_s;
	struct token_t      m_buffer;
	struct line_info_t  m_line_info;
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
	union
	{
		const unsigned char* m_str;
		double               m_dbl;
		uint64_t             m_u64;
	};
	size_t             m_str_len;
	size_t             m_arity;
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
	CHAR_MAX_VALID = 0x1FFFF, /* Greatest valid unicode char */
	CHAR_NEED_MORE,
	CHAR_EOF,
	CHAR_ILLEGAL_SEQ
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
	unsigned int count = 0;
	uint32_t val;
	const unsigned char* c = *p;

	if (c == pe)
	{
		if (eof)
			return CHAR_EOF;

		return CHAR_NEED_MORE;
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
			unsigned int i;
			for (i=1;i<count;++i)
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

static uint32_t token_get_char_conv(struct context_t* context, const unsigned char** p, const unsigned char* pe, int eof, size_t* line, size_t* col)
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

static enum eTokenType parse_token(struct context_t* context, enum eState* state, const unsigned char** p, const unsigned char* pe, int eof, struct token_t* token, struct line_info_t* info)
{
	const unsigned char* peek = *p;
	size_t peek_line = info->m_end_line;
	size_t peek_col = info->m_end_col;
	uint32_t c;
	uint32_t meta;

	switch (*state)
	{
	case eStart:
		/* Clear the current token */
		token->m_len = 0;

		c = token_get_char_conv(context,p,pe,eof,&info->m_end_line,&info->m_end_col);
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
		c = token_get_char_conv(context,p,pe,eof,&info->m_end_line,&info->m_end_col);
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
					info->m_start_line = info->m_end_line;
					info->m_start_col = info->m_end_col;
					break;

				case eaShortName:
					*state = eStart;
					return token_append_char(context,token,(unsigned char)c) != 0 ? tokNoMem : tokName;

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
					info->m_start_line = info->m_end_line;
					info->m_start_col = info->m_end_col;
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
					info->m_start_line = info->m_end_line;
					info->m_start_col = info->m_end_col;
					*state = eLayout;
				}
				else if (c != '*')
					*state = eMultiComment2;
			}

			/* Get the next character */
			c = token_get_char_conv(context,p,pe,eof,&info->m_end_line,&info->m_end_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;
		}

	multi_comment:
		peek = *p;
		peek_line = info->m_end_line;
		peek_col = info->m_end_col;

	case eMultiComment1:
		c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c != '*')
		{
			if (token_append_char(context,token,'/') != 0)
				return tokNoMem;
			
			*state = eGraphicName;
			goto graphic_name;
		}

		*p = peek;
		info->m_end_line = peek_line;
		info->m_end_col = peek_col;
		*state = eMultiComment2;
		goto layout;

	dot:
		peek = *p;
		peek_col = info->m_end_col;

	case eDot:
		c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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

		if (token_append_char(context,token,'.') != 0)
			return tokNoMem;
		
		*state = eGraphicName;
		goto graphic_name;

	quote:
		peek = *p;
		peek_col = info->m_end_col;

	case eSingleQuote:
	case eDoubleQuote:
	case eBackQuote:
		for (;;)
		{
			c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
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
				info->m_end_line = peek_line;
				info->m_end_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidSeq;

			case '\\':
				c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
				if (c == CHAR_NEED_MORE)
					return tokNeedMore;

				if (!token_meta_char(context,c,token))
				{
					if (c == 'o')
					{
						c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
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
						c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
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
						info->m_end_line = peek_line;
						info->m_end_col = peek_col;
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
					c = token_get_char(&peek2,pe,eof,&peek_line2,&peek_col2);
					if (c == CHAR_NEED_MORE)
						return tokNeedMore;

					if (c != '\'')
					{
						info->m_end_line = peek_line;
						info->m_end_col = peek_col;
						*p = peek;
						return tokName;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				if (token_append_char(context,token,(unsigned char)c) != 0)
					return tokNoMem;
				break;

			case '"':
				if (*state == eDoubleQuote)
				{
					size_t peek_line2 = peek_line;
					size_t peek_col2 = peek_col;
					const unsigned char* peek2 = peek;
					c = token_get_char(&peek2,pe,eof,&peek_line2,&peek_col2);
					if (c == CHAR_NEED_MORE)
						return tokNeedMore;

					if (c != '"')
					{
						info->m_end_line = peek_line;
						info->m_end_col = peek_col;
						*p = peek;
						return tokDQL;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				if (token_append_char(context,token,(unsigned char)c) != 0)
					return tokNoMem;
				break;

			case '`':
				if (*state == eBackQuote)
				{
					size_t peek_line2 = peek_line;
					size_t peek_col2 = peek_col;
					const unsigned char* peek2 = peek;
					c = token_get_char(&peek2,pe,eof,&peek_line2,&peek_col2);
					if (c == CHAR_NEED_MORE)
						return tokNeedMore;

					if (c != '`')
					{
						info->m_end_line = peek_line;
						info->m_end_col = peek_col;
						*p = peek;
						return tokBackQuote;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				if (token_append_char(context,token,(unsigned char)c) != 0)
					return tokNoMem;
				break;

			default:
				if (c < 32)
				{
					info->m_end_line = peek_line;
					info->m_end_col = peek_col;
					*p = peek;
					*state = eStart;
					return tokInvalidChar;
				}
				if (token_append_unicode_char(context,token,c) != 0)
					return tokNoMem;
				break;
			}

			info->m_end_line = peek_line;
			info->m_end_col = peek_col;
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
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c < '0' || c > '7')
			{
				info->m_end_line = peek_line;
				info->m_end_col = peek_col;
				*p = peek;

				if (c == '\\')
				{
					if (token_append_unicode_char(context,token,meta) != 0)
						return tokNoMem;
					
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
				info->m_end_line = peek_line;
				info->m_end_col = peek_col;
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
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')))
			{
				info->m_end_line = peek_line;
				info->m_end_col = peek_col;
				*p = peek;

				if (c == '\\')
				{
					if (token_append_unicode_char(context,token,meta) != 0)
						return tokNoMem;
					
					else if (*state == eSingleQuoteHex)
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
				info->m_end_line = peek_line;
				info->m_end_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidEscape;
			}
		}

	case eZero:
	zero:
		c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c == '\'')
		{
			/* No char_conversion for single_quoted_character */
			c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c == '\'')
			{
				c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
				if (c == CHAR_NEED_MORE)
					return tokNeedMore;

				if (c == '\'')
				{
					if (token_append_char(context,token,'\'') != 0)
						return tokNoMem;
					
					info->m_end_line = peek_line;
					info->m_end_col = peek_col;
					*p = peek;
					return tokCharCode;
				}
			}
			else if (c == '\\')
			{
				c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
				if (c == CHAR_NEED_MORE)
					return tokNeedMore;

				if (token_meta_char(context,c,token))
				{
					info->m_end_line = peek_line;
					info->m_end_col = peek_col;
					*p = peek;
					return tokCharCode;
				}
				else if (c == 'o')
				{
					c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
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
					c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
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
				if (token_append_unicode_char(context,token,c) != 0)
					return tokNoMem;
				
				info->m_end_line = peek_line;
				info->m_end_col = peek_col;
				*p = peek;
				return tokCharCode;
			}
		}
		else if (c == 'b')
		{
			c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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
			c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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
			c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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
		peek_col = info->m_end_col + 4;

		meta = 0;
		for (;;)
		{
			c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c == '\\')
			{
				if (token_append_unicode_char(context,token,meta) != 0)
					return tokNoMem;
				
				info->m_end_line = peek_line;
				info->m_end_col = peek_col;
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
				peek_line = info->m_end_line;
				peek_col = info->m_end_col + 1;
				goto next_char;
			}

		octal_char_code:
			meta = (meta << 3) | (c - '0');
			if (meta > CHAR_MAX_VALID)
			{
				info->m_end_line = peek_line;
				info->m_end_col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidEscape;
			}
		}

	case eHexCharCode:
		/* We know *p == "0'\x" */
		peek = *p + 4;
		peek_col = info->m_end_col + 4;
		meta = 0;
		for (;;)
		{
			c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c == '\\')
			{
				if (token_append_unicode_char(context,token,meta) != 0)
					return tokNoMem;
				
				info->m_end_line = peek_line;
				info->m_end_col = peek_col;
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
				peek_line = info->m_end_line;
				peek_col = info->m_end_col + 1;
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
				info->m_end_line = peek_line;
				info->m_end_col = peek_col;
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
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c < '0' || c > '9')
		{
			*state = eStart;
			return tokInt;
		}

		if (token_append_char(context,token,'.') != 0)
			return tokNoMem;
		
		*state = eFraction;
		goto next_char;

	case eExponent:
		/* We know *p == 'E' or 'e' */
		++peek;
		++peek_col;

	exponent:
		c = token_get_char(&peek,pe,eof,&peek_line,&peek_col);
		if (c == CHAR_NEED_MORE)
			return tokNeedMore;

		if (c >= '0' && c <= '9')
		{
			if (token_append_char(context,token,'e') != 0)
				return tokNoMem;
			
			*state = eMantissa;
			goto next_char;
		}

		if (c == '-' || c == '+')
		{
			/* Check the next char */
			uint32_t c2 = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
			if (c2 == CHAR_NEED_MORE)
				return tokNeedMore;

			if (c2 >= '0' && c2 <= '9')
			{
				if (token_append_char(context,token,'e') != 0 || token_append_char(context,token,(unsigned char)c) != 0)
					return tokNoMem;
				
				*state = eMantissa;
				c = c2;
				goto next_char;
			}
		}

		*state = eStart;
		return tokFloat;

	append_chars:
		peek = *p;
		peek_line = info->m_end_line;
		peek_col = info->m_end_col;

		if (token_append_unicode_char(context,token,c) != 0)
			return tokNoMem;
		
	default:
		for (;;)
		{
			c = token_get_char_conv(context,&peek,pe,eof,&peek_line,&peek_col);
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
				break;
			}

		next_char:
			if (token_append_unicode_char(context,token,c) != 0)
				return tokNoMem;
			
			info->m_end_line = peek_line;
			info->m_end_col = peek_col;
			*p = peek;
		}
	}
}

static enum eTokenType token_next(struct context_t* context, struct parser_t* parser, struct token_t* token)
{
	enum eTokenType tok;
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
			else if (token_append_char(context,&parser->m_buffer,c) != 0)
				return tokNoMem;
		}

		p = start = parser->m_buffer.m_str;
		pe = start + parser->m_buffer.m_len;

		tok = parse_token(context,&state,&p,pe,parser->m_eof,token,&parser->m_line_info);

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
	new_node->m_arity = 0;

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
		if (isnan(dval))
			node->m_u64 = PACK_EXP_16(0x7FF8);
		else
			node->m_dbl = dval;
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
		node->m_u64 = v;
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
		node->m_u64 = v;
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
			node->m_dbl = -node->m_dbl;
		else
		{
			int32_t v = node->m_u64;
			if (v > 0)
				node->m_u64 = (uint32_t)(-v);
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
	node->m_str = next->m_str;
	node->m_str_len = next->m_len;
	memset(next,0,sizeof(struct token_t));

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
		node = atom_to_compound(context,node,ast_err);
		if (node)
		{
			node->m_arity = 1;
			node->m_params[0] = parse_term(context,parser,op->m_specifier == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,ast_err);
			if (!node->m_params[0])
				node = NULL;
		}
	}

	return node;
}

static struct ast_node_t* parse_compound_term(struct context_t* context, struct parser_t* parser, struct ast_node_t* node, enum eTokenType* next_type, struct token_t* next, enum eASTError* ast_err)
{
	size_t alloc_arity = 1;
	node = atom_to_compound(context,node,ast_err);
	if (!node)
		return NULL;

	do
	{
		*next_type = token_next(context,parser,next);

		if (node->m_arity == alloc_arity)
		{
			size_t new_arity = alloc_arity * 2;
			struct ast_node_t* new_node = stack_realloc(&context->m_scratch_stack,node,sizeof(struct ast_node_t) + (alloc_arity * sizeof(struct ast_node_t*)),sizeof(struct ast_node_t) + (new_arity * sizeof(struct ast_node_t*)));
			if (!new_node)
				return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

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
		*tail = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t));
		if (!(*tail))
			return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

		(*tail)->m_type = AST_TYPE_ATOM;
		(*tail)->m_str = (const unsigned char*)"[]";
		(*tail)->m_str_len = 2;
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
	node->m_str = next->m_str;
	node->m_str_len = next->m_len;
	memset(next,0,sizeof(struct token_t));

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
	node->m_str = token->m_str;
	node->m_str_len = token->m_len;
	memset(token,0,sizeof(struct token_t));

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
		node->m_arity = UINT64_C(-1);
		node->m_str = next->m_str;
		node->m_str_len = next->m_len;
		memset(next,0,sizeof(struct token_t));
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

		node = parse_chars_and_codes(context,context->m_module->m_flags.double_quotes,next,ast_err);
		break;

	case tokBackQuote:
		if (context->m_module->m_flags.back_quotes == 2 /* atom */)
			return parse_name(context,parser,max_prec,next_type,next,ast_err);

		node = parse_chars_and_codes(context,context->m_module->m_flags.back_quotes,next,ast_err);
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
		node->m_str = (const unsigned char*)"[]";
		node->m_str_len = 2;
		node->m_arity = 0;
		break;
		
	case tokOpenC:
		node = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t) + sizeof(struct ast_node_t*));
		if (!node)
			return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

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
		const unsigned char* name = next->m_str;
		size_t name_len = next->m_len;

		if (*next_type == tokName)
		{
			struct operator_t* op = lookup_op(context,name,name_len);
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
			struct operator_t* op;

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
			struct ast_node_t* next_node = stack_malloc(&context->m_scratch_stack,sizeof(struct ast_node_t) + ((1 + binary) * sizeof(struct ast_node_t*)));
			if (!next_node)
				return syntax_error(AST_ERR_OUTOFMEMORY,ast_err);

			next_node->m_type = AST_TYPE_COMPOUND;
			next_node->m_str = name;
			next_node->m_str_len = name_len;
			next_node->m_arity = 1 + binary;
			next_node->m_params[0] = node;

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

struct var_info_t
{
	size_t               m_use_count;
	const unsigned char* m_name;
	size_t               m_name_len;
};

static enum eEmitStatus emit_node_vars(struct context_t* context, struct var_info_t** varinfo, struct ast_node_t* node)
{
	enum eEmitStatus status = EMIT_OK;

	if (!context->m_substs)
	{
		context->m_substs = stack_malloc(&context->m_call_stack,sizeof(struct substs_t));
		if (!context->m_substs)
			return ENOMEM;

		context->m_substs->m_count = 0;
	}

	if (node->m_type == AST_TYPE_VAR)
	{
		size_t i = context->m_substs->m_count;
		if (node->m_str_len != 1 || node->m_str[0] != '_')
		{
			for (i = 0; i < context->m_substs->m_count; ++i)
			{
				if (node->m_str_len == (*varinfo)[i].m_name_len && memcmp(node->m_str,(*varinfo)[i].m_name,node->m_str_len) == 0)
				{
					++(*varinfo)[i].m_use_count;
					break;
				}
			}
		}

		if (i == context->m_substs->m_count)
		{
			struct var_info_t* new_varinfo;
			struct substs_t* new_substs;
			
			// Check for variable index overflow
			if (i+1 >= UINT64_C(1) << 47)
				return EMIT_NOMEM;

			new_substs = stack_realloc(&context->m_call_stack,context->m_substs,sizeof(struct substs_t) + (context->m_substs->m_count * sizeof(union packed_t)),sizeof(struct substs_t) + ((context->m_substs->m_count+1) * sizeof(union packed_t)));
			if (!new_substs)
				return EMIT_NOMEM;

			new_varinfo = stack_realloc(&context->m_scratch_stack,*varinfo,sizeof(struct var_info_t) * context->m_substs->m_count,sizeof(struct var_info_t) * (context->m_substs->m_count+1));
			if (!new_varinfo)
				return EMIT_NOMEM;

			context->m_substs = new_substs;
			context->m_substs->m_values[context->m_substs->m_count] = NULL;
			context->m_substs->m_count = context->m_substs->m_count+1;

			*varinfo = new_varinfo;
			(*varinfo)[context->m_substs->m_count].m_name = node->m_str;
			(*varinfo)[context->m_substs->m_count].m_name_len = node->m_str_len;
			(*varinfo)[context->m_substs->m_count].m_use_count = 1;
		}

		node->m_arity = i;
	}
	else if (node->m_type == AST_TYPE_COMPOUND)
	{
		size_t i;
		for (i = 0; !status && i < node->m_arity; ++i)
			status = emit_node_vars(context,varinfo,node->m_params[i]);
	}

	return status;
}

static enum eEmitStatus emit_atomic(struct context_t* context, struct ast_node_t* node, enum tag_type_t type, union packed_t** term, size_t* term_size)
{
	enum eEmitStatus status = EMIT_OK;
	union packed_t b;
	uint32_t builtin;
	if (node->m_str_len <= 5)
	{
		switch (node->m_str_len)
		{
		case 5:
			b.m_u64val = PACK_TYPE_EMBED(type,0,5,node->m_str[0],node->m_str[1],node->m_str[2],node->m_str[3],node->m_str[4]);
			break;

		case 4:
			b.m_u64val = PACK_TYPE_EMBED(type,0,4,node->m_str[0],node->m_str[1],node->m_str[2],node->m_str[3],0);
			break;

		case 3:
			b.m_u64val = PACK_TYPE_EMBED(type,0,3,node->m_str[0],node->m_str[1],node->m_str[2],0,0);
			break;

		case 2:
			b.m_u64val = PACK_TYPE_EMBED(type,0,2,node->m_str[0],node->m_str[1],0,0,0);
			break;

		case 1:
			b.m_u64val = PACK_TYPE_EMBED(type,0,1,node->m_str[0],0,0,0,0);
			break;

		default:
			b.m_u64val = PACK_TYPE_EMBED(type,0,0,0,0,0,0,0);
			break;
		}
	}
	else if ((builtin = is_builtin_string(node->m_str,node->m_str_len)) != -1)
		b.m_u64val = PACK_TYPE(type) | PACK_HI16(0x4000) | PACK_LOW32(builtin);
	else
	{
		struct string_ptr_t* s = pack_stack_string(&context->m_call_stack,&context->m_strings,node->m_str,node->m_str_len);
		if (!s)
			status = EMIT_NOMEM;
		else
			b.m_u64val = PACK_TYPE(type) | pack_pointer(s);
	}

	if (status == EMIT_OK)
	{
		*term = stack_realloc(&context->m_call_stack,*term,*term_size * sizeof(union packed_t),((*term_size)+1) * sizeof(union packed_t));
		if (!*term)
			status = EMIT_NOMEM;
		else
			(*term)[(*term_size)++] = b;
	}

	return status;
}

static enum eEmitStatus emit_compound(struct context_t* context, struct ast_node_t* node, union packed_t** term, size_t* term_size)
{
	enum eEmitStatus status = EMIT_OK;
	uint32_t builtin;
	if (node->m_arity <= MAX_ARITY_EMBED && node->m_str_len <= 5)
	{
		union packed_t b;
		switch (node->m_str_len)
		{
		case 5:
			b.m_u64val = PACK_TYPE_EMBED(prolite_compound,node->m_arity,5,node->m_str[0],node->m_str[1],node->m_str[2],node->m_str[3],node->m_str[4]);
			break;

		case 4:
			b.m_u64val = PACK_TYPE_EMBED(prolite_compound,node->m_arity,4,node->m_str[0],node->m_str[1],node->m_str[2],node->m_str[3],0);
			break;

		case 3:
			b.m_u64val = PACK_TYPE_EMBED(prolite_compound,node->m_arity,3,node->m_str[0],node->m_str[1],node->m_str[2],0,0);
			break;

		case 2:
			b.m_u64val = PACK_TYPE_EMBED(prolite_compound,node->m_arity,2,node->m_str[0],node->m_str[1],0,0,0);
			break;

		case 1:
			b.m_u64val = PACK_TYPE_EMBED(prolite_compound,node->m_arity,1,node->m_str[0],0,0,0,0);
			break;

		default:
			b.m_u64val = PACK_TYPE_EMBED(prolite_compound,node->m_arity,0,0,0,0,0,0);
			break;
		}

		*term = stack_realloc(&context->m_call_stack,*term,*term_size * sizeof(union packed_t),((*term_size)+1) * sizeof(union packed_t));
		if (!*term)
			status = EMIT_NOMEM;
		else
			(*term)[(*term_size)++] = b;
	}
	else if (node->m_arity <= MAX_ARITY_BUILTIN && (builtin = is_builtin_string(node->m_str,node->m_str_len)) != -1)
	{
		*term = stack_realloc(&context->m_call_stack,*term,*term_size * sizeof(union packed_t),((*term_size)+1) * sizeof(union packed_t));
		if (!*term)
			status = EMIT_NOMEM;
		else
			(*term)[(*term_size)++].m_u64val = PACK_TYPE(prolite_compound) | PACK_HI16(0x4000 | node->m_arity) | PACK_LOW32(builtin);
	}
	else
	{
		*term = stack_realloc(&context->m_call_stack,*term,*term_size * sizeof(union packed_t),((*term_size)+2) * sizeof(union packed_t));
		if (!*term)
			status = EMIT_NOMEM;
		else
		{
			(*term)[(*term_size)++].m_u64val = PACK_TYPE(prolite_compound) | PACK_MANT_48(node->m_arity);

			status = emit_atomic(context,node,prolite_atom,term,term_size);
		}
	}

	return status;
}

static enum eEmitStatus emit_node_value(struct context_t* context, struct ast_node_t* node, union packed_t** term, size_t* term_size)
{
	enum eEmitStatus status = EMIT_OK;
	switch (node->m_type)
	{
	case AST_TYPE_COMPOUND:
		status = emit_compound(context,node,term,term_size);
		if (status == EMIT_OK)
		{
			size_t i;
			for (i = 0; status == EMIT_OK && i < node->m_arity; ++i)
				status = emit_node_value(context,node->m_params[i],term,term_size);
		}
		break;

	case AST_TYPE_ATOM:
		status = emit_atomic(context,node,prolite_atom,term,term_size);
		break;

	case AST_TYPE_CHARS:
		status = emit_atomic(context,node,prolite_chars,term,term_size);
		break;

	case AST_TYPE_CODES:
		status = emit_atomic(context,node,prolite_charcodes,term,term_size);
		break;

	case AST_TYPE_VAR:
	case AST_TYPE_DOUBLE:
	case AST_TYPE_INTEGER:
		{
			union packed_t b;
			if (node->m_type == AST_TYPE_VAR)
				b.m_u64val = PACK_TYPE(prolite_var) | PACK_MANT_48(node->m_arity);
			else if (node->m_type == AST_TYPE_DOUBLE)
				b.m_dval = node->m_dbl;
			else if (node->m_type == AST_TYPE_INTEGER)
				b.m_u64val = PACK_TYPE(prolite_int32) | PACK_MANT_48((uint32_t)node->m_u64);

			*term = stack_realloc(&context->m_call_stack,*term,*term_size * sizeof(union packed_t),((*term_size)+1) * sizeof(union packed_t));
			if (!*term)
				status = EMIT_NOMEM;
			else
				(*term)[(*term_size)++] = b;
		}
		break;
	}

	if (status == EMIT_OK)
	{
		// TODO: Debug info
	}

	return status;
}

static enum eEmitStatus emit_error_line_info(struct context_t* context, struct line_info_t* info, union packed_t** term, size_t* term_size)
{
	*term = stack_realloc(&context->m_scratch_stack,*term,*term_size * sizeof(union packed_t),((*term_size)+1) * sizeof(union packed_t));
	if (!*term)
		return EMIT_NOMEM;

	if (!info)
		(*term)[(*term_size)++].m_u64val = PACK_ATOM_EMBED_5('f','a','l','s','e');
	else
	{
		// TODO: !!
		(*term)[(*term_size)++].m_u64val = PACK_ATOM_EMBED_4('T','o','d','o');
	}

	return EMIT_OK;
}

static enum eEmitStatus copy_error_term(struct context_t* context, struct string_ptr_t** strings, union packed_t const** v, union packed_t** new_term, size_t* term_size)
{
	enum tag_type_t type = UNPACK_TYPE((*v)->m_u64val);
	switch (type)
	{
	case prolite_compound:
		{
			uint64_t arity = UNPACK_MANT_48((*v)->m_u64val);
			unsigned int hi16 = (arity >> 32);
			if (hi16 & 0x8000)
				arity = (hi16 & (MAX_ARITY_EMBED << 11)) >> 11;
			else if ((hi16 & 0xC000) == 0x4000)
				arity = (hi16 & MAX_ARITY_BUILTIN);
			else
			{
				// Copy functor atom
				if (append_packed_t(&context->m_scratch_stack,v,new_term,term_size))
					return EMIT_NOMEM;
			}

			if (append_packed_t(&context->m_scratch_stack,v,new_term,term_size))
				return EMIT_NOMEM;

			if (UNPACK_TYPE((*v)->m_u64val) == PROLITE_DEBUG_INFO)
			{
				// TODO: Debug info
			}

			while (arity--)
			{
				enum eEmitStatus result = copy_error_term(context,strings,v,new_term,term_size);
				if (result != EMIT_OK)
					return result;
			}			
		}
		break;

	case prolite_var:
		{
			const union packed_t* ptr = deref_term(context->m_substs,*v);
			if (ptr == *v)
			{
				// TODO: Sort this out!
				assert(0);
			}

			return copy_error_term(context,strings,&ptr,new_term,term_size);
		}
		break;

	case prolite_atom:
	case prolite_chars:
	case prolite_charcodes:
		{
			unsigned int hi16 = UNPACK_HI16((*v)->m_u64val);
			if (hi16 & 0xC000)
			{
				if (append_packed_t(&context->m_scratch_stack,v,new_term,term_size) != 0)
					return EMIT_NOMEM;
			}
			else
			{
				union packed_t b[1];
				struct string_ptr_t* s = unpack_pointer((*v)->m_u64val);
				s = pack_stack_string(&context->m_scratch_stack,strings,s->m_str,s->m_len);
				if (!s)
					return EMIT_NOMEM;

				b[0].m_u64val = PACK_TYPE(type) | pack_pointer(s);
				if (append_packed_t(&context->m_scratch_stack,(const union packed_t**)&b,new_term,term_size) != 0)
					return EMIT_NOMEM;
			}
		}
		break;

	default:
		if (append_packed_t(&context->m_scratch_stack,v,new_term,term_size) != 0)
			return EMIT_NOMEM;
	}

	if (UNPACK_TYPE((*v)->m_u64val) == PROLITE_DEBUG_INFO)
	{
		// TODO: Debug info
	}

	return EMIT_OK;
}

static enum eEmitStatus emit_error(struct context_t* context, struct line_info_t* info, uint64_t error_functor, unsigned int arity, ...)
{
	enum eEmitStatus result = EMIT_OK;
	va_list args;
	unsigned int a;
	union packed_t** pball;
	size_t term_size = 2;
	struct string_ptr_t* strings = NULL;

	stack_reset(&context->m_scratch_stack,0);
	if (stack_push_ptr(&context->m_scratch_stack,NULL) == -1)
		return EMIT_NOMEM;

	pball = (union packed_t**)stack_at(context->m_scratch_stack,0);
	*pball = stack_malloc(&context->m_scratch_stack,term_size * sizeof(union packed_t));
	if (!*pball)
		result = EMIT_NOMEM;
	else
	{
		(*pball)[0].m_u64val = PACK_COMPOUND_EMBED_5(2,'e','r','r','o','r');
		(*pball)[1].m_u64val = error_functor;
	}

	va_start(args,arity);

	for (a = 0; result == EMIT_OK && a < arity; ++a)
	{
		const union packed_t* v = va_arg(args,union packed_t*);
		if (copy_error_term(context,&strings,&v,pball,&term_size) != 0)
			result = EMIT_NOMEM;
	}

	va_end(args);

	if (result == EMIT_OK)
		result = emit_error_line_info(context,info,pball,&term_size);

	if (result == EMIT_OK)
		result = EMIT_THROW;

	return result;
}

static enum eEmitStatus emit_syntax_error_missing(struct context_t* context, uint64_t missing_atom, struct line_info_t* info)
{
	union packed_t args[5];
	args[0].m_u64val = PACK_COMPOUND_BUILTIN(missing,1);
	args[1].m_u64val = missing_atom;

	return emit_error(context,info,PACK_COMPOUND_BUILTIN(syntax_error,1),1,&args[0]);
}

static enum eEmitStatus emit_ast_error(struct context_t* context, enum eASTError ast_err, struct line_info_t* info)
{
	union packed_t arg;

	switch (ast_err)
	{
	case AST_ERR_FLOAT_OVERFLOW:
		arg.m_u64val = PACK_ATOM_BUILTIN(float_overflow);
		return emit_error(context,info,PACK_COMPOUND_BUILTIN(evaluation_error,1),1,&arg);

	case AST_ERR_FLOAT_UNDERFLOW:
		arg.m_u64val = PACK_ATOM_BUILTIN(underflow);
		return emit_error(context,info,PACK_COMPOUND_BUILTIN(evaluation_error,1),1,&arg);

	case AST_ERR_MAX_INTEGER:
		arg.m_u64val = PACK_ATOM_BUILTIN(max_integer);
		return emit_error(context,info,PACK_COMPOUND_BUILTIN(representation_error,1),1,&arg);

	case AST_ERR_MIN_INTEGER:
		arg.m_u64val = PACK_ATOM_BUILTIN(min_integer);
		return emit_error(context,info,PACK_COMPOUND_BUILTIN(representation_error,1),1,&arg);

	case AST_ERR_MAX_ARITY:
		arg.m_u64val = PACK_ATOM_BUILTIN(max_arity);
		return emit_error(context,info,PACK_COMPOUND_BUILTIN(representation_error,1),1,&arg);

	case AST_SYNTAX_ERR_MISSING_CLOSE:
		return emit_syntax_error_missing(context,PACK_ATOM_EMBED_1(')'),info);

	case AST_SYNTAX_ERR_MISSING_CLOSE_L:
		return emit_syntax_error_missing(context,PACK_ATOM_EMBED_1(']'),info);

	case AST_SYNTAX_ERR_MISSING_CLOSE_C:
		return emit_syntax_error_missing(context,PACK_ATOM_EMBED_1('}'),info);

	case AST_SYNTAX_ERR_MISSING_SQ:
		return emit_syntax_error_missing(context,PACK_ATOM_EMBED_1('\''),info);

	case AST_SYNTAX_ERR_MISSING_DQ:
		return emit_syntax_error_missing(context,PACK_ATOM_EMBED_1('"'),info);

	case AST_SYNTAX_ERR_MISSING_BQ:
		return emit_syntax_error_missing(context,PACK_ATOM_EMBED_1('`'),info);

	case AST_SYNTAX_ERR_INVALID_ARG:
		arg.m_u64val = PACK_ATOM_BUILTIN(invalid_argument);
		return emit_error(context,info,PACK_COMPOUND_BUILTIN(syntax_error,1),1,&arg);

	case AST_SYNTAX_ERR_UNEXPECTED_TOKEN:
		arg.m_u64val = PACK_ATOM_BUILTIN(unexpected_token);
		return emit_error(context,info,PACK_COMPOUND_BUILTIN(syntax_error,1),1,&arg);

	case AST_SYNTAX_ERR_INVALID_CHAR:
		arg.m_u64val = PACK_ATOM_BUILTIN(invalid_character);
		return emit_error(context,info,PACK_COMPOUND_BUILTIN(syntax_error,1),1,&arg);

	case AST_SYNTAX_ERR_INVALID_ESCAPE:
		arg.m_u64val = PACK_ATOM_BUILTIN(invalid_escape);
		return emit_error(context,info,PACK_COMPOUND_BUILTIN(syntax_error,1),1,&arg);

	case AST_SYNTAX_ERR_INVALID_UTF8:
		arg.m_u64val = PACK_ATOM_BUILTIN(invalid_utf8);
		return emit_error(context,info,PACK_COMPOUND_BUILTIN(syntax_error,1),1,&arg);

	default:
		assert(0);
		return EMIT_OK;
	}
}

static enum eEmitStatus parse_emit_term(struct context_t* context, struct parser_t* parser, union packed_t** term, struct var_info_t** varinfo)
{
	enum eEmitStatus status = EMIT_OK;
	enum eASTError ast_err = AST_ERR_NONE;

	struct token_t next = {0};
	enum eTokenType next_type = token_next(context,parser,&next);
	struct ast_node_t* node = parse_term(context,parser,1201,&next_type,&next,&ast_err);
	if (!node || next_type != tokEnd)
	{
		/* Reset scratch stack */
		stack_reset(&context->m_scratch_stack,0);

		/* Reset token because we just trashed it by resetting the stack */
		next.m_alloc = 0;
		next.m_len = 0;
		next.m_str = NULL;

		if (node)
			status = emit_syntax_error_missing(context,PACK_ATOM_EMBED_1('.'),&parser->m_line_info);
		else if (next_type != tokEOF)
			status = emit_ast_error(context,ast_err,&parser->m_line_info);
		else
			status = EMIT_EOF;

		/* Skip to what looks like the end of a complete term */
		while (next_type != tokEnd && next_type != tokEOF)
		{
			next_type = token_next(context,parser,&next);
			if (next_type == tokNoMem)
			{
				/* Reset scratch stack */
				stack_reset(&context->m_scratch_stack,0);

				/* Reset token because we just trashed it by resetting the stack */
				next.m_alloc = 0;
				next.m_len = 0;
				next.m_str = NULL;
			}
		}
	}
	else
	{
		/* Emit the node */
		status = emit_node_vars(context,varinfo,node);
		if (status == EMIT_OK)
		{
			size_t term_len = 0;
			status = emit_node_value(context,node,term,&term_len);
			if (status == EMIT_OK)
			{
				/* Emit one more packed_t as it can be read while looking for debug info */
				*term = stack_realloc(&context->m_call_stack,*term,term_len * sizeof(union packed_t),(term_len + 1) * sizeof(union packed_t));
				if (!*term)
					status = EMIT_NOMEM;
				else
					(*term)[term_len++].m_u64val = 0;
			}
		}
	}

	return status;
}

#ifdef OLD
static enum eEmitStatus clause_directive(struct context_t* context, const union packed_t* directive)
{
	assert(0);
}

static enum eEmitStatus ensure_loaded(struct context_t* context, const union packed_t* directive)
{
	assert(0);
}

static enum eEmitStatus compile_initializer(struct context_t* context, const union packed_t* directive)
{
	assert(0);
}

enum eSolveResult assert_clause(struct context_t* context, const union packed_t* clause, int z, int dynamic);

static enum eEmitStatus directive_solve(struct context_t* context, const union packed_t* directive)
{
	assert(0);

	switch (solve(context,directive))
	{
	case SOLVE_TRUE:
		return EMIT_OK;

	case SOLVE_NOMEM:
		return EMIT_NOMEM;

	case SOLVE_THROW:
		return EMIT_THROW;

	default:
		// Should never happen
		assert(0);
		return (emit_error(context,get_debug_info(directive),PACK_ATOM_BUILTIN(system_error),0) == EMIT_OK ? EMIT_THROW : EMIT_NOMEM);
	}
}

static enum eEmitStatus include(struct context_t* context, const union packed_t* directive);

static enum eEmitStatus load_file(struct context_t* context, struct stream_t* s)
{
	enum eEmitStatus status;

	struct parser_t parser = {0};
	parser.m_s = s;
	parser.m_line_info.m_end_line = 1;

	do
	{
		union packed_t* term = NULL;
		size_t stack_base = stack_top(context->m_call_stack);
		struct var_info_t* varinfo = NULL;

		status = parse_emit_term(context,&parser,&term,&varinfo);
		if (status == EMIT_OK)
		{
			if (term->m_u64val == PACK_COMPOUND_EMBED_2(1,':','-'))
			{
				term = (union packed_t*)first_arg(term);
				switch (term->m_u64val)
				{
				case PACK_COMPOUND_BUILTIN(dynamic,1):
				case PACK_COMPOUND_BUILTIN(multifile,1):
				case PACK_COMPOUND_BUILTIN(discontiguous,1):
					status = clause_directive(context,term);
					break;

				case PACK_COMPOUND_BUILTIN(include,1):
					status = include(context,term);
					break;

				case PACK_COMPOUND_BUILTIN(ensure_loaded,1):
					status = ensure_loaded(context,term);
					break;

				case PACK_COMPOUND_BUILTIN(initialization,1):
					status = compile_initializer(context,term);
					break;

				default:
					status = directive_solve(context,term);
					break;
				}
			}
			else
			{
				/* Assert the term */
				assert(0);

				assert_clause(context,term,1,0);
			}
		}

		if (status == EMIT_OK)
		{
			// TODO: Undo pointers...
			stack_reset(&context->m_scratch_stack,0);
			stack_reset(&context->m_call_stack,stack_base);
		}
	}
	while (status == EMIT_OK);

	if (status == EMIT_EOF)
		status = EMIT_OK;

	/* Compact the scratch stack because we may have allocated a lot */
	stack_compact(context->m_scratch_stack);

	return status;
}

static enum eEmitStatus include(struct context_t* context, const union packed_t* directive)
{
	enum eEmitStatus status;
	struct stream_t* s = NULL;

	/* TODO: Open stream 'directive' */

	/* TODO: Twiddle with context */

	status = load_file(context,s);
	if (status == EMIT_OK)
	{
		/* TODO: Untwiddle context */
	}

	return status;
}
#endif

enum eEmitStatus prepare_term(struct context_t* context, struct stream_t* s, union packed_t** term, const char*** varnames)
{
	enum eEmitStatus result;
	struct var_info_t* varinfo = NULL;

	struct parser_t parser = {0};
	parser.m_s = s;
	parser.m_line_info.m_end_line = 1;

	result = parse_emit_term(context,&parser,term,&varinfo);
	if (result == EMIT_EOF)
	{
		union packed_t arg;
		arg.m_u64val = PACK_ATOM_BUILTIN(past_end_of_stream);
		result = emit_error(context,&parser.m_line_info,PACK_COMPOUND_BUILTIN(syntax_error,1),1,&arg);
	}

	if (result == EMIT_OK)
	{
		// Copy varinfo to call_stack
		*varnames = stack_malloc(&context->m_call_stack,sizeof(union packed_t*) * context->m_substs->m_count);
		if (!*varnames)
			result = EMIT_NOMEM;
		else
		{
			size_t i;
			for (i = 0; result == EMIT_OK && i < context->m_substs->m_count; ++i)
			{
				char* n = stack_malloc(&context->m_call_stack,varinfo[i].m_name_len+1);
				if (!n)
					result = EMIT_NOMEM;
				else
				{
					memcpy(n,varinfo[i].m_name,varinfo[i].m_name_len);
					n[varinfo[i].m_name_len] = '\0';
					(*varnames)[i] = n;
				}
			}
		}
	}

	if (result == EMIT_OK)
		stack_reset(&context->m_scratch_stack,0);

	// We have just made heavy use of the scratch stack
	stack_compact(context->m_scratch_stack);

	return result;
}
