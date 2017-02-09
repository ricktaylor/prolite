
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <malloc.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>

#include "prolite.h"

struct Stream;

ssize_t read(struct Stream* s, void* dest, size_t len);


uint32_t char_conversion(uint32_t in_char);

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

struct Token
{
	size_t m_alloc;
	size_t m_len;
	unsigned char*  m_str;  /* Need not be zero-terminated! */
};

struct Tokenizer
{
	struct Stream* m_s;
	struct Token m_buffer;
	int m_eof;
	size_t m_start_line;
	size_t m_start_col;
	size_t m_line;
	size_t m_col;
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

struct Operator
{
	enum eOpSpec m_specifier;
	unsigned int m_precedence;
};

#define TAG_VAR        (0xE << 28)

#define TAG_COMPOUND   (0xC << 28)

#define TAG_NUMBER     (0xA << 28)
#define TAG_INT_28     TAG_NUMBER
#define TAG_INT_60     (0xB << 28)
#define TAG_INT_64     (TAG_INT_60 | 1)
#define TAG_FLOAT_64   (TAG_INT_60 | 2)

#define TAG_ATOM       (0x8 << 28)
#define TAG_ATOM_PTR   TAG_ATOM
#define TAG_ATOM_EMBED (0x9 << 28)

#define TAG_DQL        (0x6 << 28)
#define TAG_DQL_PTR    TAG_DQL
#define TAG_DQL_EMBED  (0x7 << 28)

#define TAG_BQ         (0x4 << 28)
#define TAG_BQ_PTR     TAG_BQ
#define TAG_BQ_EMBED   (0x5 << 28)

#define TAG_CUSTOM     (0x2 << 28)

//#define TAG_UNUSED   (0x0 << 28)

#define TAG_MASK       (0xE << 28)


struct ASTNode
{
	uint32_t m_tag;
};

struct ASTAtomNode
{
	struct ASTNode m_base;
	uint32_t m_offset;
};

struct ASTNumericNode
{
	struct ASTNode m_base;

	union ASTNumericVal
	{
		uint32_t m_u32;
		int64_t m_i64;
		double m_f64;
	} m_val;
};

struct ASTCompoundNode
{
	struct ASTNode m_base;
	struct ASTAtomNode m_functor;

	struct ASTNode* m_params[1];
};

struct TermBuilder
{
	size_t m_alloc;
	size_t m_len;
	uint32_t* m_data;
}; 

struct ASTError
{
	int err;
};

#define char_max 0x1FFFF /* Greatest valid unicode char */
#define char_more (char_max+1)
#define char_eof  (char_max+2)
#define char_ilseq (char_max+3)

static int tokenAppend(struct Token* token, unsigned char c)
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

static int tokenAppendUnicode(struct Token* token, uint32_t unicode_char)
{
	if (unicode_char <= 0x7F)
	{
		return tokenAppend(token,(unsigned char)unicode_char);
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
			if (tokenAppend(token,chars[i]) == -1)
				return -1;
		}

		return 0;
	}
}

static uint32_t skip_ilseq(const unsigned char** p, const unsigned char* pe, int eof, size_t* col, unsigned int i)
{
	for (;;)
	{
		if ((*p)+i == pe)
		{
			if (eof)
			{
				*p += i;
				*col += i;
				return char_ilseq;
			}

			return char_more;
		}

		if ((*p)[i] <= 0x7f)
		{
			*p += i;
			*col += i;
			return char_ilseq;
		}

		++i;
	}
}

static uint32_t get_char(const unsigned char** p, const unsigned char* pe, int eof, size_t* line, size_t* col)
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
					return skip_ilseq(p,pe,eof,col,1);

				return char_more;
			}

			if ((c[0] == 0xE0 && c[1] >= 0x80 && c[1] <= 0x9F) ||
				(c[0] == 0xED && c[1] >= 0xA0 && c[1] <= 0xBF))
			{
				return skip_ilseq(p,pe,eof,col,2);
			}

			count = 3;
			val = (c[0] & 0x0F);
		}
		else if ((c[0] & 0xF8) == 0xF0)
		{
			if (c+1 == pe)
			{
				if (eof)
					return skip_ilseq(p,pe,eof,col,1);

				return char_more;
			}

			if ((c[0] == 0xF0 && c[1] >= 0x80 && c[1] <= 0x8F) ||
				(c[0] == 0xF4 && c[1] >= 0x90 && c[1] <= 0xBF))
			{
				return skip_ilseq(p,pe,eof,col,2);
			}

			count = 4;
			val = (c[0] & 0x7);
		}
		else
			return skip_ilseq(p,pe,eof,col,1);

		if (c > pe - count)
		{
			if (eof)
				return skip_ilseq(p,pe,eof,col,1);

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
		return skip_ilseq(p,pe,eof,col,1);

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

static uint32_t get_char_conv(const unsigned char** p, const unsigned char* pe, int eof, size_t* line, size_t* col)
{
	uint32_t c = get_char(p,pe,eof,line,col);
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

static int meta_char(uint32_t meta, struct Token* token)
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

	if (tokenAppend(token,c) == -1)
		return -1;

	return 1;
}

static enum eTokenType read_token(enum eState* state, const unsigned char** p, const unsigned char* pe, int eof, struct Token* token, size_t* line, size_t* col, size_t* start_line, size_t* start_col)
{
	const unsigned char* peek = *p;
	size_t peek_line = *line;
	size_t peek_col = *col;
	uint32_t c;
	uint32_t meta;

	switch (*state)
	{
	case eStart:
		/* Clear the current token */
		token->m_len = 0;

		c = get_char_conv(p,pe,eof,line,col);
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
		c = get_char_conv(p,pe,eof,line,col);
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
					*start_line = *line;
					*start_col = *col;
					break;

				case eaShortName:
					*state = eStart;
					if (tokenAppend(token,(unsigned char)c) != 0)
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
					*start_line = *line;
					*start_col = *col;
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
					*start_line = *line;
					*start_col = *col;
					*state = eLayout;
				}
				else if (c != '*')
					*state = eMultiComment2;
			}

			/* Get the next character */
			c = get_char_conv(p,pe,eof,line,col);
			if (c == char_more)
				return tokMore;
		}

	multi_comment:
		peek = *p;
		peek_line = *line;
		peek_col = *col;

	case eMultiComment1:
		c = get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c != '*')
		{
			if (tokenAppend(token,'/') != 0)
			{
				*state = eStart;
				return tokNoMem;
			}
			*state = eGraphicName;
			goto graphic_name;
		}

		*p = peek;
		*line = peek_line;
		*col = peek_col;
		*state = eMultiComment2;
		goto layout;

	dot:
		peek = *p;
		peek_col = *col;

	case eDot:
		c = get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
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

		if (tokenAppend(token,'.') != 0)
		{
			*state = eStart;
			return tokNoMem;
		}
		*state = eGraphicName;
		goto graphic_name;

	quote:
		peek = *p;
		peek_col = *col;

	case eSingleQuote:
	case eDoubleQuote:
	case eBackQuote:
		for (;;)
		{
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			switch (c)
			{
			case char_more:
				return tokMore;

			case char_eof:
				*state = eStart;
				return tokHalfQuote;

			case char_ilseq:
				*line = peek_line;
				*col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidSeq;

			case '\\':
				c = get_char(&peek,pe,eof,&peek_line,&peek_col);
				if (c == char_more)
					return tokMore;

				if (!meta_char(c,token))
				{
					if (c == 'o')
					{
						c = get_char(&peek,pe,eof,&peek_line,&peek_col);
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
						c = get_char(&peek,pe,eof,&peek_line,&peek_col);
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
						*line = peek_line;
						*col = peek_col;
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
					c = get_char(&peek2,pe,eof,&peek_line2,&peek_col2);
					if (c == char_more)
						return tokMore;

					if (c != '\'')
					{
						*line = peek_line;
						*col = peek_col;
						*p = peek;
						return tokName;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				if (tokenAppend(token,(unsigned char)c) != 0)
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
					c = get_char(&peek2,pe,eof,&peek_line2,&peek_col2);
					if (c == char_more)
						return tokMore;

					if (c != '"')
					{
						*line = peek_line;
						*col = peek_col;
						*p = peek;
						return tokDQL;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				if (tokenAppend(token,(unsigned char)c) != 0)
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
					c = get_char(&peek2,pe,eof,&peek_line2,&peek_col2);
					if (c == char_more)
						return tokMore;

					if (c != '`')
					{
						*line = peek_line;
						*col = peek_col;
						*p = peek;
						return tokBackQuote;
					}

					peek_line = peek_line2;
					peek_col = peek_col2;
					peek = peek2;
				}
				if (tokenAppend(token,(unsigned char)c) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}
				break;

			default:
				if (c < 32)
				{
					*line = peek_line;
					*col = peek_col;
					*p = peek;
					*state = eStart;
					return tokInvalidChar;
				}
				if (tokenAppendUnicode(token,c) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}
				break;
			}

			*line = peek_line;
			*col = peek_col;
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
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c < '0' || c > '7')
			{
				*line = peek_line;
				*col = peek_col;
				*p = peek;

				if (c == '\\')
				{
					if (tokenAppendUnicode(token,meta) != 0)
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
				*line = peek_line;
				*col = peek_col;
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
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')))
			{
				*line = peek_line;
				*col = peek_col;
				*p = peek;

				if (c == '\\')
				{
					if (tokenAppendUnicode(token,meta) != 0)
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
				*line = peek_line;
				*col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidEscape;
			}
		}

	case eZero:
	zero:
		c = get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c == '\'')
		{
			/* No char_conversion for single_quoted_character */
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c == '\'')
			{
				c = get_char(&peek,pe,eof,&peek_line,&peek_col);
				if (c == char_more)
					return tokMore;

				if (c == '\'')
				{
					if (tokenAppend(token,'\'') != 0)
					{
						*state = eStart;
						return tokNoMem;
					}

					*line = peek_line;
					*col = peek_col;
					*p = peek;
					return tokCharCode;
				}
			}
			else if (c == '\\')
			{
				c = get_char(&peek,pe,eof,&peek_line,&peek_col);
				if (c == char_more)
					return tokMore;

				if (meta_char(c,token))
				{
					*line = peek_line;
					*col = peek_col;
					*p = peek;
					return tokCharCode;
				}
				else if (c == 'o')
				{
					c = get_char(&peek,pe,eof,&peek_line,&peek_col);
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
					c = get_char(&peek,pe,eof,&peek_line,&peek_col);
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
				if (tokenAppendUnicode(token,c) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}

				*line = peek_line;
				*col = peek_col;
				*p = peek;
				return tokCharCode;
			}
		}
		else if (c == 'b')
		{
			c = get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
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
			c = get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
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
			c = get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
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
		peek_line = *line;
		peek_col = *col;
		goto next_char;

	case eOctalCharCode:
		/* We know *p == "0'\o" */
		peek = *p + 4;
		peek_col = *col + 4;

		meta = 0;
		for (;;)
		{
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c == '\\')
			{
				if (tokenAppendUnicode(token,meta) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}

				*line = peek_line;
				*col = peek_col;
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
				peek_line = *line;
				peek_col = *col + 1;
				goto next_char;
			}

		octal_char_code:
			meta = (meta << 3) | (c - '0');
			if (meta > char_max)
			{
				*line = peek_line;
				*col = peek_col;
				*p = peek;
				*state = eStart;
				return tokInvalidEscape;
			}
		}

	case eHexCharCode:
		/* We know *p == "0'\x" */
		peek = *p + 4;
		peek_col = *col + 4;
		meta = 0;
		for (;;)
		{
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c == '\\')
			{
				if (tokenAppendUnicode(token,meta) != 0)
				{
					*state = eStart;
					return tokNoMem;
				}

				*line = peek_line;
				*col = peek_col;
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
				peek_line = *line;
				peek_col = *col + 1;
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
				*line = peek_line;
				*col = peek_col;
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
		c = get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c < '0' || c > '9')
		{
			*state = eStart;
			return tokInt;
		}

		if (tokenAppend(token,'.') != 0)
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
		c = get_char(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c >= '0' && c <= '9')
		{
			if (tokenAppend(token,'e') != 0)
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
			uint32_t c2 = get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
			if (c2 == char_more)
				return tokMore;

			if (c2 >= '0' && c2 <= '9')
			{
				if (tokenAppend(token,'e') != 0 || tokenAppend(token,(unsigned char)c) != 0)
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
			c = get_char_conv(&peek,pe,eof,&peek_line,&peek_col);
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
			}

		next_char:
			if (tokenAppendUnicode(token,c) != 0)
			{
				*state = eStart;
				return tokNoMem;
			}

			*line = peek_line;
			*col = peek_col;
			*p = peek;
		}
	}
}

static enum eTokenType next_token(struct Tokenizer* t, struct Token* token)
{
	enum eTokenType tok;
	enum eState state = eStart;

	t->m_start_line = t->m_line;
	t->m_start_col = t->m_col;

	do
	{
		const unsigned char* start;
		const unsigned char* p;
		const unsigned char* pe;

		if (!t->m_eof)
		{
			unsigned char c;
			ssize_t i = read(t->m_s,&c,1);
			if (i == 0)
				t->m_eof = 1;
			else if (tokenAppend(&t->m_buffer,c) != 0)
				return tokNoMem;
		}

		p = start = t->m_buffer.m_str;
		pe = start + t->m_buffer.m_len;

		tok = read_token(&state,&p,pe,t->m_eof,token,&t->m_line,&t->m_col,&t->m_start_line,&t->m_start_col);

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

static struct Operator* lookup_op(const unsigned char* name, size_t len)
{
	struct Operator* op = NULL;

	/* Try to find a infix/suffix op, otherwise find prefix */

	/* Lookup and return */

	return op;
}

static struct Operator* lookup_prefix_op(struct ASTNode* node)
{
	struct Operator* op = NULL;

	/* Try to find a prefix op, otherwise find infix/suffix */

	/* Lookup and return */

	return op;
}

static struct ASTError* syntax_error(const char* message, size_t line, size_t col)
{
	// TODO!!
	void* TODO;

	return (struct ASTError*)1;
}

static int alloc_ast_atom(struct TermBuilder* b, struct Token* token, struct ASTAtomNode* atom, struct ASTError** err_node)
{
	atom->m_base.m_tag = TAG_ATOM_PTR | token->m_len;
	atom->m_offset = alloc_atom(b,token);
	if (atom->m_offset == (uint32_t)-1)
		return 0;

	return 1;
}

static struct ASTNode* alloc_ast_atom_node(struct TermBuilder* b, struct Tokenizer* t, struct Token* token, struct ASTError** err_node)
{
	struct ASTNode* node = NULL;
	if (token->m_len <= 3)
	{
		node = malloc(sizeof(struct ASTNode));
		if (!node)
			*err_node = oom_error();
		else
		{
			if (token->m_len == 1)
				node->m_tag = TAG_ATOM_EMBED | (1 << 24) | (token->m_str[0]);
			else if (token->m_len == 2)
				node->m_tag = TAG_ATOM_EMBED | (2 << 24) | (token->m_str[0] << 8) | (token->m_str[1]);
			else
				node->m_tag = TAG_ATOM_EMBED | (3 << 24) | (token->m_str[0] << 16) | (token->m_str[1] << 8) | (token->m_str[2]);
		}
	}
	else
	{
		struct ASTAtomNode* atom_node = malloc(sizeof(struct ASTAtomNode));
		if (!atom_node)
			*err_node = oom_error();
		else if (token->m_len <= 7)
		{
			atom_node->m_tag = TAG_ATOM_EMBED | (token->m_len << 24);
			memcpy(((unsigned char*)atom_node) + (8 - token->m_len),token->m_str,token->m_len);
			node = &atom_node->m_base;
		}
		else if (token->m_len > 0x0FFFFFFF)
		{
			*err_node = syntax_error("Atom name too long",t->m_start_line,t->m_start_col);
			free(atom_node);
		}
		else if (!alloc_ast_atom(b,token,atom_node,err_node))
		{
			free(atom_node);
		}
		node = &atom_node->m_base;
	}
	return node;
}

static struct ASTCompoundNode* alloc_ast_compound_node(struct TermBuilder* b, struct Tokenizer* t, struct Token* token, uint32_t arity, struct ASTError** err_node)
{
	struct ASTCompoundNode* node = malloc(sizeof(struct ASTCompoundNode) + ((arity-1) * sizeof(struct ASTNode*)));
	if (!node)
		*err_node = oom_error();
	else
	{
		node->m_base.m_tag = TAG_COMPOUND | arity;
		if (token->m_len <= 7)
		{
			node->m_functor.m_base.m_tag = TAG_ATOM_EMBED | (token->m_len << 24);
			memcpy(((unsigned char*)node) + (8 - token->m_len),token->m_str,token->m_len);
		}
		else if (token->m_len > 0x0FFFFFFF)
		{
			*err_node = syntax_error("Functor too long",t->m_start_line,t->m_start_col);
			free(node);
			return NULL;
		}
		else if (!alloc_ast_atom(b,token,&node->m_functor,err_node))
		{
			free(node);
			return NULL;
		}
		memset(node->m_params,0,arity * sizeof(struct ASTNode*));
	}
	return node;
}

static struct ASTNode* free_ast_node(struct ASTNode* node)
{
	if (node)
	{
		if ((node->m_tag & TAG_MASK) == TAG_COMPOUND)
		{
			uint32_t arity = (node->m_tag & ~TAG_MASK);

			while (arity--)
				free_ast_node(((struct ASTCompoundNode*)node)->m_params[arity]);
		}

		free(node);
	}
	return NULL;
}

static struct ASTCompoundNode* convert_ast_atom_to_compound(struct ASTNode* node)
{
	struct ASTCompoundNode* compound_node; 
	uint32_t prev_tag = node->m_tag;
	uint32_t prev_offset = 0;
		
	if ((prev_tag & TAG_ATOM_EMBED) == TAG_ATOM_PTR)
		prev_offset = ((struct ASTAtomNode*)node)->m_offset;
	
	compound_node = realloc(node,sizeof(struct ASTCompoundNode));
	if (!compound_node)
	{
		free_ast_node(node);
		*err_node = oom_error();
		return NULL;
	}

	compound_node->m_base.m_tag = TAG_COMPOUND | 1;
	compound_node->m_functor.m_base.m_tag = prev_tag;
	compound_node->m_functor.m_offset = prev_offset;
	compound_node->m_params[0] = NULL;

	return compound_node;
}

static struct ASTNode* read_ast_number(struct Tokenizer* t, struct ASTNode* node, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	struct ASTNumericNode* numeric_node;
	if (node)
	{
		numeric_node = realloc(node,sizeof(struct ASTNumericNode));
		if (!numeric_node)
		{
			free_ast_node(node);
			*err_node = oom_error();
			return NULL;
		}
	}
	else
	{
		numeric_node = malloc(sizeof(struct ASTNumericNode));
		if (!numeric_node)
		{
			free_ast_node(node);
			*err_node = oom_error();
			return NULL;
		}
	}
	
	if (*next_type == tokFloat)
	{
		errno = 0;
		numeric_node->m_val.m_f64 = strtod((const char*)next->m_str,NULL);
		if (numeric_node->m_val.m_f64 == HUGE_VAL)
		{
			*err_node = syntax_error("Floating point number overflow",t->m_start_line,t->m_start_col);
			return free_ast_node(node);
		}
		else if (numeric_node->m_val.m_f64 == 0.0 && errno == ERANGE)
		{
			*err_node = syntax_error("Floating point number underflow",t->m_start_line,t->m_start_col);
			return free_ast_node(node); 
		}

		numeric_node->m_base.m_tag = TAG_FLOAT_64;
	}
	else if (*next_type == tokCharCode)
	{
		uint32_t v = token->m_str[0];

		if ((token->m_str[0] & 0xE0) == 0xC0)
			val = (token->m_str[0] & 0x1F);
		else if ((token->m_str[0] & 0xF0) == 0xE0)
			val = (token->m_str[0] & 0x0F);
		else
			val = (token->m_str[0] & 0x7);

		for (i=1;i<(unsigned int)token->m_len;++i)
		{
			val = (val << 6) | (token->m_str[i] & 0x3F);
		}

		numeric_node->m_base.m_tag = TAG_INT_28 | v;
	}
	else
	{
		size_t i;
		uint64_t v = 0;

		if (*next_type == tokInt)
		{
			for (i=0;i<next->m_len;++i)
			{
				if (v > INT64_MAX / 10)
				{
					*err_node = syntax_error("Integer overflow",t->m_start_line,t->m_start_col);
					return free_ast_node(node); 
				}
				v = (v * 10) + (next->m_str[i] - '0');
			}
		}
		else if (*next_type == tokBinaryInt)
		{
			for (i=0;i<next->m_len;++i)
			{
				if (v & 0x8000000000000000ULL)
				{
					*err_node = syntax_error("Integer overflow",t->m_start_line,t->m_start_col);
					return free_ast_node(node); 
				}
				v = (v << 1) | (next->m_str[i] - '0');
			}
		}
		else if (*next_type == tokOctalInt)
		{
			for (i=0;i<next->m_len;++i)
			{
				if (v & 0xE000000000000000ULL)
				{
					*err_node = syntax_error("Integer overflow",t->m_start_line,t->m_start_col);
					return free_ast_node(node); 
				}
				v = (v << 3) | (next->m_str[i] - '0');
			}
		}
		else /* tokHexInt */
		{
			for (i=0;i<next->m_len;++i)
			{
				if (v & 0xF000000000000000ULL)
				{
					*err_node = syntax_error("Integer overflow",t->m_start_line,t->m_start_col);
					return free_ast_node(node); 
				}
				v <<= 4;

				if (next->m_str[i] >= 'a')
					v |= (next->m_str[i] - 'a');
				else if (next->m_str[i] >= 'A')
					v |= (next->m_str[i] - 'A');
				else
					v |= (next->m_str[i] - '0');
			}
		}

		if (v > 0x07FFFFFFFFFFFFFFULL)
		{
			numeric_node->m_base.m_tag = TAG_INT_64;
			numeric_node->m_val.m_i64 = v;
		}
		else if (v > 0x07FFFFFF)
		{
			numeric_node->m_base.m_tag = TAG_INT_60 | (uint32_t)(v >> 32);
			numeric_node->m_val.m_u32 = (uint32_t)(v & 0xFFFFFFFF);
		}
		else
			numeric_node->m_base.m_tag = TAG_INT_28 | (uint32_t)v;
	}

	*next_type = next_token(t,next);
	return &numeric_node->m_base;
}

static struct ASTNode* read_ast_negative(struct Tokenizer* t, struct ASTNode* node, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	node = read_ast_number(t,node,next_type,next,err_node);
	if (node)
	{
		if (node->m_tag == TAG_FLOAT)
			((struct ASTNumericNode*)node)->m_val.m_f64 = -((struct ASTNumericNode*)node)->m_val.m_f64;
		else if (node->m_tag == TAG_INT_64)
			((struct ASTNumericNode*)node)->m_val.m_i64 = -((struct ASTNumericNode*)node)->m_val.m_i64;
		else
		{
			int32_t v = node->m_tag & ~TAG_INT_60;
			v = -v;
			node->m_tag = TAG_INT_28 | ((uint32_t)v & ~TAG_INT_60);
		}
	}
	return node;
}

static struct ASTNode* read_ast_term(struct Tokenizer* t, unsigned int max_prec, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node);
static struct ASTNode* read_ast_compound_term(struct TermBuilder* b, struct Tokenizer* t, struct ASTNode* node, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node);

static struct ASTNode* read_ast_arg(struct TermBuilder* b, struct Tokenizer* t, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	if (*next_type == tokName)
	{
		struct Operator* op;
		struct ASTNode* node = alloc_ast_atom_node(b,t,next,err_node);
		if (!node)
			return node;

		*next_type = next_token(t,next);

		if (*next_type == tokOpenCt)
			return read_ast_compound_term(b,t,node,next_type,next,err_node);

		if (node->m_tag == (TAG_ATOM_EMBED | (1 << 24) | '-'))
		{
			if (*next_type >= tokInt && *next_type <= tokFloat)
				return read_ast_negative(t,node,next_type,next,err_node);
		}

		op = lookup_prefix_op(node);
		if (op && op->m_precedence <= 999 && (op->m_specifier == eFX || op->m_specifier == eFY))
		{
			struct ASTCompoundNode* compound_node = convert_ast_atom_to_compound(node);
			if (compound_node)
			{
				node = &compound_node->m_base;
				compound_node->m_params[0] = read_ast_term(t,op->m_specifier == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,err_node);
				if (!compound_node->m_params[0])
					node = free_ast_node(node);
			}
		}

		return node;
	}

	return read_ast_term(t,999,next_type,next,err_node);
}

static struct ASTNode* read_ast_compound_term(struct TermBuilder* b, struct Tokenizer* t, struct ASTNode* node, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	uint32_t arity = 0;
	uint32_t alloc_arity = 1;
	struct ASTCompoundNode* compound_node = convert_ast_atom_to_compound(node);
	if (!compound_node)
		return NULL;

	do
	{
		*next_type = next_token(t,next);

		if ((compound_node->m_base.m_tag & ~TAG_MASK) == alloc_arity)
		{
			struct ASTCompoundNode* new_node;
			uint32_t new_arity = 4;
			if (alloc_arity > 2)
				new_arity = alloc_arity * 2;
							
			new_node = realloc(compound_node,sizeof(struct ASTCompoundNode) + ((new_arity-1) * sizeof(struct ASTNode*)));
			if (!new_node)
			{
				free_ast_node(&compound_node->m_base);
				*err_node = oom_error();
				return NULL;
			}

			alloc_arity = new_arity;
			compound_node = new_node;
		}

		if (arity == ~TAG_MASK)
		{
			*err_node = syntax_error("Too many arguments/ max_arity",t->m_start_line,t->m_start_col);
			return free_ast_node(&compound_node->m_base); 
		}

		compound_node->m_params[arity] = read_ast_arg(b,t,next_type,next,err_node);
		if (!compound_node->m_params[arity])
			return free_ast_node(&compound_node->m_base);

		++arity;
	}
	while (*next_type == tokComma);

	compound_node->m_base.m_tag = TAG_COMPOUND | arity;
	node = &compound_node->m_base;

	if (*next_type != tokClose)
	{
		node = free_ast_node(node);

		if (*next_type == tokNoMem)
			*err_node = oom_error();
		else
			*err_node = syntax_error("Missing )",t->m_start_line,t->m_start_col);
	}
	else
		*next_type = next_token(t,next);
	
	return node;
}

static struct ASTNode* read_ast_list_term(struct TermBuilder* b, struct Tokenizer* t, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	struct ASTNode* node = NULL;
	struct ASTNode** tail = &node;
	
	do
	{
		struct ASTCompoundNode* compound_node = malloc(sizeof(struct ASTCompoundNode) + sizeof(struct ASTNode*));
		if (!compound_node)
		{
			free_ast_node(node);
			*err_node = oom_error();
			return NULL;
		}
		
		compound_node->m_base.m_tag = TAG_COMPOUND | 2;
		compound_node->m_functor.m_base.m_tag = TAG_ATOM_EMBED | (1 << 24) | '.';
		*tail = &compound_node->m_base;

		*next_type = next_token(t,next);
		
		compound_node->m_params[0] = read_ast_arg(b,t,next_type,next,err_node);
		if (!compound_node->m_params[0])
			return free_ast_node(node);
			
		tail = &compound_node->m_params[1];
	}
	while (*next_type == tokComma);

	if (*next_type == tokBar)
	{
		*next_type = next_token(t,next);

		*tail = read_ast_arg(b,t,next_type,next,err_node);
		if (!(*tail))
			return free_ast_node(node);

		*next_type = next_token(t,next);
	}
	else if (*next_type == tokClose)
	{
		/* Append [] */
		*tail = malloc(sizeof(struct ASTNode));
		if (!(*tail))
		{
			free_ast_node(node);
			*err_node = oom_error();
			return NULL;
		}

		(*tail)->m_tag = TAG_ATOM_EMBED | (2 << 24) | ('[' << 8) | ']';
	}
	
	if (*next_type != tokClose)
	{
		node = free_ast_node(node);
		if (*next_type == tokNoMem)
			*err_node = oom_error();
		else
			*err_node = syntax_error("Missing ]",t->m_start_line,t->m_start_col);
	}
	
	return node;
}

static struct ASTNode* read_ast_name(struct TermBuilder* b, struct Tokenizer* t, unsigned int* max_prec, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	struct Operator* op;
	struct ASTNode* node = alloc_ast_atom_node(b,t,next,err_node);
	if (!node)
		return node;

	*next_type = next_token(t,next);

	if (*next_type == tokOpenCt)
	{
		*max_prec = 0;
		return read_ast_compound_term(b,t,node,next_type,next,err_node);
	}

	if (node->m_tag == (TAG_ATOM_EMBED | (1 << 24) | '-'))
	{
		if (*next_type >= tokInt && *next_type <= tokFloat)
		{
			*max_prec = 0;
			return read_ast_negative(t,node,next_type,next,err_node);
		}
	}

	op = lookup_prefix_op(node);
	if (op)
	{
		if (op->m_precedence > *max_prec && (op->m_specifier == eFX || op->m_specifier == eFY))
		{
			struct ASTCompoundNode* compound_node = convert_ast_atom_to_compound(node);
			if (compound_node)
			{
				node = &compound_node->m_base;
				compound_node->m_params[0] = read_ast_term(t,op->m_specifier == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,err_node);
				if (!compound_node->m_params[0])
					node = free_ast_node(node);
				
				*max_prec = op->m_precedence;
			}
			return node;
		}

		if (*max_prec < 1201)
		{
			*err_node = syntax_error("Invalid argument",t->m_start_line,t->m_start_col);
			return free_ast_node(node);
		}

		*max_prec = 1201;
	}
	else
		*max_prec = 0;

	return node;
}

static struct ASTNode* read_ast_term_base(struct TermBuilder* b, struct Tokenizer* t, unsigned int* max_prec, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	struct ASTNode* node = NULL;

	switch (*next_type)
	{
	case tokName:
		node = read_ast_name(b,t,max_prec,next_type,next,err_node);
		break;

	case tokVar:
		node = malloc(sizeof(struct ASTNode) - sizeof(struct ASTNode*));
		if (!node)
			*err_node = oom_error();
		else
		{
			TODO;

			node->m_type = termVar;
			node->m_value.m_compound.m_str = next->m_str;
			node->m_value.m_compound.m_len = next->m_len;
			memset(next,0,sizeof(struct Token));

			*next_type = next_token(t,next);
			*max_prec = 0;
		}
		break;

	case tokInt:
	case tokBinaryInt:
	case tokOctalInt:
	case tokHexInt:
	case tokCharCode:
	case tokFloat:
		node = read_ast_number(t,NULL,next_type,next,err_node);
		*max_prec = 0;
		break;

	case tokDQL:
		if (prolog_flag("double_quotes") == 0 /* atom */)
		{
			/* ISO/IEC 13211-1:1995/Cor.1:2007 */
			node = read_ast_name(t,max_prec,next_type,next,err_node);
		}
		else
		{
			//TODO!
			void* TODO;

			*max_prec = 0;
		}
		break;

	case tokBackQuote:
		if (prolog_flag("back_quotes") == 0 /* atom */)
			node = read_ast_name(t,max_prec,next_type,next,err_node);
		else
		{
			//TODO!
			void* TODO;

			*max_prec = 0;
		}
		break;

	case tokOpen:
	case tokOpenCt:
		*next_type = next_token(t,next);
		node = read_ast_term(t,1201,next_type,next,err_node);
		if (node)
		{
			if (*next_type != tokClose)
			{
				*err_node = syntax_error("Missing )",t->m_start_line,t->m_start_col);
				node = free_ast_node(node);
			}
			else
			{
				*next_type = next_token(t,next);
				*max_prec = 0;
			}
		}
		break;

	case tokOpenL:
		*next_type = next_token(t,next);
		if (*next_type == tokCloseL)
		{
			node = malloc(sizeof(struct ASTNode));
			if (!node)
				*err_node = oom_error();
			else
			{
				node->m_tag = TAG_ATOM_EMBED | (2 << 24) | ('[' << 8) | ']';
				*next_type = next_token(t,next);
			}
		}
		else
			node = read_ast_list_term(b,t,next_type,next,err_node);
		
		*max_prec = 0;
		break;

	case tokOpenC:
		if (tokenAppend(next,'}') != 0)
			*err_node = oom_error();
		else
		{
			struct ASTCompoundNode* compound_node = alloc_ast_compound_node(b,t,next,1,err_node);
			if (compound_node)
			{
				*next_type = next_token(t,next);

				compound_node->m_params[0] = read_ast_term(t,1201,next_type,next,err_node);
				if (!compound_node->m_params[0])
					node = free_ast_node(node);
				else
				{
					node = &compound_node->m_base;
					if (*next_type != tokCloseC)
					{
						node = free_ast_node(node);
						*err_node = syntax_error("Missing }",t->m_start_line,t->m_start_col);
					}
					else
					{
						*next_type = next_token(t,next);
						*max_prec = 0;
					}
				}
			}
		}
		break;

	case tokMore: 
		/* Shouldn't happen... */

	case tokComma:
	case tokClose:
	case tokCloseL:
	case tokCloseC:
	case tokBar:
	case tokEnd:
		/* Unexpected token! */
		*err_node = syntax_error("Unexpected token",t->m_start_line,t->m_start_col);
		break;

	case tokInvalidChar:
		*err_node = syntax_error("Invalid character",t->m_line,t->m_col);
		break;

	case tokEOF:
		*err_node = syntax_error("past-end-of-stream,",t->m_line,t->m_col);
		break;

	case tokInvalidSeq:
		*err_node = syntax_error("Invalid UTF-8 sequence",t->m_line,t->m_col);
		break;

	case tokInvalidEscape:
		*err_node = syntax_error("Invalid character",t->m_line,t->m_col);
		break;

	case tokHalfQuote:
		/* TODO Errors */
		*err_node = syntax_error("Unterminated quoted string",t->m_line,t->m_col);
		break;

	case tokNoMem:
		*err_node = oom_error();
		break;
	}

	return node;
}

static struct ASTNode* read_ast_term(struct Tokenizer* t, unsigned int max_prec, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	unsigned int prev_prec = max_prec;
	struct ASTNode* node = read_ast_term_base(t,&prev_prec,next_type,next,err_node);

	/* This is precedence climbing, if you're interested */
	while (node)
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
				return node;

			binary = 1;
			left_prec = 999;
			right_prec = 1000;

			if (tokenAppend(next,',') != 0)
			{
				node = free_ast_node(node);
				*err_node = oom_error();
				return node;
			}
		}
		else if (*next_type == tokBar)
		{
			/* ISO/IEC 13211-1:1995/Cor.2:2012 */

			struct Operator* op = lookup_op((const unsigned char*)"|",1);
			if (!op || op->m_precedence > max_prec)
				return node;

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

			if (tokenAppend(next,'|') != 0)
			{
				node = free_ast_node(node);
				*err_node = oom_error();
				return node;
			}
		}
		else
			return node;

		if (prev_prec > left_prec)
			return node;
		else
		{
			struct ASTCompoundNode* next_node = alloc_ast_compound_node(b,t,next,1 + binary,err_node);
			if (!next_node)
				node = free_ast_node(node);
			else
			{
				next_node->m_params[0] = node;
				node = &next_node->m_base;
				*next_type = next_token(t,next);

				if (binary)
				{
					next_node->m_params[1] = read_ast_term(t,right_prec,next_type,next,err_node);
					if (!next_node->m_params[1])
						node = free_ast_node(node);
				}
			}
		}
	}

	return node;
}

static struct ASTNode* read_ast(struct Tokenizer* t, struct ASTError** err_node)
{
	struct Token next = {0};
	enum eTokenType next_type = next_token(t,&next);

	struct ASTNode* node = read_ast_term(t,1201,&next_type,&next,err_node);
	if (node)
	{
		if (next_type != tokEnd)
		{
			node = free_ast_node(node);
			if (next_type == tokNoMem)
				*err_node = oom_error();
			else
				*err_node = syntax_error("Unexpected token",t->m_start_line,t->m_start_col);
		}
	}

	free(next.m_str);

	return node;
}

int read_term(struct Stream* s)
{
	struct Tokenizer t = {0};
	struct ASTNode* node = NULL;
	struct ASTError* err_node = NULL;

	t.m_line = 1;
	t.m_s = s;

	node = read_ast(&t,&err_node);
	if (!node)
	{
		/* Do something with err_node */
	}
	else
	{

	}

	return 0;
}

int read_term_atom(const char* str)
{
	void* TODO; /* Build simple stream */
	return read_term(NULL);
}
