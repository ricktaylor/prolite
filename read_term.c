
struct Stream;

enum eTokenType
{
	tokMore = 0,
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
    char*  m_str;  /* Need not be zero-terminated! */
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
	const unsigned char* m_name;
	enum eOpSpec m_spec;
	unsigned int m_precedence;
};


struct ASTNode
{
	enum eTokenType m_type;
	struct Token m_functor;
	unsigned int m_arity;
	struct ASTNode* m_params[1];
};

struct ASTError
{
	int err;
};

static const uint32_t char_max = 0x1FFFF; /* Greatest valid unicode char */
static const uint32_t char_more = char_max + 1;
static const uint32_t char_eof = char_max + 2;
static const uint32_t char_ilseq = char_max + 3;

static int tokenAppend(struct Token* token, char c)
{
	if (token->m_alloc == token->m_len)
	{
		size_t new_size = (token->m_alloc == 0 ? 32 : token->m_alloc * 2);
		char* new = realloc(token->m_str,new_size);
		if (!new)
			return errno;

		token->m_alloc = new_size;
		token->m_str = new;
	}

	token->m_str[token->m_len++] = c;
	return 0;
}

static int tokenAppendUnicode(struct Token* token, uint32_t unicode_char)
{
	if (unicode_char <= 0x7F)
	{
		return tokenAppend(token,(char)unicode_char);
	}
	else
	{
		int err = 0;
		char chars[4] = {0};
		unsigned int count;

		if (unicode_char <= 0x7FF)
		{
			chars[0] = (char)(0xC0 | ((unicode_char & 0x7C0) >> 6));
			chars[1] = (char)(0x80 | (unicode_char & 0x3F));
			count = 2;
		}
		else if (unicode_char <= 0xFFFF)
		{
			chars[0] = (char)(0xE0 | ((unicode_char & 0xF000) >> 12));
			chars[1] = (char)(0x80 | ((unicode_char & 0xFC0) >> 6));
			chars[2] = (char)(0x80 | (unicode_char & 0x3F));
			count = 3;
		}
		else
		{
			chars[0] = (char)(0xF0 | ((unicode_char & 0x1C0000) >> 18));
			chars[1] = (char)(0x80 | ((unicode_char & 0x3F000) >> 12));
			chars[2] = (char)(0x80 | ((unicode_char & 0xFC0) >> 6));
			chars[3] = (char)(0x80 | (unicode_char & 0x3F));
			count = 3;
		}

		for (unsigned int i=0; !err && i<count; ++i)
			err = tokenAppend(token,chars[i]);

		return err;
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

			val = (val << 6) | (cbuf[i] & 0x3F);
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

static int meta_char(uint32_t c, struct Token* token)
{
	switch (c)
	{
	case '\'':
	case '"':
	case '\\':
	case '`':
		tokenAppend(c);
		return 1;

	case 'a':
		tokenAppend(7);
		return 1;

	case 'b':
		tokenAppend(127);
		return 1;

	case 'f':
		tokenAppend(12);
		return 1;

	case 'n':
		tokenAppend(10);
		return 1;

	case 'r':
		tokenAppend(13);
		return 1;

	case 't':
		tokenAppend(9);
		return 1;

	case 'v':
		tokenAppend(11);
		return 1;

	default:
		return 0;
	}
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

		c = get_char(p,pe,eof,line,col);
		if (c == char_more)
			return tokMore;

		/* Check for ( first*/
		if (c <= char_max)
		{
			c = char_conversion(c);
			if (c == '(')
				return tokOpenCt;
		}

		*state = eLayout;
		goto layout;

	case eLayout:
	case eSingleComment:
	case eMultiComment2:
	case eMultiComment3:
		c = get_char(p,pe,eof,line,col);
		if (c == char_more)
			return tokMore;

		if (c <= char_max)
			c = char_conversion(c);

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
					tokenAppend(token,c);
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
					goto next;

				case eaName:
					*state = eName;
					goto next;

				case eaVar:
					*state = eVar;
					goto next;

				case eaZero:
					*state = eZero;
					goto zero;

				case eaNumber:
					*state = eInteger;
					goto next;

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
			c = get_char(p,pe,eof,line,col);
			if (c == char_more)
				return tokMore;

			if (c <= char_max)
				c = char_conversion(c);
		}

	multi_comment:
		peek = *p;
		peek_line = *line;
		peek_col = *col;

	case eMultiComment1:
		c = get_char(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c <= char_max)
			c = char_conversion(c);

		if (c != '*')
		{
			tokenAppend(token,'/');
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
		peek_line = *line;
		peek_col = *col;

	case eDot:
		c = get_char(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c == char_eof)
			return tokEnd;

		if (c <= char_max)
			c = char_conversion(c);

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

		tokenAppend(token,'.');
		*state = eGraphicName;
		goto graphic_name;

	quote:
		peek = *p;
		peek_line = *line;
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
								*state == eDoubleQuoteOct;
							else
								*state == eBackQuoteOct;
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
								*state == eDoubleQuoteHex;
							else
								*state == eBackQuoteHex;
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
				tokenAppend(token,c);
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
				tokenAppend(token,c);
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
				tokenAppend(token,c);
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
				tokenAppendUnicode(token,c);
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
		get_char(&peek,pe,eof,&peek_line,&peek_col);
		get_char(&peek,pe,eof,&peek_line,&peek_col);
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
					tokenAppendUnicode(token,meta);
					if (*state == eSingleQuoteOct)
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
		get_char(&peek,pe,eof,&peek_line,&peek_col);
		get_char(&peek,pe,eof,&peek_line,&peek_col);
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
					tokenAppendUnicode(token,meta);
					if (*state == eSingleQuoteHex)
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
		c = get_char(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c <= char_max)
			c = char_conversion(c);

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
					tokenAppend(token,'\'');
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
				tokenAppendUnicode(token,c);
				*line = peek_line;
				*col = peek_col;
				*p = peek;
				return tokCharCode;
			}
		}
		else if (c == 'b')
		{
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c <= char_max)
				c = char_conversion(c);

			if (c == '0' || c == '1')
			{
				*state = eBinaryInt;
				goto next_char;
			}
		}
		else if (c == 'o')
		{
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c <= char_max)
				c = char_conversion(c);

			if (c >= '0' && c <= '7')
			{
				*state = eOctalInt;
				goto next_char;
			}
		}
		else if (c == 'x')
		{
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c <= char_max)
				c = char_conversion(c);

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
		get_char(p,pe,eof,line,col);
		peek = *p;
		peek_line = *line;
		peek_col = *col;

		get_char(&peek,pe,eof,&peek_line,&peek_col);
		get_char(&peek,pe,eof,&peek_line,&peek_col);
		get_char(&peek,pe,eof,&peek_line,&peek_col);

		meta = 0;
		for (;;)
		{
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c == '\\')
			{
				tokenAppendUnicode(token,meta);
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
				peek = *p;
				peek_line = *line;
				peek_col = *col;
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
		get_char(p,pe,eof,line,col);
		peek = *p;
		peek_line = *line;
		peek_col = *col;

		get_char(&peek,pe,eof,&peek_line,&peek_col);
		get_char(&peek,pe,eof,&peek_line,&peek_col);
		get_char(&peek,pe,eof,&peek_line,&peek_col);

		meta = 0;
		for (;;)
		{
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c == '\\')
			{
				tokenAppendUnicode(token,meta);
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
				peek = *p;
				peek_line = *line;
				peek_col = *col;
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
		get_char(&peek,pe,eof,&peek_line,&peek_col);

	decimal:
		c = get_char(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c <= char_max)
			c = char_conversion(c);

		if (c < '0' || c > '9')
		{
			*state = eStart;
			return tokInt;
		}

		tokenAppend(token,'.');
		*state = eFraction;
		goto next_char;

	case eExponent:
		/* We know *p == 'E' or 'e' */
		get_char(&peek,pe,eof,&peek_line,&peek_col);

	exponent:
		c = get_char(&peek,pe,eof,&peek_line,&peek_col);
		if (c == char_more)
			return tokMore;

		if (c <= char_max)
			c = char_conversion(c);

		if (c >= '0' && c <= '9')
		{
			tokenAppend(token,'e');
			*state = eMantissa;
			goto next_char;
		}

		if (c == '-' || c == '+')
		{
			/* Check the next char */
			uint32_t c2 = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c2 == char_more)
				return tokMore;

			if (c2 <= char_max)
				c2 = char_conversion(c2);

			if (c2 >= '0' && c2 <= '9')
			{
				tokenAppend(token,'e');
				tokenAppend(token,c);
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
			c = get_char(&peek,pe,eof,&peek_line,&peek_col);
			if (c == char_more)
				return tokMore;

			if (c <= char_max)
				c = char_conversion(c);

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
			tokenAppendUnicode(token,c);
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
			ssize_t i = read(t->m_s,&c,1);
			if (i == 0)
				t->m_eof = 1;
			else
				tokenAppend(t->m_buffer,c);
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

static struct Operator* lookup_op(const unsigned char* name, size_t len, int prefix)
{
	struct Operator* op = NULL;

	/* If prefix, then try to find a prefix op, otherwise default to a infix/suffix, and vice-versa */

	/* Lookup and return */

	return op;
}

static struct ASTNode* alloc_ast_node(enum eTokenType type, struct Token* token, unsigned int arity)
{
	struct ASTNode* node = malloc(sizeof(struct ASTNode) - sizeof(struct ASTNode*) + (arity * sizeof(struct ASTNode*)));
	if (node)
	{
		node->m_type = type;
		node->m_functor = *token;
		memset(token,0,sizeof(struct Token));
		m_arity = arity;
		if (arity)
			memset(node->m_params,0,arity * sizeof(struct ASTNode*));
	}
	return node;
}

static struct ASTNode* free_ast_node(struct ASTNode* node)
{
	if (node)
	{
		while (node->m_arity--)
			free_ast_node(node->m_params[node->m_arity]);

		free(node->m_functor.m_str);
		free(node);
	}
	return NULL;
}

static struct ASTNode* realloc_ast_node(struct ASTNode* node, unsigned int arity)
{
	unsigned int old_arity = node->m_arity;
	if (arity > old_arity)
	{
		struct ASTNode* new_node = realloc(node,sizeof(struct ASTNode) - sizeof(struct ASTNode*) + (arity * sizeof(struct ASTNode*)));
		if (!new_node)
			node = free_ast_node(node);
		else
		{
			node = new_node;
			node->m_arity = arity;
			if (arity)
				memset(&node->m_params[old_arity],0,(arity - old_arity) * sizeof(struct ASTNode*));
		}
	}
	return node;
}

static struct ASTNode* read_ast_term(struct Tokenizer* t, unsigned int max_prec, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node);
static struct ASTNode* read_ast_compound_term(struct Tokenizer* t, struct ASTNode** functor_node, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node);

static struct ASTNode* read_ast_arg(struct Tokenizer* t, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	if (*next_type == tokName)
	{
		struct Operator* op;
		struct ASTNode* node = alloc_ast_node(*next_type,next,0);
		if (!node)
			return node;

		*next_type = next_token(t,next);

		if (*next_type == tokOpenCt)
			return read_ast_compound_term(t,&node,next_type,next,err_node);

		if (node->m_functor.m_len == 1 && node->m_functor.m_str[0] == '-')
		{
			if (*next_type == tokInt ||
				*next_type == tokFloat ||
				*next_type == tokBinaryInt ||
				*next_type == tokOctalInt ||
				*next_type == tokHexInt ||
				*next_type == tokCharCode)
			{
				node = realloc_ast_node(node,1);
				if (node)
				{
					node->m_params[0] = alloc_ast_node(*next_type,next,0);
					if (!node->m_params[0])
						node = free_ast_node(node);
					else
						*next_type = next_token(t,next);

					*max_prec = 0;
				}
				return node;
			}
		}

		if ((op = lookup_op(node->m_functor.m_str,node->m_functor.m_len,1)) &&
				op->m_precedence <= 999 && (op->m_spec == eFX || op->m_spec == eFY))
		{
			node = realloc_ast_node(node,1);
			if (node)
			{
				node->m_params[0] = read_ast_term(t,op->m_spec == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,err_node);
				if (!node->m_params[0])
					node = free_ast_node(node);
			}
		}

		return node;
	}

	return read_ast_term(t,999,next_type,next,err_node);
}

static struct ASTNode* read_ast_compound_term(struct Tokenizer* t, struct ASTNode** functor_node, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	struct ASTNode* node = *functor_node;
	unsigned int alloc_arity = node->m_arity;

	do
	{
		*next_type = next_token(t,next);

		if (node->m_arity == alloc_arity)
		{
			unsigned int new_size = alloc_arity < 2 ? 4 : alloc_arity * 2;
			struct ASTNode* new_node = realloc(node,sizeof(struct ASTNode) + ((new_size-1) * sizeof(struct ASTNode*)));
			if (!new_node)
				return free_ast_node(node);

			alloc_arity = new_size;
			node = new_node;
		}

		node->m_params[node->m_arity] = read_ast_arg(t,next_type,next,err_node);
		if (!node->m_params[node->m_arity])
			return free_ast_node(node);

		++node->m_arity;
	}
	while (node && *next_type == tokComma);

	if (node)
	{
		if (*next_type != tokClose)
		{
			*err_node = syntax_error("Missing )",t->m_start_line,t->m_start_col);
			node = free_ast_node(node);
		}
		else
			*next_type = next_token(t,next);
	}

	return node;
}

static struct ASTNode* read_ast_list_term(struct Tokenizer* t, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	struct ASTNode* node = NULL;
	struct ASTNode** tail = &node;

	do
	{
		static const Token dot = {0,1,"."};

		*tail = alloc_ast_node(tokName,&dot,2);
		if (!(*tail))
			node = free_ast_node(node);
		else
		{
			*next_type = next_token(t,next);

			(*tail)->m_params[0] = read_ast_arg(t,next_type,next,err_node);
			if (!(*tail)->m_params[0])
				node = free_ast_node(node);
			else
				tail = &(*tail)->m_params[1];
		}
	}
	while (node && *next_type == tokComma);

	if (node)
	{
		if (*next_type == tokBar)
		{
			*next_type = next_token(t,next);

			*tail = read_ast_arg(t,next_type,next,err_node);
			if (!(*tail))
				node = free_ast_node(node);
		}
		else if (*next_type == tokClose)
		{
			/* Append [] */
			static const Token end_list = {0,2,"[]"};

			*tail = alloc_ast_node(tokName,&end_list,0);
			if (!(*tail))
				node = free_ast_node(node);
		}
	}

	if (node)
	{
		if (*next_type != tokClose)
		{
			*err_node = syntax_error("Missing ]",t->m_start_line,t->m_start_col);
			node = free_ast_node(node);
		}
		else
			*next_type = next_token(t,next);
	}

	return node;
}

static struct ASTNode* read_ast_term_name(struct Tokenizer* t, unsigned int* max_prec, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	struct Operator* op;
	struct ASTNode* node = alloc_ast_node(*next_type,next,0);
	if (!node)
		return node;

	*next_type = next_token(t,next);

	if (*next_type == tokOpenCt)
	{
		*max_prec = 0;
		return read_ast_compound_term(t,&node,next_type,next,err_node);
	}

	if (node->m_functor.m_len == 1 && node->m_functor.m_str[0] == '-')
	{
		if (*next_type == tokInt ||
			*next_type == tokFloat ||
			*next_type == tokBinaryInt ||
			*next_type == tokOctalInt ||
			*next_type == tokHexInt ||
			*next_type == tokCharCode)
		{
			node = realloc_ast_node(node,1);
			if (node)
			{
				node->m_params[0] = alloc_ast_node(*next_type,next,0);
				if (!node->m_params[0])
					node = free_ast_node(node);
				else
					*next_type = next_token(t,next);

				*max_prec = 0;
			}
			return node;
		}
	}

	if ((op = lookup_op(node->m_functor.m_str,node->m_functor.m_len,1)))
	{
		if (op->m_precedence > *max_prec && (op->m_spec == eFX || op->m_spec == eFY))
		{
			node = realloc_ast_node(node,1);
			if (node)
			{
				node->m_params[0] = read_ast_term(t,op->m_spec == eFX ? op->m_precedence-1 : op->m_precedence,next_type,next,err_node);
				if (!node->m_params[0])
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

static struct ASTNode* read_ast_term_basic(struct Tokenizer* t, unsigned int* max_prec, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	struct ASTNode* node = NULL;

	switch (next_type)
	{
	case tokName:
		node = read_ast_term_name(t,max_prec,next_type,next,err_node);
		break;

	case tokVar:
	case tokInt:
	case tokBinaryInt:
	case tokOctalInt:
	case tokHexInt:
	case tokCharCode:
	case tokFloat:
	case tokDQL:
	case tokBackQuote:
		node = alloc_ast_node(*next_type,next,0);
		if (node)
		{
			*next_type = next_token(t,next);
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
			node = alloc_ast_node(tokOpenL,next,0);
			if (node)
			{
				tokenAppend(node->m_functor,']');
				*next_type = next_token(t,next);
			}
		}
		else
		{
			node = read_ast_list_term(t,&node,next_type,next,err_node);
		}
		*max_prec = 0;
		break;

	case tokOpenC:
		node = alloc_ast_node(*next_type,next,1);
		if (node)
		{
			tokenAppend(node->m_functor,'}');

			*next_type = next_token(t,next);

			node->m_params[0] = read_ast_term(t,1201,next_type,next,err_node);
			if (!node->m_params[0])
				node = free_ast_node(node);
			else
			{
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

		break;

	case tokMore: /* Shouldn't happen... */

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
	}

	return node;
}

static struct ASTNode* read_ast_term(struct Tokenizer* t, unsigned int max_prec, enum eTokenType* next_type, struct Token* next, struct ASTError** err_node)
{
	unsigned int prev_prec = max_prec;
	struct ASTNode* node = read_ast_term_basic(t,&prev_prec,next_type,next,err_node);

	/* This is precedence climbing, if you're interested */
	while (node)
	{
		struct ASTNode* next_node;
		unsigned int right_prec;
		unsigned int left_prec;
		int binary = 0;

		if (*next_type == tokName)
		{
			struct Operator* op = lookup_op(next->m_str,next->m_len,0);
			if (!op || op->m_precedence > max_prec)
				return node;

			if (op->m_spec)
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

			*next_type = tokName;
			tokenAppend(next,',');
		}
		else if (*next_type == tokBar)
		{
			/* ISO/IEC 13211-1:1995/Cor.2:2012 */

			struct Operator* op = lookup_op("|",1,0);
			if (!op || op->m_precedence > max_prec)
				return node;

			if (op->m_spec)
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

			*next_type = tokName;
			tokenAppend(next,'|');
		}
		else
			return node;

		if (prev_prec > left_prec)
			return node;

		next_node = alloc_ast_node(*next_type,next,1 + binary);
		if (!next_node)
			node = free_ast_node(node);
		else
		{
			next_node->m_params[0] = node;
			node = next_node;
			*next_type = next_token(t,next);

			if (binary)
			{
				node->m_params[1] = read_ast_term(t,right_prec,next_type,next,err_node);
				if (!node->m_params[1])
					node = free_ast_node(node);
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
			*err_node = syntax_error("Unexpected token",t->m_start_line,t->m_start_col);
			node = free_ast_node(node);
		}
	}

	return node;
}
