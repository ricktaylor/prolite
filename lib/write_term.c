#include "write_term.h"

#include <stdio.h>
#include <inttypes.h>
#include <math.h>

typedef struct write_context
{
	context_t*              m_context;
	prolite_stream_t*       m_s;
	write_options_t         m_options;
	const operator_table_t* m_operators;
	unsigned int            m_precedence;
	_Bool                   m_add_spaces;
	jmp_buf                 m_jmp;
} write_context_t;

typedef enum char_class
{
	ccErr = 0,
	ccWhitespace,
	ccGraphic,
	ccUpper,
	ccLower,
	ccNumeric,
	ccCut,
	ccSemiColon,
	ccSingleQuote,
	ccDot,
	ccSingleComment,
	ccFwSlash,
	ccOpen,
	ccClose,
	ccOpenL,
	ccCloseL,
	ccOpenC,
	ccCloseC,
	ccBar,
	ccComma
} char_class_t;

static const char_class_t c_char_classes[256] = {
	/* \x0 */ ccErr, /* \x1 */ ccErr, /* \x2 */ ccErr, /* \x3 */ ccErr,
	/* \x4 */ ccErr, /* \x5 */ ccErr, /* \x6 */ ccErr, /* \x7 */ ccErr,
	/* \x8 */ ccErr, /* \t */ ccWhitespace,/* \n */ ccWhitespace,/* \xb */ ccErr,
	/* \xc */ ccErr, /* \xd */ ccErr, /* \xe */ ccErr, /* \xf */ ccErr,
	/* \x10 */ ccErr,/* \x11 */ ccErr,/* \x12 */ ccErr,/* \x13 */ ccErr,
	/* \x14 */ ccErr,/* \x15 */ ccErr,/* \x16 */ ccErr,/* \x17 */ ccErr,
	/* \x18 */ ccErr,/* \x19 */ ccErr,/* \x1a */ ccErr,/* \x1b */ ccErr,
	/* \x1c */ ccErr,/* \x1d */ ccErr,/* \x1e */ ccErr,/* \x1f */ ccErr,
	/* space */ ccWhitespace, /* ! */ ccCut, /* " */ ccErr, /* # */ ccGraphic,
	/* $ */ ccGraphic, /* % */ ccSingleComment, /* & */ ccGraphic, /* ' */ ccSingleQuote,
	/* ( */ ccOpen, /* ) */ ccClose, /* * */ ccGraphic, /* + */ ccGraphic,
	/* , */ ccComma, /* - */ ccGraphic, /* . */ ccDot, /* / */ ccFwSlash,
	/* 0 */ ccNumeric, /* 1 */ ccNumeric, /* 2 */ ccNumeric, /* 3 */ ccNumeric,
	/* 4 */ ccNumeric, /* 5 */ ccNumeric, /* 6 */ ccNumeric, /* 7 */ ccNumeric,
	/* 8 */ ccNumeric, /* 9 */ ccNumeric, /* : */ ccGraphic, /* ; */ ccSemiColon,
	/* < */ ccGraphic, /* = */ ccGraphic, /* > */ ccGraphic, /* ? */ ccGraphic,
	/* @ */ ccGraphic, /* A */ ccUpper, /* B */ ccUpper, /* C */ ccUpper,
	/* D */ ccUpper, /* E */ ccUpper, /* F */ ccUpper, /* G */ ccUpper,
	/* H */ ccUpper, /* I */ ccUpper, /* J */ ccUpper, /* K */ ccUpper,
	/* L */ ccUpper, /* M */ ccUpper, /* N */ ccUpper, /* O */ ccUpper,
	/* P */ ccUpper, /* Q */ ccUpper, /* R */ ccUpper, /* S */ ccUpper,
	/* T */ ccUpper, /* U */ ccUpper, /* V */ ccUpper, /* W */ ccUpper,
	/* X */ ccUpper, /* Y */ ccUpper, /* Z */ ccUpper, /* [ */ ccOpenL,
	/* \ */ ccGraphic, /* ] */ ccCloseL, /* ^ */ ccGraphic, /* _ */ ccUpper,
	/* ` */ ccErr, /* a */ ccLower, /* b */ ccLower, /* c */ ccLower,
	/* d */ ccLower, /* e */ ccLower, /* f */ ccLower, /* g */ ccLower,
	/* h */ ccLower, /* i */ ccLower, /* j */ ccLower, /* k */ ccLower,
	/* l */ ccLower, /* m */ ccLower, /* n */ ccLower, /* o */ ccLower,
	/* p */ ccLower, /* q */ ccLower, /* r */ ccLower, /* s */ ccLower,
	/* t */ ccLower, /* u */ ccLower, /* v */ ccLower, /* w */ ccLower,
	/* x */ ccLower, /* y */ ccLower, /* z */ ccLower, /* { */ ccOpenC,
	/* | */ ccBar, /* } */ ccCloseC, /* ~ */ ccGraphic, /*0x7f */ ccErr
};

static char_class_t write_term_inner(write_context_t* context, char_class_t cc_prev, const term_t* term);

static void write_chars(write_context_t* context, const void* src, size_t len)
{
	prolite_stream_error_t err = prolite_stream_error_none;
	int w = (*context->m_s->m_fn_write)(context->m_s,src,len,&err);
	if (!w)
	{
		// TODO: Push some kind of error

		longjmp(context->m_jmp,err);
	}
}

static char_class_t write_quoted(write_context_t* context, const unsigned char* src, size_t len)
{
	write_chars(context,"'",1);

	for (size_t i = 0; i < len; ++i)
	{
		switch (src[i])
		{
		case '\'':
			write_chars(context,"''",2);
			break;

		case '\a':
			write_chars(context,"\\a",2);
			break;

		case '\b':
			write_chars(context,"\\b",2);
			break;

		case '\f':
			write_chars(context,"\\f",2);
			break;

		case '\n':
			write_chars(context,"\\n",2);
			break;

		case '\r':
			write_chars(context,"\\r",2);
			break;

		case '\t':
			write_chars(context,"\\t",2);
			break;

		case '\v':
			write_chars(context,"\\v",2);
			break;

		case 0x7F:
			write_chars(context,"\\x7F\\",5);
			break;

		default:
			if (src[i] < 32 || src[i] > 0x7F)
			{
				size_t count = 0;
				if ((src[i] & 0xE0) == 0xC0)
					count = 1;
				else if ((src[i] & 0xF0) == 0xE0)
					count = 2;
				else if ((src[i] & 0xF8) == 0xF0)
					count = 3;

				if (!count || i + count >= len)
				{
					// This should catch malformed UTF-8 and non-printables
					char buf[6];
					int p = snprintf(buf,sizeof(buf)-1,"\\x%X\\",src[i]);
					write_chars(context,buf,p);
				}
				else
				{
					write_chars(context,&src[i],count + 1);
					i += count;
				}
			}
			else
				write_chars(context,&src[i],1);
			break;
		}
	}

	write_chars(context,"'",1);
	return ccSingleQuote;
}

static int should_quote_atom(const string_t* str)
{
	if (str->m_len == 0)
		return 1;

	switch (c_char_classes[str->m_str[0]])
	{
	case ccSemiColon:
	case ccCut:
		return str->m_len == 1 ? 0 : 1;

	case ccOpenL:
		return (str->m_len == 2 && str->m_str[1] == ']') ? 0 : 1;

	case ccOpenC:
		return (str->m_len == 2 && str->m_str[1] == '}') ? 0 : 1;

	case ccLower:
		for (size_t i = 1; i < str->m_len; ++i)
		{
			char_class_t cc = c_char_classes[str->m_str[i]];
			if (cc != ccLower && cc != ccUpper && cc != ccNumeric)
				return 1;
		}
		return 0;

	case ccDot:
		if (str->m_len == 1)
			return 1;
		else
		{
			char_class_t cc = c_char_classes[str->m_str[1]];
			if (cc == ccWhitespace || cc == ccSingleComment)
				return 1;
		}
		goto graphic;

	case ccFwSlash:
		if (str->m_len > 1 && str->m_str[1] == '*')
			return 1;
		goto graphic;

	case ccGraphic:
	graphic:
		for (size_t i = 1; i < str->m_len; ++i)
		{
			char_class_t cc = c_char_classes[str->m_str[i]];
			if (cc != ccGraphic && cc != ccFwSlash && cc != ccDot)
				return 1;
		}
		return 0;

	default:
		return 1;
	}
}

static int write_variable_name_inner(write_context_t* context, char_class_t cc_prev, const term_t* term, const term_t* vn)
{
	if (MASK_DEBUG_INFO(vn->m_u64val) == PACK_COMPOUND_EMBED_1(2,'='))
	{
		const term_t* a = get_first_arg(vn,NULL);
		if (unpack_term_type(a) == prolite_atom)
		{
			const term_t* v = get_next_arg(a);
			if (term_compare(v,term))
			{
				int was_quoted = context->m_options.quoted;
				context->m_options.quoted = 0;

				write_term_inner(context,cc_prev,a);

				context->m_options.quoted = was_quoted;
				return 1;
			}
		}
	}

	return 0;
}

static int write_variable_name(write_context_t* context, char_class_t cc_prev, const term_t* term)
{
	int ret = 0;
	const term_t* vn = context->m_options.variable_names;
	while (MASK_DEBUG_INFO(vn->m_u64val) == PACK_COMPOUND_EMBED_1(2,'.') && !ret)
	{
		vn = get_first_arg(vn,NULL);

		ret = write_variable_name_inner(context,cc_prev,term,vn);

		vn = get_next_arg(vn);
	}

	if (!ret && MASK_DEBUG_INFO(vn->m_u64val) != PACK_ATOM_EMBED_2('[',']'))
		ret = write_variable_name_inner(context,cc_prev,term,vn);

	return ret;
}

static char_class_t write_compound(write_context_t* context, char_class_t cc_prev, const term_t* term, const string_t* str, size_t arity, int quoted)
{
	if (str->m_len == 0)
		write_chars(context,"''",2);
	else
	{
		if (quoted)
			quoted = should_quote_atom(str);

		if (quoted)
			write_chars(context,"'",1);

		if (str->m_len)
			write_chars(context,str->m_str,str->m_len);

		if (quoted)
			write_chars(context,"'",1);
	}

	write_chars(context,"(",1);

	unsigned int prev_precedence = context->m_precedence;
	context->m_precedence = 999;

	const term_t* arg = get_first_arg(term,NULL);
	for (size_t i = 0; i < arity; ++i)
	{
		if (i)
		{
			write_chars(context,",",1);
			cc_prev = ccComma;
		}

		write_term_inner(context,cc_prev,arg);
		arg = get_next_arg(arg);
	}

	context->m_precedence = prev_precedence;

	write_chars(context,")",1);
	return ccClose;
}

static char_class_t write_operator(write_context_t* context, char_class_t cc_prev, const term_t* term, const string_t* str, size_t arity, const operator_t* op)
{
	switch (op->m_specifier)
	{
	case eFX:
	case eFY:
	case eXF:
	case eYF:
		if (arity != 1)
			return write_compound(context,cc_prev,term,str,arity,1);
		break;

	default:
		if (arity != 2)
			return write_compound(context,cc_prev,term,str,arity,1);
		break;
	}

	unsigned int left_prec = 0;
	unsigned int right_prec = 0;
	switch (op->m_specifier)
	{
	case eFX:
		right_prec = op->m_precedence - 1;
		break;

	case eFY:
		right_prec = op->m_precedence;
		break;

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

	_Bool prev_add_spaces = context->m_add_spaces;
	unsigned int prev_precedence = context->m_precedence;
	if (prev_precedence < op->m_precedence)
	{
		write_chars(context,"(",1);
		cc_prev = ccOpen;
	}

	const term_t* arg = get_first_arg(term,NULL);
	if (left_prec)
	{
		context->m_precedence = left_prec;

		cc_prev = write_term_inner(context,cc_prev,arg);

		switch (c_char_classes[str->m_str[0]])
		{
		case ccGraphic:
			if (cc_prev == ccGraphic)
				write_chars(context," ",1);
			break;

		case ccLower:
		case ccUpper:
		case ccNumeric:
			if (cc_prev == ccLower || cc_prev == ccUpper || cc_prev == ccNumeric)
				write_chars(context," ",1);
			break;

		default:
			break;
		}

		if (right_prec)
			arg = get_next_arg(arg);
	}

	write_chars(context,str->m_str,str->m_len);
	cc_prev = c_char_classes[str->m_str[str->m_len-1]];

	if (right_prec)
	{
		context->m_add_spaces = 1;
		context->m_precedence = right_prec;
		cc_prev = write_term_inner(context,cc_prev,arg);
	}

	if (prev_precedence < op->m_precedence)
	{
		write_chars(context,")",1);
		cc_prev = ccClose;
	}

	context->m_precedence = prev_precedence;
	context->m_add_spaces = prev_add_spaces;
	return cc_prev;
}

static int is_write_number_vars(const term_t* term)
{
	if (MASK_DEBUG_INFO(term->m_u64val) == PACK_COMPOUND_EMBED_4(1,'$','V','A','R'))
	{
		size_t arity;
		const term_t* arg = get_first_arg(term,&arity);
		if (arity == 1 && unpack_term_type(arg) == prolite_number && nearbyint(arg->m_dval) == arg->m_dval && arg->m_dval >= 0)
			return 1;
	}

	return 0;
}

static char_class_t write_term_inner(write_context_t* context, char_class_t cc_prev, const term_t* term)
{
	prolite_type_t type = unpack_term_type(term);
	switch (type)
	{
	case prolite_var:
		{
			if (context->m_context)
			{
				const term_t* v = deref_local_var(context->m_context,term);
				if (v != term)
					return write_term_inner(context,cc_prev,v);
			}

			if (context->m_options.variable_names)
				return write_variable_name(context,cc_prev,term);

			if (context->m_add_spaces)
			{
				switch (cc_prev)
				{
				case ccLower:
				case ccUpper:
				case ccNumeric:
					write_chars(context," ",1);
					break;

				default:
					break;
				}
			}

			char buf[22];
			int p = snprintf(buf,sizeof(buf)-1,"_%zu",unpack_var_index(term));
			write_chars(context,buf,p);
			cc_prev = ccNumeric;
		}
		break;

	case prolite_number:
		{
			if (context->m_add_spaces)
			{
				if (cc_prev == ccLower || cc_prev == ccUpper || cc_prev == ccNumeric)
					write_chars(context," ",1);
			}

			char buf[30];
			int p = snprintf(buf,sizeof(buf)-1,"%.17g",term->m_dval);
			if (context->m_options.quoted)
			{
				// TODO: round trip check
			}
			write_chars(context,buf,p);
			cc_prev = ccNumeric;
		}
		break;

	case prolite_atom:
		{
			string_t str;
			unpack_string(term,&str,NULL);
			if (str.m_len == 0)
			{
				write_chars(context,"''",2);
				return ccSingleQuote;
			}

			if (context->m_options.quoted && should_quote_atom(&str))
				return write_quoted(context,str.m_str,str.m_len);

			if (context->m_add_spaces)
			{
				switch (c_char_classes[str.m_str[0]])
				{
				case ccGraphic:
					if (cc_prev == ccGraphic)
						write_chars(context," ",1);
					break;

				case ccLower:
				case ccUpper:
				case ccNumeric:
					if (cc_prev == ccLower || cc_prev == ccUpper || cc_prev == ccNumeric)
						write_chars(context," ",1);
					break;

				default:
					break;
				}
			}

			write_chars(context,str.m_str,str.m_len);
			cc_prev = c_char_classes[str.m_str[str.m_len-1]];
		}
		break;

	case prolite_chars:
		{
			string_t str;
			unpack_string(term,&str,NULL);

			if (str.m_len == 0 || !context->m_options.ignore_ops)
				write_chars(context,"[",1);

			size_t closes = 0;
			for (size_t i = 0; i < str.m_len; ++i)
			{
				if (i)
					write_chars(context,",",1);

				if (context->m_options.ignore_ops)
					write_chars(context,"'.'(",4);

				if (str.m_str[i] < 32 || str.m_str[i] >= 0x7F)
				{
					unsigned int count = 0;
					if ((str.m_str[i] & 0xE0) == 0xC0)
						count = 1;
					else if ((str.m_str[i] & 0xF0) == 0xE0)
						count = 2;
					else if ((str.m_str[i] & 0xF8) == 0xF0)
						count = 3;

					if (!count || i + count >= str.m_len)
					{
						// This should catch malformed UTF-8 and non-printables
						char buf[8];
						int p = snprintf(buf,sizeof(buf)-1,"'\\x%X\\'",str.m_str[i]);
						write_chars(context,buf,p);
					}
					else
					{
						write_chars(context,&str.m_str[i],count + 1);
						i += count;
					}
				}
				else
					write_chars(context,&str.m_str[i],1);

				++closes;
			}

			cc_prev = ccCloseL;
			if (str.m_len == 0 || !context->m_options.ignore_ops)
				write_chars(context,"]",1);
			else
			{
				write_chars(context,"[]",2);

				for (size_t i = 0; i < closes; ++i)
					write_chars(context,")",1);

				cc_prev = ccClose;
			}
		}
		break;

	case prolite_charcodes:
		{
			char buf[20];
			string_t str;
			unpack_string(term,&str,NULL);

			if (str.m_len == 0 || !context->m_options.ignore_ops)
				write_chars(context,"[",1);

			size_t closes = 0;
			for (size_t i = 0; i < str.m_len; ++i)
			{
				if (i)
					write_chars(context,",",1);

				if (context->m_options.ignore_ops)
					write_chars(context,"'.'(",4);

				uint32_t val = str.m_str[i];
				if (str.m_str[i] > 0x7F)
				{
					unsigned int count = 0;
					if ((str.m_str[i] & 0xE0) == 0xC0)
					{
						count = 1;
						val = (str.m_str[i] & 0x1F);
					}
					else if ((str.m_str[i] & 0xF0) == 0xE0)
					{
						count = 2;
						val = (str.m_str[i] & 0x0F);
					}
					else if ((str.m_str[i] & 0xF8) == 0xF0)
					{
						count = 3;
						val = (str.m_str[i] & 0x7);
					}

					if (count && i + count < str.m_len)
					{
						for (unsigned int j = 0; j < count; ++j)
							val = (val << 6) | (str.m_str[i + j + 1] & 0x3F);

						i += count;
					}
				}

				int p = snprintf(buf,sizeof(buf)-1,"%u",val);
				write_chars(context,buf,p);

				++closes;
			}

			cc_prev = ccCloseL;
			if (str.m_len == 0 || !context->m_options.ignore_ops)
				write_chars(context,"]",1);
			else
			{
				write_chars(context,"[]",2);

				for (size_t i = 0; i < closes; ++i)
					write_chars(context,")",1);

				cc_prev = ccClose;
			}
		}
		break;

	case prolite_compound:
		if (context->m_options.numbervars && is_write_number_vars(term))
		{
			const term_t* arg = get_first_arg(term,NULL);
			size_t N = arg->m_dval;
			char prefix = 'A' + (N % 26);
			write_chars(context,&prefix,1);
			if (N < 26)
				cc_prev = ccUpper;
			else
			{
				N /= 26;
				char buf[22];
				int p = snprintf(buf,sizeof(buf)-1,"%zu",N);
				write_chars(context,buf,p);
				cc_prev = ccNumeric;
			}
		}
		else if (!context->m_options.ignore_ops && MASK_DEBUG_INFO(term->m_u64val) == PACK_COMPOUND_EMBED_1(2,','))
		{
			unsigned int prev_precedence = context->m_precedence;
			context->m_precedence = 999;

			do
			{
				term = get_first_arg(term,NULL);

				write_term_inner(context,cc_prev,term);

				write_chars(context,",",1);
				cc_prev = ccComma;

				term = get_next_arg(term);
			}
			while (MASK_DEBUG_INFO(term->m_u64val) == PACK_COMPOUND_EMBED_1(2,','));

			context->m_precedence = prev_precedence;
			cc_prev = write_term_inner(context,cc_prev,term);
		}
		else if (!context->m_options.ignore_ops && MASK_DEBUG_INFO(term->m_u64val) == PACK_COMPOUND_EMBED_1(2,'.'))
		{
			unsigned int prev_precedence = context->m_precedence;
			context->m_precedence = 999;

			write_chars(context,"[",1);
			cc_prev = ccOpenL;

			for (;;)
			{
				term = get_first_arg(term,NULL);

				write_term_inner(context,cc_prev,term);

				term = get_next_arg(term);

				if (MASK_DEBUG_INFO(term->m_u64val) != PACK_COMPOUND_EMBED_1(2,'.'))
					break;

				write_chars(context,",",1);
				cc_prev = ccComma;
			}

			if (MASK_DEBUG_INFO(term->m_u64val) != PACK_ATOM_EMBED_2('[',']'))
			{
				write_chars(context,"|",1);
				write_term_inner(context,ccBar,term);
			}

			context->m_precedence = prev_precedence;
			write_chars(context,"]",1);
			cc_prev = ccCloseL;
		}
		else if (!context->m_options.ignore_ops && MASK_DEBUG_INFO(term->m_u64val) == PACK_COMPOUND_EMBED_2(1,'{','}'))
		{
			write_chars(context,"{",1);

			unsigned int prev_precedence = context->m_precedence;
			context->m_precedence = 1201;

			write_term_inner(context,ccOpenC,get_first_arg(term,NULL));

			context->m_precedence = prev_precedence;
			write_chars(context,"}",1);
			cc_prev = ccCloseC;
		}
		else
		{
			string_t str;
			size_t arity = unpack_predicate(term,&str,NULL);

			const operator_t* op = NULL;
			if (arity == 1)
				op = lookup_prefix_op(context->m_operators,str.m_str,str.m_len);
			else
				op = lookup_op(context->m_operators,str.m_str,str.m_len);

			if (op && !context->m_options.ignore_ops)
				return write_operator(context,cc_prev,term,&str,arity,op);

			cc_prev = write_compound(context,cc_prev,term,&str,arity,op ? 1 : 0);
		}
		break;
	}

	return cc_prev;
}

prolite_stream_error_t write_term(context_t* context, prolite_stream_t* s, const term_t* term, const write_options_t* options, const operator_table_t* ops)
{
	//throw_permission_error(context,PACK_ATOM_BUILTIN(output),PACK_ATOM_BUILTIN(stream),op);

	write_context_t wc = {
		.m_context = context,
		.m_s = s,
		.m_operators = ops,
		.m_precedence = 1201
	};
	if (options)
		wc.m_options = *options;

	prolite_stream_error_t err = setjmp(wc.m_jmp);
	if (!err)
		write_term_inner(&wc,ccWhitespace,term);

	return err;
}

#if ENABLE_TESTS
prolite_stream_error_t dump_term(prolite_stream_t* s, const term_t* term)
{
	write_context_t wc = {
		.m_s = s,
		.m_precedence = 1201
	};

	prolite_stream_error_t err = setjmp(wc.m_jmp);
	if (!err)
		write_term_inner(&wc,ccWhitespace,term);

	return err;
}
#endif
