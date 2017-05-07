
struct context_t;

union box_t
{
	double   m_dval;
	uint64_t m_uval;
};

enum eProliteType
{
	PROLITE_TYPE_VAR,
	PROLITE_TYPE_COMPOUND,
	PROLITE_TYPE_DOUBLE,
	PROLITE_TYPE_INTEGER,
	PROLITE_TYPE_ATOM,
	PROLITE_TYPE_CHARS,
	PROLITE_TYPE_CODES,
};

// Bits required:  51 - 48 = 3 bits

// Var : MaxVars  0xFFF7
// Compound : MaxArity  0xFFF6
// Int : 32  0xFFF5
// Atom
//	- Atom embed : 43  0xFFF1/3
//  - Atom local:      0xFFF1/C
//  - Atom ptr  : 48  0xFFF4
// Chars
//	- Chars embed : 43  0xFFF1/2
//  - Chars local       0xFFF1/8
//  - Chars ptr  : 48  0xFFF3
// Codes
//	- Codes embed : 43  0xFFF1/1
//  - Codes local     0xFFF1/4
//  - Codes ptr : 48 0xFFF2

enum eBoxTag
{
	BOX_TAG_MASK      = UINT64_C(0xFFFF) << 48,

	BOX_TAG_VAR       = UINT64_C(0xFFF7) << 48,
	BOX_TAG_COMPOUND  = UINT64_C(0xFFF6) << 48,
	BOX_TAG_INT32     = UINT64_C(0xFFF5) << 48,
	BOX_TAG_ATOM_PTR  = UINT64_C(0xFFF4) << 48,
	BOX_TAG_CHARS_PTR = UINT64_C(0xFFF3) << 48,
	BOX_TAG_CODES_PTR = UINT64_C(0xFFF2) << 48,
	BOX_TAG_EXTEND    = UINT64_C(0xFFF1) << 48,

	BOX_TAG_EXTEND_MASK = BOX_TAG_EXTEND | (UINT64_C(0xC) << 44),

	BOX_TAG_ATOM_STACK  = BOX_TAG_EXTEND | (UINT64_C(0xC) << 44),
	BOX_TAG_ATOM_EMBED  = BOX_TAG_EXTEND | (UINT64_C(0xD) << 44),
	BOX_TAG_CHARS_STACK = BOX_TAG_EXTEND | (UINT64_C(0x8) << 44),
	BOX_TAG_CHARS_EMBED = BOX_TAG_EXTEND | (UINT64_C(0x9) << 44),
	BOX_TAG_CODES_STACK = BOX_TAG_EXTEND | (UINT64_C(0x4) << 44),
	BOX_TAG_CODES_EMBED = BOX_TAG_EXTEND | (UINT64_C(0x5) << 44),
};

static inline void box_pointer(union box_t* val, void* ptr)
{
#if UINTPTR_MAX == UINT32_MAX
	val->m_uval |= (uintptr_t)ptr;
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	val->m_uval |= ((uintptr_t)ptr & ~BOX_TAG_MASK);
#else
#error No idea what to do with addresses on your architecture!
#endif
}

static inline void* unbox_pointer(const union box_t* val)
{
#if UINTPTR_MAX == UINT32_MAX
	return (void*)(uintptr_t)(val->m_uval);
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	/* Sign extend to make an x86_64 canonical address */
	return (void*)(uintptr_t)(((val->m_uval & ~BOX_TAG_MASK) << 16) >> 16);
#else
#error No idea what to do with addresses on your architecture!
#endif
}

struct string_ptr_t
{
	struct string_ptr_t* m_prev;
	size_t               m_len;
	unsigned char        m_str[];
};

int box_string_ptr(struct context_t* context, union box_t* b, enum eProliteType type, const unsigned char* str, size_t len);

static inline int box_string(struct context_t* context, union box_t* b, enum eProliteType type, const unsigned char* str, size_t len)
{
	if (len > 5)
		return box_string_ptr(context,b,type,str,len);

	switch (type)
	{
	case PROLITE_TYPE_ATOM:
		b->m_uval = BOX_TAG_ATOM_EMBED;
		break;

	case PROLITE_TYPE_CHARS:
		b->m_uval = BOX_TAG_CHARS_EMBED;
		break;

	case PROLITE_TYPE_CODES:
		b->m_uval = BOX_TAG_CODES_EMBED;
		break;

	default:
		break;
	}

	b->m_uval |= (uint64_t)(len & 0xF) << 40;
	const unsigned char* s = str;
	switch (len)
	{
	case 5:
		b->m_uval |= (uint64_t)(*s++) << 32;
	case 4:
		b->m_uval |= (*s++) << 24;
	case 3:
		b->m_uval |= (*s++) << 16;
	case 2:
		b->m_uval |= (*s++) << 8;
	case 1:
		b->m_uval |= *s;
	default:
		break;
	}
	return 1;
}

static inline const unsigned char* unbox_string(struct context_t* context, const union box_t* val, size_t* len)
{
	uint64_t masked = (val->m_uval & (BOX_TAG_MASK | (UINT64_C(0x1) << 44)));

	if (masked == (BOX_TAG_EXTEND | (UINT64_C(0x1) << 44)))
	{
		*len = val->m_uval & (UINT64_C(0xF) << 40);
		return ((const unsigned char*)val) + 3;
	}

	struct string_ptr_t* s;
	if (masked == BOX_TAG_EXTEND)
	{
		/* TODO: Deref from context */
		return NULL;
	}
	else
		s = (struct string_ptr_t*)unbox_pointer(val);

	*len = s->m_len;
	return s->m_str;
}

static inline union box_t box_double(double d)
{
	union box_t r = { .m_dval = d };
	if (isnan(d))
		r.m_uval = UINT64_C(0xFFF8) << 48;
	return r;
}

static inline double unbox_double(const union box_t* b)
{
	return b->m_dval;
}

static inline union box_t box_int32(int32_t n)
{
	union box_t r = { .m_uval = BOX_TAG_INT32 | (uint32_t)n };
	return r;
}

static inline int32_t unbox_int32(const union box_t* b)
{
	return (int32_t)(b->m_uval & 0xFFFFFFFF);
}

struct context_t
{
	// Heap strings
	// Stack strings

	struct context_flags_t
	{
		unsigned char_conversion : 1;
		unsigned double_quotes : 2;
		unsigned back_quotes : 2;
		unsigned debug : 1;
		unsigned unknown : 2;

	} m_flags;
};

struct stream_t;
int64_t stream_read(struct stream_t* s, void* dest, size_t len);

uint32_t context_convert_char(struct context_t* context, uint32_t in_char);

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

/* Try to find a infix/suffix op, otherwise find prefix */
struct Operator* lookup_op(struct context_t* context, const union box_t* b);

/* Try to find a prefix op, otherwise find infix/suffix */
struct Operator* lookup_prefix_op(struct context_t* context, const union box_t* b);
