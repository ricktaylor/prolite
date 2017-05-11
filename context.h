
#include "stack.h"

union box_t
{
	double   m_dval;
	uint64_t m_uval;
};

static const uint64_t BOX_TAG_MASK        = UINT64_C(0xFFFF) << 48;

static const uint64_t BOX_TAG_VAR         = UINT64_C(0xFFF7) << 48;
static const uint64_t BOX_TAG_COMPOUND    = UINT64_C(0xFFF6) << 48;
static const uint64_t BOX_TAG_ATOM_PTR    = UINT64_C(0xFFF5) << 48;
static const uint64_t BOX_TAG_CHARS_PTR   = UINT64_C(0xFFF4) << 48;
static const uint64_t BOX_TAG_CODES_PTR   = UINT64_C(0xFFF3) << 48;
static const uint64_t BOX_TAG_CUSTOM_PTR  = UINT64_C(0xFFF2) << 48;
static const uint64_t BOX_TAG_EXTEND      = UINT64_C(0xFFF1) << 48;

static const uint64_t BOX_TAG_EXTEND_MASK = (UINT64_C(0xFFF1) << 48) | (UINT64_C(0xF) << 44);

static const uint64_t BOX_TAG_ATOM_EMBED  = (UINT64_C(0xFFF1) << 48) | (UINT64_C(0xF) << 44);
static const uint64_t BOX_TAG_ATOM_STACK  = (UINT64_C(0xFFF1) << 48) | (UINT64_C(0xE) << 44);
static const uint64_t BOX_TAG_CHARS_EMBED = (UINT64_C(0xFFF1) << 48) | (UINT64_C(0xD) << 44);
static const uint64_t BOX_TAG_CHARS_STACK = (UINT64_C(0xFFF1) << 48) | (UINT64_C(0xC) << 44);
static const uint64_t BOX_TAG_CODES_EMBED = (UINT64_C(0xFFF1) << 48) | (UINT64_C(0xB) << 44);
static const uint64_t BOX_TAG_CODES_STACK = (UINT64_C(0xFFF1) << 48) | (UINT64_C(0xA) << 44);
static const uint64_t BOX_TAG_INT32       = (UINT64_C(0xFFF1) << 48) | (UINT64_C(0x9) << 44);

struct context_t
{
	struct stack_t* m_scratch_stack;
	struct stack_t* m_exec_stack;

	struct context_flags_t
	{
		unsigned char_conversion : 1;
		unsigned double_quotes : 2;
		unsigned back_quotes : 2;
		unsigned debug : 1;
		unsigned unknown : 2;
	} m_flags;
};

#if defined(_MSC_VER)
#define inline __inline
#endif

static inline void box_pointer(union box_t* b, void* ptr)
{
#if UINTPTR_MAX == UINT32_MAX
	b->m_uval |= (uintptr_t)ptr;
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	b->m_uval |= ((uintptr_t)ptr & ~BOX_TAG_MASK);
#else
#error No idea what to do with addresses on your architecture!
#endif
}

static inline void* unbox_pointer(const union box_t* b)
{
#if UINTPTR_MAX == UINT32_MAX
	return (void*)(uintptr_t)(b->m_uval);
#elif defined (__x86_64__) || defined(_M_X64) || defined(__aarch64__)
	/* Sign extend to make an x86_64 canonical address */
	return (void*)(uintptr_t)(((b->m_uval & ~BOX_TAG_MASK) << 16) >> 16);
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

int box_string_ptr(struct context_t* context, union box_t* b, const unsigned char* str, size_t len);

static inline int box_string(struct context_t* context, union box_t* b, const unsigned char* str, size_t len)
{
	const unsigned char* s;
	if (len > 5)
		return box_string_ptr(context,b,str,len);

	s = str;
	b->m_uval = BOX_TAG_ATOM_EMBED | ((len & UINT64_C(0xF)) << 40);
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

struct string_ptr_t const* unbox_string_stack(struct context_t* context, const union box_t* b);

static inline const unsigned char* unbox_string(struct context_t* context, const union box_t* b, size_t* len)
{
	struct string_ptr_t const* s;
	uint64_t tag = (b->m_uval & (BOX_TAG_MASK | (UINT64_C(0x1) << 44)));
	if (tag == (BOX_TAG_EXTEND | (UINT64_C(0x1) << 44)))
	{
		*len = (size_t)(b->m_uval & (UINT64_C(0xF) << 40));
		return ((const unsigned char*)b) + 3;
	}

	if (tag == BOX_TAG_EXTEND)
		s = unbox_string_stack(context,b);
	else
		s = (struct string_ptr_t const*)unbox_pointer(b);

	*len = s->m_len;
	return s->m_str;
}

static inline union box_t box_double(double d)
{
	union box_t r;
	r.m_dval = d;
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
	union box_t r;
	r.m_uval = BOX_TAG_INT32 | (uint32_t)n;
	return r;
}

static inline int32_t unbox_int32(const union box_t* b)
{
	return (int32_t)(b->m_uval & UINT64_C(0xFFFFFFFF));
}

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
