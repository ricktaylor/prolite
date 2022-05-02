/*
 * types.h
 *
 *  Created on: 27 May 2017
 *      Author: rick
 */

#ifndef TYPES_H_
#define TYPES_H_

#include "btree.h"

typedef enum prolite_type
{
	prolite_number = 0,
	prolite_var = 1,
	prolite_atom = 2,
	prolite_compound = 3,	
	prolite_chars = 4,
	prolite_charcodes = 5,
	//prolite_userdata1 = 6,
	//prolite_userdata2 = 7

} prolite_type_t;

static const uint16_t prolite_debug_info = 0x8000;

#include "packed_types.h"

typedef union term
{
	double      m_dval;
	uint64_t    m_u64val;
	const void* m_pval;
} term_t;

/* Macro magic to declare the builtin string constants */
#define DECLARE_BUILTIN_STRING(name) BUILTIN_ATOM_##name,
typedef enum builtin_atom_id
{
#include "builtin_strings.h"
	MAX_BUILTIN_ATOM
} builtin_atom_id_t;

void pack_string(term_t out[2], const unsigned char* str, size_t len);

typedef struct debug_info
{
	int TODO;
} debug_info_t;

const debug_info_t* unpack_debug_info(const term_t* t);

static inline size_t unpack_var_index(const term_t* v)
{
	return UNPACK_MANT_48(v->m_u64val);
}

static inline prolite_type_t unpack_term_type(const term_t* t)
{
	uint16_t exp = UNPACK_EXP_16(t->m_u64val);
	if ((exp & 0x7FF0) != 0x7FF0)
		return prolite_number;

	return (prolite_type_t)(exp & 0x7);
}

static inline unsigned int unpack_term_subtype(const term_t* t)
{
	return ((UNPACK_MANT_48(t->m_u64val) >> 32) >> 14);
}

typedef struct string
{
	size_t               m_len;
	const unsigned char* m_str;
	unsigned char        m_data[5]; //< May be used for embedded strings
} string_t;

size_t unpack_predicate(const term_t* t, string_t* str, const debug_info_t** debug_info);
void unpack_string(const term_t* t, string_t* str, const debug_info_t** debug_info);
double unpack_number(const term_t* t, const debug_info_t** debug_info);

const term_t* get_first_arg(const term_t* compound, size_t* arity);
const term_t* get_next_arg(const term_t* t);

int predicate_compare(const term_t* c1, const term_t* c2);
int term_compare(const term_t* t1, const term_t* t2);

typedef struct emit_buffer
{
	prolite_allocator_t* m_a;
	btree_t*             m_pool;
	size_t               m_alloc;
	size_t               m_count;
	term_t*              m_buf;
} emit_buffer_t;

term_t* emit_buffer_append(emit_buffer_t* out, size_t count);
const term_t* emit_string(emit_buffer_t* out, prolite_type_t type, const unsigned char* str, size_t len, int external, const debug_info_t* debug_info);
const term_t* emit_predicate(emit_buffer_t* out, size_t arity, const unsigned char* functor, size_t functor_len, int external, const debug_info_t* debug_info);
const term_t* emit_var(emit_buffer_t* out, size_t idx, const debug_info_t* debug_info);
const term_t* emit_number(emit_buffer_t* out, double d, const debug_info_t* debug_info);

#endif /* TYPES_H_ */
