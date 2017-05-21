#ifndef BUILTIN_STRINGS_H_INCLUDED_
#define BUILTIN_STRINGS_H_INCLUDED_

/* DO NOT INCLUDE THIS FILE DIRECTLY */

#if !defined(DECLARE_BUILTIN_STRING)
#error Do not include this file directly
#else

/* These MUST be sorted by length then text */
DECLARE_BUILTIN_STRING(memory)
DECLARE_BUILTIN_STRING(dynamic)
DECLARE_BUILTIN_STRING(include)
DECLARE_BUILTIN_STRING(integer)
DECLARE_BUILTIN_STRING(missing)
DECLARE_BUILTIN_STRING(callable)
DECLARE_BUILTIN_STRING(character)
DECLARE_BUILTIN_STRING(max_arity)
DECLARE_BUILTIN_STRING(multifile)
DECLARE_BUILTIN_STRING(procedure)
DECLARE_BUILTIN_STRING(underflow)
DECLARE_BUILTIN_STRING(type_error)
DECLARE_BUILTIN_STRING(max_integer)
DECLARE_BUILTIN_STRING(min_integer)
DECLARE_BUILTIN_STRING(domain_error)
DECLARE_BUILTIN_STRING(invalid_utf8)
DECLARE_BUILTIN_STRING(syntax_error)
DECLARE_BUILTIN_STRING(system_error)
DECLARE_BUILTIN_STRING(discontiguous)
DECLARE_BUILTIN_STRING(ensure_loaded)
DECLARE_BUILTIN_STRING(out_of_memory)
DECLARE_BUILTIN_STRING(float_overflow)
DECLARE_BUILTIN_STRING(initialization)
DECLARE_BUILTIN_STRING(invalid_escape)
DECLARE_BUILTIN_STRING(resource_error)
DECLARE_BUILTIN_STRING(char_conversion)
DECLARE_BUILTIN_STRING(existence_error)
DECLARE_BUILTIN_STRING(set_prolog_flag)
DECLARE_BUILTIN_STRING(evaluation_error)
DECLARE_BUILTIN_STRING(invalid_argument)
DECLARE_BUILTIN_STRING(permission_error)
DECLARE_BUILTIN_STRING(unexpected_token)
DECLARE_BUILTIN_STRING(invalid_character)
DECLARE_BUILTIN_STRING(operator_priority)
DECLARE_BUILTIN_STRING(operator_specifier)
DECLARE_BUILTIN_STRING(past_end_of_stream)
DECLARE_BUILTIN_STRING(instantiation_error)
DECLARE_BUILTIN_STRING(representation_error)

#endif

#endif /* BUILTIN_STRINGS_H_INCLUDED_ */