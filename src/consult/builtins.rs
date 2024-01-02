use phf::phf_map;

pub enum BuiltinType {
    Control,
    Builtin,
}

static BUILTINS: phf::Map<&'static str, BuiltinType> = phf_map! {
    "true/0" => BuiltinType::Control,
    "fail/0" => BuiltinType::Control,
    "call/1" => BuiltinType::Control,
    "!/0" => BuiltinType::Control,
    ",/2" => BuiltinType::Control,
    ";/2" => BuiltinType::Control,
    "->/2" => BuiltinType::Control,
    "catch/3" => BuiltinType::Control,
    "throw/1" => BuiltinType::Control,
    "=/2" => BuiltinType::Builtin,
    "unify_with_occurs_check/2" => BuiltinType::Builtin,
    "\\=/2" => BuiltinType::Builtin,
    "var/1" => BuiltinType::Builtin,
    "atom/1" => BuiltinType::Builtin,
    "integer/1" => BuiltinType::Builtin,
    "float/1" => BuiltinType::Builtin,
    "atomic/1" => BuiltinType::Builtin,
    "compound/1" => BuiltinType::Builtin,
    "nonvar/1" => BuiltinType::Builtin,
    "number/1" => BuiltinType::Builtin,
    "@=</2" => BuiltinType::Builtin,
    "==/2" => BuiltinType::Builtin,
    "\\==/2" => BuiltinType::Builtin,
    "@</2" => BuiltinType::Builtin,
    "@>/2" => BuiltinType::Builtin,
    "@>=/2" => BuiltinType::Builtin,
    "functor/3" => BuiltinType::Builtin,
    "arg/3" => BuiltinType::Builtin,
    "=../2" => BuiltinType::Builtin,
    "copy_term/2" => BuiltinType::Builtin,
    "is/2" => BuiltinType::Builtin,
    "=:=/2" => BuiltinType::Builtin,
    "=\\=/2" => BuiltinType::Builtin,
    "</2" => BuiltinType::Builtin,
    "=</2" => BuiltinType::Builtin,
    ">/2" => BuiltinType::Builtin,
    ">=/2" => BuiltinType::Builtin,
    "clause/2" => BuiltinType::Builtin,
    "current_predicate/1" => BuiltinType::Builtin,
    "asserta/1" => BuiltinType::Builtin,
    "assertz/1" => BuiltinType::Builtin,
    "retract/1" => BuiltinType::Builtin,
    "abolish/1" => BuiltinType::Builtin,
    "findall/3" => BuiltinType::Builtin,
    "bagof/3" => BuiltinType::Builtin,
    "setof/3" => BuiltinType::Builtin,
    "current_input/1" => BuiltinType::Builtin,
    "current_output/1" => BuiltinType::Builtin,
    "set_input/1" => BuiltinType::Builtin,
    "set_output/1" => BuiltinType::Builtin,
    "open/4" => BuiltinType::Builtin,
    "open/3" => BuiltinType::Builtin,
    "close/2" => BuiltinType::Builtin,
    "close/1" => BuiltinType::Builtin,
    "flush_output/1" => BuiltinType::Builtin,
    "flush_output/0" => BuiltinType::Builtin,
    "stream_property/2" => BuiltinType::Builtin,
    "at_end_of_stream/0" => BuiltinType::Builtin,
    "at_end_of_stream/1" => BuiltinType::Builtin,
    "set_stream_position/2" => BuiltinType::Builtin,
    "get_char/2" => BuiltinType::Builtin,
    "get_char/1" => BuiltinType::Builtin,
    "get_code/1" => BuiltinType::Builtin,
    "get_code/2" => BuiltinType::Builtin,
    "peek_char/2" => BuiltinType::Builtin,
    "peek_char/1" => BuiltinType::Builtin,
    "peek_code/1" => BuiltinType::Builtin,
    "peek_code/2" => BuiltinType::Builtin,
    "put_char/2" => BuiltinType::Builtin,
    "put_char/1" => BuiltinType::Builtin,
    "put_code/1" => BuiltinType::Builtin,
    "put_code/2" => BuiltinType::Builtin,
    "nl/0" => BuiltinType::Builtin,
    "nl/1" => BuiltinType::Builtin,
    "get_byte/2" => BuiltinType::Builtin,
    "get_byte/1" => BuiltinType::Builtin,
    "peek_byte/2" => BuiltinType::Builtin,
    "peek_byte/1" => BuiltinType::Builtin,
    "put_byte/2" => BuiltinType::Builtin,
    "put_byte/1" => BuiltinType::Builtin,
    "read_term/3" => BuiltinType::Builtin,
    "read_term/2" => BuiltinType::Builtin,
    "read/1" => BuiltinType::Builtin,
    "read/2" => BuiltinType::Builtin,
    "write_term/3" => BuiltinType::Builtin,
    "write_term/2" => BuiltinType::Builtin,
    "write/1" => BuiltinType::Builtin,
    "write/2" => BuiltinType::Builtin,
    "writeq/1" => BuiltinType::Builtin,
    "writeq/2" => BuiltinType::Builtin,
    "write_canonical/1" => BuiltinType::Builtin,
    "write_canonical/2" => BuiltinType::Builtin,
    "op/3" => BuiltinType::Builtin,
    "current_op/3" => BuiltinType::Builtin,
    "char_conversion/2" => BuiltinType::Builtin,
    "current_char_conversion/2" => BuiltinType::Builtin,
    "\\+/1" => BuiltinType::Builtin,
    "once/1" => BuiltinType::Builtin,
    "repeat/0" => BuiltinType::Builtin,
    "atom_length/2" => BuiltinType::Builtin,
    "atom_concat/3" => BuiltinType::Builtin,
    "sub_atom/5" => BuiltinType::Builtin,
    "atom_chars/2" => BuiltinType::Builtin,
    "atom_codes/2" => BuiltinType::Builtin,
    "char_code/2" => BuiltinType::Builtin,
    "number_chars/2" => BuiltinType::Builtin,
    "number_codes/2" => BuiltinType::Builtin,
    "set_prolog_flag/2" => BuiltinType::Builtin,
    "current_prolog_flag/2" => BuiltinType::Builtin,
    "halt/0" => BuiltinType::Builtin,
    "halt/1" => BuiltinType::Builtin,
};

pub fn is_builtin(pi: &str) -> bool {
    BUILTINS.get(pi).is_some() || pi.starts_with("call/")
}
