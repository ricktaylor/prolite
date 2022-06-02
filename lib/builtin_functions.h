#if defined(DECLARE_BUILTIN_INTRINSIC)
DECLARE_BUILTIN_INTRINSIC(true,PACK_ATOM_EMBED_4('t','r','u','e'))
DECLARE_BUILTIN_INTRINSIC(false,PACK_ATOM_EMBED_4('f','a','i','l'))
DECLARE_BUILTIN_INTRINSIC(false,PACK_ATOM_EMBED_5('f','a','l','s','e'))
DECLARE_BUILTIN_INTRINSIC(call,PACK_COMPOUND_EMBED_4(1,'c','a','l','l'))
DECLARE_BUILTIN_INTRINSIC(cut,PACK_ATOM_EMBED_1('!'))
DECLARE_BUILTIN_INTRINSIC(and,PACK_COMPOUND_EMBED_1(2,','))
DECLARE_BUILTIN_INTRINSIC(or,PACK_COMPOUND_EMBED_1(2,';'))
DECLARE_BUILTIN_INTRINSIC(if_then,PACK_COMPOUND_EMBED_2(2,'-','>'))
DECLARE_BUILTIN_INTRINSIC(catch,PACK_COMPOUND_EMBED_5(3,'c','a','t','c','h'))
DECLARE_BUILTIN_INTRINSIC(throw,PACK_COMPOUND_EMBED_5(1,'t','h','r','o','w'))
DECLARE_BUILTIN_INTRINSIC(not_proveable,PACK_COMPOUND_EMBED_2(1,'\\','+'))
DECLARE_BUILTIN_INTRINSIC(once,PACK_COMPOUND_EMBED_4(1,'o','n','c','e'))
DECLARE_BUILTIN_INTRINSIC(repeat,PACK_ATOM_BUILTIN(repeat))
DECLARE_BUILTIN_INTRINSIC(halt,PACK_COMPOUND_EMBED_4(1,'h','a','l','t'))
DECLARE_BUILTIN_INTRINSIC(halt,PACK_ATOM_EMBED_4('h','a','l','t'))
DECLARE_BUILTIN_INTRINSIC(unify,PACK_COMPOUND_EMBED_1(2,'='))
DECLARE_BUILTIN_INTRINSIC(not_unifiable,PACK_COMPOUND_EMBED_2(2,'\\','='))
DECLARE_BUILTIN_INTRINSIC(callable,PACK_COMPOUND_BUILTIN(callable,1))
DECLARE_BUILTIN_INTRINSIC(unify_with_occurs_check,PACK_COMPOUND_BUILTIN(unify_with_occurs_check,2))
DECLARE_BUILTIN_INTRINSIC(var,PACK_COMPOUND_EMBED_3(1,'v','a','r'))
DECLARE_BUILTIN_INTRINSIC(atom,PACK_COMPOUND_EMBED_4(1,'a','t','o','m'))
DECLARE_BUILTIN_INTRINSIC(integer,PACK_COMPOUND_BUILTIN(integer,1))
DECLARE_BUILTIN_INTRINSIC(float,PACK_COMPOUND_EMBED_5(1,'f','l','o','a','t'))
DECLARE_BUILTIN_INTRINSIC(atomic,PACK_COMPOUND_BUILTIN(atomic,1))
DECLARE_BUILTIN_INTRINSIC(compound,PACK_COMPOUND_BUILTIN(compound,1))
DECLARE_BUILTIN_INTRINSIC(nonvar,PACK_COMPOUND_BUILTIN(nonvar,1))
DECLARE_BUILTIN_INTRINSIC(number,PACK_COMPOUND_BUILTIN(number,1))
DECLARE_BUILTIN_INTRINSIC(ground,PACK_COMPOUND_BUILTIN(ground,1))
DECLARE_BUILTIN_INTRINSIC(is,PACK_COMPOUND_EMBED_2(2,'i','s'))
#endif

DECLARE_BUILTIN_FUNCTION(set_prolog_flag,2,PACK_COMPOUND_BUILTIN(set_prolog_flag,2))
DECLARE_BUILTIN_FUNCTION(char_conversion,2,PACK_COMPOUND_BUILTIN(char_conversion,2))
DECLARE_BUILTIN_FUNCTION(current_char_conversion,2,PACK_COMPOUND_BUILTIN(current_char_conversion,2))
DECLARE_BUILTIN_FUNCTION(op,3,PACK_COMPOUND_EMBED_2(3,'o','p'))
DECLARE_BUILTIN_FUNCTION(current_op,3,PACK_COMPOUND_BUILTIN(current_op,3))
DECLARE_BUILTIN_FUNCTION(asserta,1,PACK_COMPOUND_BUILTIN(asserta,1))
DECLARE_BUILTIN_FUNCTION(assertz,1,PACK_COMPOUND_BUILTIN(assertz,1))

DECLARE_BUILTIN_FUNCTION(write_term,3,PACK_COMPOUND_BUILTIN(write_term,3))
DECLARE_BUILTIN_FUNCTION(write_term,2,PACK_COMPOUND_BUILTIN(write_term,2))
DECLARE_BUILTIN_FUNCTION(write,1,PACK_COMPOUND_EMBED_5(1,'w','r','i','t','e'))
DECLARE_BUILTIN_FUNCTION(write,2,PACK_COMPOUND_EMBED_5(2,'w','r','i','t','e'))
DECLARE_BUILTIN_FUNCTION(writeq,1,PACK_COMPOUND_BUILTIN(writeq,1))
DECLARE_BUILTIN_FUNCTION(writeq,2,PACK_COMPOUND_BUILTIN(writeq,2))
DECLARE_BUILTIN_FUNCTION(write_canonical,1,PACK_COMPOUND_BUILTIN(write_canonical,1))
DECLARE_BUILTIN_FUNCTION(write_canonical,2,PACK_COMPOUND_BUILTIN(write_canonical,2))
DECLARE_BUILTIN_FUNCTION(put_char,1,PACK_COMPOUND_BUILTIN(put_char,1))
DECLARE_BUILTIN_FUNCTION(put_char,2,PACK_COMPOUND_BUILTIN(put_char,2))
DECLARE_BUILTIN_FUNCTION(put_code,1,PACK_COMPOUND_BUILTIN(put_code,1))
DECLARE_BUILTIN_FUNCTION(put_code,2,PACK_COMPOUND_BUILTIN(put_code,2))
DECLARE_BUILTIN_FUNCTION(nl,0,PACK_ATOM_EMBED_2('n','l'))
DECLARE_BUILTIN_FUNCTION(nl,1,PACK_COMPOUND_EMBED_2(1,'n','l'))
