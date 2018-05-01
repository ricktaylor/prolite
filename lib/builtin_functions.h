
// These are the inline control functions
#if defined(DECLARE_BUILTIN_INLINE)
DECLARE_BUILTIN_INLINE(cut,BOX_ATOM_EMBED_1('!'))
DECLARE_BUILTIN_INLINE(call,BOX_COMPOUND_EMBED_4(1,'c','a','l','l'))
DECLARE_BUILTIN_INLINE(true,BOX_ATOM_EMBED_4('t','r','u','e'))
DECLARE_BUILTIN_INLINE(fail,BOX_ATOM_EMBED_4('f','a','i','l'))
DECLARE_BUILTIN_INLINE(false,BOX_ATOM_EMBED_5('f','a','l','s','e'))
DECLARE_BUILTIN_INLINE(repeat,BOX_ATOM_BUILTIN(repeat))
DECLARE_BUILTIN_INLINE(halt,BOX_ATOM_EMBED_4('h','a','l','t'))
DECLARE_BUILTIN_INLINE(halt,BOX_COMPOUND_EMBED_4(1,'h','a','l','t'))
#endif

// Order isn't important
#if defined(DECLARE_BUILTIN_STATIC)
DECLARE_BUILTIN_STATIC(and,BOX_COMPOUND_EMBED_1(2,','))
DECLARE_BUILTIN_STATIC(or,BOX_COMPOUND_EMBED_1(2,';'))
DECLARE_BUILTIN_STATIC(unify,BOX_COMPOUND_EMBED_1(2,'='))
DECLARE_BUILTIN_STATIC(not_unifiable,BOX_COMPOUND_EMBED_2(2,'\\','='))
DECLARE_BUILTIN_STATIC(if_then,BOX_COMPOUND_EMBED_2(2,'-','>'))
DECLARE_BUILTIN_STATIC(throw,BOX_COMPOUND_EMBED_5(1,'t','h','r','o','w'))
DECLARE_BUILTIN_STATIC(catch,BOX_COMPOUND_EMBED_5(3,'c','a','t','c','h'))
DECLARE_BUILTIN_STATIC(not_proveable,BOX_COMPOUND_EMBED_2(1,'\\','+'))
DECLARE_BUILTIN_STATIC(once,BOX_ATOM_EMBED_4('o','n','c','e'))
#endif

// Order isn't important
#if defined(DECLARE_BUILTIN_FUNCTION)
DECLARE_BUILTIN_FUNCTION(set_prolog_flag,BOX_COMPOUND_BUILTIN(set_prolog_flag,2))
DECLARE_BUILTIN_FUNCTION(char_conversion,BOX_COMPOUND_BUILTIN(char_conversion,2))
DECLARE_BUILTIN_FUNCTION(op,BOX_COMPOUND_EMBED_2(3,'o','p'))
#endif
