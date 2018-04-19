
#if !defined(BUILTIN_FUNCTIONS_H_INCLUDED_) || defined(DECLARE_BUILTIN_FUNCTION)

#if !defined(DECLARE_BUILTIN_FUNCTION)
#define BUILTIN_FUNCTIONS_H_INCLUDED_
#define DECLARE_BUILTIN_FUNCTION(f,n) \
	enum eSolveResult solve_##f(struct context_t* context, struct term_t* goal);

#define DECLARE_BUILTIN_CONTROL(f,n)
#endif

// These are the inline control functions
DECLARE_BUILTIN_CONTROL(and,BOX_COMPOUND_EMBED_1(2,','))
DECLARE_BUILTIN_CONTROL(or,BOX_COMPOUND_EMBED_1(2,';'))
DECLARE_BUILTIN_CONTROL(if_then,BOX_COMPOUND_EMBED_2(2,'-','>'))
DECLARE_BUILTIN_CONTROL(cut,BOX_ATOM_EMBED_1('!'))
DECLARE_BUILTIN_CONTROL(call,BOX_COMPOUND_EMBED_4(1,'c','a','l','l'))
DECLARE_BUILTIN_CONTROL(true,BOX_ATOM_EMBED_4('t','r','u','e'))
DECLARE_BUILTIN_CONTROL(fail,BOX_ATOM_EMBED_4('f','a','i','l'))
DECLARE_BUILTIN_CONTROL(throw,BOX_COMPOUND_EMBED_5(1,'t','h','r','o','w'))
DECLARE_BUILTIN_CONTROL(repeat,BOX_ATOM_BUILTIN(repeat))
DECLARE_BUILTIN_CONTROL(halt,BOX_ATOM_EMBED_4('h','a','l','t'))

// Order isn't important
DECLARE_BUILTIN_FUNCTION(catch,BOX_COMPOUND_EMBED_5(3,'c','a','t','c','h'))
DECLARE_BUILTIN_FUNCTION(not_proveable,BOX_COMPOUND_EMBED_2(1,'\\','+'))
DECLARE_BUILTIN_FUNCTION(once,BOX_ATOM_EMBED_4('o','n','c','e'))
DECLARE_BUILTIN_FUNCTION(halt,BOX_COMPOUND_EMBED_4(1,'h','a','l','t'))
DECLARE_BUILTIN_FUNCTION(set_prolog_flag,BOX_COMPOUND_BUILTIN(set_prolog_flag,2))
DECLARE_BUILTIN_FUNCTION(char_conversion,BOX_COMPOUND_BUILTIN(char_conversion,2))
DECLARE_BUILTIN_FUNCTION(op,BOX_COMPOUND_EMBED_2(3,'o','p'))

#endif /* BUILTIN_FUNCTIONS_H_INCLUDED_ */
