
#if !defined(BUILTIN_FUNCTIONS_H_INCLUDED_) || defined(DECLARE_BUILTIN_FUNCTION)

#if !defined(DECLARE_BUILTIN_FUNCTION)
#define BUILTIN_FUNCTIONS_H_INCLUDED_
#define DECLARE_BUILTIN_FUNCTION(f,n) \
	enum eSolveResult solve_##f(struct context_t* context, struct term_t* goal);
#endif

// Order isn't important
DECLARE_BUILTIN_FUNCTION(halt,BOX_ATOM_EMBED_4('h','a','l','t'))
DECLARE_BUILTIN_FUNCTION(halt,BOX_COMPOUND_EMBED_4(1,'h','a','l','t'))
DECLARE_BUILTIN_FUNCTION(not_proveable,BOX_COMPOUND_EMBED_2(1,'\\','+'))
DECLARE_BUILTIN_FUNCTION(once,BOX_ATOM_EMBED_4('o','n','c','e'))
DECLARE_BUILTIN_FUNCTION(if_then,BOX_COMPOUND_EMBED_2(2,'-','>'))
DECLARE_BUILTIN_FUNCTION(catch,BOX_COMPOUND_EMBED_5(3,'c','a','t','c','h'))
DECLARE_BUILTIN_FUNCTION(throw,BOX_COMPOUND_EMBED_5(1,'t','h','r','o','w'))

#endif /* BUILTIN_FUNCTIONS_H_INCLUDED_ */
