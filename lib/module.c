

#include "types.h"

#include <string.h>
#include <assert.h>

struct predicate_t* find_predicate(struct module_t* module, const union box_t* head)
{
	struct predicate_t* pred = NULL;

	return pred;
}

int add_predicate(struct module_t* module, struct predicate_t* pred)
{
	// TODO: add predicate to module

	return -1;
}

struct module_t* module_new(struct context_t* context, const char* name)
{
	// TODO: Much more here!!

	struct module_t* module = NULL;
	struct stack_t* s = stack_new(8000,&malloc,&free);
	if (s)
	{
		module = stack_malloc(&s,sizeof(struct module_t));
		if (module)
		{
			memset(module,0,sizeof(struct module_t));
			module->m_flags.char_conversion = 1;
			module->m_flags.back_quotes = 1;
			module->m_stack = s;
		}

		if (!module)
			stack_delete(s);
	}

	return module;
}

void module_delete(struct module_t* module)
{
	// TODO
}
