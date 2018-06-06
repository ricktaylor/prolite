

#include "types.h"

#include <string.h>
#include <assert.h>

void delete_clause(struct clause_t* clause);

struct predicate_t* new_predicate(struct module_t* module, const union box_t* head, int dynamic)
{
	struct predicate_t* pred = NULL;
	struct stack_t* s = stack_new(100,module->m_stack->m_fn_malloc,module->m_stack->m_fn_free);
	if (s)
	{
		pred = stack_malloc(&s,sizeof(struct predicate_t));
		if (pred)
		{
			memset(pred,0,sizeof(struct predicate_t));
			pred->m_stack = s;
			pred->m_module = module;

			pred->m_indicator = copy_term(NULL,&pred->m_stack,&pred->m_strings,head);
			if (!pred->m_indicator)
			{
				stack_delete(s);
				return NULL;
			}
		}
		else
			stack_delete(s);
	}

	return pred;
}

void delete_predicate(struct predicate_t* predicate)
{
	if (predicate)
	{
		struct clause_t* c;
		for (c = predicate->m_first_clause; c != NULL; c = c->m_next)
			delete_clause(c);

		stack_delete(predicate->m_stack);
	}
}

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
