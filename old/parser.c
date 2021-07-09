

static enum eEmitStatus clause_directive(struct context_t* context, const union packed_t* directive)
{
	assert(0);
}

static enum eEmitStatus ensure_loaded(struct context_t* context, const union packed_t* directive)
{
	assert(0);
}

static enum eEmitStatus compile_initializer(struct context_t* context, const union packed_t* directive)
{
	assert(0);
}

enum eSolveResult assert_clause(struct context_t* context, const union packed_t* clause, int z, int dynamic);

static enum eEmitStatus directive_solve(struct context_t* context, const union packed_t* directive)
{
	assert(0);

	switch (solve(context,directive))
	{
	case SOLVE_TRUE:
		return EMIT_OK;

	case SOLVE_NOMEM:
		return EMIT_NOMEM;

	case SOLVE_THROW:
		return EMIT_THROW;

	default:
		// Should never happen
		assert(0);
		return (emit_error(context,get_debug_info(directive),PACK_ATOM_BUILTIN(system_error),0) == EMIT_OK ? EMIT_THROW : EMIT_NOMEM);
	}
}

static enum eEmitStatus include(struct context_t* context, const union packed_t* directive);

static enum eEmitStatus load_file(struct context_t* context, struct stream_t* s)
{
	enum eEmitStatus status;

	struct parser_t parser = {0};
	parser.m_s = s;
	parser.m_line_info.m_end_line = 1;

	do
	{
		union packed_t* term = NULL;
		size_t stack_base = stack_top(context->m_call_stack);
		struct var_info_t* varinfo = NULL;

		status = parse_emit_term(context,&parser,&term,&varinfo);
		if (status == EMIT_OK)
		{
			if (term->m_u64val == PACK_COMPOUND_EMBED_2(1,':','-'))
			{
				term = (union packed_t*)first_arg(term);
				switch (term->m_u64val)
				{
				case PACK_COMPOUND_BUILTIN(dynamic,1):
				case PACK_COMPOUND_BUILTIN(multifile,1):
				case PACK_COMPOUND_BUILTIN(discontiguous,1):
					status = clause_directive(context,term);
					break;

				case PACK_COMPOUND_BUILTIN(include,1):
					status = include(context,term);
					break;

				case PACK_COMPOUND_BUILTIN(ensure_loaded,1):
					status = ensure_loaded(context,term);
					break;

				case PACK_COMPOUND_BUILTIN(initialization,1):
					status = compile_initializer(context,term);
					break;

				default:
					status = directive_solve(context,term);
					break;
				}
			}
			else
			{
				/* Assert the term */
				assert(0);

				assert_clause(context,term,1,0);
			}
		}

		if (status == EMIT_OK)
		{
			// TODO: Undo pointers...
			stack_reset(&context->m_scratch_stack,0);
			stack_reset(&context->m_call_stack,stack_base);
		}
	}
	while (status == EMIT_OK);

	if (status == EMIT_EOF)
		status = EMIT_OK;

	/* Compact the scratch stack because we may have allocated a lot */
	stack_compact(context->m_scratch_stack);

	return status;
}

static enum eEmitStatus include(struct context_t* context, const union packed_t* directive)
{
	enum eEmitStatus status;
	struct stream_t* s = NULL;

	/* TODO: Open stream 'directive' */

	/* TODO: Twiddle with context */

	status = load_file(context,s);
	if (status == EMIT_OK)
	{
		/* TODO: Untwiddle context */
	}

	return status;
}
