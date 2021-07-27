

#include "compile.h"

#include <stdio.h>

static void fmtFlags(uint64_t v, char* buf)
{
	char* s = buf;

	if (v & FLAG_FAIL)
		*buf++ = 'F';

	if (v & FLAG_CUT)
		*buf++ = 'C';

	if (v & FLAG_THROW)
		*buf++ = 'T';

	if (v & FLAG_HALT)
		*buf++ = 'H';

	if (buf == s)
		*buf++ = '0';

	*buf = '\0';
}

static void dumpTerm(const term_t* t, FILE* f)
{
	switch (get_term_type(t))
	{
	case prolite_atom:
		{
			string_t s = get_string(t,NULL);
			fprintf(f,"%.*s",(int)s.m_len,s.m_str);
		}
		break;

	case prolite_compound:
		{
			uint64_t arity;
			string_t s = get_predicate(t,&arity,NULL);
			fprintf(f,"%.*s(",(int)s.m_len,s.m_str);

			t = get_first_arg(t,NULL,NULL);
			for (size_t i = 0; i < arity; ++i)
			{
				if (i)
					fprintf(f,",");

				dumpTerm(t,f);
				t = get_next_arg(t,NULL);
			}
			fprintf(f,")");
		}
		break;

	case prolite_var:
		fprintf(f,"var%zu",(size_t)get_var_index(t));
		break;

	case prolite_int32:
		fprintf(f,"%lu",(long)get_integer(t));
		break;

	default:
		fprintf(f,"%p",t);
		break;
	}
}

static size_t opInc(enum optype op, size_t i)
{
	switch (op)
	{
	case OP_JMP:
	case OP_CALL:
	case OP_THROW:
	case OP_SET_FLAGS:
	case OP_CLEAR_FLAGS:
	case OP_CLEAR_VAR:
	case OP_PUSH_TERM:
		++i;
		break;

	case OP_BRANCH:
	case OP_BUILTIN:
    case OP_UNIFY_VAR:
		i+=2;
		break;

	default:
		break;
	}

	return i+1;
}

static void dumpCFGBlock(const cfg_block_t* blk, FILE* f)
{
	char buf[5] = {0};

	fprintf(f,"\tnode [shape=record];\n");
	fprintf(f,"\tN%p [label=\"{",blk);

	if (!blk->m_count)
	{
		fprintf(f,"<f0> WTF?!?!");
	}

	for (size_t i=0;i < blk->m_count; i = opInc(blk->m_ops[i].m_opcode,i))
	{
		if (i)
			fprintf(f,"|");

		fprintf(f,"<f%zu> ",i);

		switch (blk->m_ops[i].m_opcode)
		{
		case OP_NOP:
			fprintf(f,"(NOP)");
			break;

		case OP_TRUE:
			fprintf(f,"Success!");
			break;

		case OP_END:
			fprintf(f,"End");
			break;

		case OP_JMP:
			fprintf(f,"Jmp");
			break;

		case OP_CALL:
			fprintf(f,"Call");
			break;

		case OP_RET:
			fprintf(f,"Ret");
			break;

		case OP_BUILTIN:
			fprintf(f,"Builtin\\ %s|<f%zu> ...\\ if\\ true,\\ Call",(*(builtin_fn_t)blk->m_ops[i+1].m_pval)(),i+1);
			break;

		case OP_THROW:
			fprintf(f,"Throwing\\ %s",(*(builtin_fn_t)blk->m_ops[i+1].m_pval)());
			break;

		case OP_SET_FLAGS:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"Set\\ Flags\\ %s",buf);
			break;

		case OP_CLEAR_FLAGS:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"Clear\\ Flags\\ %s",buf);
			break;

		case OP_PUSH_CUT:
			fprintf(f,"Push\\ Cut");
			break;

		case OP_POP_CUT:
			fprintf(f,"Pop\\ Cut");
			break;

		case OP_BRANCH:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"Branch|<f%zu> ...\\ if \\%s",i+1,buf);
			break;

		case OP_PUSH_TERM:
			fprintf(f,"Push\\ ");
			dumpTerm(blk->m_ops[i+1].m_pval,f);
			break;

		case OP_UNIFY_VAR:
			fprintf(f,"Unify var%zu\\ =\\ ",(size_t)blk->m_ops[i+1].m_u64val);
			dumpTerm(blk->m_ops[i+2].m_pval,f);
			break;

		case OP_CLEAR_VAR:
			fprintf(f,"Clear\\ var%zu",(size_t)blk->m_ops[i+1].m_u64val);
			break;

		default:
			fprintf(f,"WTF? %zu",(size_t)blk->m_ops[i].m_opcode);
			break;
		}
	}

	fprintf(f,"}\"];\n");

	for (size_t i=0;i < blk->m_count; i = opInc(blk->m_ops[i].m_opcode,i))
	{
		switch (blk->m_ops[i].m_opcode)
		{
		case OP_BRANCH:
			fmtFlags(blk->m_ops[i+1].m_u64val,buf);
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [label=\"%s\"];\n",blk,i+1,blk->m_ops[i+2].m_pval,buf);
			break;

		case OP_CALL:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [dir=both];\n",blk,i,blk->m_ops[i+1].m_pval);
			break;

		case OP_JMP:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0>;\n",blk,i,blk->m_ops[i+1].m_pval);
			break;

		case OP_BUILTIN:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [dir=both];\n",blk,i+1,blk->m_ops[i+2].m_pval);
			break;

		default:
			break;
		}
	}
}

typedef struct cfg_vec
{
	size_t len;
	const cfg_block_t** blks;
} cfg_vec_t;

static int addCFG(cfg_vec_t* blks, const cfg_block_t* blk)
{
	for (size_t i=0; i < blks->len; ++i)
	{
		if (blk == blks->blks[i])
			return 0;
	}

	blks->blks = realloc(blks->blks,(blks->len + 1) * sizeof(void*));
	blks->blks[blks->len++] = blk;

	return 1;
}

static void walkCFG(cfg_vec_t* blks, const cfg_block_t* blk)
{
	if (addCFG(blks,blk))
	{
		for (size_t i=0;i < blk->m_count; i = opInc(blk->m_ops[i].m_opcode,i))
		{
			switch (blk->m_ops[i].m_opcode)
			{
			case OP_JMP:
			case OP_CALL:
				walkCFG(blks,blk->m_ops[i+1].m_pval);
				break;

			case OP_BRANCH:
			case OP_BUILTIN:
            	walkCFG(blks,blk->m_ops[i+2].m_pval);
				break;

			default:
				break;
			}
		}
	}
}

void dumpCFG(const cfg_block_t* b, FILE* f)
{
	if (f)
	{
		fprintf(f,"digraph cfg {\n");

		if (b)
		{
			fprintf(f,"\tstart [shape=point];\n");

			cfg_vec_t blks = {0};
			walkCFG(&blks,b);

			for (size_t i=0; i < blks.len; ++i)
				dumpCFGBlock(blks.blks[i],f);

			fprintf(f,"\tstart -> N%p:<f0>;\n",b);
		}

		fprintf(f,"}");
	}
}
