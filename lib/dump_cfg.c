
#include "compile.h"
#include "builtins.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#undef DECLARE_BUILTIN_INTRINSIC
#define DECLARE_BUILTIN_INTRINSIC(f,p)

#undef DECLARE_BUILTIN_FUNCTION
#define DECLARE_BUILTIN_FUNCTION(f,p,a) \
	{ &builtin_##f, #f },

static const char* builtinName(const builtin_fn_t fn)
{
	static const struct builtin_names
	{
		builtin_fn_t fn;
		const char* name;
	} bns[] =
	{
		#include "builtin_functions.h"

		{ &builtin_call, "call" },
		{ &builtin_callN, "call/N" },
		{ &builtin_catch, "catch" },
		{ &builtin_throw, "throw", },
		{ &builtin_halt, "halt" },
		{ &builtin_callable, "callable" },
		{ &builtin_user_defined, "user_defined" },
		{ &builtin_occurs_check, "occurs_check" },
		{ &builtin_term_compare, "term_compare" }
	};

	for (size_t i=0; i < sizeof(bns)/sizeof(bns[0]); ++i)
	{
		if (fn == bns[i].fn)
			return bns[i].name;
	}

	return "unknown";
}

static void fmtFlags(exec_flags_t v, char* buf)
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

static void fmtTypeFlags(prolite_type_flags_t v, char* buf)
{
	char* s = buf;

	if (v & type_flag_double)
		*buf++ = 'D';

	if (v & type_flag_var)
		*buf++ = 'V';

	if (v & type_flag_int32)
		*buf++ = 'I';

	if (v & type_flag_atom)
		*buf++ = 'A';

	if (v & (type_flag_compound | type_flag_chars | type_flag_charcodes))
		*buf++ = 'C';

	if (buf == s)
		*buf++ = '0';

	*buf = '\0';
}

static void dumpTerm(const term_t* t, FILE* f, int ref)
{
	switch (get_term_type(t))
	{
	case prolite_atom:
		{
			string_t s;
			get_string(t,&s,NULL);
			fprintf(f,"%.*s",(int)s.m_len,s.m_str);
		}
		break;

	case prolite_compound:
		{
			size_t arity;
			string_t s;
			get_predicate(t,&s,&arity,NULL);
			fprintf(f,"%s%.*s(",ref ? "&" : "",(int)s.m_len,s.m_str);

			t = get_first_arg(t,NULL);
			for (size_t i = 0; i < arity; ++i)
			{
				if (i)
					fprintf(f,",");

				dumpTerm(t,f,0);
				t = get_next_arg(t);
			}
			fprintf(f,")");
		}
		break;

	case prolite_var:
		fprintf(f,"%slocal[%zu]",ref ? "" : "*",(size_t)get_var_index(t));
		break;

	case prolite_integer:
		fprintf(f,"%lu",(long)get_integer(t));
		break;

	default:
		fprintf(f,"%p",t);
		break;
	}
}

static void dumpCFGBlock(const cfg_block_t* blk, FILE* f)
{
	char buf[10] = {0};

	fprintf(f,"\tnode [shape=record];\n");
	fprintf(f,"\tN%p [label=\"{",blk);

	if (!blk->m_count)
	{
		fprintf(f,"<f0> WTF?!?!");
	}

	for (size_t i=0;i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		if (i)
			fprintf(f,"|");
		fprintf(f,"<f%zu> ",i);
		
		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_NOP:
			fprintf(f,"(NOP)");
			break;

		case OP_SUCCEEDS:
			fprintf(f,"Success!");
			break;

		case OP_END:
			fprintf(f,"End");
			break;

		case OP_JMP:
			fprintf(f,"Jmp");
			break;

		case OP_GOSUB:
			fprintf(f,"Gosub");
			break;

		case OP_RET:
			fprintf(f,"Ret");
			break;

		case OP_BUILTIN:
			fprintf(f,"Builtin\\ %s|<f%zu> ...\\ if\\ !FTH,\\ Gosub",builtinName(blk->m_ops[i+1].m_term.m_pval),i+1);
			break;

		case OP_SET_FLAGS:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"Set\\ Flags\\ %s",buf);
			break;

		case OP_CLEAR_FLAGS:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"Clear\\ Flags\\ %s",buf);
			break;

		case OP_PUSH_CUT:
			fprintf(f,"Push\\ Cut");
			break;

		case OP_POP_CUT:
			fprintf(f,"Pop\\ Cut");
			break;

		case OP_BRANCH:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"Branch \\%s",buf);
			break;

		case OP_BRANCH_NOT:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"Branch !\\%s",buf);
			break;

		case OP_PUSH_TERM_REF:
			fprintf(f,"Push\\ ");
			dumpTerm(blk->m_ops[i+1].m_term.m_pval,f,1);
			break;

		case OP_SET_VAR:
			fprintf(f,"Set local[%zu]\\ =\\ ",(size_t)blk->m_ops[i+1].m_term.m_u64val);
			dumpTerm(blk->m_ops[i+2].m_term.m_pval,f,1);
			break;

		case OP_CLEAR_VAR:
			fprintf(f,"Clear\\ local[%zu]",(size_t)blk->m_ops[i+1].m_term.m_u64val);
			break;

		case OP_TYPE_TEST:
			fmtTypeFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"Type(local[%zu])\\ ==\\ %s",(size_t)blk->m_ops[i+1].m_term.m_u64val,buf);
			break;

		default:
			fprintf(f,"WTF? %zu",(size_t)blk->m_ops[i].m_term.m_u64val);
			break;
		}
	}

	fprintf(f,"}\"];\n");

	for (size_t i=0;i < blk->m_count; i += inc_ip(blk->m_ops[i].m_opcode.m_op))
	{
		switch (blk->m_ops[i].m_opcode.m_op)
		{
		case OP_BRANCH:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [label=\"%s\"];\n",blk,i,blk->m_ops[i+1].m_term.m_pval,buf);
			break;

		case OP_BRANCH_NOT:
			fmtFlags(blk->m_ops[i].m_opcode.m_arg,buf);
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [label=\"!%s\"];\n",blk,i,blk->m_ops[i+1].m_term.m_pval,buf);
			break;

		case OP_GOSUB:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [dir=both];\n",blk,i,blk->m_ops[i+1].m_term.m_pval);
			break;

		case OP_JMP:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0>;\n",blk,i,blk->m_ops[i+1].m_term.m_pval);
			break;

		case OP_BUILTIN:
			fprintf(f,"\tN%p:<f%zu> -> N%p:<f0> [dir=both label=\"!FTH\"];\n",blk,i+1,blk->m_ops[i+2].m_term.m_pval);
			break;

		default:
			break;
		}
	}
}

void dumpCFG(const cfg_vec_t* blks, const char* filename)
{
	FILE* f = fopen(filename,"w");

	fprintf(f,"digraph cfg {\n\tstart [shape=point];\n");

	for (size_t i=0; i < blks->m_count; ++i)
		dumpCFGBlock(blks->m_blks[i].m_blk,f);

	fprintf(f,"\tstart -> N%p:<f0>;\n}",blks->m_blks[0].m_blk);

	fclose(f);
}

void dumpTrace(const opcode_t* code, size_t count, const char* filename)
{
	FILE* f = fopen(filename,"w");

	char buf[10] = {0};

	int spaces = 0;
	for (size_t c = count; c; c /= 10)
		++spaces;
	
	const opcode_t* start = code;
	for (const opcode_t* end = code + count; code < end; code += inc_ip(code->m_opcode.m_op))
	{		
		fprintf(f,"%*zu: ",spaces,code - start);

		switch (code->m_opcode.m_op)
		{
		case OP_NOP:
			fprintf(f,"NOP;\n");
			break;

		case OP_SUCCEEDS:
			fprintf(f,"success;\n");
			break;

		case OP_END:
			fprintf(f,"end;\n");
			break;

		case OP_JMP:
			fprintf(f,"goto %+d (%zu);\n",(int)code[1].m_term.m_u64val,(size_t)((code + 1 - start) + (int64_t)code[1].m_term.m_u64val));
			break;

		case OP_GOSUB:
			fprintf(f,"gosub %+d (%zu);\n",(int)code[1].m_term.m_u64val,(size_t)((code + 1 - start) + (int64_t)code[1].m_term.m_u64val));
			break;

		case OP_RET:
			fprintf(f,"return;\n");
			break;

		case OP_BUILTIN:
			fprintf(f,"extern %s() { if (!FTH) gosub %+d (%zu); }\n",builtinName(code[1].m_term.m_pval),(int)code[2].m_term.m_u64val,(size_t)((code + 2 - start) + (int64_t)code[2].m_term.m_u64val));
			break;

		case OP_SET_FLAGS:
			fmtFlags(code->m_opcode.m_arg,buf);
			fprintf(f,"flags |= %s;\n",buf);
			break;

		case OP_CLEAR_FLAGS:
			fmtFlags(code->m_opcode.m_arg,buf);
			fprintf(f,"flags &= ~(%s);\n",buf);
			break;

		case OP_PUSH_CUT:
			fprintf(f,"push cut;\n");
			break;

		case OP_POP_CUT:
			fprintf(f,"pop cut;\n");
			break;

		case OP_BRANCH:
			fmtFlags(code->m_opcode.m_arg,buf);
			fprintf(f,"if (flags & %s) goto %+d (%zu);\n",buf,(int)code[1].m_term.m_u64val,(size_t)((code + 1 - start) + (int64_t)code[1].m_term.m_u64val));
			break;

		case OP_BRANCH_NOT:
			fmtFlags(code->m_opcode.m_arg,buf);
			fprintf(f,"if (!(flags & %s)) goto %+d (%zu);\n",buf,(int)code[1].m_term.m_u64val,(size_t)((code + 1 - start) + (int64_t)code[1].m_term.m_u64val));
			break;

		case OP_PUSH_TERM_REF:
			fprintf(f,"push ");
			dumpTerm(code[1].m_term.m_pval,f,1);
			fprintf(f,";\n");
			break;

		case OP_SET_VAR:
			fprintf(f,"local[%zu] = ",(size_t)code[1].m_term.m_u64val);
			dumpTerm(code[2].m_term.m_pval,f,1);
			fprintf(f,";\n");
			break;

		case OP_CLEAR_VAR:
			fprintf(f,"local[%zu] = NULL;\n",(size_t)code[1].m_term.m_u64val);
			break;

		case OP_TYPE_TEST:
			fmtTypeFlags(code->m_opcode.m_arg,buf);
			fprintf(f,"if (typeof(local[%zu]) & %s) flags &= F;\n",(size_t)code[1].m_term.m_u64val,buf);
			break;

		default:
			fprintf(f,"WTF? %zu;\n",(size_t)code->m_term.m_u64val);
			break;
		}
	}

	fclose(f);
}
