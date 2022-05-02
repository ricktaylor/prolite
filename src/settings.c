

#include "settings.h"

#include <string.h>
#include <stdio.h>

static int set_bool(const char* arg, void* param)
{
	*(_Bool*)param = 1;
	return 1;
}

static int set_const_char(const char* arg, void* param)
{
	*(const char**)param = arg;
	return 1;
}

int parse_cmd_args(int argc, char* argv[], settings_t* settings)
{
	const struct cmd_option
	{
		char        short_opt;
		const char* long_opt;
		int         with_arg;  // 0 == No, 1 == Yes, 2 == Maybe
		const char* opt_arg_desc;
		int (*callback)(const char* arg, void* param);
		void*       param;

	} opts[] = {
		{ 'h', "help", 0, NULL, &set_bool, &settings->m_help },
		{ 'v', "verbose", 0, NULL, &set_bool, &settings->m_verbose },
		{ 'i', "interactive", 0, NULL, &set_bool, &settings->m_interactive },
		{ 'o', "output", 1, "filename", &set_const_char, &settings->m_output_name },
	};

	settings->m_prog_name = argv[0];

	int new_argc = 0;
	int i = 1;
	while (i < argc)
	{
		if (argv[i][0] == '-' && argv[i][1] != '\0')
		{
			if (argv[i][1] == '-')
			{
				if (argv[i][2] == '\0')
				{
					// End of options '--'
					++i;
					break;
				}
				else
				{
					// Long option
					size_t o = 0;
					for (o = 0; o < sizeof(opts)/sizeof(opts[0]); ++o)
					{
						if (strcmp(argv[i] + 2,opts[o].long_opt) == 0)
						{
							const char* opt_arg = NULL;
							if (opts[o].with_arg)
							{
								// argv[i+1] is the option
								if (i < argc - 1)
									opt_arg = argv[++i];
								
								if (!opt_arg && opts[o].with_arg == 1)
								{
									fprintf(stderr,"%s: error: missing %s after '%s'\n",argv[0],opts[o].opt_arg_desc,argv[i]);
									return 0;
								}
							}

							if (!(*opts[o].callback)(opt_arg,opts[o].param))
								return 0;

							break;
						}
					}

					if (o == sizeof(opts)/sizeof(opts[0]))
					{
						fprintf(stderr,"%s: error: unrecognized command line option '%s'\n",argv[0],argv[i]);
						return 0;
					}
				}
			}
			else
			{
				// Short option
				size_t o = 0;
				for (size_t j = 1; o < sizeof(opts)/sizeof(opts[0]); ++o)
				{
					if (argv[i][j] == opts[o].short_opt)
					{
						const char* opt_arg = NULL;
						if (opts[o].with_arg)
						{
							opt_arg = argv[i] + j + 1;
							if (*opt_arg == '\0' || j > 1)
							{
								// argv[i+1] is the option
								if (i < argc - 1)
									opt_arg = argv[++i];
								else
									opt_arg = NULL;
							}
							
							if (!opt_arg && opts[o].with_arg == 1)
							{
								fprintf(stderr,"%s: error: missing %s after '-%c'\n",argv[0],opts[o].opt_arg_desc,argv[i][j]);
								return 0;
							}
						}

						if (!(*opts[o].callback)(opt_arg,opts[o].param))
							return 0;

						if (opt_arg)
							break;

						// Start again with the next character for concatenated non-arg options
						++j;
						o = -1;
					}
				}

				if (o == sizeof(opts)/sizeof(opts[0]))
				{
					fprintf(stderr,"%s: error: unrecognized command line option '%s'\n",argv[0],argv[i]);
					return 0;
				}
			}

			++i;
		}
		else
		{
			// Non option argument
			argv[new_argc++] = argv[i++];
		}
	}

	while (i < argc)
	{
		argv[new_argc++] = argv[i++];
	}

	return new_argc;
}