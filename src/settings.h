#ifndef SETTINGS_H_
#define SETTINGS_H_

typedef struct settings
{
    const char* m_prog_name;
    
    _Bool m_help;  // -h or --help
    _Bool m_verbose;  // -v or --verbose
    _Bool m_interactive; // -i or --interactive

    const char* m_output_name; // -o or --output <filename>

} settings_t;

int parse_cmd_args(int argc, char* argv[], settings_t* settings);

#endif // SETTINGS_H_
