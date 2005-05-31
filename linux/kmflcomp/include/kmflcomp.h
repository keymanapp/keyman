extern int opt_debug;
extern int opt_force;
extern int opt_verbose;
extern int errcount, errlimit, warnings, warnlimit;
extern int yydebug;
extern jmp_buf fatal_error_buf;

unsigned long compile_keyboard_to_buffer(const char * infile, void ** keyboard_buffer);
void write_keyboard(char * fname, void *keyboard_buffer, int keyboard_buffer_size);
