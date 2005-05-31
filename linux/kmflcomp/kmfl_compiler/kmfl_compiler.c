#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <sys/stat.h>
#include <setjmp.h>

#ifdef _WIN32
	#include <io.h>
	#include <conio.h>
	#include "getopt.h"
	#define strcasecmp	_stricmp
	#define rindex strrchr
	#define seek _lseek
	#define DIRDELIM	'\\'
	char *GetInputFile(void);
#else
	#include <getopt.h>
	#include <sys/types.h>
	#include <unistd.h>
	#define O_BINARY 	0
	#define DIRDELIM	'/'
#endif

#include <fcntl.h>
#include <kmflcomp.h>

int main(int argc, char *argv[]) 
{
	int opt,nopt=0;
	void * keyboard_buffer;
	unsigned long keyboard_buffer_size;
	int errcode;
    char *fname="(stdin)";

	while((opt=getopt(argc,argv,"dfvy"))!=EOF) 
	{
		switch (opt) 
		{
		case 'd':
			opt_debug=1;
			break;
		case 'f':
			opt_force=1;
			break;
		case 'v':
			opt_verbose = 1;
			break;
		case 'y':
			yydebug = 1;
			break;
		}
		nopt++;
	}

	if(argc > nopt+1) fname=argv[argc-1];

#ifdef _WIN32
	else if(!(fname=GetInputFile())) exit(0);
#endif

	// Set warnings level
	if(opt_verbose) 
	   errlimit = warnlimit = 1000;
	   
	errcode = setjmp(fatal_error_buf);
	
    if (errcode == 0)
    {
        keyboard_buffer_size = compile_keyboard_to_buffer(fname, &keyboard_buffer);
    
        write_keyboard(fname, keyboard_buffer, keyboard_buffer_size);
        free(keyboard_buffer);
    
#ifdef _WIN32	
	   if(opt_debug) getch();
#endif	

	   exit(errcount);
    } else {
        exit(errcode);
    }
}
