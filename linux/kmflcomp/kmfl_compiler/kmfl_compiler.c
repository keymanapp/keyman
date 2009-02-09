/* kmfl_compiler.c
 * Copyright (C) 2005  SIL International
 *
 * This file is part of KMFL compiler.
 *
 * KMFL compiler is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * KMFL compiler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with KMFL compiler; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */
const char * VERSION= "0.9.8";

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
const char * usagemsg=
"usage: kmflcomp [OPTION...] file\n" \
" -d     debug\n" \
" -f     force compilation\n" \
" -h     print this help message\n" \
" -V     verbose\n" \
" -v     print program version\n" \
" -y     yydebug\n";

void usage(void)
{
	fprintf(stderr, "kmflcomp version %s\n", VERSION);
	fprintf(stderr, "%s", usagemsg);
	exit(1);
}

int main(int argc, char *argv[]) 
{
	int opt,nopt=0;
	void * keyboard_buffer;
	unsigned long keyboard_buffer_size;
	int errcode;
    char *fname="(stdin)";

	while((opt=getopt(argc,argv,"dfhVvy"))!=EOF) 
	{
		switch (opt) 
		{
		case 'd':
			opt_debug=1;
			break;
		case 'f':
			opt_force=1;
			break;
		case 'h':
			usage();
			break;
		case 'V':
			opt_verbose = 1;
			break;
		case 'v':
			fprintf(stderr, "kmflcomp version %s\n", VERSION);
			exit(0);
		case 'y':
			yydebug = 1;
			break;
		}
		nopt++;
	}

	if(argc > nopt+1)
		fname=argv[argc-1];
	else
		usage();

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
