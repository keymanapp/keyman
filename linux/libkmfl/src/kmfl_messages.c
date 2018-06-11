/* kmfl_messages.c
 * Copyright (C) 2005 SIL International
 *
 * This file is part of the KMFL library.
 *
 */

/*

	Keystroke interpreter for keyboard mapping for Linux project

		Version 1.000, January 2004, John Durdin, Tavultesoft
	
	Error reporting

*/

#include <stdio.h>
#include <stdarg.h>

int kmfl_debug=0;

void DBGMSG(int debug,const char *fmt,...) 
{
	FILE * debugfile;
	va_list args;
	
	if((debug<0) || ((kmfl_debug&debug)!=0))
	{
		debugfile=fopen("/tmp/libkmfldebug.log", "a");
		fprintf(debugfile,"debug: ");
		va_start(args,fmt);
		vfprintf(debugfile,fmt,args);
		va_end(args);
		fclose(debugfile);
	}
}

void *ERRMSG(const char *fmt,...) 
{
	va_list args;
	fprintf(stderr,"error: ");
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	return(NULL);
}
