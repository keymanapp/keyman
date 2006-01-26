/* kmfl_messages.c
 * Copyright (C) 2004 Tavultesoft
 *
 * This file is part of KMFL Compiler library.
 *
 * KMFL Compiler library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * KMFL Compiler library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with KMFL Compiler library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
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

void DBGMSG(int debug,char *fmt,...) 
{
	FILE * debugfile;
	va_list args;
	
	if((debug<0) || ((kmfl_debug&debug)!=0))
	{
//		debugfile=fopen("/tmp/libkmfldebug.log", "a");
		debugfile=stderr;
		fprintf(debugfile,"debug: ");
		va_start(args,fmt);
		vfprintf(debugfile,fmt,args);
		va_end(args);
//		fclose(debugfile);
	}
}

void *ERRMSG(char *fmt,...) 
{
	va_list args;
	fprintf(stderr,"error: ");
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	return(NULL);
}
