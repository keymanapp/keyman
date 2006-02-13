/* kmflcomp.h
 * Copyright (C) 2005  SIL International and Tavultesoft Pty Ltd
 *
 * This file is part of the KMFL compiler.
 *
 * The KMFL compiler is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The KMFL compiler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the KMFL compiler; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */

#ifndef KMFLCOMP_H
#ifdef  __cplusplus
extern "C" {
#endif

extern int opt_debug;
extern int opt_force;
extern int opt_verbose;
extern int errcount, errlimit, warnings, warnlimit;
extern int yydebug;
extern jmp_buf fatal_error_buf;

unsigned long compile_keyboard_to_buffer(const char * infile, void ** keyboard_buffer);
void write_keyboard(char * fname, void *keyboard_buffer, int keyboard_buffer_size);

#ifdef  __cplusplus
}
#endif

#endif /* KMFLCOMP_H */
