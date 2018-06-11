/* kmflcomp.h
 * Copyright (C) 2005  SIL International
 *
 * This file is part of the KMFL compiler.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
