/* libkmfl.h
 * Copyright (C) 2005 SIL International
 *
 * This file is part of the KMFL library.
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

// LIBKMFL.H: Header for interpreter for Keyboard Mapping for Linux

#ifndef LIBKMFL_H

#ifdef	__cplusplus
extern "C" {
#endif

int kmfl_interpret(KMSI *p_kmsi, UINT key, UINT state);

int kmfl_load_keyboard(const char *file);
int kmfl_check_keyboard(const char *file);
int kmfl_reload_keyboard(int keyboard_number);
int kmfl_reload_all_keyboards(void);
int kmfl_unload_keyboard(int keyboard_number);
int kmfl_unload_all_keyboards(void);

KMSI *kmfl_make_keyboard_instance(void *connection);
int kmfl_delete_keyboard_instance(KMSI *p_kmsi);
int kmfl_delete_all_keyboard_instances(void);

int kmfl_attach_keyboard(KMSI *p_kmsi, int keyboard_number);
int kmfl_detach_keyboard(KMSI *p_kmsi);

int kmfl_keyboard_number(char *name);
const char *kmfl_keyboard_name(int keyboard_number);
const char *kmfl_icon_file(int keyboard_number);

int kmfl_get_header(KMSI *p_kmsi,int hdrID,char *buf,int buflen);

void DBGMSG(int debug,const char *fmt,...);
void *ERRMSG(const char *fmt,...);
void clear_history(KMSI *p_kmsi);
int deadkey_in_history(KMSI *p_kmsi);
void set_history(KMSI *p_kmsi, ITEM * items, UINT nitems);

extern int kmfl_debug;

#ifdef	__cplusplus
}
#endif

#endif /* *** end of LIBKMFL.H *** */
