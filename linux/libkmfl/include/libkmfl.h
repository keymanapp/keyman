/* libkmfl.h
 * Copyright (C) 2005 SIL International and Tavultesoft Pty Ltd
 *
 * This file is part of the KMFL library.
 *
 * The KMFL library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * The KMFL library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with the KMFL library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
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

void DBGMSG(int debug,char *fmt,...);
void *ERRMSG(char *fmt,...);
void clear_history(KMSI *p_kmsi);

extern int kmfl_debug;

#ifdef	__cplusplus
}
#endif	

#endif /* *** end of LIBKMFL.H *** */
