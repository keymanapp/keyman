/* vim:set et sts=4: */

/*
 * KMFL Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2009-2018 SIL International
 *
 * kmflutil is dual licensed under the MIT or GPL licenses as described below.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * MIT license
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
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * OR
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */

#ifndef __KMFLUTIL_H__
#define __KMFLUTIL_H__

#include <ibus.h>
#include <kmfl/kmfl.h>
#include <kmfl/libkmfl.h>

typedef enum {OUTPUT_STRING, ERASE_CHAR, FORWARD_KEYEVENT, OUTPUT_BEEP} kmfl_opcodes;

typedef struct
{  
  kmfl_opcodes opcode;
  gchar * cmdarg;
} Cmd;

typedef struct 
{
    KMSI * p_kmsi;
    GList * cmds;
} KInputContext;

typedef struct 
{
    int keyboard_number;
    gchar * keyboard_filename;
    gchar * keyboard_icon_filename;
    gchar * keyboard_id;
    gchar * keyboard_name;
    gchar * keyboard_language;
    gchar * keyboard_author;
    gchar * keyboard_copyright;
    gchar * keyboard_description;
    gchar * keyboard_layout;
    gchar * keyboard_license;
	
} KInputMethod;

void             ibus_kmfl_init             (void);
GList           *ibus_kmfl_list_engines     (void);
IBusComponent   *ibus_kmfl_get_component    (void);
KInputMethod 	*kinput_open_im				(const gchar * keyboard_filename);
KInputContext   *kmfl_create_ic             (KInputMethod *im);
void             kmfl_destroy_ic            (KInputContext *ic);
void 			 kinput_close_im			(KInputMethod * im);
void 			 kmfl_get_keyboard_info		(KInputMethod * im);
#endif
