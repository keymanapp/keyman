/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2018, 2020 SIL International
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

#ifndef __KEYMANUTIL_H__
#define __KEYMANUTIL_H__

#include <ibus.h>
#include <gmodule.h>

#include <keyman/keyboardprocessor.h>

// Number of default Keyboard processor environment options for: "platform", "baseLayout", and "baseLayoutAlt"
#define KEYMAN_ENVIRONMENT_OPTIONS 3

// Path information for Keyman keyboard options in DConf
#define KEYMAN_DCONF_NAME "com.keyman.options"
#define KEYMAN_CHILD_DCONF_NAME "com.keyman.options.child"
#define KEYMAN_DCONF_PATH "/desktop/ibus/keyman/options/"
#define KEYMAN_DCONF_OPTIONS_KEY "options"

void             ibus_keyman_init           (void);
GList           *ibus_keyman_list_engines   (void);
IBusComponent   *ibus_keyman_get_component  (void);

gchar**  keyman_get_options_fromdconf
                                            (gchar *package_id,
                                             gchar *keyboard_id);

GQueue*  keyman_get_options_queue_fromdconf
                                            (gchar *package_id,
                                             gchar *keyboard_id);

void keyman_put_options_todconf
                                            (gchar *package_id, 
                                             gchar *keyboard_id,
                                             gchar *option_key,
                                             gchar *option_value);

#endif
