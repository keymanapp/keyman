/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2018-2023 SIL Global
 *
 * keymanutil is dual licensed under the MIT or GPL licenses as described below.
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

#include <keyman/keyman_core_api.h>

// Number of default Keyboard processor environment options for: "platform", "baseLayout", and "baseLayoutAlt"
#define KEYMAN_ENVIRONMENT_OPTIONS 3

// Path information for Keyman keyboard options in DConf
#define KEYMAN_DCONF_OPTIONS_NAME "com.keyman.options"
#define KEYMAN_DCONF_OPTIONS_CHILD_NAME "com.keyman.options.child"
// TODO: migrate to /com/keyman/options to better follow Gnome recommmendations
// (https://docs.gtk.org/gio/class.Settings.html) (#9579)
#define KEYMAN_DCONF_OPTIONS_PATH "/desktop/ibus/keyman/options/"
#define KEYMAN_DCONF_OPTIONS_KEY "options"
#define KEYMAN_DCONF_OPTIONS_SIMULATEALTGR "simulate-altgr"

#define KEYMAN_DCONF_ENGINE_NAME "com.keyman.engine"
#define KEYMAN_DCONF_ENGINE_PATH "/com/keyman/engine/"
#define KEYMAN_DCONF_KEYBOARDS_KEY "additional-keyboards"

G_BEGIN_DECLS

void             ibus_keyman_init           (void);
GList           *ibus_keyman_list_engines   (void);
IBusComponent   *ibus_keyman_get_component  (void);

/**
 * Obtain Keyboard Options list from DConf
 *
 * DConf options are in a list of strings like ['option_key1=value1', 'option_key2=value2']
 *
 * @param   package_id   Package ID
 * @param   keyboard_id  Keyboard ID
 * @return               A newly allocated gchar**; free with g_strfreev()
 */
gchar **keyman_get_keyboard_options_fromdconf(const gchar *package_id, const gchar *keyboard_id);

/**
 * Obtain Keyboard Options from DConf and parse into a GQueue of struct km_core_option_item
 *
 * @param   package_id   Package ID
 * @param   keyboard_id  Keyboard ID
 * @return               A newly allocated GQueue; free with g_queue_free_full()
 */
GQueue *keyman_get_keyboard_options_queue_fromdconf(const gchar *package_id, const gchar *keyboard_id);

/**
 * Write new keyboard option to DConf
 *
 * DConf options are in a list of strings like ['option_key1=value1', 'option_key2=value2']
 * If the option key already exists, the value is updated. Otherwise a new string
 * 'option_key=option_value' is appended.
 *
 * @param package_id     Package ID
 * @param keyboard_id    Keyboard ID
 * @param option_key     Key for the new option
 * @param option_value   Value of the new option
 */
void keyman_put_keyboard_options_todconf(const gchar *package_id, const gchar *keyboard_id, const gchar *option_key, const gchar *option_value);

/**
 * Obtain (general) option value from DConf
 *
 * @param   option_key   Key of the option
 * @return               The current value of the option
 */
gboolean keyman_get_option_fromdconf(const gchar* option_key);

/**
 * Write new (general) option to DConf
 *
 * @param option_key     Key for the new option
 * @param option_value   Value of the new option
 * @return               TRUE if setting the key succeeded, FALSE if the key was not writable
 */
gboolean keyman_put_option_todconf(const gchar* option_key, gboolean option_value);

/**
 * Subscribe to changes in the (general) Keyman options
 *
 * @param callback   Callback method. Note that this should be different methods if
 *                   this method gets called for different keys.
 * @param user_data  Custom data that will passed to callback
 * @return           A transparent settings object
 */
void* keyman_subscribe_option_changes(void* callback, gpointer user_data);

/**
 * Unsubscribe from changes in the (general) option settings
 *
 * @param settings   The settings object
 * @param callback   The callback method used when subscribing
 * @param user_data  Custom data for callback
 * @return           > 0 if successfully unsubscribed, otherwise 0
 */
guint keyman_unsubscribe_option_changes(void *settings, void *callback, gpointer user_data);

G_END_DECLS

#endif
