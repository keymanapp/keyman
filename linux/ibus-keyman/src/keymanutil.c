/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2018 SIL International
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

#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <string.h>


#include "keymanutil.h"
#include "kmpdetails.h"

static GHashTable      *im_table = NULL;

#define N_(text) text

// change to keyman_get_kmpdirs_fromdir
// returns list of directories with kmp.json
GList * keyman_get_kmpdirs_fromdir( GList *keyboard_list, const gchar * path)
{
    DIR *dir = opendir(path);

    if (dir != NULL) {
        struct dirent *file = readdir(dir);
        while (file != NULL) {
            struct stat filestat;
            gchar * absfn = g_strdup_printf("%s/%s", path, file->d_name);
            stat(absfn, &filestat);

            if (S_ISDIR(filestat.st_mode))
            {
                if(g_strcmp0(file->d_name, ".") != 0 && g_strcmp0(file->d_name, "..") != 0)
                    keyboard_list = keyman_get_kmpdirs_fromdir(keyboard_list, absfn);
            }
            // Looking for kmp.json
            else if (S_ISREG(filestat.st_mode) && g_strcmp0(file->d_name, "kmp.json") == 0)
            {
                g_message("adding kmp path %s", path);
                keyboard_list=g_list_append(keyboard_list, g_strdup(path));
            }
            g_free(absfn);

            file = readdir(dir);
        }
        closedir(dir);
    }
    return keyboard_list;
}

gchar * keyman_get_icon_file(const gchar *kmx_file)
{
    // Now there will only be the .png
    // which at some point will get extracted from the .kmx during installation
    gchar *filename, *full_path_to_icon_file, *p;

    p=rindex(kmx_file,'.');
    filename = g_strndup(kmx_file, p-kmx_file);
    full_path_to_icon_file=g_strdup_printf("%s.ico.png", filename);
    g_free(filename);

    if (!g_file_test(full_path_to_icon_file, G_FILE_TEST_EXISTS)) {
        g_free(full_path_to_icon_file);
        full_path_to_icon_file=g_strdup("/usr/share/keyman/icons/default.png");
    }
    return full_path_to_icon_file;
}

static IBusEngineDesc *
ibus_keyman_engine_new (gchar * file_name,
                      gchar *name,
                      gchar *description,
                      gchar *copyright,
                      gchar *lang,
                      gchar *license,
                      gchar *author,
                      gchar *icon,
                      gchar *layout,
                      gchar *version)
{
   IBusEngineDesc *engine_desc;
    gchar * desc;

    if (description == NULL) {
        desc = g_strdup_printf("%s", copyright);
    }
    else {
        desc = g_strdup_printf("%s\n%s", description, copyright);
    }

    engine_desc = ibus_engine_desc_new_varargs ("name", file_name,
                                    "longname", name,
                                    "description", desc,
                                    "language", lang,
                                    "license", license ? license : "",
                                    "author", author ? author : "",
                                    "icon", icon,
                                    "layout", layout,
                                    "version", version,
                                    NULL);

    return engine_desc;
}

GList * 
ibus_keyman_add_engines(GList * engines, GList * kmpdir_list)
{
    GList *p, *l;

    for (p=kmpdir_list; p != NULL; p = p->next) {
        gchar * kmp_dir = (gchar *) p->data;

        kmp_details *details = g_new0(kmp_details, 1);
        get_kmp_details(kmp_dir, details);

        for (l=details->keyboards; l != NULL; l = l->next) {
            gchar *lang=NULL;
            kmp_keyboard *keyboard = (kmp_keyboard *) l->data;
            gchar *abs_kmx = g_strjoin("/", kmp_dir, keyboard->kmx_file, NULL);

            if (keyboard->languages != NULL)
            {
                // Only gets the first language because ibus can only handle one
                kmp_language *language = (kmp_language *) keyboard->languages->data;
                if (language->id != NULL) {
                    gchar **tagparts = g_strsplit(language->id, "-", 2);
                    lang = g_strdup(tagparts[0]);
                    g_strfreev(tagparts);
                }
            }
            gchar *json_file = g_strjoin(".", keyboard->id, "json", NULL);
            keyboard_details *kbd_details = g_new0(keyboard_details, 1);
            get_keyboard_details(kmp_dir, json_file, kbd_details);
            g_free(json_file);

            g_message("adding engine %s", abs_kmx);
            engines = g_list_append (engines,
                ibus_keyman_engine_new (abs_kmx, // kmx full path
                        keyboard->name, // longname
                        kbd_details->description, // description
                        details->info.copyright, // copyright if available
                        lang, // language, most are ignored by ibus except major languages
                        kbd_details->license, // license
                        details->info.author_desc, // author name only, not email
                        keyman_get_icon_file(abs_kmx), // icon full path
                        "en", // layout defaulting to en (en-US)
                        keyboard->version));
            free_keyboard_details(kbd_details);
            g_free(kbd_details);
            g_free(abs_kmx);
            g_free(lang);
        }
        free_kmp_details(details);
        g_free(details);
    }
    return engines;
}

GList *
ibus_keyman_list_engines (void)
{
    GList *engines = NULL;
    GList *keyboard_list;
    gchar *local_keyboard_path, *xdgenv;

    g_message("adding from /usr/share/keyman");
    keyboard_list = keyman_get_kmpdirs_fromdir(NULL, "/usr/share/keyman");
    g_message("adding from /usr/share/keyman");
    keyboard_list = keyman_get_kmpdirs_fromdir(keyboard_list, "/usr/local/share/keyman");
    xdgenv = getenv("XDG_DATA_HOME");
    if (xdgenv != NULL){
        local_keyboard_path= g_strdup_printf("%s/keyman", xdgenv);
    }
    else {
        xdgenv = getenv("HOME");
        local_keyboard_path= g_strdup_printf("%s/.local/share/keyman", xdgenv);
    }
    g_message("adding from %s", local_keyboard_path);
    keyboard_list = keyman_get_kmpdirs_fromdir(keyboard_list, local_keyboard_path);
    g_free(local_keyboard_path);
    engines = ibus_keyman_add_engines(engines, keyboard_list);
    g_list_free(keyboard_list);

    return engines;
}

IBusComponent *
ibus_keyman_get_component (void)
{
    GList *engines, *p;
    IBusComponent *component;

    component = ibus_component_new ("org.freedesktop.IBus.Keyman",
                                    N_("Keyman"),
                                    "10.99.0",
                                    "GPL",
                                    "Daniel Glassey <wdg@debian.org>",
                                    "http://www.keyman.com",
                                    "",
                                    "ibus-keyman");

    engines = ibus_keyman_list_engines ();

    for (p = engines; p != NULL; p = p->next) {
        ibus_component_add_engine (component, (IBusEngineDesc *) p->data);
    }

    g_list_free (engines);
    return component;
}

#ifdef DEBUG
#include <locale.h>

int main ()
{
    IBusComponent *component;
    GString *output;

    setlocale (LC_ALL, "");
    ibus_init ();

    component = ibus_keyman_get_component ();

    output = g_string_new ("");

    ibus_component_output (component, output, 1);

    g_debug ("\n%s", output->str);

    g_string_free (output, TRUE);
    g_object_unref (component);

    return 0;
}
#endif

