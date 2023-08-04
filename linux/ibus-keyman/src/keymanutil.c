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
#include <keyman/keyboardprocessor.h>

#include "bcp47util.h"
#include "keymanutil.h"
#include "kmpdetails.h"
#include "keyman-version.h"

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
    full_path_to_icon_file=g_strdup_printf("%s.bmp.png", filename);
    g_free(filename);

    if (!g_file_test(full_path_to_icon_file, G_FILE_TEST_EXISTS)) {
        g_free(full_path_to_icon_file);
        full_path_to_icon_file=g_strdup("/usr/share/keyman/icons/default.png");
    }
    return full_path_to_icon_file;
}

static IBusEngineDesc *
ibus_keyman_engine_desc_new (gchar * file_name,
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
                                    "language", lang ? lang : "other",
                                    "license", license ? license : "",
                                    "author", author ? author : "",
                                    "icon", icon,
                                    "layout", layout,
                                    "version", version ? version : "",
                                    NULL);
    g_free(desc);

    return engine_desc;
}

GList *
ibus_keyman_add_engines(GList * engines, GList * kmpdir_list)
{
    GList *p, *k, *l, *e;

    for (p=kmpdir_list; p != NULL; p = p->next) {
        gchar * kmp_dir = (gchar *) p->data;

        kmp_details *details = g_new0(kmp_details, 1);
        get_kmp_details(kmp_dir, details);

        for (k=details->keyboards; k != NULL; k = k->next) {
            kmp_keyboard *keyboard = (kmp_keyboard *) k->data;
            gboolean alreadyexists = FALSE;

            for (e=engines; e != NULL && alreadyexists == FALSE; e = e->next) {
                IBusEngineDesc *engine_desc = (IBusEngineDesc *) e->data;
                const gchar *version = ibus_engine_desc_get_version(engine_desc);
                const gchar *engine_name = ibus_engine_desc_get_version(engine_desc);
                gchar *kmx_file = g_path_get_basename(engine_name);
                if (g_strcmp0(kmx_file, keyboard->kmx_file) == 0  && g_strcmp0(version, keyboard->version) >= 0) {
                    alreadyexists = TRUE;
                    g_debug("keyboard %s already exists at version %s which is newer or same as %s", kmx_file, version, keyboard->version);
                }
                g_free(kmx_file);
            }

            if (!alreadyexists) {
                gchar *abs_kmx = g_strjoin("/", kmp_dir, keyboard->kmx_file, NULL);
                gchar *json_file = g_strjoin(".", keyboard->id, "json", NULL);
                keyboard_details *kbd_details = g_new0(keyboard_details, 1);
                get_keyboard_details(kmp_dir, json_file, kbd_details);
                g_free(json_file);

                if (keyboard->languages != NULL) {
                    for (l=keyboard->languages; l != NULL; l = l->next) {
                        kmp_language *language = (kmp_language *) l->data;
                        if (language->id != NULL) {
                          int capacity          = 255;
                          gchar *name_with_lang = NULL;
                          gchar *minimized_tag  = g_new0(gchar, capacity);
                          int result = bcp47_minimize(language->id, minimized_tag, capacity);
                          if (result < 0) {
                            g_strlcpy(minimized_tag, language->id, capacity);
                          }

                          gchar *lang_code = g_new0(gchar, capacity);
                          if (!bcp47_get_language_code(minimized_tag, lang_code, capacity)) {
                            g_strlcpy(lang_code, minimized_tag, capacity);
                          }

                          // If ibus doesn't know about the language then append the
                          // language name to the keyboard name
                          if (language->name != NULL) {
                            gchar *ibus_lang = ibus_get_untranslated_language_name(lang_code);
                            g_debug("%s: untranslated ibus language for %s: %s", __FUNCTION__, minimized_tag, ibus_lang);
                            if (g_strcmp0(ibus_lang, "Other") == 0) {
                              name_with_lang = g_strjoin(" - ", keyboard->name, language->name, NULL);
                            }
                            g_free(ibus_lang);
                          }

                          gchar *id_with_lang = g_strjoin(":", minimized_tag, abs_kmx, NULL);

                          g_message("adding engine %s", id_with_lang);
                          engines = g_list_append(
                              engines,
                              ibus_keyman_engine_desc_new(
                                  id_with_lang,                                      // lang:kmx full path
                                  name_with_lang ? name_with_lang : keyboard->name,  // longname
                                  kbd_details->description,                          // description
                                  details->info.copyright,                           // copyright if available
                                  lang_code,                      // language, most are ignored by ibus except major languages
                                  kbd_details->license,           // license
                                  details->info.author_desc,      // author name only, not email
                                  keyman_get_icon_file(abs_kmx),  // icon full path
                                  "us",                           // layout defaulting to us (en-US)
                                  keyboard->version));
                          g_free(lang_code);
                          g_free(minimized_tag);
                          g_free(id_with_lang);
                          g_free(name_with_lang);
                        }
                    }
                }
                else {
                    g_message("adding engine %s", abs_kmx);
                    engines = g_list_append (engines,
                        ibus_keyman_engine_desc_new (abs_kmx, // kmx full path
                                keyboard->name, // longname
                                kbd_details->description, // description
                                details->info.copyright, // copyright if available
                                NULL, // language, most are ignored by ibus except major languages
                                kbd_details->license, // license
                                details->info.author_desc, // author name only, not email
                                keyman_get_icon_file(abs_kmx), // icon full path
                                "us", // layout defaulting to us (en-US)
                                keyboard->version));
                }
                free_keyboard_details(kbd_details);
                g_free(kbd_details);
                g_free(abs_kmx);
            }
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

    g_debug("adding from /usr/share/keyman");
    keyboard_list = keyman_get_kmpdirs_fromdir(NULL, "/usr/share/keyman");
    g_debug("adding from /usr/local/share/keyman");
    keyboard_list = keyman_get_kmpdirs_fromdir(keyboard_list, "/usr/local/share/keyman");
    xdgenv = getenv("XDG_DATA_HOME");
    if (xdgenv != NULL){
        local_keyboard_path= g_strdup_printf("%s/keyman", xdgenv);
    }
    else {
        xdgenv = getenv("HOME");
        local_keyboard_path= g_strdup_printf("%s/.local/share/keyman", xdgenv);
    }
    g_debug("adding from %s", local_keyboard_path);
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
                                    KEYMAN_VERSION,
                                    "GPL",
                                    "Keyman team <support@keyman.com>",
                                    "https://keyman.com",
                                    "",
                                    "ibus-keyman");

    engines = ibus_keyman_list_engines ();

    for (p = engines; p != NULL; p = p->next) {
        ibus_component_add_engine (component, (IBusEngineDesc *) p->data);
    }

    g_list_free (engines);
    return component;
}


// Obtain Keyboard Options list from DConf
// DConf options are in a list of strings like ['option_key1=value1', 'option_key2=value2']
//
// Parameters:
// package_id  (gchar *): Package ID
// keyboard_id (gchar *): Keyboard ID
//
// Returns a newly allocated gchar**; free with g_strfreev()
gchar**
keyman_get_options_fromdconf(gchar *package_id,
                             gchar *keyboard_id)
{
    g_message("keyman_get_options_fromdconf");

    // Obtain keyboard options from DConf
    gchar *path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_PATH, package_id, keyboard_id);
    GSettings *child_settings = g_settings_new_with_path(KEYMAN_CHILD_DCONF_NAME, path);
    gchar **options = NULL;
    if (child_settings != NULL)
    {
        options = g_settings_get_strv(child_settings, KEYMAN_DCONF_OPTIONS_KEY);
    }

    g_object_unref(G_OBJECT(child_settings));
    g_free(path);

    return options;
}

// Obtain Keyboard Options from DConf and parse into a GQueue of struct km_kbp_option_item
//
// Parameters:
// package_id  (gchar *): Package ID
// keyboard_id (gchar *): Keyboard ID
//
// Return a newly allocated GQueue; free with g_queue_free_full()
GQueue*
keyman_get_options_queue_fromdconf(gchar *package_id,
                                   gchar *keyboard_id)
{
    g_message("keyman_get_options_queue_fromdconf");
    GQueue *queue_options = g_queue_new();

    // Obtain keyboard options from DConf
    gchar **options = keyman_get_options_fromdconf(package_id, keyboard_id);

    // Parse options into queue_options
    if (options != NULL)
    {
        int index = 0;
        while (options[index] != NULL)
        {
            gchar **option_tokens = g_strsplit(options[index], "=", 2);
            if (option_tokens != NULL && option_tokens[0] != NULL && option_tokens[1] != NULL)
            {
                g_message("Keyboard Option [%d], %s=%s", index, option_tokens[0], option_tokens[1]);
                km_kbp_option_item *opt = g_new0(km_kbp_option_item, 1);
                opt[0].scope = KM_KBP_OPT_KEYBOARD;
                km_kbp_cp *ocp = g_utf8_to_utf16(option_tokens[0], -1, NULL, NULL, NULL);
                opt[0].key = ocp;
                ocp = g_utf8_to_utf16 (option_tokens[1], -1, NULL, NULL, NULL);
                opt[0].value = ocp;
                g_queue_push_tail(queue_options, opt);
            }
            index++;
        }
        g_strfreev(options);
    }

    return queue_options;
}

// Write new keyboard option to DConf.
// DConf options are in a list of strings like ['option_key1=value1', 'option_key2=value2']
// If the option key already exists, the value is updated. Otherwise a new string
// 'option_key=option_value' is appended.
//
// Parameters:
// package_id   (gchar *): Package ID
// keyboard_id  (gchar *): Keyboard ID
// option_key   (gchar *): Key for the new option
// option_value (gchar *): Value of the new option
void
keyman_put_options_todconf(gchar *package_id,
                           gchar *keyboard_id,
                           gchar *option_key,
                           gchar *option_value)
{
    g_message("keyman_put_options_todconf");
    if (package_id == NULL || keyboard_id == NULL || option_key == NULL || option_value == NULL)
    {
        return;
    }

    // Obtain keyboard options from DConf
    gchar **options = keyman_get_options_fromdconf(package_id, keyboard_id);
    gchar *needle = g_strdup_printf("%s=", option_key);
    gchar *kvp = g_strdup_printf("%s=%s", option_key, option_value);

    if (options != NULL)
    {
        int index = 0;
        gboolean option_updated = FALSE;
        while (options[index] != NULL)
        {
            // If option_key already exists, update value with option_value
            if (g_strrstr(options[index], needle) != NULL)
            {
                g_free(options[index]);
                options[index] = kvp;
                option_updated = TRUE;
                break;
            }
            index++;
        }

        if (!option_updated)
        {
            // Resize to add new option and null-terminate
            int size = index + 2; // old size: index + 1, plus 1 new
            options = g_renew(gchar*, options, size);
            options[index] = kvp;
            options[index+1] = NULL;
        }
    }
    else
    {
      // we never should come here - keyman_get_options_fromdconf will create empty
      // options if they don't yet exist.
      // Allocate space for new option and null-terminate
      options    = g_new(gchar *, 2);
      options[0] = kvp;
      options[1] = NULL;
    }

    // Write to DConf
    gchar *path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_PATH, package_id, keyboard_id);
    GSettings *child_settings = g_settings_new_with_path(KEYMAN_CHILD_DCONF_NAME, path);
    if (child_settings != NULL)
    {
        g_message("writing keyboard options to DConf");
        g_settings_set_strv(child_settings, KEYMAN_DCONF_OPTIONS_KEY, (const gchar *const *)options);
    }

    g_object_unref(G_OBJECT(child_settings));
    g_free(path);
    g_free(needle);
    g_strfreev(options);
    // kvp got assigned to options[x] and so got freed by g_strfreev()
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

