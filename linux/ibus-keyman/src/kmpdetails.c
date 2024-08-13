/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2018 SIL International
 *
 * kmpdetails is dual licensed under the MIT or GPL licenses as described below.
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



// APIs to get info that ibus-keyman will want from
// the kmp.json from each kmp
//
// get list of keyboards from this
// associated languages

#include <locale.h>
#include <stdlib.h>
#include <stdio.h>
#include <glib.h>
#include <json-glib/json-glib.h>
#include "kmpdetails.h"


void get_detail_description_from_object(JsonObject *object, const gchar *member_name, gchar **description, gchar **url)
{
    const gchar *value;
    JsonNode *member_node;
    JsonObject *member_object;

    member_node = json_object_get_member(object, member_name);
    if (member_node != NULL && JSON_NODE_HOLDS_VALUE (member_node)) {
        value = json_object_get_string_member (object, member_name);
        *description = g_strdup(value);
        if (url != NULL) {
            *url = NULL;
        }
    }
    else if (member_node != NULL && JSON_NODE_HOLDS_OBJECT (member_node)) {
        member_object = json_node_get_object(member_node);

        member_node = json_object_get_member(member_object, "description");
        if (member_node != NULL && JSON_NODE_HOLDS_VALUE (member_node)) {
            value = json_object_get_string_member (member_object, "description");
            *description = g_strdup(value);
        }
        else {
            g_warning("could not find description in %s\n", member_name);
            *description = NULL;
            if (url != NULL) {
                *url = NULL;
            }
        }
        if (url != NULL) {
            member_node = json_object_get_member(member_object, "url");
            if (member_node != NULL && JSON_NODE_HOLDS_VALUE (member_node)) {
                value = json_object_get_string_member (member_object, "url");
                *url = g_strdup(value);
            }
        }
    }
    else {
        g_message("could not find %s in json\n", member_name);
        *description = NULL;
        if (url != NULL) {
            *url = NULL;
        }
    }
}


void get_detail_from_object(JsonObject *object, const gchar *member_name, gchar **detail)
{
    const gchar *value;
    JsonNode *member_node;

    member_node = json_object_get_member(object, member_name);
    if (member_node != NULL && JSON_NODE_HOLDS_VALUE (member_node)) {
        value = json_object_get_string_member (object, member_name);
        *detail = g_strdup(value);
    }
    else {
        *detail = NULL;
    }
}

void kmp_languages_foreach (JsonArray *array,
                     guint index_,
                     JsonNode *element_node,
                     gpointer user_data)
{
    JsonObject *lang_object;
    kmp_keyboard *keyboard;
    kmp_language *language;

    if (JSON_NODE_HOLDS_OBJECT(element_node)){
        lang_object = json_node_get_object(element_node);
        keyboard = (kmp_keyboard *) user_data;
        language = g_new0(kmp_language, 1);

        get_detail_from_object(lang_object, "name",
            &(language->name));
        get_detail_from_object(lang_object, "id",
            &(language->id));
        keyboard->languages = g_list_append(keyboard->languages, language);
    }
}


void kmp_keyboards_foreach (JsonArray *array,
                     guint index_,
                     JsonNode *element_node,
                     gpointer user_data)
{
    JsonObject *keyboard_object;
    JsonNode *languages_node;
    GList *l;
    g_autofree gchar *l_kmx_file = NULL;
    g_autofree gchar *l_kvk_file = NULL;
    kmp_fileinfo *fileinfo;
    kmp_details *details;
    kmp_keyboard *keyboard;

    if (JSON_NODE_HOLDS_OBJECT(element_node)){
        keyboard_object = json_node_get_object(element_node);
        details = (kmp_details *) user_data;
        keyboard = g_new0(kmp_keyboard, 1);

        get_detail_from_object(keyboard_object, "name",
            &(keyboard->name));
        get_detail_from_object(keyboard_object, "id",
            &(keyboard->id));
        get_detail_from_object(keyboard_object, "version",
            &(keyboard->version));
        // languages is another array
        languages_node = json_object_get_member(keyboard_object, "languages");
        if (languages_node != NULL && JSON_NODE_HOLDS_ARRAY (languages_node)) {
            array = json_node_get_array (languages_node);
            if (array)
            {
                json_array_foreach_element (array,
                            kmp_languages_foreach,
                            keyboard);
            }
        }

        if (keyboard->id)
        {
            l_kmx_file = g_strdup_printf("%s.kmx", keyboard->id);
            l_kvk_file = g_strdup_printf("%s.kvk", keyboard->id);
            if (details->files != NULL) {
                for (l = details->files; l != NULL; l = l->next) {
                    fileinfo = (kmp_fileinfo *) l->data;
                    if (g_strcmp0(fileinfo->name, l_kmx_file)) {
                            keyboard->kmx_file = g_strdup(l_kmx_file);
                    }
                    else if (g_strcmp0(fileinfo->name, l_kvk_file)) {
                            keyboard->kvk_file = g_strdup(l_kvk_file);
                    }
                }
            }
        }
        details->keyboards = g_list_append(details->keyboards, keyboard);
    }
}

void kmp_files_foreach (JsonArray *array,
                     guint index_,
                     JsonNode *element_node,
                     gpointer user_data)
{
    JsonObject *file_object;
    kmp_details *details;
    kmp_fileinfo *fileinfo;

    if (JSON_NODE_HOLDS_OBJECT(element_node)){
        file_object = json_node_get_object(element_node);
        details = (kmp_details *) user_data;
        fileinfo = g_new0(kmp_fileinfo, 1);

        get_detail_from_object(file_object, "name",
            &(fileinfo->name));
        get_detail_from_object(file_object, "description",
            &(fileinfo->description));
        details->files = g_list_append(details->files, fileinfo);
    }
}

// Get kmp_details from `<kbd>.json`
kmp_json_status get_keyboard_details(const char *kmp_dir, const char *id, keyboard_details *keyboard)
{
    g_autoptr(JsonParser) parser;
    JsonObject *object;
    JsonNode *root;
    g_autofree gchar *json_file;
    g_autoptr(GError) error = NULL;

    parser = json_parser_new ();
    json_file = g_strjoin("/", kmp_dir, id, NULL);

    if (!g_file_test(json_file, G_FILE_TEST_EXISTS)) {
        return JSON_FILE_NOT_EXISTS;
    }

    json_parser_load_from_file (parser,
        json_file,
        &error);
    if (error)
    {
      g_warning ("Unable to parse `%s': %s\n", json_file, error->message);
      return JSON_PARSE_ERROR;
    }

    root = json_parser_get_root (parser);

    if (JSON_NODE_HOLDS_OBJECT(root)){
        object = json_node_get_object(root);
        get_detail_from_object(object, "id",
            &(keyboard->id));
        get_detail_from_object(object, "description",
            &(keyboard->description));
        get_detail_from_object(object, "license",
            &(keyboard->license));
    }
    return JSON_OK;
}

// Get kmp_details from `kmp.json` file
kmp_json_status get_kmp_details(const char *kmp_dir, kmp_details *details)
{
    g_autoptr(JsonParser) parser;
    JsonNode *root;
    JsonObject *root_object, *object;
    JsonArray *array;
    JsonNode *kmp_system, *kmp_info, *kmp_options, *kmp_files, *kmp_keyboards;
    g_autofree gchar *kmp_json;
    g_autoptr(GError) error = NULL;

    parser = json_parser_new ();
    kmp_json = g_strdup_printf("%s/kmp.json", kmp_dir);

    if (!g_file_test(kmp_json, G_FILE_TEST_EXISTS)) {
        return JSON_FILE_NOT_EXISTS;
    }

    json_parser_load_from_file (parser,
        kmp_json,
        &error);
    if (error)
    {
      g_warning ("Unable to parse `%s': %s\n", kmp_json, error->message);
      return JSON_PARSE_ERROR;
    }

    root = json_parser_get_root (parser);

    if (JSON_NODE_HOLDS_OBJECT(root)){
        root_object = json_node_get_object(root);
        kmp_system = json_object_get_member(root_object, "system");
        if (kmp_system)
        {
            object = json_node_get_object(kmp_system);
            get_detail_from_object(object, "keymanDeveloperVersion",
                &(details->system.keymanDeveloperVersion));
            get_detail_from_object(object, "fileVersion",
                &(details->system.fileVersion));
        }

        kmp_info = json_object_get_member(root_object, "info");
        if (kmp_info)
        {
            object = json_node_get_object(kmp_info);
            get_detail_description_from_object(object, "name",
                &(details->info.name), NULL);
            get_detail_description_from_object(object, "version",
                &(details->info.version), NULL);
            get_detail_description_from_object(object, "copyright",
                &(details->info.copyright), NULL);
            get_detail_description_from_object(object, "author",
                &(details->info.author_desc), &(details->info.author_url));
            get_detail_description_from_object(object, "website",
                &(details->info.website_desc), &(details->info.website_url));
        }

        kmp_options = json_object_get_member(root_object, "options");
        if (kmp_options)
        {
            object = json_node_get_object(kmp_options);
            get_detail_from_object(object, "readmeFile",
                &(details->options.readmeFile));
            get_detail_from_object(object, "graphicFile",
                &(details->options.graphicFile));
        }

        kmp_files = json_object_get_member(root_object, "files");
        details->files = NULL;
        if (kmp_files) {
            array = json_node_get_array (kmp_files);
            if (array)
            {
                json_array_foreach_element (array,
                            kmp_files_foreach,
                            details);
            }
        }

        kmp_keyboards = json_object_get_member(root_object, "keyboards");
        details->keyboards = NULL;
        if (kmp_keyboards) {
            array = json_node_get_array (kmp_keyboards);
            if (array)
            {
                json_array_foreach_element (array,
                            kmp_keyboards_foreach,
                            details);
            }
        }
    }
    return JSON_OK;
}

kmp_json_status print_kmp_details(kmp_details *details)
{
    GList *l, *p;
    kmp_keyboard *keyboard;
    kmp_language *language;
    kmp_fileinfo *fileinfo;
    int i, j;

    g_assert(details != NULL);
    g_print("system.fileVersion: %s\n", details->system.fileVersion);
    g_print("system.keymanDeveloperVersion: %s\n", details->system.keymanDeveloperVersion);
    g_print("options.readmeFile: %s\n", details->options.readmeFile);
    g_print("options.graphicFile: %s\n", details->options.graphicFile);
    g_print("info.name: %s\n", details->info.name);
    g_print("info.version: %s\n", details->info.version);
    g_print("info.copyright: %s\n", details->info.copyright);
    g_print("info.author_desc: %s\n", details->info.author_desc);
    g_print("info.author_url: %s\n", details->info.author_url);
    g_print("info.website_desc: %s\n", details->info.website_desc);
    g_print("info.website_url: %s\n", details->info.website_url);
    if (details->keyboards != NULL) {
        for (l = details->keyboards, i=0; l != NULL; l = l->next, i++) {
            keyboard = (kmp_keyboard *) l->data;
            g_print("keyboard.%d.name %s\n", i, keyboard->name);
            g_print("keyboard.%d.id %s\n", i, keyboard->id);
            g_print("keyboard.%d.version %s\n", i, keyboard->version);
            g_print("keyboard.%d.kmx_file %s\n", i, keyboard->kmx_file);
            g_print("keyboard.%d.kvk_file %s\n", i, keyboard->kvk_file);
            if (keyboard->languages != NULL) {
                for (p = keyboard->languages, j=0; p != NULL; p = p->next, j++) {
                    language = (kmp_language *) p->data;
                    g_print("keyboard.%d.lang%d.name %s\n", i, j, language->name);
                    g_print("keyboard.%d.lang%d.id %s\n", i, j, language->id);
                }
            }
        }
    }
    if (details->files != NULL) {
        for (l = details->files, i=0; l != NULL; l = l->next, i++) {
            fileinfo = (kmp_fileinfo *) l->data;
            g_print("file.%d.name %s\n", i, fileinfo->name);
            g_print("file.%d.description %s\n", i, fileinfo->description);
        }
    }
    return JSON_OK;
}

void free_language(gpointer data)
{
    kmp_language *language = (kmp_language *) data;
    g_free(language->name);
    g_free(language->id);
    g_free(language);
}

void free_keyboard(gpointer data)
{
    kmp_keyboard *keyboard = (kmp_keyboard *) data;
    g_free(keyboard->name);
    g_free(keyboard->id);
    g_free(keyboard->version);
    g_free(keyboard->kmx_file);
    g_free(keyboard->kvk_file);
    if (keyboard->languages != NULL) {
        g_list_free_full(keyboard->languages, (GDestroyNotify)free_language);
    }
    g_free(keyboard);
}

void free_fileinfo(gpointer data)
{
    kmp_fileinfo *fileinfo = (kmp_fileinfo *) data;
    g_free(fileinfo->name);
    g_free(fileinfo->description);
    g_free(fileinfo);
}

kmp_json_status free_keyboard_details(keyboard_details *kbd_details)
{
    g_free(kbd_details->id);
    g_free(kbd_details->description);
    g_free(kbd_details->license);
    g_free(kbd_details);
    return JSON_OK;
}

void free_info(gpointer data) {
  kmp_info *info = (kmp_info *)data;
  if (info->name)
    g_free(info->name);
  if (info->version)
    g_free(info->version);
  if (info->copyright)
    g_free(info->copyright);
  if (info->author_desc)
    g_free(info->author_desc);
  if (info->author_url)
    g_free(info->author_url);
  if (info->website_desc)
    g_free(info->website_desc);
  if (info->website_url)
    g_free(info->website_url);
}

kmp_json_status free_kmp_details(kmp_details * details)
{
    if (details == NULL)
      return JSON_OK;

    g_free(details->system.fileVersion);
    g_free(details->system.keymanDeveloperVersion);
    g_free(details->options.readmeFile);
    g_free(details->options.graphicFile);
    free_info(&details->info);
    if (details->keyboards != NULL) {
      g_list_free_full(details->keyboards, (GDestroyNotify)free_keyboard);
    }
    if (details->files != NULL) {
        g_list_free_full(details->files, (GDestroyNotify)free_fileinfo);
    }
    g_free(details);
    return JSON_OK;
}
