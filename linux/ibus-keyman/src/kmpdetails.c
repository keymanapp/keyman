// APIs to get info that ibus-keyman will want from
// the kmp.json from each kmp
//
// get list of keyboards from this
// associated languages

#include <locale.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
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
        //g_warning("could not find %s in json\n", member_name);
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

void kmp_keyboards_foreach (JsonArray *array,
                     guint index_,
                     JsonNode *element_node,
                     gpointer user_data)
{
    JsonObject *keyboard_object;
    kmp_details *details = (kmp_details *) user_data;
    kmp_keyboard *keyboard = g_new(kmp_keyboard, 1);

    if (JSON_NODE_HOLDS_OBJECT(element_node)){
        keyboard_object = json_node_get_object(element_node);
        get_detail_from_object(keyboard_object, "name",
            &(keyboard->name));
        get_detail_from_object(keyboard_object, "id",
            &(keyboard->id));
        get_detail_from_object(keyboard_object, "version",
            &(keyboard->version));
        details->keyboards = g_list_append(details->keyboards, keyboard);
        // languages?
        // is another array
    }
}

void kmp_files_foreach (JsonArray *array,
                     guint index_,
                     JsonNode *element_node,
                     gpointer user_data)
{
    JsonObject *file_object;
    kmp_details *details = (kmp_details *) user_data;
    kmp_fileinfo *fileinfo = g_new(kmp_fileinfo, 1);

    if (JSON_NODE_HOLDS_OBJECT(element_node)){
        file_object = json_node_get_object(element_node);
        get_detail_from_object(file_object, "name",
            &(fileinfo->name));
        get_detail_from_object(file_object, "description",
            &(fileinfo->description));
        details->files = g_list_append(details->files, fileinfo);
    }
}


void get_kmp_details(const char *kmp_json, kmp_details *details)
{
    JsonParser *parser;
    JsonNode *root, *member_node;
    JsonObject *root_object, *object;
    JsonArray *array;
    JsonNode *kmp_system, *kmp_info, *kmp_options, *kmp_files, *kmp_keyboards;
    const gchar *value = NULL;
    GError *error = NULL;

    parser = json_parser_new ();

    json_parser_load_from_file (parser,
        kmp_json,
        &error);
    if (error)
    {
      g_warning ("Unable to parse `%s': %s\n", kmp_json, error->message);
      g_error_free (error);
      g_object_unref (parser);
      return;
    }

    root = json_parser_get_root (parser);

    if (JSON_NODE_HOLDS_OBJECT(root)){
        root_object = json_node_get_object(root);
        kmp_system = json_object_get_member(root_object, "system");
        if (kmp_system)
        {
            g_print("have system\n");
            object = json_node_get_object(kmp_system);
            get_detail_from_object(object, "keymanDeveloperVersion",
                &(details->system.keymanDeveloperVersion));
            get_detail_from_object(object, "fileVersion",
                &(details->system.fileVersion));
        }
        kmp_info = json_object_get_member(root_object, "info");
        if (kmp_info)
        {
            g_print("have info\n");
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
            g_print("have options\n");
            object = json_node_get_object(kmp_options);
            get_detail_from_object(object, "readmeFile",
                &(details->options.readmeFile));
            get_detail_from_object(object, "graphicFile",
                &(details->options.graphicFile));
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
    }
    g_object_unref(parser);
}

void print_kmp_details(kmp_details *details)
{
    GList *l;
    kmp_keyboard *keyboard;
    kmp_fileinfo *fileinfo;
    int i;

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
        g_print("got at least one keyboard\n");
        for (l = details->keyboards, i=0; l != NULL; l = l->next, i++) {
            keyboard = (kmp_keyboard *) l->data;
            g_print("keyboard.%d.name %s\n", i, keyboard->name);
            g_print("keyboard.%d.id %s\n", i, keyboard->id);
            g_print("keyboard.%d.version %s\n", i, keyboard->version);
        }
    }
    if (details->files != NULL) {
        g_print("got at least one file\n");
        for (l = details->files, i=0; l != NULL; l = l->next, i++) {
            fileinfo = (kmp_fileinfo *) l->data;
            g_print("file.%d.name %s\n", i, fileinfo->name);
            g_print("file.%d.description %s\n", i, fileinfo->description);
        }
    }
}

void free_keyboard(gpointer data)
{
    kmp_keyboard *keyboard = (kmp_keyboard *) data;
    g_free(keyboard->name);
    g_free(keyboard->id);
    g_free(keyboard->version);
}

void free_fileinfo(gpointer data)
{
    kmp_fileinfo *fileinfo = (kmp_fileinfo *) data;
    g_free(fileinfo->name);
    g_free(fileinfo->description);
}

void free_kmp_details(kmp_details * details)
{
    g_assert(details != NULL);
    g_free(details->system.fileVersion);
    g_free(details->system.keymanDeveloperVersion);
    g_free(details->options.readmeFile);
    g_free(details->options.graphicFile);
    g_free(details->info.name);
    g_free(details->info.version);
    g_free(details->info.copyright);
    g_free(details->info.author_desc);
    g_free(details->info.author_url);
    g_free(details->info.website_desc);
    g_free(details->info.website_url);
    if (details->keyboards != NULL) {
        g_list_free_full(details->keyboards, (GDestroyNotify)free_keyboard);
    }
    if (details->files != NULL) {
        g_list_free_full(details->files, (GDestroyNotify)free_fileinfo);
    }
}
