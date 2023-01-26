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


void node_holds(JsonNode *node, const gchar *name)
{
    if (JSON_NODE_HOLDS_OBJECT(node)){
        g_print("%s holds object\n", name);
    }

    if (JSON_NODE_HOLDS_ARRAY(node)){
        g_print("%s holds array\n", name);
    }

    if (JSON_NODE_HOLDS_VALUE(node)){
        g_print("%s holds value\n", name);
    }

    if (JSON_NODE_HOLDS_NULL(node)){
        g_print("%s holds null\n", name);
    }
}

void print_object(JsonObject *object, const gchar *objname)
{
    GList *members, *l;
    const gchar *value = NULL;
    gchar *modname;
    JsonNode *member_node;
    JsonObject *member_object;

    g_print("d");
    members = json_object_get_members (object);
    for (l = members; l != NULL; l = l->next)
    {
        gchar *member_name = (gchar *) l->data;
        g_print("(member)%s\n", member_name);
        member_node = json_object_get_member(object, member_name);
        if (member_node != NULL && g_strcmp0(objname, "root") != 0)
        {
            if (JSON_NODE_HOLDS_VALUE (member_node))
            {
                value = json_object_get_string_member (object, member_name);
                g_print("  %s:%s\n", member_name, value);
            }
            else
            {
                g_print("  __%s\n", member_name);
                if (JSON_NODE_HOLDS_OBJECT (member_node))
                {
                    member_object = json_node_get_object(member_node);
                    modname = g_strdup_printf("  %s", member_name);

                    g_print("(mod)%s\n", modname);
                    print_object(member_object, modname);
                    g_free(modname);
                }
                else
                {
                    g_print("(member node no object)");
                }
            }
        }
        else
        {
            if (g_strcmp0(objname, "root") != 0)
            {
                g_print("(!root)");
            }
            if (member_node == NULL)
            {
                g_print("(tip)");
            }
            g_print("x  %s\n", member_name);
        }
    }
    g_list_free(members);
    g_print("z");
}

void print_node(JsonNode *node, const gchar *name)
{
    JsonObject *object;

    g_print("\n(node)%s:\n", name);
    object = json_node_get_object(node);
    print_object(object, name);
}

void kmp_files_foreach (JsonArray *array,
                     guint index_,
                     JsonNode *element_node,
                     gpointer user_data)
{
    // JsonObject *file_object;
    // g_print("kmp_files_foreach:");
    // node_holds(element_node, "files array member");
    gchar *name = g_strdup_printf("File %d", index_);
    print_node(element_node, name);
    g_free(name);
}

void kmp_keyboards_foreach (JsonArray *array,
                     guint index_,
                     JsonNode *element_node,
                     gpointer user_data)
{
    // JsonObject *file_object;
    // g_print("kmp_files_foreach:");
    // node_holds(element_node, "files array member");
    gchar *name = g_strdup_printf("Keyboard %d", index_);
    print_node(element_node, name);
    g_free(name);
}



int
main (gint argc, gchar **argv)
{
    struct stat filestat;
    JsonParser *parser;
    JsonNode *root;
    JsonObject *object;
    JsonArray *array;
    JsonNode *kmp_system, *kmp_info, *kmp_options, *kmp_files, *kmp_keyboards;
    GError *error = NULL;
    if (argc < 2)
    {
      g_print ("Usage: kmpdetails <kmp.json>\n");
      return EXIT_FAILURE;
    }

    stat(argv[1], &filestat);
    if (!S_ISREG(filestat.st_mode))
    {
      g_print ("Usage: kmpdetails <kmp.json>\n");
      return EXIT_FAILURE;
    }

    setlocale (LC_ALL, "C.UTF-8");


            // // Only .kmx extensions are valid keyboard files
            // else if (S_ISREG(filestat.st_mode)
            //     && (g_str_has_suffix(absfn, ".kmx")/* && keyman_check_keyboard(absfn) == 0*/))
            // {
            //     keyboard_list=g_list_append(keyboard_list, absfn);
            // }
            // else
            // {
            //     g_free(absfn);
            // }

    parser = json_parser_new ();

    json_parser_load_from_file (parser,
        argv[1],
        &error);
    if (error)
    {
      g_print ("Unable to parse `%s': %s\n", argv[1], error->message);
      g_error_free (error);
      g_object_unref (parser);
      return EXIT_FAILURE;
    }

    root = json_parser_get_root (parser);
    node_holds(root, "root");

    if (JSON_NODE_HOLDS_OBJECT(root)){
        print_node(root, "root");
        object = json_node_get_object(root);

        kmp_system    = json_object_get_member(object, "system");
        if (kmp_system)
        {
            print_node(kmp_system, "system");
        }
        kmp_info      = json_object_get_member(object, "info");
        if (kmp_info)
        {
            print_node(kmp_info, "info");
        }
        kmp_options   = json_object_get_member(object, "options");
        if (kmp_options)
        {
            print_node(kmp_options, "options");
        }
        kmp_files     = json_object_get_member(object, "files");
        if (kmp_files)
        {
            g_print("\nfiles:\n");
            // node_holds(kmp_files, "files");
            array = json_node_get_array (kmp_files);
            if (array)
            {
                g_print("files array:\n");
                json_array_foreach_element (array,
                            kmp_files_foreach,
                            NULL);
            }
        }
        kmp_keyboards = json_object_get_member(object, "keyboards");
        if (kmp_keyboards)
        {
            g_print("\nkeyboards:\n");
            // node_holds(kmp_keyboards, "keyboards");
            array = json_node_get_array (kmp_keyboards);
            if (array)
            {
                g_print("keyboards array:\n");
                json_array_foreach_element (array,
                            kmp_keyboards_foreach,
                            NULL);
            }
        }
    }

    // reader = json_reader_new (root);

    // json_reader_read_member (reader, "system");
    // json_reader_read_member (reader, "fileVersion");

    // g_object_unref(reader);
    g_object_unref(parser);
    return EXIT_SUCCESS;
}

