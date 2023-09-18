#include <glib-object.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <ibus.h>
#include "kmpdetails.h"
#include "keymanutil.h"
#include "keymanutil_internal.h"

#define TEST_FIXTURE "keymanutil-test"

void
delete_options_key(gchar* testname) {
  g_autofree gchar *path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_OPTIONS_PATH, TEST_FIXTURE, testname);
  g_autoptr(GSettings) settings = g_settings_new_with_path(KEYMAN_DCONF_OPTIONS_CHILD_NAME, path);
  g_settings_reset(settings, KEYMAN_DCONF_OPTIONS_KEY);
}

void
set_options_key(gchar* testname, gchar** options) {
  g_autofree gchar* path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_OPTIONS_PATH, TEST_FIXTURE, testname);
  g_autoptr(GSettings) settings = g_settings_new_with_path(KEYMAN_DCONF_OPTIONS_CHILD_NAME, path);
  g_settings_set_strv(settings, KEYMAN_DCONF_OPTIONS_KEY, (const gchar* const*)options);
}

gchar**
get_options_key(gchar* testname) {
  g_autofree gchar* path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_OPTIONS_PATH, TEST_FIXTURE, testname);
  g_autoptr(GSettings) settings = g_settings_new_with_path(KEYMAN_DCONF_OPTIONS_CHILD_NAME, path);
  gchar** result = g_settings_get_strv(settings, KEYMAN_DCONF_OPTIONS_KEY);
  return result;
}

kmp_keyboard*
_get_kmp_keyboard(gchar* version, gchar** languages) {
  kmp_keyboard* keyboard = g_new0(kmp_keyboard, 1);
  keyboard->name         = g_strdup("Testing");
  keyboard->id           = g_strdup("tst");
  keyboard->version      = g_strdup(version);
  keyboard->kmx_file     = g_strdup("tst.kmx");
  keyboard->kvk_file     = g_strdup("");
  keyboard->languages    = NULL;
  for (gchar* lang = *languages++; lang; lang = *languages++) {
    gchar** tokens         = g_strsplit(lang, ":", 2);
    kmp_language* kmp_lang = g_new0(kmp_language, 1);
    kmp_lang->id           = g_strdup(tokens[0]);
    kmp_lang->name         = g_strdup(tokens[1]);
    g_strfreev(tokens);
    keyboard->languages = g_list_append(keyboard->languages, kmp_lang);
  }
  return keyboard;
}

kmp_info*
_get_kmp_info(gchar * copyright, gchar * author_desc, gchar * author_url) {
  kmp_info* info    = g_new0(kmp_info, 1);
  info->copyright   = g_strdup(copyright);
  info->author_desc = g_strdup(author_desc);
  info->author_url  = g_strdup(author_url);
  return info;
}

keyboard_details*
_get_keyboard_details(gchar * description, gchar * license) {
  keyboard_details* details = g_new0(keyboard_details, 1);
  details->id               = g_strdup("tst");
  details->description      = g_strdup(description);
  details->license          = g_strdup(license);
  return details;
}

add_keyboard_data*
_get_keyboard_data() {
  add_keyboard_data* kb_data = g_new0(add_keyboard_data, 1);
  kb_data->engines_list      = NULL;
  kb_data->info              = _get_kmp_info(NULL, NULL, NULL);
  kb_data->kmp_dir           = "/tmp";
  return kb_data;
}

void
free_kb_data(add_keyboard_data* kb_data) {
  if (kb_data->engines_list)
    g_list_free(kb_data->engines_list);
  if (kb_data->info)
    g_free(kb_data->info);
  g_free(kb_data);
}

// defined in kmpdetails
void free_keyboard(gpointer data);
void free_info(gpointer data);
kmp_json_status free_keyboard_details(keyboard_details* kbd_details);

G_DEFINE_AUTOPTR_CLEANUP_FUNC(add_keyboard_data, free_kb_data)
G_DEFINE_AUTOPTR_CLEANUP_FUNC(kmp_keyboard, free_keyboard)
G_DEFINE_AUTOPTR_CLEANUP_FUNC(kmp_info, free_info)
G_DEFINE_AUTOPTR_CLEANUP_FUNC(keyboard_details, free_keyboard_details)
G_DEFINE_AUTOPTR_CLEANUP_FUNC(IBusEngineDesc, g_object_unref)

//----------------------------------------------------------------------------------------------
void
test_keyman_put_options_todconf__invalid() {
  // Initialize
  gchar* testname = "test_keyman_put_options_todconf__test_keyman_put_options_todconf__invalid";
  delete_options_key(testname);

  // Execute
  keyman_put_options_todconf(TEST_FIXTURE, testname, "new_key", NULL);

  // Verify
  g_auto(GStrv) options = get_options_key(testname);
  g_assert_nonnull(options);
  g_assert_null(options[0]);

  // Cleanup
  delete_options_key(testname);
}

void
test_keyman_put_options_todconf__new_key() {
  // Initialize
  gchar* testname = "test_keyman_put_options_todconf__new_key";
  delete_options_key(testname);
  g_autofree gchar* value = g_strdup_printf("%d", g_test_rand_int());

  // Execute
  keyman_put_options_todconf(TEST_FIXTURE, testname, "new_key", value);

  // Verify
  g_auto(GStrv) options = get_options_key(testname);
  g_autofree gchar* expected = g_strdup_printf("new_key=%s", value);
  g_assert_nonnull(options);
  g_assert_cmpstr(options[0], ==, expected);
  g_assert_null(options[1]);

  // Cleanup
  delete_options_key(testname);
}

void
test_keyman_put_options_todconf__other_keys() {
  // Initialize
  gchar* testname = "test_keyman_put_options_todconf__other_keys";
  delete_options_key(testname);
  gchar* existingKeys[] = {"key1=val1", "key2=val2", NULL};
  set_options_key(testname, existingKeys);
  g_autofree gchar* value = g_strdup_printf("%d", g_test_rand_int());

  // Execute
  keyman_put_options_todconf(TEST_FIXTURE, testname, "new_key", value);

  // Verify
  g_auto(GStrv) options = get_options_key(testname);
  g_autofree gchar* expected = g_strdup_printf("new_key=%s", value);
  g_assert_nonnull(options);
  g_assert_cmpstr(options[0], ==, "key1=val1");
  g_assert_cmpstr(options[1], ==, "key2=val2");
  g_assert_cmpstr(options[2], ==, expected);
  g_assert_null(options[3]);

  // Cleanup
  delete_options_key(testname);
}

void
test_keyman_put_options_todconf__existing_key() {
  // Initialize
  gchar* testname = "test_keyman_put_options_todconf__existing_key";
  delete_options_key(testname);
  gchar* existingKeys[] = {"key1=val1", "new_key=val2", NULL};
  set_options_key(testname, existingKeys);
  g_autofree gchar* value = g_strdup_printf("%d", g_test_rand_int());

  // Execute
  keyman_put_options_todconf(TEST_FIXTURE, testname, "new_key", value);

  // Verify
  g_auto(GStrv) options = get_options_key(testname);
  g_autofree gchar* expected = g_strdup_printf("new_key=%s", value);
  g_assert_nonnull(options);
  g_assert_cmpstr(options[0], ==, "key1=val1");
  g_assert_cmpstr(options[1], ==, expected);
  g_assert_null(options[2]);

  // Cleanup
  delete_options_key(testname);
}

//----------------------------------------------------------------------------------------------
void
test_ibus_keyman_engine_desc_new__all_set() {
  // Execute
  g_autoptr(IBusEngineDesc) desc = ibus_keyman_engine_desc_new("name", "longname", "description", "copyright", "lang", "license", "author", "icon", "layout", "version");

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "name");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "longname");
  g_assert_cmpstr(ibus_engine_desc_get_description(desc), ==, "description\ncopyright");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "lang");
  g_assert_cmpstr(ibus_engine_desc_get_license(desc), ==, "license");
  g_assert_cmpstr(ibus_engine_desc_get_author(desc), ==, "author");
  g_assert_cmpstr(ibus_engine_desc_get_icon(desc), ==, "icon");
  g_assert_cmpstr(ibus_engine_desc_get_layout(desc), ==, "layout");
  g_assert_cmpstr(ibus_engine_desc_get_version(desc), ==, "version");
}

void
test_ibus_keyman_engine_desc_new__only_description() {
  // Execute
  g_autoptr(IBusEngineDesc) desc = ibus_keyman_engine_desc_new(
      "name", "longname", "description", NULL, "lang", "license", "author", "icon", "layout", "version");

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "name");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "longname");
  g_assert_cmpstr(ibus_engine_desc_get_description(desc), ==, "description\n(null)");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "lang");
  g_assert_cmpstr(ibus_engine_desc_get_license(desc), ==, "license");
  g_assert_cmpstr(ibus_engine_desc_get_author(desc), ==, "author");
  g_assert_cmpstr(ibus_engine_desc_get_icon(desc), ==, "icon");
  g_assert_cmpstr(ibus_engine_desc_get_layout(desc), ==, "layout");
  g_assert_cmpstr(ibus_engine_desc_get_version(desc), ==, "version");
}

void
test_ibus_keyman_engine_desc_new__only_copyright() {
  // Execute
  g_autoptr(IBusEngineDesc) desc = ibus_keyman_engine_desc_new(
      "name", "longname", NULL, "copyright", "lang", "license", "author", "icon", "layout", "version");

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "name");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "longname");
  g_assert_cmpstr(ibus_engine_desc_get_description(desc), ==, "copyright");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "lang");
  g_assert_cmpstr(ibus_engine_desc_get_license(desc), ==, "license");
  g_assert_cmpstr(ibus_engine_desc_get_author(desc), ==, "author");
  g_assert_cmpstr(ibus_engine_desc_get_icon(desc), ==, "icon");
  g_assert_cmpstr(ibus_engine_desc_get_layout(desc), ==, "layout");
  g_assert_cmpstr(ibus_engine_desc_get_version(desc), ==, "version");
}

void
test_ibus_keyman_engine_desc_new__no_language_license_author_version() {
  // Execute
  g_autoptr(IBusEngineDesc) desc = ibus_keyman_engine_desc_new(
      "name", "longname", "description", "copyright", NULL, NULL, NULL, "icon", "layout", NULL);

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "name");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "longname");
  g_assert_cmpstr(ibus_engine_desc_get_description(desc), ==, "description\ncopyright");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "other");
  g_assert_cmpstr(ibus_engine_desc_get_license(desc), ==, "");
  g_assert_cmpstr(ibus_engine_desc_get_author(desc), ==, "");
  g_assert_cmpstr(ibus_engine_desc_get_icon(desc), ==, "icon");
  g_assert_cmpstr(ibus_engine_desc_get_layout(desc), ==, "layout");
  g_assert_cmpstr(ibus_engine_desc_get_version(desc), ==, "");
}

//----------------------------------------------------------------------------------------------
void
test_get_engine_for_language__null_language() {
  // Initialize
  gchar* languages[]        = {"en:English", NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_kmp_keyboard("1.0", languages);
  g_autoptr(kmp_info) info            = _get_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_keyboard_details("my description", "MIT");

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", NULL, NULL);

  // Verify
  g_assert_null(desc);
}

void
test_get_engine_for_language__empty_language() {
  // Initialize
  gchar* languages[]        = {"en:English", NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_kmp_keyboard("1.0", languages);
  g_autoptr(kmp_info) info            = _get_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_keyboard_details("my description", "MIT");

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", "", "");

  // Verify
  g_assert_null(desc);
}

void
test_get_engine_for_language__one_language() {
  // Initialize
  gchar* languages[]        = {"en:English", NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_kmp_keyboard("1.0", languages);
  g_autoptr(kmp_info) info            = _get_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_keyboard_details("my description", "MIT");

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", "en", "English");

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "en:/tmp/tst.kmx");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "Testing");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "en");
}

void
test_get_engine_for_language__one_unknown_language() {
  // Initialize
  gchar* languages[]        = {"foo:Foo", NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_kmp_keyboard("1.0", languages);
  g_autoptr(kmp_info) info            = _get_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_keyboard_details("my description", "MIT");

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", "foo", "Foo");

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "foo:/tmp/tst.kmx");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "Testing - Foo");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "foo");
}

void
test_get_engine_for_language__different_languages() {
  // Initialize
  gchar* languages[]        = {"en:English", NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_kmp_keyboard("1.0", languages);
  g_autoptr(kmp_info) info            = _get_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_keyboard_details("my description", "MIT");

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", "foo", "Foo");

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "foo:/tmp/tst.kmx");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "Testing - Foo");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "foo");
}

void
test_get_engine_for_language__no_kbd_language() {
  // Initialize
  gchar* languages[]        = {NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_kmp_keyboard("1.0", languages);
  g_autoptr(kmp_info) info            = _get_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_keyboard_details("my description", "MIT");

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", "en", "English");

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "en:/tmp/tst.kmx");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "Testing");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "en");
}

//----------------------------------------------------------------------------------------------
void
test_keyman_add_keyboard__no_language() {
  // Initialize
  gchar* languages[]         = {NULL};
  g_autoptr(kmp_keyboard) keyboard     = _get_kmp_keyboard("1.0", languages);
  g_autoptr(add_keyboard_data) kb_data = _get_keyboard_data();

  // Execute
  keyman_add_keyboard(keyboard, kb_data);

  // Verify
  g_assert_nonnull(kb_data->engines_list->data);
  IBusEngineDesc* desc = (IBusEngineDesc*)kb_data->engines_list->data;
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "/tmp/tst.kmx");
  g_assert_null(kb_data->engines_list->next);
}

void
test_keyman_add_keyboard__one_language() {
  // Initialize
  gchar* languages[]         = {"en:English", NULL};
  g_autoptr(kmp_keyboard) keyboard     = _get_kmp_keyboard("1.0", languages);
  g_autoptr(add_keyboard_data) kb_data = _get_keyboard_data();

  // Execute
  keyman_add_keyboard(keyboard, kb_data);

  // Verify
  g_assert_nonnull(kb_data->engines_list->data);
  IBusEngineDesc* desc = (IBusEngineDesc*)kb_data->engines_list->data;
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "en:/tmp/tst.kmx");
  g_assert_null(kb_data->engines_list->next);
}

void
test_keyman_add_keyboard__two_languages() {
  // Initialize
  gchar* languages[]         = {"en:English", "fr:French", NULL};
  g_autoptr(kmp_keyboard) keyboard     = _get_kmp_keyboard("1.0", languages);
  g_autoptr(add_keyboard_data) kb_data = _get_keyboard_data();

  // Execute
  keyman_add_keyboard(keyboard, kb_data);

  // Verify
  g_assert_nonnull(kb_data->engines_list->data);
  IBusEngineDesc* desc = (IBusEngineDesc*)kb_data->engines_list->data;
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "en:/tmp/tst.kmx");
  g_assert_nonnull(kb_data->engines_list->next);
  g_assert_nonnull(kb_data->engines_list->next->data);
  desc = (IBusEngineDesc*)kb_data->engines_list->next->data;
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "fr:/tmp/tst.kmx");
  g_assert_null(kb_data->engines_list->next->next);
}

void
test_keyman_add_keyboard__prev_engine_adding_same_version() {
  // Initialize
  gchar* languages[]         = {"en:English", NULL};
  g_autoptr(kmp_keyboard) keyboard     = _get_kmp_keyboard("1.0", languages);
  g_autoptr(add_keyboard_data) kb_data = _get_keyboard_data();
  IBusEngineDesc* desc = ibus_keyman_engine_desc_new("en:/usr/share/keyman/tst.kmx", "Testing", NULL, NULL, "en", NULL, NULL, "", "us", "1.0");
  kb_data->engines_list = g_list_append(kb_data->engines_list, desc);

  // Execute
  keyman_add_keyboard(keyboard, kb_data);

  // Verify
  GList* list = kb_data->engines_list;
  g_assert_nonnull(list->data);
  g_assert_cmpstr(ibus_engine_desc_get_name((IBusEngineDesc*)list->data), ==, "en:/usr/share/keyman/tst.kmx");
  g_assert_null(list->next);
}

void
test_keyman_add_keyboard__prev_engine_adding_newer_version() {
  // Initialize
  gchar* languages[]         = {"en:English", "fr:French", NULL};  // New version adds French
  g_autoptr(kmp_keyboard) keyboard     = _get_kmp_keyboard("1.1", languages);
  g_autoptr(add_keyboard_data) kb_data = _get_keyboard_data();
  IBusEngineDesc* desc =
      ibus_keyman_engine_desc_new("en:/usr/share/keyman/tst.kmx", "Testing", NULL, NULL, "en", NULL, NULL, "", "us", "1.0");
  kb_data->engines_list = g_list_append(kb_data->engines_list, desc);

  // Execute
  keyman_add_keyboard(keyboard, kb_data);

  // Verify
  GList* list = kb_data->engines_list;
  g_assert_nonnull(list->data);
  g_assert_cmpstr(ibus_engine_desc_get_name((IBusEngineDesc*)list->data), ==, "en:/usr/share/keyman/tst.kmx");
  g_assert_nonnull(list->next);
  list = list->next;
  g_assert_nonnull(list->data);
  g_assert_cmpstr(ibus_engine_desc_get_name((IBusEngineDesc*)list->data), ==, "en:/tmp/tst.kmx");
  g_assert_nonnull(list->next);
  list = list->next;
  g_assert_nonnull(list->data);
  g_assert_cmpstr(ibus_engine_desc_get_name((IBusEngineDesc*)list->data), ==, "fr:/tmp/tst.kmx");
  g_assert_null(list->next);
}

void
test_keyman_add_keyboard__prev_engine_adding_newer_version_9593() {
  // This tests bug #9593: We add keyboard version 1.10 while 1.9 is already in
  // the list.

  // Initialize
  gchar* languages[]         = {"en:English", "fr:French", NULL};  // New version adds French
  g_autoptr(kmp_keyboard) keyboard     = _get_kmp_keyboard("1.10", languages);
  g_autoptr(add_keyboard_data) kb_data = _get_keyboard_data();
  IBusEngineDesc* desc =
      ibus_keyman_engine_desc_new("en:/usr/share/keyman/tst.kmx", "Testing", NULL, NULL, "en", NULL, NULL, "", "us", "1.9");
  kb_data->engines_list = g_list_append(kb_data->engines_list, desc);

  // Execute
  keyman_add_keyboard(keyboard, kb_data);

  // Verify
  GList* list = kb_data->engines_list;
  g_assert_nonnull(list->data);
  g_assert_cmpstr(ibus_engine_desc_get_name((IBusEngineDesc*)list->data), ==, "en:/usr/share/keyman/tst.kmx");
  g_assert_nonnull(list->next);
  list = list->next;
  g_assert_nonnull(list->data);
  g_assert_cmpstr(ibus_engine_desc_get_name((IBusEngineDesc*)list->data), ==, "en:/tmp/tst.kmx");
  g_assert_nonnull(list->next);
  list = list->next;
  g_assert_nonnull(list->data);
  g_assert_cmpstr(ibus_engine_desc_get_name((IBusEngineDesc*)list->data), ==, "fr:/tmp/tst.kmx");
  g_assert_null(list->next);
}

void
test_keyman_add_keyboard__prev_engine_adding_older_version() {
  // Initialize
  gchar* languages[]         = {"en:English", "fr:French", NULL};  // Old version has additional French
  g_autoptr(kmp_keyboard) keyboard     = _get_kmp_keyboard("0.9", languages);
  g_autoptr(add_keyboard_data) kb_data = _get_keyboard_data();
  IBusEngineDesc* desc =
      ibus_keyman_engine_desc_new("en:/usr/share/keyman/tst.kmx", "Testing", NULL, NULL, "en", NULL, NULL, "", "us", "1.0");
  kb_data->engines_list = g_list_append(kb_data->engines_list, desc);

  // Execute
  keyman_add_keyboard(keyboard, kb_data);

  // Verify
  GList* list = kb_data->engines_list;
  g_assert_nonnull(list->data);
  g_assert_cmpstr(ibus_engine_desc_get_name((IBusEngineDesc*)list->data), ==, "en:/usr/share/keyman/tst.kmx");
  g_assert_null(list->next);
}

//----------------------------------------------------------------------------------------------
gchar* testdata_dir;

gboolean
delete_directory(gchar* path) {
  g_autoptr(GFile) file = g_file_new_for_path(path);
  if (g_file_test(path, G_FILE_TEST_IS_DIR)) {
    g_autoptr(GFileEnumerator) enumerator = NULL;

    enumerator = g_file_enumerate_children(file, G_FILE_ATTRIBUTE_STANDARD_NAME, G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, NULL, NULL);

    while (enumerator != NULL) {
      GFile* child;

      if (!g_file_enumerator_iterate(enumerator, NULL, &child, NULL, NULL))
        return FALSE;
      if (child == NULL)
        break;
      if (!delete_directory(g_file_get_path(child)))
        return FALSE;
    }
  }

  return g_file_delete(file, NULL, NULL);
}

void clear_kmpdir(gchar* dir) {
  delete_directory(dir);
  g_free(dir);
}

G_DEFINE_AUTOPTR_CLEANUP_FUNC(gchar, clear_kmpdir);

void copy_test_data(gchar* kmp_dir, gchar* name) {
  g_autofree gchar* oldfile = g_strdup_printf("%s/%s", testdata_dir, name);
  g_autoptr(GFile) source   = g_file_new_for_path(oldfile);
  g_autofree gchar* newfile = g_strdup_printf("%s/kmp.json", kmp_dir);
  g_autoptr(GFile) dest     = g_file_new_for_path(newfile);
  g_file_copy(source, dest, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, NULL);
}

void
test_keyman_add_keyboards_from_dir__no_kmpjson() {
  // Initialize
  g_autolist(GList) keyboards = NULL;
  g_autoptr(gchar) kmp_dir    = g_dir_make_tmp(NULL, NULL);

  // Execute
  keyman_add_keyboards_from_dir(kmp_dir, &keyboards);

  // Verify
  g_assert_cmpint(g_list_length(keyboards), ==, 0);
}

void
test_keyman_add_keyboards_from_dir__one_dir() {
  // Initialize
  g_autoptr(GList) keyboards            = NULL;
  g_autoptr(gchar) kmp_dir    = g_dir_make_tmp(NULL, NULL);
  copy_test_data(kmp_dir, "kmp1.json");

  // Execute
  keyman_add_keyboards_from_dir(kmp_dir, &keyboards);

  // Verify
  g_assert_cmpint(g_list_length(keyboards), ==, 1);

  IBusEngineDesc* desc = IBUS_ENGINE_DESC(keyboards->data);
  g_autofree gchar* expected_name = g_strdup_printf("bza:%s/test1.kmx", kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name);
}

void
test_keyman_add_keyboards_from_dir__two_langs() {
  // Initialize
  g_autoptr(GList) keyboards = NULL;
  g_autoptr(gchar) kmp_dir  = g_dir_make_tmp(NULL, NULL);
  copy_test_data(kmp_dir, "kmp2.json");

  // Execute
  keyman_add_keyboards_from_dir(kmp_dir, &keyboards);

  // Verify
  g_assert_cmpint(g_list_length(keyboards), ==, 2);

  IBusEngineDesc* desc = IBUS_ENGINE_DESC(keyboards->data);
  g_autofree gchar* expected_name1 = g_strdup_printf("bmf-Latn:%s/test2.kmx", kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name1);
  desc = IBUS_ENGINE_DESC(keyboards->next->data);
  g_autofree gchar* expected_name2 = g_strdup_printf("bun-Latn:%s/test2.kmx", kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name2);
}

void
test_keyman_add_keyboards_from_dir__prev_keyboards() {
  // Initialize
  g_autoptr(GList) keyboards = NULL;
  g_autoptr(gchar) kmp_dir  = g_dir_make_tmp(NULL, NULL);
  copy_test_data(kmp_dir, "kmp2.json");
  keyman_add_keyboards_from_dir(kmp_dir, &keyboards);
  copy_test_data(kmp_dir, "kmp1.json");

  // Execute
  keyman_add_keyboards_from_dir(kmp_dir, &keyboards);

  // Verify
  g_assert_cmpint(g_list_length(keyboards), ==, 3);

  IBusEngineDesc* desc = IBUS_ENGINE_DESC(keyboards->data);
  g_autofree gchar* expected_name1 = g_strdup_printf("bmf-Latn:%s/test2.kmx", kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name1);
  desc = IBUS_ENGINE_DESC(keyboards->next->data);
  g_autofree gchar* expected_name2 = g_strdup_printf("bun-Latn:%s/test2.kmx", kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name2);
  desc = IBUS_ENGINE_DESC(keyboards->next->next->data);
  g_autofree gchar* expected_name = g_strdup_printf("bza:%s/test1.kmx", kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name);
}

//----------------------------------------------------------------------------------------------
void
print_usage() {
  printf(
      "Usage: %s --testdata <path/to/testdata>\n\n",
      g_get_prgname());
  printf("Arguments:\n");
  printf("\t--testdata <path/to/testdata>\tThe directory containing test kmp.json files for the tests.\n\n");
}

int main(int argc, char* argv[]) {
  gtk_init(&argc, &argv);
  g_test_init(&argc, &argv, NULL);
  g_test_set_nonfatal_assertions();

  if (argc < 3 || strcmp(argv[1], "--testdata") != 0) {
    print_usage();
    return 1;
  }

  testdata_dir = argv[2];

  if (!g_file_test(testdata_dir, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_DIR)) {
    printf("ERROR: Testdata directory %s does not exist\n\n", testdata_dir);
    print_usage();
    return 2;
  }

  // Add tests
  g_test_add_func("/keymanutil/keyman_put_options_todconf/invalid", test_keyman_put_options_todconf__invalid);
  g_test_add_func("/keymanutil/keyman_put_options_todconf/new_key", test_keyman_put_options_todconf__new_key);
  g_test_add_func("/keymanutil/keyman_put_options_todconf/other_keys", test_keyman_put_options_todconf__other_keys);
  g_test_add_func("/keymanutil/keyman_put_options_todconf/existing_key", test_keyman_put_options_todconf__existing_key);

  g_test_add_func("/keymanutil/ibus_keyman_engine_desc_new/all_set", test_ibus_keyman_engine_desc_new__all_set);
  g_test_add_func("/keymanutil/ibus_keyman_engine_desc_new/only_description", test_ibus_keyman_engine_desc_new__only_description);
  g_test_add_func("/keymanutil/ibus_keyman_engine_desc_new/only_copyright", test_ibus_keyman_engine_desc_new__only_copyright);
  g_test_add_func(
      "/keymanutil/ibus_keyman_engine_desc_new/no_language_license_author_version",
      test_ibus_keyman_engine_desc_new__no_language_license_author_version);

  g_test_add_func("/keymanutil/get_engine_for_language/null_language", test_get_engine_for_language__null_language);
  g_test_add_func("/keymanutil/get_engine_for_language/empty_language", test_get_engine_for_language__empty_language);
  g_test_add_func("/keymanutil/get_engine_for_language/one_language", test_get_engine_for_language__one_language);
  g_test_add_func("/keymanutil/get_engine_for_language/one_unknown_language", test_get_engine_for_language__one_unknown_language);
  g_test_add_func("/keymanutil/get_engine_for_language/different_languages", test_get_engine_for_language__different_languages);
  g_test_add_func("/keymanutil/get_engine_for_language/no_kbd_language", test_get_engine_for_language__no_kbd_language);

  g_test_add_func("/keymanutil/keyman_add_keyboard/no_language", test_keyman_add_keyboard__no_language);
  g_test_add_func("/keymanutil/keyman_add_keyboard/one_language", test_keyman_add_keyboard__one_language);
  g_test_add_func("/keymanutil/keyman_add_keyboard/two_languages", test_keyman_add_keyboard__two_languages);
  g_test_add_func(
      "/keymanutil/keyman_add_keyboard/prev_engine_adding_same_version",
      test_keyman_add_keyboard__prev_engine_adding_same_version);
  g_test_add_func(
      "/keymanutil/keyman_add_keyboard/prev_engine_adding_newer_version",
      test_keyman_add_keyboard__prev_engine_adding_newer_version);
  // #9593
  // g_test_add_func(
  //     "/keymanutil/keyman_add_keyboard/prev_engine_adding_newer_version_versioncompare",
  //     test_keyman_add_keyboard__prev_engine_adding_newer_version_9593);
  g_test_add_func(
      "/keymanutil/keyman_add_keyboard/prev_engine_adding_older_version",
      test_keyman_add_keyboard__prev_engine_adding_older_version);

  g_test_add_func("/keymanutil/keyman_add_keyboards_from_dir/no_kmpjson", test_keyman_add_keyboards_from_dir__no_kmpjson);
  g_test_add_func("/keymanutil/keyman_add_keyboards_from_dir/one_dir", test_keyman_add_keyboards_from_dir__one_dir);
  g_test_add_func("/keymanutil/keyman_add_keyboards_from_dir/two_langs", test_keyman_add_keyboards_from_dir__two_langs);
  g_test_add_func("/keymanutil/keyman_add_keyboards_from_dir/prev_keyboards", test_keyman_add_keyboards_from_dir__prev_keyboards);

  // Run tests
  int retVal = g_test_run();

  return retVal;
}
