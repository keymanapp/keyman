#include <glib-object.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <ibus.h>
#include <unicode/uvernum.h>
#include "kmpdetails.h"
#include "keymanutil.h"
#include "keymanutil_internal.h"

#define TEST_FIXTURE "keymanutil-test"

void
_delete_tst_keyboard_options_key(const gchar* testname) {
  g_autofree gchar *path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_OPTIONS_PATH, TEST_FIXTURE, testname);
  g_autoptr(GSettings) settings = g_settings_new_with_path(KEYMAN_DCONF_OPTIONS_CHILD_NAME, path);
  g_settings_reset(settings, KEYMAN_DCONF_OPTIONS_KEY);
}

void
_set_tst_keyboard_options_key(const gchar* testname, const gchar* const* options) {
  g_autofree gchar* path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_OPTIONS_PATH, TEST_FIXTURE, testname);
  g_autoptr(GSettings) settings = g_settings_new_with_path(KEYMAN_DCONF_OPTIONS_CHILD_NAME, path);
  g_settings_set_strv(settings, KEYMAN_DCONF_OPTIONS_KEY, options);
}

gchar**
_get_tst_keyboard_options_key(const gchar* testname) {
  g_autofree gchar* path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_OPTIONS_PATH, TEST_FIXTURE, testname);
  g_autoptr(GSettings) settings = g_settings_new_with_path(KEYMAN_DCONF_OPTIONS_CHILD_NAME, path);
  gchar** result = g_settings_get_strv(settings, KEYMAN_DCONF_OPTIONS_KEY);
  return result;
}

void _reset_tst_option(const gchar* key) {
  g_autoptr(GSettings) settings = g_settings_new(KEYMAN_DCONF_OPTIONS_NAME);
  g_settings_reset(settings, key);
}

void _set_tst_option(const gchar* key, gboolean value) {
  g_autoptr(GSettings) settings = g_settings_new(KEYMAN_DCONF_OPTIONS_NAME);
  g_settings_set_boolean(settings, key, value);
}

gboolean _get_tst_option(const gchar* key) {
  g_autoptr(GSettings) settings = g_settings_new(KEYMAN_DCONF_OPTIONS_NAME);
  return g_settings_get_boolean(settings, key);
}

void
_delete_tst_kbds_key() {
  g_autoptr(GSettings) settings = g_settings_new(KEYMAN_DCONF_ENGINE_NAME);
  g_settings_reset(settings, KEYMAN_DCONF_KEYBOARDS_KEY);
}

void
_set_tst_kbds_key(gchar** keyboards) {
  g_autoptr(GSettings) settings = g_settings_new(KEYMAN_DCONF_ENGINE_NAME);
  g_settings_set_strv(settings, KEYMAN_DCONF_KEYBOARDS_KEY, (const gchar* const*)keyboards);
}

gchar**
_get_tst_kbds_key() {
  g_autoptr(GSettings) settings = g_settings_new(KEYMAN_DCONF_ENGINE_NAME);
  return g_settings_get_strv(settings, KEYMAN_DCONF_KEYBOARDS_KEY);
}

kmp_keyboard*
_get_tst_kmp_keyboard(gchar* version, gchar** languages, gchar* id) {
  g_assert(id != NULL);
  kmp_keyboard* keyboard = g_new0(kmp_keyboard, 1);
  keyboard->name         = g_strdup("Testing");
  keyboard->id           = g_strdup(id);
  keyboard->version      = g_strdup(version);
  keyboard->kmx_file     = g_strdup_printf("%s.kmx", id);
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
_get_tst_kmp_info(gchar * copyright, gchar * author_desc, gchar * author_url) {
  kmp_info* info    = g_new0(kmp_info, 1);
  info->copyright   = g_strdup(copyright);
  info->author_desc = g_strdup(author_desc);
  info->author_url  = g_strdup(author_url);
  return info;
}

keyboard_details*
_get_tst_keyboard_details(gchar * description, gchar * license) {
  keyboard_details* details = g_new0(keyboard_details, 1);
  details->id               = g_strdup("tst");
  details->description      = g_strdup(description);
  details->license          = g_strdup(license);
  return details;
}

add_keyboard_data*
_get_tst_keyboard_data() {
  add_keyboard_data* kb_data = g_new0(add_keyboard_data, 1);
  kb_data->engines_list      = NULL;
  kb_data->info              = _get_tst_kmp_info(NULL, NULL, NULL);
  kb_data->kmp_dir           = "/tmp";
  return kb_data;
}

void
_free_tst_kb_data(add_keyboard_data* kb_data) {
  if (kb_data->engines_list)
    g_list_free(kb_data->engines_list);
  if (kb_data->info)
    g_free(kb_data->info);
  g_free(kb_data);
}

G_DEFINE_AUTOPTR_CLEANUP_FUNC(add_keyboard_data, _free_tst_kb_data)

// Newer glib versions have g_assert_cmpstrv which would allow to do
// g_assert_cmpstrv(result, keyboards);
// but unfortunately Ubuntu 20.04 Focal doesn't have that, so we roll
// our own
void
_kmn_assert_cmpstrv(gchar** result, gchar** expected) {
  g_assert_nonnull(result);
  g_assert_nonnull(expected);
  int i = 0;
  for (; result[i] && expected[i]; i++) {
    g_assert_cmpstr(result[i], ==, expected[i]);
  }
  g_assert_null(result[i]);
  g_assert_null(expected[i]);
}

//----------------------------------------------------------------------------------------------
void
test_keyman_put_keyboard_options_todconf__invalid() {
  // Initialize
  const gchar* testname = __FUNCTION__;
  _delete_tst_keyboard_options_key(testname);

  // Execute
  keyman_put_keyboard_options_todconf(TEST_FIXTURE, testname, "new_key", NULL);

  // Verify
  g_auto(GStrv) options = _get_tst_keyboard_options_key(testname);
  g_assert_nonnull(options);
  g_assert_null(options[0]);

  // Cleanup
  _delete_tst_keyboard_options_key(testname);
}

void
test_keyman_put_keyboard_options_todconf__new_key() {
  // Initialize
  const gchar* testname = __FUNCTION__;
  _delete_tst_keyboard_options_key(testname);
  g_autofree gchar* value = g_strdup_printf("%d", g_test_rand_int());

  // Execute
  keyman_put_keyboard_options_todconf(TEST_FIXTURE, testname, "new_key", value);

  // Verify
  g_auto(GStrv) options = _get_tst_keyboard_options_key(testname);
  g_autofree gchar* expected = g_strdup_printf("new_key=%s", value);
  g_assert_nonnull(options);
  g_assert_cmpstr(options[0], ==, expected);
  g_assert_null(options[1]);

  // Cleanup
  _delete_tst_keyboard_options_key(testname);
}

void
test_keyman_put_keyboard_options_todconf__other_keys() {
  // Initialize
  const gchar* testname = __FUNCTION__;
  _delete_tst_keyboard_options_key(testname);
  const gchar* const existingKeys[] = {"key1=val1", "key2=val2", NULL};
  _set_tst_keyboard_options_key(testname, existingKeys);
  g_autofree gchar* value = g_strdup_printf("%d", g_test_rand_int());

  // Execute
  keyman_put_keyboard_options_todconf(TEST_FIXTURE, testname, "new_key", value);

  // Verify
  g_auto(GStrv) options = _get_tst_keyboard_options_key(testname);
  g_autofree gchar* expected = g_strdup_printf("new_key=%s", value);
  g_assert_nonnull(options);
  g_assert_cmpstr(options[0], ==, "key1=val1");
  g_assert_cmpstr(options[1], ==, "key2=val2");
  g_assert_cmpstr(options[2], ==, expected);
  g_assert_null(options[3]);

  // Cleanup
  _delete_tst_keyboard_options_key(testname);
}

void
test_keyman_put_keyboard_options_todconf__existing_key() {
  // Initialize
  const gchar* testname = __FUNCTION__;
  _delete_tst_keyboard_options_key(testname);
  const gchar* const existingKeys[] = {"key1=val1", "new_key=val2", NULL};
  _set_tst_keyboard_options_key(testname, existingKeys);
  g_autofree gchar* value = g_strdup_printf("%d", g_test_rand_int());

  // Execute
  keyman_put_keyboard_options_todconf(TEST_FIXTURE, testname, "new_key", value);

  // Verify
  g_auto(GStrv) options = _get_tst_keyboard_options_key(testname);
  g_autofree gchar* expected = g_strdup_printf("new_key=%s", value);
  g_assert_nonnull(options);
  g_assert_cmpstr(options[0], ==, "key1=val1");
  g_assert_cmpstr(options[1], ==, expected);
  g_assert_null(options[2]);

  // Cleanup
  _delete_tst_keyboard_options_key(testname);
}

//----------------------------------------------------------------------------------------------
void test_keyman_put_option_todconf__set_to_true() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);

  // Execute
  gboolean result = keyman_put_option_todconf(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, TRUE);

  // Verify
  g_assert_true(result);
  g_assert_true(_get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR));

  // Cleanup
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

void test_keyman_put_option_todconf__set_to_false() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);

  // Execute
  gboolean result = keyman_put_option_todconf(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, FALSE);

  // Verify
  g_assert_true(result);
  g_assert_false(_get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR));

  // Cleanup
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

//----------------------------------------------------------------------------------------------
void test_keyman_get_option_fromdconf__default() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);
  _reset_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);

  // Execute
  gboolean result = keyman_get_option_fromdconf(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);

  // Verify
  g_assert_false(result);

  // Cleanup
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

//----------------------------------------------------------------------------------------------
static gint count = 0;
static gpointer data = NULL;

void on_settings_change(GSettings* settings, gchar* key, gpointer user_data) {
  count++;
  data = user_data;
}

void test_keyman_subscribe_option_changes__create() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);
  void* settings     = NULL;
  count              = 0;
  data               = NULL;
  gchar* user_data   = "foo";

  // Execute
  settings = keyman_subscribe_option_changes(on_settings_change, user_data);

  // Verify
  g_assert_nonnull(settings);

  // Cleanup
  keyman_unsubscribe_option_changes(settings, on_settings_change, user_data);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

void test_keyman_subscribe_option_changes__callback_called_init_false() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, FALSE);
  count              = 0;
  data               = NULL;
  gchar* user_data   = "foo";
  void* settings     = keyman_subscribe_option_changes(on_settings_change, user_data);

  // Execute
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, TRUE);

  // Verify
  g_assert_cmpint(count, ==, 1);
  g_assert_cmpstr(data, ==, user_data);

  // Cleanup
  keyman_unsubscribe_option_changes(settings, on_settings_change, user_data);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

void test_keyman_subscribe_option_changes__callback_called_init_true() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, TRUE);
  count              = 0;
  data               = NULL;
  void* settings     = keyman_subscribe_option_changes(on_settings_change, NULL);

  // Execute
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, FALSE);

  // Verify
  g_assert_cmpint(count, ==, 1);

  // Cleanup
  keyman_unsubscribe_option_changes(settings, on_settings_change, NULL);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

void test_keyman_subscribe_option_changes__callback_called_toggle() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, TRUE);
  count              = 0;
  data               = NULL;
  void* settings     = keyman_subscribe_option_changes(on_settings_change, NULL);

  // Execute
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, FALSE);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, TRUE);

  // Verify
  g_assert_cmpint(count, ==, 2);

  // Cleanup
  keyman_unsubscribe_option_changes(settings, on_settings_change, NULL);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

void test_keyman_subscribe_option_changes__callback_called_no_toggle() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, TRUE);
  count              = 0;
  data               = NULL;
  void* settings     = keyman_subscribe_option_changes(on_settings_change, NULL);

  // Execute
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, TRUE);

  // Verify
  g_assert_cmpint(count, ==, 1);

  // Cleanup
  keyman_unsubscribe_option_changes(settings, on_settings_change, NULL);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

//----------------------------------------------------------------------------------------------
void test_keyman_unsubscribe_option_changes__create() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);
  count              = 0;
  data               = NULL;
  void* settings     = keyman_subscribe_option_changes(on_settings_change, NULL);

  // Execute
  guint retval = keyman_unsubscribe_option_changes(settings, on_settings_change, NULL);

  // Verify
  g_assert_cmpint(count, ==, 0);
  g_assert_cmpint(retval, ==, 1);

  // Cleanup
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

void test_keyman_unsubscribe_option_changes__null_setting() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);
  count              = 0;
  data               = NULL;
  void* settings     = NULL;

  // Execute
  guint retval = keyman_unsubscribe_option_changes(settings, on_settings_change, NULL);

  // Verify
  g_assert_cmpint(retval, ==, 0);

  // Cleanup
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

void test_keyman_unsubscribe_option_changes__null_callback() {
  // Initialize
  gboolean prevValue = _get_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR);
  count              = 0;
  data               = NULL;
  void* settings     = keyman_subscribe_option_changes(on_settings_change, NULL);

  // Execute
  guint retval = keyman_unsubscribe_option_changes(settings, NULL, NULL);

  // Verify
  g_assert_cmpint(retval, ==, 0);

  // Cleanup
  keyman_unsubscribe_option_changes(settings, on_settings_change, NULL);
  _set_tst_option(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR, prevValue);
}

//----------------------------------------------------------------------------------------------
void
test_keyman_get_custom_keyboard_dictionary__values() {
  // Initialize
  gchar* keyboards[] = {"fr:/tmp/test/test.kmx", "en:/tmp/foo/foo.kmx", "fr:/tmp/foo/foo.kmx", NULL};
  _set_tst_kbds_key(keyboards);

  // Execute
  g_autoptr(GHashTable) result = keyman_get_custom_keyboard_dictionary();

  // Verify
  GPtrArray* value = g_hash_table_lookup(result, "/tmp/test/test.kmx");
  g_assert_cmpint(value->len, ==, 1);
  cust_kbd* pdata = value->pdata[0];
  g_assert_cmpstr(pdata->lang->id, ==, "fr");
  g_assert_cmpstr(pdata->kb_id_with_lang, ==, "fr:/tmp/test/test.kmx");

  value = g_hash_table_lookup(result, "/tmp/foo/foo.kmx");
  g_assert_cmpint(value->len, ==, 2);
  pdata = value->pdata[0];
  g_assert_cmpstr(pdata->lang->id, ==, "en");
  g_assert_cmpstr(pdata->kb_id_with_lang, ==, "en:/tmp/foo/foo.kmx");
  pdata = value->pdata[1];
  g_assert_cmpstr(pdata->lang->id, ==, "fr");
  g_assert_cmpstr(pdata->kb_id_with_lang, ==, "fr:/tmp/foo/foo.kmx");

  // Cleanup
  _delete_tst_kbds_key();
}

void
test_keyman_get_custom_keyboard_dictionary__invalid() {
  // Initialize
  gchar* keyboards[] = {"/tmp/test/test.kmx", NULL};
  _set_tst_kbds_key(keyboards);

  // Execute
  g_autoptr(GHashTable) result = keyman_get_custom_keyboard_dictionary();

  // Verify
  g_assert_nonnull(result);

  // Cleanup
  _delete_tst_kbds_key();
}

void
test_keyman_get_custom_keyboard_dictionary__empty() {
  // Initialize
  gchar* keyboards[] = {NULL};
  _set_tst_kbds_key(keyboards);

  // Execute
  g_autoptr(GHashTable) result = keyman_get_custom_keyboard_dictionary();

  // Verify
  g_assert_null(result);

  // Cleanup
  _delete_tst_kbds_key();
}

void
test_keyman_get_custom_keyboard_dictionary__null() {
  // Initialize
  _delete_tst_kbds_key();

  // Execute
  g_autoptr(GHashTable) result = keyman_get_custom_keyboard_dictionary();

  // Verify
  g_assert_null(result);

  // Cleanup
  _delete_tst_kbds_key();
}

//----------------------------------------------------------------------------------------------
void
test_keyman_set_custom_keyboards__new_key() {
  // Initialize
  _delete_tst_kbds_key();
  gchar* keyboards[] = {"fr:/tmp/test/test.kmx", NULL};

  // Execute
  keyman_set_custom_keyboards(keyboards);

  // Verify
  g_auto(GStrv) result = _get_tst_kbds_key();
  g_assert_nonnull(result);
  _kmn_assert_cmpstrv(result, keyboards);

  // Cleanup
  _delete_tst_kbds_key();
}

void
test_keyman_set_custom_keyboards__overwrite_key() {
  // Initialize
  gchar* initialKbds[] = {"fr:/tmp/test/test.kmx", NULL};
  _set_tst_kbds_key(initialKbds);

  gchar* keyboards[] = {"fr:/tmp/test/test.kmx", "en:/tmp/foo/foo.kmx", NULL};

  // Execute
  keyman_set_custom_keyboards(keyboards);

  // Verify
  g_auto(GStrv) result = _get_tst_kbds_key();
  g_assert_nonnull(result);
  _kmn_assert_cmpstrv(result, keyboards);

  // Cleanup
  _delete_tst_kbds_key();
}

void
test_keyman_set_custom_keyboards__delete_key_NULL() {
  // Initialize
  gchar* initialKbds[] = {"fr:/tmp/test/test.kmx", NULL};
  _set_tst_kbds_key(initialKbds);

  // Execute
  keyman_set_custom_keyboards(NULL);

  // Verify
  g_auto(GStrv) result = _get_tst_kbds_key();
  gchar* expected[] = {NULL};
  g_assert_nonnull(result);
  _kmn_assert_cmpstrv(result, expected);

  // Cleanup
  _delete_tst_kbds_key();
}

void
test_keyman_set_custom_keyboards__delete_key_empty_array() {
  // Initialize
  gchar* initialKbds[] = {"fr:/tmp/test/test.kmx", NULL};
  _set_tst_kbds_key(initialKbds);

  gchar* keyboards[] = {NULL};

  // Execute
  keyman_set_custom_keyboards(keyboards);

  // Verify
  g_auto(GStrv) result = _get_tst_kbds_key();
  g_assert_nonnull(result);
  _kmn_assert_cmpstrv(result, keyboards);

  // Cleanup
  _delete_tst_kbds_key();
}

void
test_keyman_set_custom_keyboards__invalid_values() {
  // Initialize
  _delete_tst_kbds_key();
  gchar* keyboards[] = {"invalid", "fr:/tmp/test/test.kmx", "", ":/tmp/bla.kmx", "fr:", NULL};

  // Execute
  keyman_set_custom_keyboards(keyboards);

  // Verify
  g_auto(GStrv) result = _get_tst_kbds_key();
  g_assert_nonnull(result);
  gchar* expected[] = {"fr:/tmp/test/test.kmx", NULL};
  _kmn_assert_cmpstrv(result, expected);

  // Cleanup
  _delete_tst_kbds_key();
}

//----------------------------------------------------------------------------------------------
void
test_keyman_get_custom_keyboards__value() {
  // Initialize
  gchar* keyboards[] = {"fr:/tmp/test/test.kmx", NULL};
  _set_tst_kbds_key(keyboards);

  // Execute
  g_auto(GStrv) result = keyman_get_custom_keyboards();

  // Verify
  g_assert_nonnull(result);
  _kmn_assert_cmpstrv(result, keyboards);

  // Cleanup
  _delete_tst_kbds_key();
}

void
test_keyman_get_custom_keyboards__invalid_values() {
  // Initialize
  gchar* keyboards[] = {"invalid", "fr:/tmp/test/test.kmx", "", NULL};
  _set_tst_kbds_key(keyboards);

  // Execute
  g_auto(GStrv) result = keyman_get_custom_keyboards();

  // Verify
  gchar* expected[] = {"fr:/tmp/test/test.kmx", NULL};
  g_assert_nonnull(result);
  _kmn_assert_cmpstrv(result, expected);

  // Cleanup
  _delete_tst_kbds_key();
}

void
test_keyman_get_custom_keyboards__no_key() {
  // Initialize
  _delete_tst_kbds_key();

  // Execute
  g_auto(GStrv) result = keyman_get_custom_keyboards();

  // Verify
  g_assert_null(result);

  // Cleanup
  _delete_tst_kbds_key();
}

void
test_keyman_get_custom_keyboards__empty() {
  // Initialize
  gchar* keyboards[] = {NULL};
  _set_tst_kbds_key(keyboards);

  // Execute
  g_auto(GStrv) result = keyman_get_custom_keyboards();

  // Verify
  g_assert_null(result);

  // Cleanup
  _delete_tst_kbds_key();
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
  g_autoptr(kmp_keyboard) keyboard    = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(kmp_info) info            = _get_tst_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_tst_keyboard_details("my description", "MIT");
  kmp_language lang                   = {NULL, NULL};

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", &lang);

  // Verify
  g_assert_null(desc);
}

void
test_get_engine_for_language__empty_language() {
  // Initialize
  gchar* languages[]        = {"en:English", NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(kmp_info) info            = _get_tst_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_tst_keyboard_details("my description", "MIT");
  kmp_language lang                   = {"", ""};

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", &lang);

  // Verify
  g_assert_null(desc);
}

void
test_get_engine_for_language__one_language() {
  // Initialize
  gchar* languages[]        = {"en:English", NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(kmp_info) info            = _get_tst_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_tst_keyboard_details("my description", "MIT");
  kmp_language lang                   = {"English", "en"};

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", &lang);

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "en:/tmp/tst.kmx");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "Testing");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "en");
}

void
test_get_engine_for_language__one_unknown_language() {
  // Initialize
  gchar* languages[]        = {"foo:Foo", NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(kmp_info) info            = _get_tst_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_tst_keyboard_details("my description", "MIT");
  kmp_language lang                   = {"Foo", "foo"};

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", &lang);

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "foo:/tmp/tst.kmx");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "Testing - Foo");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "foo");
}

void
test_get_engine_for_language__different_languages() {
  // Initialize
  gchar* languages[]        = {"en:English", NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(kmp_info) info            = _get_tst_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_tst_keyboard_details("my description", "MIT");
  kmp_language lang                   = {"Foo", "foo"};

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", &lang);

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "foo:/tmp/tst.kmx");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "Testing - Foo");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "foo");
}

void
test_get_engine_for_language__no_kbd_language() {
  // Initialize
  gchar* languages[]        = {NULL};
  g_autoptr(kmp_keyboard) keyboard    = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(kmp_info) info            = _get_tst_kmp_info("Copyright by me", "My Author", "myauthor@example.com");
  g_autoptr(keyboard_details) details = _get_tst_keyboard_details("my description", "MIT");
  kmp_language lang                   = {"English", "en"};

  // Execute
  g_autoptr(IBusEngineDesc) desc = get_engine_for_language(keyboard, info, details, "/tmp", &lang);

  // Verify
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "en:/tmp/tst.kmx");
  g_assert_cmpstr(ibus_engine_desc_get_longname(desc), ==, "Testing");
  g_assert_cmpstr(ibus_engine_desc_get_language(desc), ==, "en");
}

extern GHashTable* custom_keyboards;

//----------------------------------------------------------------------------------------------
void
test_keyman_add_keyboard__no_language() {
  // Initialize
  gchar* languages[] = {NULL};
  custom_keyboards                 = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  g_autoptr(kmp_keyboard) keyboard     = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(add_keyboard_data) kb_data = _get_tst_keyboard_data();

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
  gchar* languages[] = {"en:English", NULL};
  custom_keyboards                 = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  g_autoptr(kmp_keyboard) keyboard     = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(add_keyboard_data) kb_data = _get_tst_keyboard_data();

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
  gchar* languages[] = {"en:English", "fr:French", NULL};
  custom_keyboards                 = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  g_autoptr(kmp_keyboard) keyboard     = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(add_keyboard_data) kb_data = _get_tst_keyboard_data();

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
test_keyman_add_keyboard__one_language_plus_custom() {
  // Initialize
  gchar* languages[]                   = {"en:English", NULL};
  custom_keyboards                     = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  g_hash_table_insert(custom_keyboards, g_strdup("/tmp/tst.kmx"), g_ptr_array_new_full(1, g_free));
  GPtrArray* language_keyboards = g_hash_table_lookup(custom_keyboards, "/tmp/tst.kmx");
  cust_kbd* data                = g_new0(cust_kbd, 1);
  data->kb_id_with_lang         = g_strdup("ldb:/tmp/tst.kmx");
  data->lang                    = g_new0(kmp_language, 1);
  data->lang->id                = g_strdup("ldb");
  g_ptr_array_add(language_keyboards, data);
  g_autoptr(kmp_keyboard) keyboard     = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(add_keyboard_data) kb_data = _get_tst_keyboard_data();

  // Execute
  keyman_add_keyboard(keyboard, kb_data);

  // Verify
  g_assert_nonnull(kb_data->engines_list->data);
  IBusEngineDesc* desc = (IBusEngineDesc*)kb_data->engines_list->data;
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "en:/tmp/tst.kmx");
  g_assert_nonnull(kb_data->engines_list->next);
  g_assert_nonnull(kb_data->engines_list->next->data);
  desc = (IBusEngineDesc*)kb_data->engines_list->next->data;
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "ldb:/tmp/tst.kmx");
  g_assert_null(kb_data->engines_list->next->next);
}

void
test_keyman_add_keyboard__one_language_plus_two_custom() {
  // Initialize
  gchar* languages[]            = {"en:English", NULL};
  custom_keyboards              = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  g_hash_table_insert(custom_keyboards, g_strdup("/tmp/tst.kmx"), g_ptr_array_new_full(1, g_free));
  GPtrArray* language_keyboards = g_hash_table_lookup(custom_keyboards, "/tmp/tst.kmx");
  cust_kbd* data                = g_new0(cust_kbd, 1);
  data->kb_id_with_lang         = g_strdup("ldb:/tmp/tst.kmx");
  data->lang                    = g_new0(kmp_language, 1);
  data->lang->id                = g_strdup("ldb");
  g_ptr_array_add(language_keyboards, data);
  data                          = g_new0(cust_kbd, 1);
  data->kb_id_with_lang         = g_strdup("lda:/tmp/tst.kmx");
  data->lang                    = g_new0(kmp_language, 1);
  data->lang->id                = g_strdup("lda");
  g_ptr_array_add(language_keyboards, data);
  g_autoptr(kmp_keyboard) keyboard     = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(add_keyboard_data) kb_data = _get_tst_keyboard_data();

  // Execute
  keyman_add_keyboard(keyboard, kb_data);

  // Verify
  GList* list = kb_data->engines_list;
  g_assert_nonnull(list);
  IBusEngineDesc* desc = (IBusEngineDesc*)list->data;
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "en:/tmp/tst.kmx");
  g_assert_nonnull(list->next);
  list = list->next;
  g_assert_nonnull(list->data);
  desc = (IBusEngineDesc*)list->data;
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "ldb:/tmp/tst.kmx");
  g_assert_nonnull(list->next);
  list = list->next;
  g_assert_nonnull(list->data);
  desc = (IBusEngineDesc*)list->data;
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, "lda:/tmp/tst.kmx");
  g_assert_null(list->next);
}

void
test_keyman_add_keyboard__prev_engine_adding_same_version() {
  // Initialize
  gchar* languages[] = {"en:English", NULL};
  custom_keyboards                     = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  g_autoptr(kmp_keyboard) keyboard     = _get_tst_kmp_keyboard("1.0", languages, "tst");
  g_autoptr(add_keyboard_data) kb_data = _get_tst_keyboard_data();
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
  gchar* languages[] = {"en:English", "fr:French", NULL};  // New version adds French
  custom_keyboards                     = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  g_autoptr(kmp_keyboard) keyboard     = _get_tst_kmp_keyboard("1.1", languages, "tst");
  g_autoptr(add_keyboard_data) kb_data = _get_tst_keyboard_data();
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
  gchar* languages[] = {"en:English", "fr:French", NULL};  // New version adds French
  custom_keyboards                     = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  g_autoptr(kmp_keyboard) keyboard     = _get_tst_kmp_keyboard("1.10", languages, "tst");
  g_autoptr(add_keyboard_data) kb_data = _get_tst_keyboard_data();
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
  gchar* languages[] = {"en:English", "fr:French", NULL};  // Old version has additional French
  custom_keyboards                     = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  g_autoptr(kmp_keyboard) keyboard     = _get_tst_kmp_keyboard("0.9", languages, "tst");
  g_autoptr(add_keyboard_data) kb_data = _get_tst_keyboard_data();
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

#if U_ICU_VERSION_MAJOR_NUM >= 73
// ICU 73 uses CLDR 43 which contains various corrections so that ICU
// now returns the correct bmf instead of bmf-Latn.
#define BCP47_BMF "bmf"
#define BCP47_BUN "bun"
#else
#define BCP47_BMF "bmf-Latn"
#define BCP47_BUN "bun-Latn"
#endif

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
  g_autofree gchar* expected_name1 = g_strdup_printf("%s:%s/test2.kmx", BCP47_BMF, kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name1);
  desc = IBUS_ENGINE_DESC(keyboards->next->data);
  g_autofree gchar* expected_name2 = g_strdup_printf("%s:%s/test2.kmx", BCP47_BUN, kmp_dir);
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
  g_autofree gchar* expected_name1 = g_strdup_printf("%s:%s/test2.kmx", BCP47_BMF, kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name1);
  desc = IBUS_ENGINE_DESC(keyboards->next->data);
  g_autofree gchar* expected_name2 = g_strdup_printf("%s:%s/test2.kmx", BCP47_BUN, kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name2);
  desc = IBUS_ENGINE_DESC(keyboards->next->next->data);
  g_autofree gchar* expected_name = g_strdup_printf("bza:%s/test1.kmx", kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name);
}

void
test_keyman_add_keyboards_from_dir__one_language_plus_two_different_custom() {
  // Initialize
  g_autoptr(GList) keyboards = NULL;
  g_autoptr(gchar) kmp_dir   = g_dir_make_tmp(NULL, NULL);
  copy_test_data(kmp_dir, "kmp1.json");

  custom_keyboards = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  gchar* kmx_path  = g_strdup_printf("%s/test1.kmx", kmp_dir);
  g_hash_table_insert(custom_keyboards, g_strdup(kmx_path), g_ptr_array_new_full(1, g_free));
  GPtrArray* language_keyboards = g_hash_table_lookup(custom_keyboards, kmx_path);
  cust_kbd* data                = g_new0(cust_kbd, 1);
  data->kb_id_with_lang         = g_strdup_printf("ldb:%s", kmx_path);
  data->lang                    = g_new0(kmp_language, 1);
  data->lang->id                = g_strdup("ldb");
  g_ptr_array_add(language_keyboards, data);

  // This custom keyboard will be ignored since we don't have this keyboard installed
  g_hash_table_insert(custom_keyboards, g_strdup("/tmp/foo.kmx"), g_ptr_array_new_full(1, g_free));
  language_keyboards    = g_hash_table_lookup(custom_keyboards, "/tmp/foo.kmx");
  data                  = g_new0(cust_kbd, 1);
  data->kb_id_with_lang = g_strdup("lda:/tmp/foo.kmx");
  data->lang            = g_new0(kmp_language, 1);
  data->lang->id        = g_strdup("foo");
  g_ptr_array_add(language_keyboards, data);

  // Execute
  keyman_add_keyboards_from_dir(kmp_dir, &keyboards);

  // Verify
  g_assert_cmpint(g_list_length(keyboards), ==, 2);

  IBusEngineDesc* desc             = IBUS_ENGINE_DESC(keyboards->data);
  g_autofree gchar* expected_name = g_strdup_printf("bza:%s/test1.kmx", kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name);
  desc                            = IBUS_ENGINE_DESC(keyboards->next->data);
  expected_name                   = g_strdup_printf("ldb:%s/test1.kmx", kmp_dir);
  g_assert_cmpstr(ibus_engine_desc_get_name(desc), ==, expected_name);
}

void
test_keyman_compare_version_equal() {
  g_assert_cmpint(keyman_compare_version("1.0", "1.0"), ==, 0);
}

void
test_keyman_compare_version_equal_with_patch() {
  g_assert_cmpint(keyman_compare_version("1.0", "1.0.0"), ==, 0);
}

void
test_keyman_compare_version_equal_nondigit() {
  g_assert_cmpint(keyman_compare_version("1.1", "1.1-beta2"), ==, 0);
}

void
test_keyman_compare_version_equal_doubledot() {
  g_assert_cmpint(keyman_compare_version("1..2", "1.0.2"), ==, 0);
}

void
test_keyman_compare_version_less() {
  g_assert_cmpint(keyman_compare_version("1.0", "1.1"), <, 0);
}

void
test_keyman_compare_version_minor_less_with_patch() {
  g_assert_cmpint(keyman_compare_version("1.0.1", "1.1"), <, 0);
}

void
test_keyman_compare_version_patch_less() {
  g_assert_cmpint(keyman_compare_version("1.0", "1.0.1"), <, 0);
}

void
test_keyman_compare_version_less_twodigit() {
  g_assert_cmpint(keyman_compare_version("99.9", "100"), <, 0);
}

void
test_keyman_compare_version_greater() {
  g_assert_cmpint(keyman_compare_version("1.10", "1.9"), >, 0);
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
  g_test_add_func("/keymanutil/keyman_put_keyboard_options_todconf/invalid", test_keyman_put_keyboard_options_todconf__invalid);
  g_test_add_func("/keymanutil/keyman_put_keyboard_options_todconf/new_key", test_keyman_put_keyboard_options_todconf__new_key);
  g_test_add_func("/keymanutil/keyman_put_keyboard_options_todconf/other_keys", test_keyman_put_keyboard_options_todconf__other_keys);
  g_test_add_func("/keymanutil/keyman_put_keyboard_options_todconf/existing_key", test_keyman_put_keyboard_options_todconf__existing_key);

  g_test_add_func("/keymanutil/keyman_put_option_todconf/set_to_true", test_keyman_put_option_todconf__set_to_true);
  g_test_add_func("/keymanutil/keyman_put_option_todconf/set_to_false", test_keyman_put_option_todconf__set_to_false);

  g_test_add_func("/keymanutil/keyman_get_option_fromdconf/default", test_keyman_get_option_fromdconf__default);

  g_test_add_func("/keymanutil/keyman_subscribe_option_changes/create", test_keyman_subscribe_option_changes__create);
  g_test_add_func("/keymanutil/keyman_subscribe_option_changes/callback_called_init_false", test_keyman_subscribe_option_changes__callback_called_init_false);
  g_test_add_func("/keymanutil/keyman_subscribe_option_changes/callback_called_init_true", test_keyman_subscribe_option_changes__callback_called_init_true);
  g_test_add_func("/keymanutil/keyman_subscribe_option_changes/callback_called_toggle", test_keyman_subscribe_option_changes__callback_called_toggle);
  g_test_add_func("/keymanutil/keyman_subscribe_option_changes/callback_called_no_toggle", test_keyman_subscribe_option_changes__callback_called_no_toggle);

  g_test_add_func("/keymanutil/keyman_unsubscribe_option_changes/create", test_keyman_unsubscribe_option_changes__create);
  g_test_add_func("/keymanutil/keyman_unsubscribe_option_changes/null_setting", test_keyman_unsubscribe_option_changes__null_setting);
  g_test_add_func("/keymanutil/keyman_unsubscribe_option_changes/null_callback", test_keyman_unsubscribe_option_changes__null_callback);

  g_test_add_func("/keymanutil/keyman_get_custom_keyboard_dictionary/values", test_keyman_get_custom_keyboard_dictionary__values);
  g_test_add_func("/keymanutil/keyman_get_custom_keyboard_dictionary/invalid", test_keyman_get_custom_keyboard_dictionary__invalid);
  g_test_add_func("/keymanutil/keyman_get_custom_keyboard_dictionary/empty", test_keyman_get_custom_keyboard_dictionary__empty);
  g_test_add_func("/keymanutil/keyman_get_custom_keyboard_dictionary/null", test_keyman_get_custom_keyboard_dictionary__null);

  g_test_add_func("/keymanutil/keyman_set_custom_keyboards/new_key", test_keyman_set_custom_keyboards__new_key);
  g_test_add_func("/keymanutil/keyman_set_custom_keyboards/overwrite_key", test_keyman_set_custom_keyboards__overwrite_key);
  g_test_add_func("/keymanutil/keyman_set_custom_keyboards/delete_key_NULL", test_keyman_set_custom_keyboards__delete_key_NULL);
  g_test_add_func(
      "/keymanutil/keyman_set_custom_keyboards/delete_key_empty_array",
      test_keyman_set_custom_keyboards__delete_key_empty_array);
  g_test_add_func("/keymanutil/keyman_set_custom_keyboards/invalid_values", test_keyman_set_custom_keyboards__invalid_values);

  g_test_add_func("/keymanutil/keyman_get_custom_keyboards/value", test_keyman_get_custom_keyboards__value);
  g_test_add_func("/keymanutil/keyman_get_custom_keyboards/invalid_values", test_keyman_get_custom_keyboards__invalid_values);
  g_test_add_func("/keymanutil/keyman_get_custom_keyboards/no_key", test_keyman_get_custom_keyboards__no_key);
  g_test_add_func("/keymanutil/keyman_get_custom_keyboards/empty", test_keyman_get_custom_keyboards__empty);

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
  g_test_add_func("/keymanutil/keyman_add_keyboard/one_language_plus_custom", test_keyman_add_keyboard__one_language_plus_custom);
  g_test_add_func("/keymanutil/keyman_add_keyboard/one_language_plus_two_custom", test_keyman_add_keyboard__one_language_plus_two_custom);
  g_test_add_func(
      "/keymanutil/keyman_add_keyboard/prev_engine_adding_same_version",
      test_keyman_add_keyboard__prev_engine_adding_same_version);
  g_test_add_func(
      "/keymanutil/keyman_add_keyboard/prev_engine_adding_newer_version",
      test_keyman_add_keyboard__prev_engine_adding_newer_version);
  g_test_add_func(
      "/keymanutil/keyman_add_keyboard/prev_engine_adding_newer_version_versioncompare",
      test_keyman_add_keyboard__prev_engine_adding_newer_version_9593);
  g_test_add_func(
      "/keymanutil/keyman_add_keyboard/prev_engine_adding_older_version",
      test_keyman_add_keyboard__prev_engine_adding_older_version);

  g_test_add_func("/keymanutil/keyman_add_keyboards_from_dir/no_kmpjson", test_keyman_add_keyboards_from_dir__no_kmpjson);
  g_test_add_func("/keymanutil/keyman_add_keyboards_from_dir/one_dir", test_keyman_add_keyboards_from_dir__one_dir);
  g_test_add_func("/keymanutil/keyman_add_keyboards_from_dir/two_langs", test_keyman_add_keyboards_from_dir__two_langs);
  g_test_add_func("/keymanutil/keyman_add_keyboards_from_dir/prev_keyboards", test_keyman_add_keyboards_from_dir__prev_keyboards);
  g_test_add_func(
      "/keymanutil/keyman_add_keyboards_from_dir/one_language_plus_two_different_custom",
      test_keyman_add_keyboards_from_dir__one_language_plus_two_different_custom);

  g_test_add_func("/keymanutil/keyman_compare_version/1.0==1.0", test_keyman_compare_version_equal);
  g_test_add_func("/keymanutil/keyman_compare_version/1.0==1.0.0", test_keyman_compare_version_equal_with_patch);
  g_test_add_func("/keymanutil/keyman_compare_version/1.1==1.1-beta", test_keyman_compare_version_equal_nondigit);
  g_test_add_func("/keymanutil/keyman_compare_version/1..2==1.0.2", test_keyman_compare_version_equal_doubledot);
  g_test_add_func("/keymanutil/keyman_compare_version/1.0<1.1", test_keyman_compare_version_less);
  g_test_add_func("/keymanutil/keyman_compare_version/1.0.1<1.1", test_keyman_compare_version_minor_less_with_patch);
  g_test_add_func("/keymanutil/keyman_compare_version/1.0<1.0.1", test_keyman_compare_version_patch_less);
  g_test_add_func("/keymanutil/keyman_compare_version/99.9<100",test_keyman_compare_version_less_twodigit);
  g_test_add_func("/keymanutil/keyman_compare_version/1.10>1.9", test_keyman_compare_version_greater);

  // Run tests
  int retVal = g_test_run();

  return retVal;
}
