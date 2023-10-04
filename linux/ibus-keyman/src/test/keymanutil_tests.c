#include <glib-object.h>
#include <glib.h>
#include <gtk/gtk.h>
#include "keymanutil.h"

#define TEST_FIXTURE "keymanutil-test"

void
delete_options_key(gchar* testname) {
  gchar *path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_OPTIONS_PATH, TEST_FIXTURE, testname);
  GSettings *settings = g_settings_new_with_path(KEYMAN_DCONF_OPTIONS_CHILD_NAME, path);
  g_settings_reset(settings, KEYMAN_DCONF_OPTIONS_KEY);
  g_object_unref(G_OBJECT(settings));
  g_free(path);
}

void
set_options_key(gchar* testname, gchar** options) {
  gchar *path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_OPTIONS_PATH, TEST_FIXTURE, testname);
  GSettings *settings = g_settings_new_with_path(KEYMAN_DCONF_OPTIONS_CHILD_NAME, path);
  g_settings_set_strv(settings, KEYMAN_DCONF_OPTIONS_KEY, (const gchar* const*)options);
  g_object_unref(G_OBJECT(settings));
  g_free(path);
}

gchar**
get_options_key(gchar* testname) {
  gchar* path = g_strdup_printf("%s%s/%s/", KEYMAN_DCONF_OPTIONS_PATH, TEST_FIXTURE, testname);
  GSettings* settings = g_settings_new_with_path(KEYMAN_DCONF_OPTIONS_CHILD_NAME, path);
  gchar** result = g_settings_get_strv(settings, KEYMAN_DCONF_OPTIONS_KEY);
  g_object_unref(G_OBJECT(settings));
  g_free(path);
  return result;
}

void
test_keyman_put_options_todconf__new_key() {
  // Initialize
  gchar* testname = "test_keyman_put_options_todconf__new_key";
  delete_options_key(testname);
  gchar* value = g_strdup_printf("%d", g_test_rand_int());

  // Execute
  keyman_put_options_todconf(TEST_FIXTURE, testname, "new_key", value);

  // Verify
  gchar** options = get_options_key(testname);
  gchar* expected = g_strdup_printf("new_key=%s", value);
  g_assert_nonnull(options);
  g_assert_cmpstr(options[0], ==, expected);
  g_assert_null(options[1]);

  // Cleanup
  g_free(expected);
  g_free(value);
  g_strfreev(options);
  delete_options_key(testname);
}

void
test_keyman_put_options_todconf__other_keys() {
  // Initialize
  gchar* testname = "test_keyman_put_options_todconf__other_keys";
  delete_options_key(testname);
  gchar* existingKeys[] = {"key1=val1", "key2=val2", NULL};
  set_options_key(testname, existingKeys);
  gchar* value = g_strdup_printf("%d", g_test_rand_int());

  // Execute
  keyman_put_options_todconf(TEST_FIXTURE, testname, "new_key", value);

  // Verify
  gchar** options = get_options_key(testname);
  gchar* expected = g_strdup_printf("new_key=%s", value);
  g_assert_nonnull(options);
  g_assert_cmpstr(options[0], ==, "key1=val1");
  g_assert_cmpstr(options[1], ==, "key2=val2");
  g_assert_cmpstr(options[2], ==, expected);
  g_assert_null(options[3]);

  // Cleanup
  g_free(expected);
  g_free(value);
  g_strfreev(options);
  delete_options_key(testname);
}

void
test_keyman_put_options_todconf__existing_key() {
  // Initialize
  gchar* testname = "test_keyman_put_options_todconf__existing_key";
  delete_options_key(testname);
  gchar* existingKeys[] = {"key1=val1", "new_key=val2", NULL};
  set_options_key(testname, existingKeys);
  gchar* value = g_strdup_printf("%d", g_test_rand_int());

  // Execute
  keyman_put_options_todconf(TEST_FIXTURE, testname, "new_key", value);

  // Verify
  gchar** options = get_options_key(testname);
  gchar* expected = g_strdup_printf("new_key=%s", value);
  g_assert_nonnull(options);
  g_assert_cmpstr(options[0], ==, "key1=val1");
  g_assert_cmpstr(options[1], ==, expected);
  g_assert_null(options[2]);

  // Cleanup
  g_free(expected);
  g_free(value);
  g_strfreev(options);
  delete_options_key(testname);
}

int
main(int argc, char* argv[]) {
  gtk_init(&argc, &argv);
  g_test_init(&argc, &argv, NULL);
  g_test_set_nonfatal_assertions();

  // Add tests
  g_test_add_func("/keymanutil/keyman_put_options_todconf/new_key", test_keyman_put_options_todconf__new_key);
  g_test_add_func("/keymanutil/keyman_put_options_todconf/other_keys", test_keyman_put_options_todconf__other_keys);
  g_test_add_func("/keymanutil/keyman_put_options_todconf/existing_key", test_keyman_put_options_todconf__existing_key);

  // Run tests
  int retVal = g_test_run();

  return retVal;
}
