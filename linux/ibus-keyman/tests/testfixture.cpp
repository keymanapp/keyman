#include <iostream>
#include <keyman/keyman_core_api.h>
#include <kmx/kmx_processevent.h>
#include <linux/input-event-codes.h>
#include <list>
#include <memory>
#include <stack>
#include <stdexcept>
#include <string>
#include "km_g_test/km_gtestutils.h"
#include "DbusTestHelper.h"
#include "keycodes.h"
#include "keymanutil.h"
#include "KeymanSystemServiceClient.h"
#include "kmx_test_source.hpp"
#include "testmodule.h"
#include "testfixture.h"
#include "KeyHandling.h"


typedef struct {
  char *test_name;
  char *test_path;
  const char *skip_reason;
  gboolean use_surrounding_text;
} TestData;

static gboolean loaded        = FALSE;
static gboolean use_wayland   = FALSE;
static GMainLoop *thread_loop = NULL;
static GTypeModule *module    = NULL;
gboolean testing              = TRUE;
GdkWindow* window             = NULL;

static void
module_register(GTypeModule *module) {
  ibus_im_context_register_type(module);
}

void
destroy(GtkWidget *widget, gpointer data) {
  gtk_main_quit();
}

static void
ibus_keyman_tests_fixture_set_up(
  IBusKeymanTestsFixture* fixture,
  gconstpointer user_data
) {
  IBusIMContextClass* classClass;
  initialize_keyman_system_service_client();

  if (!window) {
    GtkWidget* widget = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_signal_connect(widget, "destroy", G_CALLBACK(destroy), NULL);
    window = gtk_widget_get_window(widget);
  }

  if (!loaded) {
    ibus_init();

    module = test_module_new(module_register);
    g_assert_nonnull(module);

    /* Not loaded until we call ref for the first time */
    classClass = static_cast<IBusIMContextClass*>(g_type_class_peek(IBUS_TYPE_IM_CONTEXT));
    g_assert_null(classClass);

    loaded = TRUE;
  }

  fixture->bus = ibus_bus_new();
}

static void
ibus_keyman_tests_fixture_tear_down(IBusKeymanTestsFixture *fixture, gconstpointer user_data) {
  dbus_testhelper_close();
  if (thread_loop) {
    g_main_loop_unref(thread_loop);
    thread_loop = NULL;
  }

  g_clear_object(&fixture->bus);
  g_clear_object(&fixture->context);
  auto data = (TestData *)user_data;
  g_free(data->test_name);
  g_free(data->test_path);
  delete data;
}

static void
switch_keyboard(IBusKeymanTestsFixture *fixture, const gchar *keyboard) {
  ibus_bus_set_global_engine(fixture->bus, keyboard);

  IBusEngineDesc *desc = ibus_bus_get_global_engine(fixture->bus);
  g_object_ref_sink(desc);
  // g_debug("Switched to engine: '%s'", ibus_engine_desc_get_name(desc));
  g_assert_cmpstr(keyboard, ==, ibus_engine_desc_get_name(desc));
  g_clear_object(&desc);

  if (thread_loop) {
    g_main_loop_unref(thread_loop);
    thread_loop = NULL;
  }
  if (fixture->context) {
    g_clear_object(&fixture->context);
  }
  fixture->ibuscontext = ibus_im_context_new();
  fixture->context     = (GtkIMContext *)fixture->ibuscontext;

  thread_loop = g_main_loop_new(NULL, TRUE);
  dbus_testhelper_init(fixture);
  ibus_im_test_set_thread_loop(fixture->ibuscontext, thread_loop);
}

template <typename... Args>
std::string
string_format(const std::string &format, Args... args) {
  // from https://stackoverflow.com/a/26221725/487503
  int size_s = std::snprintf(nullptr, 0, format.c_str(), args...) + 1;  // Extra space for '\0'
  if (size_s <= 0) {
    throw std::runtime_error("Error during formatting.");
  }
  auto size = static_cast<size_t>(size_s);
  auto buf  = std::make_unique<char[]>(size);
  std::snprintf(buf.get(), size, format.c_str(), args...);
  return std::string(buf.get(), buf.get() + size - 1);  // We don't want the '\0' inside
}

static std::list<km::tests::key_event> get_context_keys(std::u16string context) {
  std::list<km::tests::key_event> result;
  for (auto c = context.begin(); c != context.end(); c++) {
    gchar utf8[6];
    if (g_unichar_to_utf8((gunichar)*c, utf8) != 1 || utf8[0] < 32 || utf8[0] > 127) {
      g_assertion_message_cmpstr(
          G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC, "We can only deal with ASCII characters in the context", utf8, "", "");
    }
    result.push_back(
        {km::core::kmx::s_char_to_vkey[(int)utf8[0] - 32].vk,
         (uint16_t)(km::core::kmx::s_char_to_vkey[(int)utf8[0] - 32].shifted ? KM_CORE_MODIFIER_SHIFT : 0)});
  }
  return result;
}

static void test_source(IBusKeymanTestsFixture* fixture, gconstpointer user_data) {
  auto data       = (TestData*)user_data;
  auto sourcefile = string_format("%s.kmn", data->test_path);
  auto kmxfile    = string_format("und:%s.kmx", data->test_path);

  ibus_im_test_set_surrounding_text_supported(data->use_surrounding_text);

  km::tests::KmxTestSource test_source;
  std::string keys        = "";
  std::u16string expected = u"", expected_context = u"", context = u"";
  km::tests::kmx_options options;
  bool expected_beep = false;
  // NOTE: we don't verify expected beeps since engine.c directly calls a gdk method so we
  // don't know when it gets called.
  g_assert_cmpint(test_source.load_source(sourcefile.c_str(), keys, expected, expected_context, context, options, expected_beep), ==, 0);

  for (auto & option : options) {
    if (option.type == km::tests::KOT_INPUT) {
      auto key = g_utf16_to_utf8((gunichar2 *)option.key.c_str(), option.key.length(), NULL, NULL, NULL);
      auto value = g_utf16_to_utf8((gunichar2 *)option.value.c_str(), option.value.length(), NULL, NULL, NULL);
      keyman_put_keyboard_options_todconf(data->test_name, data->test_name, key, value);
    }
  }
  g_settings_sync();

  switch_keyboard(fixture, kmxfile.c_str());
  if (test_source.caps_lock_state() != get_capslock_indicator()) {
    set_capslock_indicator((guint32)test_source.caps_lock_state());
  }

  auto contextKeys = get_context_keys(context);
  for (auto k = contextKeys.begin(); k != contextKeys.end(); k++) {
    press_key(fixture, *k);
  }

  gtk_im_context_focus_in(fixture->context);

  for (auto p = test_source.next_key(keys); p.vk != 0; p = test_source.next_key(keys)) {
    press_key(fixture, p);
  }

  if (expected.length() == 0) {
    g_assert_null(ibus_im_test_get_text(fixture->ibuscontext));
  } else {
    auto expectedText = g_utf16_to_utf8((gunichar2*)expected.c_str(), expected.length(), NULL, NULL, NULL);
    g_assert_cmpstr(ibus_im_test_get_text(fixture->ibuscontext), ==, expectedText);
  }

  // Cleanup
  g_settings_sync();
}

static void
test_skip(IBusKeymanTestsFixture *fixture, gconstpointer user_data) {
  auto data = (TestData *)user_data;
  km_g_test_skip(data->skip_reason);
}

void
print_usage() {
  printf("Usage: %s --directory <keyboarddir> [--surrounding-text] [--no-surrounding-text] [--wayland|--x11] test1 [test2 ...]\n\n", g_get_prgname());
  printf("Arguments:\n");
  printf("\t--wayland\tRun tests on Wayland\n");
  printf("\t--x11\tRun tests on X11\n");
  printf("\t--surrounding-text\tRun tests with surrounding text support\n");
  printf("\t--no-surrounding-text\tRun tests without surrounding text support\n");
  printf("\nIf neither --surrounding-text nor --no-surrounding-text are specified then the tests run with both settings.\n");
}

void
add_test(const char* directory, const char* filename, gboolean use_surrounding_text, const char* skip_reason, bool useWayland) {
  auto file         = g_file_new_for_commandline_arg(filename);
  auto testfilebase = g_file_get_basename(file);
  auto testname     = g_string_new(NULL);
  g_string_append_printf(
      testname, "/%s/integration-tests/%s/%s", useWayland ? "wayland" : "x11",
      use_surrounding_text ? "surrounding-text" : "no-surrounding-text", testfilebase);
  auto testfile = g_file_new_build_filename(directory, testfilebase, NULL);
  auto testdata                  = new TestData();
  testdata->test_name            = strdup(filename);
  testdata->test_path            = g_file_get_parse_name(testfile);
  testdata->use_surrounding_text = use_surrounding_text;
  testdata->skip_reason          = skip_reason;
  km_g_test_add(
      testname->str, IBusKeymanTestsFixture, testdata, ibus_keyman_tests_fixture_set_up, skip_reason ? test_skip : test_source,
      ibus_keyman_tests_fixture_tear_down);
  g_object_unref(file);
  g_object_unref(testfile);
  g_string_free(testname, TRUE);
}

int
main(int argc, char *argv[]) {
  gtk_init(&argc, &argv);
  km_g_test_init(&argc, &argv, NULL);
  km_g_test_set_nonfatal_assertions();

  if (argc < 4) {
    print_usage();
    return 1;
  }

  int iArg;
  char *directory = NULL;
  bool argWayland = false;
  bool argX11 = false;
  bool seenDirectory = false;
  bool seenSurroundingTextOption = false;
  bool runSurroundingTextTests = true;
  bool runNoSurroundingTextTests = true;

  for (iArg = 1; iArg < argc; iArg++) {
    if (strcmp(argv[iArg], "--directory") == 0 && iArg + 1 < argc) {
      iArg++;
      directory = argv[iArg];
      seenDirectory = true;
    } else if (strcmp(argv[iArg], "--wayland") == 0) {
      argWayland = true;
    } else if (strcmp(argv[iArg], "--x11") == 0) {
      argX11 = true;
    } else if (strcmp(argv[iArg], "--surrounding-text") == 0) {
      if (!seenSurroundingTextOption) {
        runNoSurroundingTextTests = false;
      }
      runSurroundingTextTests = true;
      seenSurroundingTextOption = true;
    } else if (strcmp(argv[iArg], "--no-surrounding-text") == 0) {
      if (!seenSurroundingTextOption) {
        runSurroundingTextTests = false;
      }
      runNoSurroundingTextTests = true;
      seenSurroundingTextOption = true;
    } else if (!seenDirectory) {
      print_usage();
      return 2;
    } else {
      break;
    }
  }

  if (argWayland && argX11) {
    printf("ERROR: --wayland and --x11 can't both be specified");
    print_usage();
    return 2;
  }

  use_wayland     = argWayland;
  int nTests      = argc - iArg;
  char **tests    = &argv[iArg];

  // Add tests
  for (int i = 0; i < nTests; i++) {
    auto filename = g_string_new(tests[i]);
    if (strstr(filename->str, ".kmx") || strstr(filename->str, ".kmn")) {
      g_string_truncate(filename, filename->len - 4);
    }

    // Check for tests to skip - #3345
    const char *skipReason = NULL;
    if (strcmp(filename->str, "k_026___system_stores") == 0 ||
        strcmp(filename->str, "k_027___system_stores_2") == 0) {
      skipReason = "mnemonic keyboards are not yet supported on Linux (#3345)";
    }

    if (runSurroundingTextTests) {
      add_test(directory, filename->str, TRUE, skipReason, use_wayland);
    }

    if (runNoSurroundingTextTests) {
      add_test(directory, filename->str, FALSE, skipReason, use_wayland);
    }

    g_string_free(filename, TRUE);
  }

  // Run tests
  int retVal = km_g_test_run();

  test_module_unuse(module);
  return retVal;
}
