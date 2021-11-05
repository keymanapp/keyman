#include <glib-object.h>
#include <glib.h>
#include <ibus.h>
#include <iostream>
#include <keyman/keyboardprocessor.h>
#include <kmx/kmx_processevent.h>
#include <linux/input-event-codes.h>
#include <list>
#include <memory>
#include <stack>
#include <stdexcept>
#include <string>
#include "ibusimcontext.h"
#include "keycodes.h"
#include "keymanutil.h"
#include "kmx_test_source.hpp"
#include "testmodule.h"

typedef struct {
  IBusBus *bus;
  GtkIMContext *context;
  IBusIMContext *ibuscontext;
} IBusKeymanTestsFixture;

typedef struct {
  char *test_name;
  char *test_path;
} TestData;

static gboolean loaded        = FALSE;
static GdkWindow *window      = NULL;
static GMainLoop *thread_loop = NULL;
static GTypeModule *module    = NULL;

static void
module_register(GTypeModule *module) {
  ibus_im_context_register_type(module);
}

void
destroy(GtkWidget *widget, gpointer data) {
  gtk_main_quit();
}

static void
ibus_keyman_tests_fixture_set_up(IBusKeymanTestsFixture *fixture, gconstpointer user_data) {
  IBusIMContextClass *classClass;

  if (!window) {
    GtkWidget *widget = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_signal_connect(widget, "destroy", G_CALLBACK(destroy), NULL);
    window = gtk_widget_get_window(widget);
  }

  if (!loaded) {
    ibus_init();

    module = test_module_new(module_register);
    g_assert_nonnull(module);

    /* Not loaded until we call ref for the first time */
    classClass = static_cast<IBusIMContextClass *>(g_type_class_peek(IBUS_TYPE_IM_CONTEXT));
    g_assert_null(classClass);

    loaded = TRUE;
  }

  fixture->bus = ibus_bus_new();
}

static void
ibus_keyman_tests_fixture_tear_down(IBusKeymanTestsFixture *fixture, gconstpointer user_data) {
  if (thread_loop) {
    g_main_loop_unref(thread_loop);
    thread_loop = NULL;
  }

  g_clear_object(&fixture->bus);
  g_clear_object(&fixture->context);
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

static unsigned short vk_to_keycode(unsigned short vk) {
  for (int i = 0; i < sizeof(keycode_to_vk); i++) {
    if (keycode_to_vk[i] == vk)
      return i;
  }
  return -1;
}

#define KEYMAN_LCTRL 29  // 0x1D
#define KEYMAN_LALT 56   // 0x38
#define KEYMAN_RCTRL 97  // 0x61
#define KEYMAN_RALT 100  // 0x64

static guint
km_modifiers_to_ibus(km::tests::KmxTestSource& test_source, km::tests::key_event event) {
  guint result = 0;
  auto state   = event.modifier_state | test_source.caps_lock_state();
  if (state & KM_KBP_MODIFIER_SHIFT) {
    result |= IBUS_SHIFT_MASK;
  }
  if (state & KM_KBP_MODIFIER_ALT) {
    result |= IBUS_MOD1_MASK;
  }
  if (state & KM_KBP_MODIFIER_RALT) {
    if (event.vk == KEYMAN_RALT)
      result |= IBUS_MOD1_MASK;
    else
      result |= IBUS_MOD5_MASK;
  }
  if (state & KM_KBP_MODIFIER_LALT) {
    result |= IBUS_MOD1_MASK;
  }
  if (state & KM_KBP_MODIFIER_CTRL) {
    result |= IBUS_CONTROL_MASK;
  }
  if (state & KM_KBP_MODIFIER_RCTRL) {
    result |= IBUS_CONTROL_MASK;
  }
  if (state & KM_KBP_MODIFIER_LCTRL) {
    result |= IBUS_CONTROL_MASK;
  }
  if (state & KM_KBP_MODIFIER_CAPS) {
    result |= IBUS_LOCK_MASK;
  }
  return result;
}

typedef struct {
  guint modifiers_keydown;
  guint modifiers_keyup;
  guint keyval;
  guint16 keycode;
  guint is_modifier;
} gdk_key_event;

static gdk_key_event
get_key_event_for_modifier(guint modifier, guint & prev_modifiers, guint keyval, guint keycode) {
  gdk_key_event event;
  event.modifiers_keydown = prev_modifiers;
  event.modifiers_keyup   = prev_modifiers | modifier | IBUS_RELEASE_MASK;
  prev_modifiers |= modifier;
  event.keyval  = keyval;
  event.keycode = keycode;
  event.is_modifier = 1;
  return event;
}

static std::list<gdk_key_event>
get_involved_keys(km::tests::KmxTestSource &test_source, km::tests::key_event test_event) {
  guint modifiers = 0;
  if (test_source.caps_lock_state()) {
    modifiers = IBUS_LOCK_MASK;
  }

  // process all modifiers
  std::list<gdk_key_event> result;
  if (test_event.modifier_state & KM_KBP_MODIFIER_SHIFT) {
    // we don't distinguish between R and L Shift
    result.push_back(get_key_event_for_modifier(IBUS_SHIFT_MASK, modifiers, GDK_KEY_Shift_L, KEY_LEFTSHIFT));
  }
  if (test_event.modifier_state & KM_KBP_MODIFIER_ALT || test_event.modifier_state & KM_KBP_MODIFIER_LALT) {
    result.push_back(get_key_event_for_modifier(IBUS_MOD1_MASK, modifiers, GDK_KEY_Alt_L, KEY_LEFTALT));
  }
  if (test_event.modifier_state & KM_KBP_MODIFIER_RALT) {
    if (test_event.vk == KEYMAN_RALT)
      result.push_back(get_key_event_for_modifier(IBUS_MOD1_MASK, modifiers, GDK_KEY_Alt_R, KEY_RIGHTALT));
    else
      result.push_back(get_key_event_for_modifier(IBUS_MOD5_MASK, modifiers, GDK_KEY_Alt_R, KEY_RIGHTALT));
  }
  if (test_event.modifier_state & KM_KBP_MODIFIER_CTRL || test_event.modifier_state & KM_KBP_MODIFIER_LCTRL) {
    result.push_back(get_key_event_for_modifier(IBUS_CONTROL_MASK, modifiers, GDK_KEY_Control_L, KEY_LEFTCTRL));
  }
  if (test_event.modifier_state & KM_KBP_MODIFIER_RCTRL) {
    result.push_back(get_key_event_for_modifier(IBUS_CONTROL_MASK, modifiers, GDK_KEY_Control_R, KEY_RIGHTCTRL));
  }

  // process key
  guint keyval = (test_event.modifier_state & KM_KBP_MODIFIER_SHIFT) ? gdk_keyval_to_upper(test_event.vk)
                                                                     : gdk_keyval_to_lower(test_event.vk);
  gdk_key_event event;
  event.modifiers_keydown = modifiers;
  event.modifiers_keyup   = modifiers | IBUS_RELEASE_MASK;
  event.keyval            = keyval;
  event.keycode           = vk_to_keycode(test_event.vk);
  event.is_modifier       = 0;
  result.push_back(event);

  return result;
}

static std::list<km::tests::key_event>
get_context_keys(std::u16string context) {
  std::list<km::tests::key_event> result;
  for (auto c = context.begin(); c != context.end(); c++) {
    gchar utf8[6];
    if (g_unichar_to_utf8((gunichar)*c, utf8) != 1 || utf8[0] < 32 || utf8[0] > 127) {
      g_assertion_message_cmpstr(
          G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC, "We can only deal with ASCII characters in the context", utf8, "", "");
    }
    result.push_back(
        {km::kbp::kmx::s_char_to_vkey[(int)utf8[0] - 32].vk,
         (uint16_t)(km::kbp::kmx::s_char_to_vkey[(int)utf8[0] - 32].shifted ? KM_KBP_MODIFIER_SHIFT : 0)});
  }
  return result;
}

static void
press_keys(IBusKeymanTestsFixture *fixture, km::tests::KmxTestSource & test_source,km::tests::key_event key_event) {
  auto involved_keys = get_involved_keys(test_source, key_event);

  // press and hold the individual modifier keys, finally the actual key
  std::stack<GdkEventKey> keys_to_release;
  for (auto key = involved_keys.begin(); key != involved_keys.end(); key++) {
    GdkEventKey keyEvent = {
        .type             = GDK_KEY_PRESS,
        .window           = window,
        .send_event       = 0,
        .time             = 0,
        .state            = key->modifiers_keydown,
        .keyval           = key->keyval,
        .length           = 0,
        .string           = NULL,
        .hardware_keycode = key->keycode,
        .group            = 0,
        .is_modifier      = key->is_modifier};
    gtk_im_context_filter_keypress(fixture->context, &keyEvent);

    keyEvent.type  = GDK_KEY_RELEASE;
    keyEvent.state = key->modifiers_keyup;
    keys_to_release.push(keyEvent);
  }

  // then release the keys in reverse order
  while (!keys_to_release.empty()) {
    auto keyEvent = keys_to_release.top();
    gtk_im_context_filter_keypress(fixture->context, &keyEvent);
    keys_to_release.pop();
  }
}

static void test_source(IBusKeymanTestsFixture *fixture, gconstpointer user_data) {
  auto data       = (TestData*)user_data;
  auto sourcefile = string_format("%s.kmn", data->test_path);
  auto kmxfile    = string_format("und:%s.kmx", data->test_path);

  km::tests::KmxTestSource test_source;
  std::string keys        = "";
  std::u16string expected = u"", context = u"";
  km::tests::kmx_options options;
  bool expected_beep = false;
  g_assert_cmpint(test_source.load_source(sourcefile.c_str(), keys, expected, context, options, expected_beep), ==, 0);

  for (auto & option : options) {
    if (option.type == km::tests::KOT_INPUT) {
      auto key = g_utf16_to_utf8((gunichar2 *)option.key.c_str(), option.key.length(), NULL, NULL, NULL);
      auto value = g_utf16_to_utf8((gunichar2 *)option.value.c_str(), option.value.length(), NULL, NULL, NULL);
      keyman_put_options_todconf(data->test_name, data->test_name, key, value);
    }
  }

  switch_keyboard(fixture, kmxfile.c_str());

  auto contextKeys = get_context_keys(context);
  for (auto k = contextKeys.begin(); k != contextKeys.end(); k++) {
    press_keys(fixture, test_source, *k);
  }

  gtk_im_context_focus_in(fixture->context);

  for (auto p = test_source.next_key(keys); p.vk != 0; p = test_source.next_key(keys)) {
    press_keys(fixture, test_source, p);
  }

  auto expectedText = g_utf16_to_utf8((gunichar2*)expected.c_str(), expected.length(), NULL, NULL, NULL);
  g_assert_cmpstr(ibus_im_test_get_text(fixture->ibuscontext), ==, expectedText);
}

void
print_usage() {
  printf("Usage: %s --directory <keyboarddir> test1 [test2 ...]", g_get_prgname());
}

int
main(int argc, char *argv[]) {
  gtk_init(&argc, &argv);
  g_test_init(&argc, &argv, NULL);
  g_test_set_nonfatal_assertions();

  if (argc < 4) {
    print_usage();
    return 1;
  }

  if (strcmp(argv[1], "--directory") != 0) {
    print_usage();
    return 2;
  }

  char *directory = argv[2];
  int nTests      = argc - 3;
  char **tests    = &argv[3];

  // Add tests
  for (int i = 0; i < nTests; i++)
  {
    auto filename = tests[i];
    if (strstr(filename, ".kmx") || strstr(filename, ".kmn")) {
      filename[strlen(filename) - 4] = '\0';
    }
    auto file     = g_file_new_for_commandline_arg(filename);
    auto testfilebase = g_file_get_basename(file);
    auto testname = g_string_new(NULL);
    g_string_append_printf(testname, "/%s", testfilebase);
    auto testfile = g_file_new_build_filename(directory, testfilebase, NULL);
    TestData testdata;
    testdata.test_name = filename;
    testdata.test_path = g_file_get_parse_name(testfile);
    g_test_add(
        testname->str, IBusKeymanTestsFixture, &testdata, ibus_keyman_tests_fixture_set_up, test_source,
        ibus_keyman_tests_fixture_tear_down);
    g_object_unref(file);
    g_object_unref(testfile);
    g_string_free(testname, TRUE);
  }

  // Run tests
  int retVal = g_test_run();

  test_module_unuse(module);
  return retVal;
}
