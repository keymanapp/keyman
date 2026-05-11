/*
 * Keyman is copyright (C)  SIL Global. MIT License.
 *
 * Mnemonic layout support for Linux
 */

#include <gtest/gtest.h>
#include <vector>
#include <gdk/gdk.h>
#include <giomm-2.4/giomm.h>
#include <glibmm-2.4/glibmm.h>
#include <gdk/gdkx.h>
#include <iostream>
#include <string>
#include <cstring>
#include <cstdlib>
#include <cstdint>
#include "mcompile.h"
#include "keymap.h"

class TestDataValues {
protected:
  guint keycode;
  KMX_WCHAR expected_char;
  std::string layout;
  guint shiftstate;

public:
  TestDataValues(guint k, KMX_WCHAR e, std::string l, guint s) : keycode(k), expected_char(e), layout(l), shiftstate(s) {
  }

  guint get_keycode() {
    return keycode;
  }

  KMX_WCHAR get_expected_char() {
    return expected_char;
  }

  std::string get_layout() {
    return layout;
  }

  guint get_shiftstate() {
    return shiftstate;
  }
};

class KeyboardTestParameters {
public:
  KeyboardTestParameters(std::vector<KMX_WCHAR> e, std::string l, guint s) : expected_keysyms(e), layout(l), shiftstate(s) {
    generate_test_data_values();
  }

  std::vector<TestDataValues> get_test_data() {
    return test_data_values;
  }

protected:
  std::vector<KMX_WCHAR> expected_keysyms;
  std::vector<TestDataValues> test_data_values = {};
  std::string layout;
  guint shiftstate;
  std::vector<guint> keycodes = {38, 56, 54, 40, 26, 41, 42, 43, 31, 44,
                                 45, 46, 58, 57, 32, 33, 24, 27, 39, 28,
                                 30, 55, 25, 53, 29, 52, 19, 10, 11, 12,
                                 13, 14, 15, 16, 17, 18, 65, 49, 20, 21,
                                 34, 35, 51, 47, 48, 59, 60, 61, 123, 94};

  void generate_test_data_values() {
    EXPECT_EQ(keycodes.size(), expected_keysyms.size()) << "Keycodes and expected keysyms vectors must be of the same size.";
    for (uint k = 0; k < keycodes.size() && k < expected_keysyms.size(); k++) {
      test_data_values.emplace_back(TestDataValues(keycodes[k], expected_keysyms[k], layout, shiftstate));
    }
  }
};

class KeyboardConversionTest : public testing::Test {
public:
protected:
  GdkKeymap* test_keymap;
  GdkDisplay* test_display;
  gint argc   = 0;
  char** argv = nullptr;
  std::string default_layout;

private:
  void initialize_keymap() {
    gdk_init(&argc, &argv);
    test_display = nullptr;
    test_keymap  = nullptr;
    test_display = gdk_display_get_default();
    ASSERT_NE(test_display, nullptr) << "ERROR: can't get display";
    test_keymap = gdk_keymap_get_for_display(test_display);
    ASSERT_NE(test_keymap, nullptr) << "ERROR: Can't get keymap";
  }

  void retrieve_default_layout() {
    std::vector<std::tuple<Glib::ustring, Glib::ustring>> sources;

    Gio::init();
    auto settings = Gio::Settings::create("org.gnome.desktop.input-sources");
    Glib::VariantBase base;
    settings->get_value("sources", base);
    using SourcesVariant = Glib::Variant<std::vector<std::tuple<Glib::ustring, Glib::ustring>>>;
    auto variant         = Glib::VariantBase::cast_dynamic<SourcesVariant>(base);
    sources              = variant.get();

    if (sources.empty()) {
      GTEST_SKIP() << "ERROR: No input sources found";
    }

    const auto& [type, system_layout] = sources[0];

    if (type == "xkb") {
      if (system_layout == "de" || system_layout == "us") {
        default_layout = system_layout;
      } else {
        GTEST_SKIP() << "Default layout is not DE or US.";
      }
    } else {
      GTEST_SKIP() << "Default input source type is not xkb.";
    }
  }

  void SetUp() override {
    initialize_keymap();
    retrieve_default_layout();
    if (!GDK_IS_X11_DISPLAY(test_display)) {
      GTEST_SKIP() << "Not running on X11 display, skipping tests that require X11 keymap functionality.";
    }
  }

  void TearDown() override {
    default_layout.clear();
    argc = 0;
    free(argv);
    argv = nullptr;
    if (test_display) {
      gdk_display_close(test_display);
      test_display = nullptr;
    }
  }
};

class GetKeyValUnderlyingFromKeyCodeUnderlyingTest : public KeyboardConversionTest,
                                                     public testing::WithParamInterface<TestDataValues> {};

TEST_P(GetKeyValUnderlyingFromKeyCodeUnderlyingTest, KmxGetKeyValUnderlyingFromKeyCodeUnderlying) {
  guint keycode;
  KMX_WCHAR expected_char;
  std::string test_layout;
  guint shiftstate;
  TestDataValues parameter = GetParam();

  keycode       = parameter.get_keycode();
  expected_char = parameter.get_expected_char();
  test_layout   = parameter.get_layout();
  shiftstate    = parameter.get_shiftstate();

  std::cout << "Testing keycode: " << keycode << " expecting char: " << expected_char << " with layout: " << test_layout
            << " and shiftstate: " << shiftstate << std::endl;
  if (test_layout != default_layout) {
    GTEST_SKIP() << "Default layout is not " << default_layout << ".";
  }

  KMX_WCHAR deadkey;
  KMX_WCHAR result = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(test_keymap, keycode, shiftstate, &deadkey);
  EXPECT_EQ(result, expected_char) << "Failed for keycode: " << keycode;
}

INSTANTIATE_TEST_SUITE_P(
    BaseUs,
    GetKeyValUnderlyingFromKeyCodeUnderlyingTest,
    testing::ValuesIn(KeyboardTestParameters(
                          {u'a', u'b', u'c', u'd', u'e', u'f', u'g', u'h', u'i', u'j',
                           u'k', u'l', u'm', u'n', u'o', u'p', u'q', u'r', u's', u't',
                           u'u', u'v', u'w', u'x', u'y', u'z', u'0', u'1', u'2', u'3',
                           u'4', u'5', u'6', u'7', u'8', u'9', u' ', u'`', u'-', u'=',
                           u'[', u']', u'\\', u';', u'\'', u',', u'.', u'/', u'\000', u'<'},
                          "us",
                          0)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    ShiftUs,
    GetKeyValUnderlyingFromKeyCodeUnderlyingTest,
    testing::ValuesIn(KeyboardTestParameters(
                          {u'A', u'B', u'C', u'D', u'E', u'F', u'G', u'H', u'I', u'J',
                           u'K', u'L', u'M', u'N', u'O', u'P', u'Q', u'R', u'S', u'T',
                           u'U', u'V', u'W', u'X', u'Y', u'Z', u')', u'!', u'@', u'#',
                           u'$', u'%', u'^', u'&', u'*', u'(', u' ', u'~', u'_', u'+',
                           u'{', u'}', u'|', u':', u'"', u'<', u'>', u'?', u'\000', u'>'},
                          "us",
                          K_SHIFTFLAG)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    AltGrUs,
    GetKeyValUnderlyingFromKeyCodeUnderlyingTest,
    testing::ValuesIn(KeyboardTestParameters(
                          {u'a', u'b', u'c', u'd',  u'e', u'f',u'g', u'h', u'i', u'j',
                           u'k', u'l', u'm', u'n', u'o', u'p', u'q', u'r', u's', u't',
                           u'u', u'v', u'w', u'x', u'y', u'z', u'0', u'1', u'2', u'3',
                           u'4', u'5', u'6', u'7', u'8', u'9', u'\000', u'`', u'-', u'=',
                           u'[', u']', u'\\', u';', u'\'', u',', u'.', u'/', u'\000', u'|'},
                          "us",
                          (LCTRLFLAG | RALTFLAG))
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    ShiftAltGrUs,
    GetKeyValUnderlyingFromKeyCodeUnderlyingTest,
    testing::ValuesIn(KeyboardTestParameters(
                          {u'A', u'B', u'C', u'D', u'E', u'F', u'G', u'H', u'I', u'J',
                           u'K', u'L', u'M', u'N', u'O', u'P', u'Q', u'R', u'S', u'T',
                           u'U', u'V', u'W', u'X', u'Y', u'Z', u')', u'!', u'@', u'#',
                           u'$', u'%', u'^', u'&', u'*', u'(', u'\000', u'~', u'_', u'+',
                           u'{', u'}', u'|', u':', u'"', u'<', u'>', u'?', u'\000', u'¦'},
                          "us",
                          (K_SHIFTFLAG | LCTRLFLAG | RALTFLAG))
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    BaseDe,
    GetKeyValUnderlyingFromKeyCodeUnderlyingTest,
    testing::ValuesIn(KeyboardTestParameters(
                          {u'a', u'b', u'c', u'd', u'e', u'f', u'g', u'h', u'i', u'j',
                           u'k', u'l', u'm', u'n', u'o', u'p', u'q', u'r', u's', u't',
                           u'u', u'v', u'w', u'x', u'z', u'y', u'0', u'1', u'2', u'3',
                           u'4', u'5', u'6', u'7', u'8', u'9', u' ', u'\xffff', u'ß', u'\xffff',
                           u'ü', u'+', u'#', u'ö', u'ä', u',', u'.', u'-', u'\000', u'<'},
                          "de",
                          0)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    ShiftDe,
    GetKeyValUnderlyingFromKeyCodeUnderlyingTest,
    testing::ValuesIn(KeyboardTestParameters(
                          {u'A', u'B', u'C', u'D', u'E', u'F', u'G', u'H', u'I', u'J',
                           u'K', u'L', u'M', u'N', u'O', u'P', u'Q', u'R', u'S', u'T',
                           u'U', u'V', u'W', u'X', u'Z', u'Y', u'=', u'!', u'"', u'§',
                           u'$', u'%', u'&', u'/', u'(', u')', u' ', u'°', u'?', u'\xffff',
                           u'Ü', u'*', u'\'', u'Ö', u'Ä', u';', u':', u'_', u'\000', u'>'},
                          "de",
                          K_SHIFTFLAG)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    AltGrDe,
    GetKeyValUnderlyingFromKeyCodeUnderlyingTest,
    testing::ValuesIn(KeyboardTestParameters(
                          {u'æ',      u'\xfffe', u'¢',      u'ð',      u'\xfffe', u'\xfffe', u'\xfffe', u'\xfffe', u'\xfffe', u'\xffff',
                           u'\xfffe', u'\xfffe', u'µ',      u'\xfffe', u'ø',      u'þ',      u'@',      u'¶',      u'\xfffe', u'\xfffe',
                           u'\xfffe', u'\xfffe', u'\xfffe', u'«',      u'\xfffe', u'»',      u'}',      u'¹',      u'²',      u'³',
                           u'¼',      u'½',      u'¬',      u'{',      u'[',      u']',      u'\000',   u'\xfffe', u'\\',     u'\xffff',
                           u'\xffff', u'~',      u'\xfffe', u'\xffff', u'\xffff', u'·',      u'\xfffe', u'\xfffe', u'\000',   u'|'},
                          "de",
                          (LCTRLFLAG | RALTFLAG))
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    ShiftAltGrDe,
    GetKeyValUnderlyingFromKeyCodeUnderlyingTest,
    testing::ValuesIn(KeyboardTestParameters(
                          {u'Æ',      u'\xfffe', u'©',      u'Ð',      u'\xfffe', u'ª',      u'\xfffe', u'\xfffe', u'\xfffe', u'\xffff',
                           u'&',      u'\xfffe', u'º',      u'\xfffe', u'Ø',      u'Þ',      u'\xfffe', u'®',      u'\xfffe', u'\xfffe',
                           u'\xfffe', u'\xfffe', u'§',      u'\xfffe', u'¥',      u'\xfffe', u'°',      u'¡',      u'\xfffe', u'£',
                           u'¤',      u'\xfffe', u'\xfffe', u'\xfffe', u'\xfffe', u'±',      u'\000',   u'\xfffe', u'¿',      u'\xffff',
                           u'\xffff', u'¯',      u'\xffff', u'\xffff', u'\xffff', u'×',      u'÷',      u'\xfffe', u'\000',   u'\xffff'},
                          "de",
                          (K_SHIFTFLAG | LCTRLFLAG | RALTFLAG))
                          .get_test_data()));

class GetKeyValFromKeyCodeTestDataValues : public TestDataValues {
public:
  GetKeyValFromKeyCodeTestDataValues(guint k, KMX_WCHAR e, std::string l, guint s, int c) : TestDataValues(k, e, l, s), caps(c) {
  }

  int get_caps() {
    return caps;
  }

protected:
  int caps;
};

class GetKeyValFromKeyCodeTestParameters {
public:
  GetKeyValFromKeyCodeTestParameters(std::vector<KMX_DWORD> e, std::string l, guint s, int c)
      : expected_keysyms(e), layout(l), shiftstate(s), caps(c) {
    generate_test_data_values();
  }

  std::vector<GetKeyValFromKeyCodeTestDataValues> get_test_data() {
    return test_data_values;
  }

protected:
  std::vector<KMX_DWORD> expected_keysyms;
  std::vector<GetKeyValFromKeyCodeTestDataValues> test_data_values = {};
  std::string layout;
  guint shiftstate;
  int caps;
  std::vector<guint> keycodes = {38, 56, 54, 40, 26, 41, 42, 43, 31, 44,
                                 45, 46, 58, 57, 32, 33, 24, 27, 39, 28,
                                 30, 55, 25, 53, 29, 52, 19, 10, 11, 12,
                                 13, 14, 15, 16, 17, 18, 65, 49, 20, 21,
                                 34, 35, 51, 47, 48, 59, 60, 61, 123, 94};

  void generate_test_data_values() {
    EXPECT_EQ(keycodes.size(), expected_keysyms.size()) << "Keycodes and expected keysyms vectors must be of the same size.";
    for (guint k = 0; k < keycodes.size() && k < expected_keysyms.size(); k++) {
      test_data_values.emplace_back(
          GetKeyValFromKeyCodeTestDataValues(keycodes[k], expected_keysyms[k], layout, shiftstate, caps));
    }
  }
};

class GetKeyValFromKeyCodeTest : public KeyboardConversionTest,
                                 public testing::WithParamInterface<GetKeyValFromKeyCodeTestDataValues> {};

TEST_P(GetKeyValFromKeyCodeTest, kmxGetKeyValFromKeyCode) {
  guint keycode;
  KMX_WCHAR expected_char;
  std::string test_layout;
  guint shiftstate;
  int caps;

  GetKeyValFromKeyCodeTestDataValues parameter = GetParam();

  keycode       = parameter.get_keycode();
  expected_char = parameter.get_expected_char();
  test_layout   = parameter.get_layout();
  shiftstate    = parameter.get_shiftstate();
  caps          = parameter.get_caps();

  std::cout << "Testing keycode: " << keycode << " expecting char: " << expected_char << " with layout: " << test_layout
            << " and shiftstate: " << shiftstate << " caps: " << caps << std::endl;
  if (test_layout != default_layout) {
    GTEST_SKIP() << "Default layout is not " << default_layout << ".";
  }

  KMX_WCHAR keyV =
      KMX_get_KeyVal_From_KeyCode(test_keymap, keycode, ShiftState(convert_Shiftstate_to_LinuxShiftstate(shiftstate)), caps);
  EXPECT_EQ(keyV, expected_char) << "Failed for keycode: " << keycode << " keyval: " << ((KMX_WCHAR)keyV) <<  " expected_char:" << ((int)expected_char);
}

INSTANTIATE_TEST_SUITE_P(
    BaseUs,
    GetKeyValFromKeyCodeTest,
    testing::ValuesIn(GetKeyValFromKeyCodeTestParameters(
                          {u'a', u'b', u'c', u'd', u'e', u'f', u'g', u'h', u'i', u'j',
                           u'k', u'l', u'm', u'n', u'o', u'p', u'q', u'r', u's', u't',
                           u'u', u'v', u'w', u'x', u'y', u'z', u'0', u'1', u'2', u'3',
                           u'4', u'5', u'6', u'7', u'8', u'9', u' ', u'`', u'-', u'=',
                           u'[', u']', u'\\', u';', u'\'', u',', u'.', u'/', u'\000', u'<'},
                          "us",
                          0,
                          0)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    ShiftUs,
    GetKeyValFromKeyCodeTest,
    testing::ValuesIn(GetKeyValFromKeyCodeTestParameters(
                          {u'A', u'B', u'C', u'D', u'E', u'F', u'G', u'H', u'I', u'J',
                           u'K', u'L', u'M', u'N', u'O', u'P', u'Q', u'R', u'S', u'T',
                           u'U', u'V', u'W', u'X', u'Y', u'Z', u')', u'!', u'@', u'#',
                           u'$', u'%', u'^', u'&', u'*', u'(', u' ', u'~', u'_', u'+',
                           u'{', u'}', u'|', u':', u'"', u'<', u'>', u'?', u'\000', u'>'},
                          "us",
                          K_SHIFTFLAG,
                          0)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    AltGrUs,
    GetKeyValFromKeyCodeTest,
    testing::ValuesIn(GetKeyValFromKeyCodeTestParameters(
                          {u'a', u'b', u'c', u'd', u'e', u'f', u'g', u'h', u'i', u'j',
                           u'k', u'l', u'm', u'n', u'o', u'p', u'q', u'r', u's', u't',
                           u'u', u'v', u'w', u'x', u'y', u'z', u'0', u'1', u'2', u'3',
                           u'4', u'5', u'6', u'7', u'8', u'9', u'\000', u'`', u'-', u'=',
                           u'[', u']', u'\\', u';', u'\'', u',', u'.', u'/', u'\000', u'|'},
                          "us",
                          (LCTRLFLAG | RALTFLAG),
                          0)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    ShiftAltGrUs,
    GetKeyValFromKeyCodeTest,
    testing::ValuesIn(GetKeyValFromKeyCodeTestParameters(
                          {u'A', u'B', u'C', u'D', u'E', u'F', u'G', u'H', u'I', u'J',
                           u'K', u'L', u'M', u'N', u'O', u'P', u'Q', u'R', u'S', u'T',
                           u'U', u'V', u'W', u'X', u'Y', u'Z', u')', u'!', u'@', u'#',
                           u'$', u'%', u'^', u'&', u'*', u'(', u'\000', u'~', u'_', u'+',
                           u'{', u'}', u'|', u':', u'"', u'<', u'>', u'?', u'\000', u'¦'},
                          "us",
                          (K_SHIFTFLAG | LCTRLFLAG | RALTFLAG),
                          0)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    BaseDe,
    GetKeyValFromKeyCodeTest,
    testing::ValuesIn(GetKeyValFromKeyCodeTestParameters(
                          {u'a', u'b', u'c', u'd', u'e', u'f', u'g', u'h', u'i', u'j',
                           u'k', u'l', u'm', u'n', u'o', u'p', u'q', u'r', u's', u't',
                           u'u', u'v', u'w', u'x', u'z', u'y', u'0', u'1', u'2', u'3',
                           u'4', u'5', u'6', u'7', u'8', u'9', u' ', u'﹒', u'ß', u'﹑',
                           u'ü', u'+', u'#', u'ö', u'ä', u',', u'.', u'-', u'\000', u'<'},
                          "de",
                          0,
                          0)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    ShiftDe,
    GetKeyValFromKeyCodeTest,
    testing::ValuesIn(GetKeyValFromKeyCodeTestParameters(
                          {u'A', u'B', u'C', u'D', u'E', u'F', u'G', u'H', u'I', u'J',
                           u'K', u'L', u'M', u'N', u'O', u'P', u'Q', u'R', u'S', u'T',
                           u'U', u'V', u'W', u'X', u'Z', u'Y', u'=', u'!', u'"', u'§',
                           u'$', u'%', u'&', u'/', u'(', u')', u' ', u'°', u'?', u'﹐',
                           u'Ü', u'*', u'\'', u'Ö', u'Ä', u';', u':', u'_', u'\000', u'>'},
                          "de",
                          K_SHIFTFLAG,
                          0)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    AltGrDe,
    GetKeyValFromKeyCodeTest,
    testing::ValuesIn(GetKeyValFromKeyCodeTestParameters(
                          {u'æ', u'\xad2', u'¢', u'ð', u'€', u'ǰ', u'ο', u'ʱ', u'ࣽ', u'﹠',
                           u'\x3a2', u'Ƴ', u'µ', u'\xad3', u'ø', u'þ', u'@', u'¶', u'ſ', u'μ',
                           u'ࣾ', u'૾', u'ſ', u'«', u'ࣻ', u'»', u'}', u'¹', u'²', u'³',
                           u'¼', u'½', u'¬', u'{', u'[', u']', u'\000', u'′', u'\\', u'﹛',
                           u'﹗', u'~', u'\xad1', u'﹙', u'﹒', u'·', u'…', u'પ', u'\000', u'|'},
                          "de",
                          (LCTRLFLAG | RALTFLAG),
                          0)
                          .get_test_data()));

INSTANTIATE_TEST_SUITE_P(
    ShiftAltGrDe,
    GetKeyValFromKeyCodeTest,
    testing::ValuesIn(GetKeyValFromKeyCodeTestParameters(
                          {u'Æ', u'ૐ', u'©', u'Ð', u'€', u'ª', u'ν', u'ʡ', u'ʹ', u'﹖',
                           u'&', u'ƣ', u'º', u'\xad1', u'Ø', u'Þ', u'ߙ', u'®', u'ẞ', u'ά',
                           u'ࣼ', u'૽', u'§', u'‹', u'¥', u'›', u'°', u'¡', u'ૃ', u'£',
                           u'¤', u'ૄ', u'ૅ', u'\xac6', u'ૉ', u'±', u'\000', u'″', u'¿', u'﹜',
                           u'﹘', u'¯', u'﹕', u'﹠', u'﹚', u'×', u'÷', u'\xaa9', u'\000', u'﹨'},
                          "de",
                          (K_SHIFTFLAG | LCTRLFLAG | RALTFLAG),
                          0)
                          .get_test_data()));
