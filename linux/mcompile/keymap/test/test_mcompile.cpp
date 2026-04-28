#include <gtest/gtest.h>
#include <vector>
#include <gdk/gdk.h>
#include <giomm-2.4/giomm.h>
#include <glibmm-2.4/glibmm.h>
#include <iostream>
#include <string>
#include <cstring>
#include <cstdlib>
#include <cstdint>
#include "mcompile.h"
#include "keymap.h"

class KeyboardConversionTest : public ::testing::Test {

public:

protected:
  GdkKeymap* test_keymap;
  GdkDisplay* test_display;
  gint argc = 0;
  char** argv = nullptr;
  Glib::ustring default_layout;

private:
  void initialize_keymap() {
    gdk_init(&argc, &argv);
    test_display = nullptr;
    test_keymap = nullptr;
    test_display = gdk_display_get_default();
    ASSERT_NE(test_display, nullptr) << "ERROR: can't get display";
    test_keymap = gdk_keymap_get_for_display(test_display);
    ASSERT_NE(test_keymap, nullptr) << "ERROR: Can't get keymap";
  }


  void get_default_layout() {
    std::vector<std::tuple<Glib::ustring, Glib::ustring>> sources;

    Gio::init();
    auto settings = Gio::Settings::create("org.gnome.desktop.input-sources");
    Glib::VariantBase base;
    settings->get_value("sources", base);
    using SourcesVariant = Glib::Variant<std::vector<std::tuple<Glib::ustring, Glib::ustring>>>;
    auto variant = Glib::VariantBase::cast_dynamic<SourcesVariant>(base);
    sources = variant.get();

    ASSERT_FALSE(sources.empty()) << "ERROR: No input sources found";

    const auto& [type, layout] = sources[0];

    std::cout << "Default input source type: " << type << ", layout: " << layout << std::endl;
    ASSERT_EQ(type, "xkb");
    if (layout == "de" || layout == "us") {
      default_layout = layout;
    }
  }

  void SetUp() override {
    initialize_keymap();
    get_default_layout();
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

guint keycodes[] = { 38, 56, 54, 40, 26, 41, 42, 43, 31, 44, 45, 46, 58, 57, 32,
                     33, 24, 27, 39, 28, 30, 55, 25, 53, 29, 52, 19, 10, 11, 12,
                     13, 14, 15, 16, 17, 18, 65, 49, 20, 21, 34, 35, 51, 47, 48,
                     59, 60, 61, 123, 94};


TEST_F(KeyboardConversionTest, KMXgetKeyValUnderlyingFromKeyCodeUnderlyingBase) {

  if (default_layout != "us") {
    GTEST_SKIP() << "Default layout is not US.";
  }

  KMX_DWORD expected_chars[] = {u'a', u'b', u'c', u'd', u'e', u'f', u'g', u'h', u'i',
                                u'j', u'k', u'l', u'm', u'n', u'o', u'p', u'q', u'r',
                                u's', u't', u'u', u'v', u'w', u'x', u'y', u'z', u'0',
                                u'1', u'2', u'3', u'4', u'5', u'6', u'7', u'8', u'9',
                                u' ', u'`', u'-', u'=', u'[', u']', u'\\', u';', u'\'',
                                u',', u'.', u'/', u'\000', u'<'};

  KMX_WCHAR deadkey;
  for (uint k = 0; k < sizeof(keycodes)/sizeof(keycodes[0]); k++) {
    KMX_WCHAR result = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(
      test_keymap,
      keycodes[k],
      0,
      &deadkey
    );
    EXPECT_EQ(result, expected_chars[k]) << "Failed for keycode: " << keycodes[k];
  }
}


TEST_F(KeyboardConversionTest, KMXgetKeyValUnderlyingFromKeyCodeUnderlyingShift) {
  // Test with valid key code - should return character or deadkey

  if (default_layout != "us") {
    GTEST_SKIP() << "Default layout is not US.";
  }
  KMX_DWORD expected_chars[] = { u'A', u'B', u'C', u'D', u'E', u'F', u'G', u'H', u'I',
                                 u'J', u'K', u'L', u'M', u'N', u'O', u'P', u'Q', u'R',
                                 u'S', u'T', u'U', u'V', u'W', u'X', u'Y', u'Z', u')',
                                 u'!', u'@', u'#', u'$', u'%', u'^', u'&', u'*', u'(',
                                 u' ', u'~', u'_', u'+', u'{', u'}', u'|', u':', u'"',
                                 u'<', u'>', u'?', u'\000', u'>'};

  KMX_WCHAR deadkey;
  for (uint k = 0; k < sizeof(keycodes)/sizeof(keycodes[0]); k++) {
    KMX_WCHAR result = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(
      test_keymap,
      keycodes[k],
      K_SHIFTFLAG,
      &deadkey
    );
    EXPECT_EQ(result, expected_chars[k]) << "Failed for keycode: " << keycodes[k] << " with shift state";
  }
}


TEST_F(KeyboardConversionTest, KMXgetKeyValUnderlyingFromKeyCodeUnderlyingLCTRLFLAGRALTFLAG) {
  // Test with valid key code - should return character or deadkey

  if (default_layout != "us") {
    GTEST_SKIP() << "Default layout is not US.";
  }

  KMX_DWORD expected_chars[] = {u'a', u'b', u'c', u'd', u'e', u'f', u'g', u'h', u'i', u'j', u'k',
                                u'l', u'm', u'n', u'o', u'p', u'q', u'r', u's', u't', u'u', u'v',
                                u'w', u'x', u'y', u'z', u'0', u'1', u'2', u'3', u'4', u'5', u'6',
                                u'7', u'8', u'9', u'\000', u'`', u'-', u'=', u'[', u']', u'\\',
                                u';', u'\'', u',', u'.', u'/', u'\000', u'|'};
  KMX_WCHAR deadkey;

  for (uint k = 0; k < sizeof(keycodes)/sizeof(keycodes[0]); k++) {
    KMX_WCHAR result = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(
      test_keymap,
      keycodes[k],
      LCTRLFLAG | RALTFLAG,
      &deadkey
    );
    EXPECT_EQ(result, expected_chars[k]) << "Failed for keycode: " << keycodes[k];
  }
}


TEST_F(KeyboardConversionTest, GetKeySymFromUnderlyingKeyCodeShiftLCTRLFLAGRALTFLAG) {
  if (default_layout != "us") {
    GTEST_SKIP() << "Default layout is not US.";
  }

  KMX_DWORD expected_chars[] = {u'A', u'B', u'C', u'D', u'E', u'F', u'G', u'H', u'I', u'J', u'K',
                                u'L', u'M', u'N', u'O', u'P', u'Q', u'R', u'S', u'T', u'U', u'V',
                                u'W', u'X', u'Y', u'Z', u')', u'!', u'@', u'#', u'$', u'%', u'^',
                                u'&', u'*', u'(', u'\000', u'~', u'_', u'+', u'{', u'}', u'|',
                                u':', u'"', u'<', u'>', u'?', u'\000', u'¦'};

  KMX_WCHAR deadkey;

  for (uint k = 0; k < sizeof(keycodes)/sizeof(keycodes[0]); k++) {
    KMX_WCHAR result = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(
      test_keymap,
      keycodes[k],
      K_SHIFTFLAG | LCTRLFLAG | RALTFLAG,
      &deadkey
    );
    EXPECT_EQ(result, expected_chars[k]) << "Failed for keycode: " << keycodes[k];
  }
}


TEST_F(KeyboardConversionTest, DeGetKeySymFromUnderlyingKeyCode) {
  if (default_layout != "de") {
    GTEST_SKIP() << "Default layout is not US.";
  }

  KMX_DWORD expected_chars[] = {u'a', u'b', u'c', u'd', u'e', u'f', u'g', u'h', u'i', u'j',
                                u'k', u'l', u'm', u'n', u'o', u'p', u'q', u'r', u's', u't',
                                u'u', u'v', u'w', u'x', u'z', u'y', u'0', u'1', u'2', u'3',
                                u'4', u'5', u'6', u'7', u'8', u'9', u' ', u'\xffff', u'ß',
                                u'\xffff', u'ü', u'+', u'#', u'ö', u'ä', u',', u'.', u'-',
                                u'\000', u'<', };

  KMX_WCHAR deadkey;

  std::cout << "Testing KMX_get_KeyValUnderlying_From_KeyCodeUnderlying with base shift state" << std::endl;
  for (uint k = 0; k < sizeof(keycodes)/sizeof(keycodes[0]); k++) {
    KMX_WCHAR result = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(
      test_keymap,
      keycodes[k],
      0,   // Base shift state
      &deadkey
    );
    EXPECT_EQ(result, expected_chars[k]) << "Failed for keycode: " << keycodes[k];
  }
}


TEST_F(KeyboardConversionTest, DeKMXgetKeyValUnderlyingFromKeyCodeUnderlyingShift) {
  if (default_layout != "de") {
    GTEST_SKIP() << "Default layout is not US.";
  }

  KMX_DWORD expected_chars[] = { u'A', u'B', u'C', u'D', u'E', u'F', u'G', u'H', u'I',
                                 u'J', u'K', u'L', u'M', u'N', u'O', u'P', u'Q', u'R',
                                 u'S', u'T', u'U', u'V', u'W', u'X', u'Z', u'Y', u'=',
                                 u'!', u'"', u'§', u'$', u'%', u'&', u'/', u'(', u')',
                                 u' ', u'°', u'?', u'\xffff', u'Ü', u'*', u'\'', u'Ö',
                                 u'Ä', u';', u':', u'_', u'\000', u'>', };

  KMX_WCHAR deadkey;

   for (uint k = 0; k < sizeof(keycodes)/sizeof(keycodes[0]); k++) {
     KMX_WCHAR result = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(
       test_keymap,
       keycodes[k],
      K_SHIFTFLAG,
      &deadkey
    );
    EXPECT_EQ(result, expected_chars[k]) << "Failed for keycode: " << keycodes[k] << " with shift state";
  }
}


TEST_F(KeyboardConversionTest, DeKMXgetKeyValUnderlyingFromKeyCodeUnderlyingLCTRLFLAGRALTFLAG) {
  // Test with valid key code - should return character or deadkey
  if (default_layout != "de") {
    GTEST_SKIP() << "Default layout is not US.";
  }

  /*KMX_DWORD expected_chars[] = {u'æ', u'\xad2', u'¢', u'ð', u'€', u'ǰ', u'ο', u'ʱ', u'ࣽ', u'\xffff',
                                u'\x3a2', u'Ƴ', u'µ', u'\xad3', u'ø', u'þ', u'@', u'¶', u'ſ', u'μ',
                                u'ࣾ', u'૾', u'ſ', u'«', u'ࣻ', u'»', u'}', u'¹', u'²', u'³', u'¼', u'½',
                                u'¬', u'{', u'[', u']', u'\000', u'′', u'\\', u'\xffff', u'\xffff',
                                u'~', u'\xad1', u'\xffff', u'\xffff', u'·', u'…', u'પ', u'\000', u'|',};
  */
  KMX_DWORD expected_chars[] = {u'æ', u'\xfffe', u'¢', u'ð', u'\xfffe', u'\xfffe', u'\xfffe', u'\xfffe',
                                u'\xfffe', u'\xffff', u'\xfffe', u'\xfffe', u'µ', u'\xfffe', u'ø', u'þ',
                                u'@', u'¶', u'\xfffe', u'\xfffe', u'\xfffe', u'\xfffe', u'\xfffe', u'«',
                                u'\xfffe', u'»', u'}', u'¹', u'²', u'³', u'¼', u'½', u'¬', u'{', u'[',
                                u']', u'\000', u'\xfffe', u'\\', u'\xffff', u'\xffff', u'~', u'\xfffe',
                                u'\xffff', u'\xffff', u'·', u'\xfffe', u'\xfffe', u'\000', u'|', };

  KMX_WCHAR deadkey;

  for (uint k = 0; k < sizeof(keycodes)/sizeof(keycodes[0]); k++) {
    KMX_WCHAR result = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(
      test_keymap,
      keycodes[k],
      LCTRLFLAG | RALTFLAG,
      &deadkey
    );
    EXPECT_EQ(result, expected_chars[k]) << "Failed for keycode: " << keycodes[k];
  }
}


TEST_F(KeyboardConversionTest, DeGetKeySymFromUnderlyingKeyCodeShiftLCTRLFLAGRALTFLAG) {
  if (default_layout != "de") {
    GTEST_SKIP() << "Default layout is not US.";
  }

  KMX_DWORD expected_chars[] = {u'Æ', u'\xfffe', u'©', u'Ð', u'\xfffe', u'ª', u'\xfffe',
                                u'\xfffe', u'\xfffe', u'\xffff', u'&', u'\xfffe', u'º',
                                u'\xfffe', u'Ø', u'Þ', u'\xfffe', u'®', u'\xfffe', u'\xfffe',
                                u'\xfffe', u'\xfffe', u'§', u'\xfffe', u'¥', u'\xfffe', u'°',
                                u'¡', u'\xfffe', u'£', u'¤', u'\xfffe', u'\xfffe', u'\xfffe',
                                u'\xfffe', u'±', u'\000', u'\xfffe', u'¿', u'\xffff', u'\xffff',
                                u'¯', u'\xffff', u'\xffff', u'\xffff', u'×', u'÷', u'\xfffe',
                                u'\000', u'\xffff', };

  KMX_WCHAR deadkey;

  for (uint k = 0; k < sizeof(keycodes)/sizeof(keycodes[0]); k++) {
    KMX_WCHAR result = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(
      test_keymap,
      keycodes[k],
      K_SHIFTFLAG | LCTRLFLAG | RALTFLAG,
      &deadkey
    );
    EXPECT_EQ(result, expected_chars[k]) << "Failed for keycode: " << keycodes[k];
  }
}
