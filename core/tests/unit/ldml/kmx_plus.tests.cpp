#include <cstdint>
#include <gtest/gtest.h>
#include <test_assert.h>
#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include "../../../src/ldml/ldml_vkeys.hpp"
#include <iostream>
#include "ldml_test_utils.hpp"

// needed for streaming operators
#include "utfcodec.hpp"

using namespace km::core::kmx;
using namespace km_vk;

TEST(KMXPlusTest, test_COMP_KMXPLUS_KEYS_KEY) {
  COMP_KMXPLUS_KEYS_KEY e[2] = {
      {
          0x00000127,  // to = U+0127 = 295
          0x00000000   // flags: !EXTEND
      },
      {
          0x0001F640,  // to
          0x00000000   // flags: !EXTEND
      }};
  COMP_KMXPLUS_ELEM_ELEMENT elems[2] = {
      {
          0x00000127,  // to = U+0127 = 295
          0x00000000   // flags: CHAR
      },
      {
          0x0001F640,  // to
          0x00000000   // flags: CHAR
      }};
  std::u16string s0 = e[0].get_to_string();
  test_assert_equal(s0.length(), 1);
  test_assert_equal(s0.at(0), 0x0127);
  test_assert(s0 == std::u16string(u"ħ"));

  std::u16string s1 = e[1].get_to_string();
  test_assert_equal(s1.length(), 2);
  test_assert_equal(s1.at(0), 0xD83D);
  test_assert_equal(s1.at(1), 0xDE40);
  test_assert(s1 == std::u16string(u"🙀"));

  // now, elems. Parallel.
  std::u16string es0 = elems[0].get_element_string();
  test_assert_equal(es0.length(), 1);
  test_assert_equal(es0.at(0), 0x0127);
  test_assert(es0 == std::u16string(u"ħ"));

  std::u16string es1 = elems[1].get_element_string();
  test_assert_equal(es1.length(), 2);
  test_assert_equal(es1.at(0), 0xD83D);
  test_assert_equal(es1.at(1), 0xDE40);
  test_assert(es1 == std::u16string(u"🙀"));
}

TEST(KMXPlusTest, test_ldml_vkeys) {
  km::core::ldml::vkeys vk;

#define ADD_VKEY(k, m) { \
  const char* str = k "-" #m ; \
  PKMX_WCHAR wstr = km::core::kmx::strtowstr(const_cast<PKMX_CHAR>(str)); \
  vk.add(km::tests::get_vk(k), m, wstr); \
  delete [] wstr; \
}

  ADD_VKEY("K_A", 0);
  ADD_VKEY("K_A", LCTRLFLAG); // K_A + left control -> "K_A-LCTRLFLAG", etc
  ADD_VKEY("K_A", RCTRLFLAG);
  ADD_VKEY("K_B", K_CTRLFLAG);
  ADD_VKEY("K_A", LALTFLAG);
  ADD_VKEY("K_A", RALTFLAG);
  ADD_VKEY("K_B", K_ALTFLAG);

  ADD_VKEY("K_C", K_ALTFLAG|K_CTRLFLAG);
  ADD_VKEY("K_D", RALTFLAG|K_CTRLFLAG);
  ADD_VKEY("K_D", LALTFLAG|K_CTRLFLAG);
  ADD_VKEY("K_E", K_ALTFLAG|LCTRLFLAG);
  ADD_VKEY("K_E", K_ALTFLAG|RCTRLFLAG);

#undef ADD_VKEY
  vk.add(km::tests::get_vk("K_F"), 0, u""); // K_F as a 'gap' key

  bool found = false;
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_F"), 0, found), u"");
  test_assert_equal(found, true); // K_F found, but empty string (gap)
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_ENTER"), 0, found), u"");
  test_assert_equal(found, false); // K_ENTER not found, empty string
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_A"), 0, found), u"K_A-0");
  test_assert_equal(found, true); // expect
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_A"), LCTRLFLAG, found), u"K_A-LCTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_A"), RCTRLFLAG, found), u"K_A-RCTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_A"), LALTFLAG, found), u"K_A-LALTFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_A"), RALTFLAG, found), u"K_A-RALTFLAG");

  // now try either-side keys :should get the same result with either or both
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), LCTRLFLAG, found), u"K_B-K_CTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), RCTRLFLAG, found), u"K_B-K_CTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), LCTRLFLAG|RCTRLFLAG, found), u"K_B-K_CTRLFLAG");

  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), LALTFLAG, found), u"K_B-K_ALTFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), RALTFLAG, found), u"K_B-K_ALTFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), LALTFLAG|RALTFLAG, found), u"K_B-K_ALTFLAG");

  // OOOkay now try BOTH side
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_C"), LCTRLFLAG|LALTFLAG, found), u"K_C-K_ALTFLAG|K_CTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_C"), LCTRLFLAG|RALTFLAG, found), u"K_C-K_ALTFLAG|K_CTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_C"), RCTRLFLAG|LALTFLAG, found), u"K_C-K_ALTFLAG|K_CTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_C"), RCTRLFLAG|RALTFLAG, found), u"K_C-K_ALTFLAG|K_CTRLFLAG");

  // OOOkay now try either alt
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_D"), LCTRLFLAG|LALTFLAG, found), u"K_D-LALTFLAG|K_CTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_D"), LCTRLFLAG|RALTFLAG, found), u"K_D-RALTFLAG|K_CTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_D"), RCTRLFLAG|LALTFLAG, found), u"K_D-LALTFLAG|K_CTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_D"), RCTRLFLAG|RALTFLAG, found), u"K_D-RALTFLAG|K_CTRLFLAG");

  // OOOkay now try either ctrl
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_E"), LCTRLFLAG|LALTFLAG, found), u"K_E-K_ALTFLAG|LCTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_E"), LCTRLFLAG|RALTFLAG, found), u"K_E-K_ALTFLAG|LCTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_E"), RCTRLFLAG|LALTFLAG, found), u"K_E-K_ALTFLAG|RCTRLFLAG");
  test_assert_equal(vk.lookup(km::tests::get_vk(
    "K_E"), RCTRLFLAG|RALTFLAG, found), u"K_E-K_ALTFLAG|RCTRLFLAG");
}

TEST(KMXPlusTest, test_uset) {
  const COMP_KMXPLUS_USET_RANGE r[] = {
    {0x61, 0x7a}, // [a-z]
    {0x127, 0x127} // [ħ]
  };

  SimpleUSet u0(&r[0], 2);
  test_assert_equal(u0.contains(0x62), true); // b
  test_assert_equal(u0.contains(0x41), false); // A
  test_assert_equal(u0.contains(0x127), true); // ħ

  SimpleUSet uempty;
  test_assert_equal(uempty.contains(0x62), false);
  test_assert_equal(uempty.contains(0x127), false);
}

/** tests of the COMP_KMXPLUS_STRS::valid_string() */
TEST(KMXPlusTest, COMP_KMXPLUS_STRS_valid_string) {
  // one simple case
  EXPECT_TRUE(COMP_KMXPLUS_STRS::valid_string(
    u"Hello", 5));

  {
    // unpaired low surrogate
    km_core_cu const s[] = {0xDC01, 0x0020};
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 1)); // at end of str
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 2)); // not followed by trailing surrogate
  }

  {
    // unpaired high surrogate
    km_core_cu const s[] = { 0xD801, 0x0020};
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 1));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 2));
  }

  {
    // valid str
    km_core_cu const s[] = { 0xD83D, 0xDE40, 0x0020}; // 🙀
    EXPECT_TRUE(COMP_KMXPLUS_STRS::valid_string(s, 2));
    EXPECT_TRUE(COMP_KMXPLUS_STRS::valid_string(s, 3));
  }

  {
    // invalid str - noncharacter U+2FFFF
    km_core_cu const s[] = { u'a', u'b', 0xD87F, 0xDFFF, u'c'};
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 5));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 4));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 3));
  }

  {
    // valid str with a marker - 'ab\m{x}c'
    km_core_cu const s[] = {u'a', u'b', 0xFFFF, 0x0008, 0x0001, u'c'};
    EXPECT_TRUE(COMP_KMXPLUS_STRS::valid_string(s, 6)); // whole thing
    EXPECT_TRUE(COMP_KMXPLUS_STRS::valid_string(s, 5)); // whole thing minus 'c'
    EXPECT_TRUE(COMP_KMXPLUS_STRS::valid_string(s+2, 3)); // just the marker
    // if we slice the marker, invalid
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 4));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 3));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s+2, 2));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s+2, 1));
  }

  {
    // valid str with a marker - 'ab\m{x}c'
    km_core_cu const s[] = {u'a', u'b', 0xFFFF, 0x0008, 0x0001, u'c'};
    EXPECT_TRUE(COMP_KMXPLUS_STRS::valid_string(s, 6));      // whole thing
    EXPECT_TRUE(COMP_KMXPLUS_STRS::valid_string(s, 5));      // whole thing minus 'c'
    EXPECT_TRUE(COMP_KMXPLUS_STRS::valid_string(s + 2, 3));  // just the marker
    // if we slice the marker, invalid
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 4));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 3));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s + 2, 2));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s + 2, 1));
  }

  {
    // invalid str - incorrect char 0009 instead of 0008
    km_core_cu const s[] = {u'a', u'b', 0xFFFF, 0x0009, 0x0001, u'c'};
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 6));      // whole thing
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 5));      // whole thing minus 'c'
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s + 2, 3));  // just the marker
    // if we slice the marker, invalid
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 4));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 3));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s + 2, 2));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s + 2, 1));
  }

  {
    // invalid str - out of range merker 0xDDDD exceeds LDML_MARKER_MAX_INDEX
    km_core_cu const s[] = {u'a', u'b', 0xFFFF, 0x0008, 0xDDDD, u'c'};
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 6));      // whole thing
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 5));      // whole thing minus 'c'
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s + 2, 3));  // just the marker
    // if we slice the marker, invalid
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 4));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 3));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s + 2, 2));
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s + 2, 1));
  }

  {
    // noncharacter FDD4
    km_core_cu const s[] = {0xFDD4};
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 1));
  }

  {
    // FFFE nonchar (mismatched BOM)
    km_core_cu const s[] = {0xFFFE};
    EXPECT_FALSE(COMP_KMXPLUS_STRS::valid_string(s, 1));
  }
}

TEST(KMXPlusTest, COMP_KMXPLUS_STRS_withGoodStrings) {
  const auto len = 10;
  KMX_DWORD mystrs[len] = {
    COMP_KMXPLUS_STRS::IDENT,
    len * 4, // size
    // ---
    0x0002, // count

    // #0
    28, // offset
    0x0000, // size

    // #1
    32, // offset
    0x0001, // size

    0x00000000,  // null

    // data @ +7
    0x0041,  // 'a'
    0x0000,  // null
  };

  const COMP_KMXPLUS_HEADER *header = reinterpret_cast<const COMP_KMXPLUS_HEADER *>(mystrs);
  ASSERT_TRUE(header->valid(mystrs[1]));
  const COMP_KMXPLUS_STRS *strs = reinterpret_cast<const COMP_KMXPLUS_STRS *>(mystrs);
  ASSERT_TRUE(strs->valid(mystrs[1]));
  KMX_DWORD_unaligned mycount = strs->count;
  ASSERT_EQ(mycount, 2);
  ASSERT_EQ(strs->get(0), u"");
  ASSERT_EQ(strs->get(1), u"A");
}


TEST(KMXPlusTest, COMP_KMXPLUS_STRS_withBadStrings) {
  const auto len = 10;
  KMX_DWORD mystrs[len] = {
    COMP_KMXPLUS_STRS::IDENT,
    len * 4, // size
    // ---
    0x0002, // count

    // #0
    28, // offset
    0x0000, // size

    // #1
    32, // offset
    0x0001, // size

    0x00000000,  // null

    // data @ +7
    0xFFFF,  // illegal nonchar at end (not a valid marker sequence)
    0x0000,  // null
  };

  const COMP_KMXPLUS_HEADER *header = reinterpret_cast<const COMP_KMXPLUS_HEADER *>(mystrs);
  ASSERT_TRUE(header->valid(mystrs[1]));
  const COMP_KMXPLUS_STRS *strs = reinterpret_cast<const COMP_KMXPLUS_STRS *>(mystrs);
  ASSERT_FALSE(strs->valid(mystrs[1]));
}

GTEST_API_ int
main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
