/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Test LDML transforms
 */

#include "../../../src/ldml/ldml_markers.hpp"
#include "../../../src/ldml/ldml_transforms.hpp"
#include "../../../src/util_regex.hpp"
#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include <iostream>
#include <string>
#include "../../../src/util_regex.hpp"

// needed for streaming operators
#include "utfcodec.hpp"

#include "../helpers/core_test_helpers.h"

using namespace km::core::ldml;
using namespace km::core::kmx;

void
prepend_hex_oct(std::u32string &str, char32_t x) {
  for (auto i = 0; i < 8; i++) {
    KMX_DWORD remainder = x & 0xF;  // get the last nibble
    const char32_t ch = remainder < 0xA ? U'0' + remainder : U'A' + (remainder - 0xA);
    str.insert(0, 1, ch);  // prepend
    x >>= 4;
  }
}

std::u32string
marker_map_to_string(const marker_map &m) {
  std::u32string s;
  for (auto i = m.rbegin(); i < m.rend(); i++) {
    if (i->end) {
      s.insert(0, U"<END>");
    }
    if (i->processed) {
      s.insert(0, U"<PROCESSED?>");
    }
    if (i->ch == MARKER_BEFORE_EOT) {
      s.insert(0, U"<EOT>");
    } else {
      prepend_hex_oct(s, i->ch);
      s.insert(0, U"=U+");
    }

    if (i->marker != LDML_MARKER_NO_INDEX) {
      prepend_hex_quad(s, i->marker);
      s.insert(0, U" \\m0x");
    }
  }
  return s;
}

bool
assert_marker_map_equal(const marker_map actual, const marker_map expected) {
  marker_map a;
  // copy everything but 'end' entries
  std::copy_if(
      actual.begin(), actual.end(), std::back_inserter(a), [](const marker_entry &m) { return m.marker != LDML_MARKER_NO_INDEX; });

  // now check equality
  return (a == expected);
}

TEST(TransformsTests, TestTransformsSimple) {
  // start with one
  transform_entry trans_entry(std::u32string(U"e\\^"), std::u32string(U"E"));  // keep it simple
  // OK now make a group do it
  transforms trans(false);
  transform_group trans_group;

  trans_group.push_back(trans_entry);

  trans.addGroup(trans_group);

  // see if we can match the same
  {
    std::u32string src(U"barQ^");
    bool res = trans.apply(src);
    EXPECT_FALSE(res);
    EXPECT_EQ(src, std::u32string(U"barQ^"));  // no change
  }

  {
    std::u32string src(U"fooe^");
    bool res = trans.apply(src);
    EXPECT_TRUE(res);
    EXPECT_EQ(src, std::u32string(U"fooE"));
  }
}

TEST(TransformsTests, TestTransformsMoreComplex) {
  transforms trans(false);

  // setup
  {
    transform_group trans_group;
    trans_group.emplace_back(std::u32string(U"za"), std::u32string(U"c"));
    trans_group.emplace_back(std::u32string(U"a"), std::u32string(U"bb"));
    trans.addGroup(trans_group);
  }
  {
    transform_group trans_group;
    trans_group.emplace_back(std::u32string(U"bb"), std::u32string(U"ccc"));
    trans.addGroup(trans_group);
  }
  {
    transform_group trans_group;
    trans_group.emplace_back(std::u32string(U"cc"), std::u32string(U"d"));
    trans.addGroup(trans_group);
  }
  {
    transform_group trans_group;
    trans_group.emplace_back(std::u32string(U"tcd"), std::u32string(U"e"));
    trans.addGroup(trans_group);
  }

  // now test

  // see if we can match the same
  {
    std::u32string src(U"ta");
    bool res = trans.apply(src);
    // pipe (|) symbol shows where the 'output' is delineated
    // t|a --> t|bb --> t|ccc --> t|cd --> |e
    EXPECT_EQ(src, std::u32string(U"e"));
    EXPECT_TRUE(res);
  }
  {
    std::u32string src(U"qza");
    bool res = trans.apply(src);
    // pipe (|) symbol shows where the 'output' is delineated
    // q|za -> q|c
    EXPECT_EQ(src, std::u32string(U"qc"));
    EXPECT_TRUE(res);
  }
  {
    std::u32string src(U"qa");
    bool res = trans.apply(src);
    EXPECT_EQ(src, std::u32string(U"qcd"));
    EXPECT_TRUE(res);
  }
  {
    std::u32string src(U"tb");
    bool res = trans.apply(src);
    EXPECT_EQ(src, std::u32string(U"tb"));
    EXPECT_FALSE(res);
  }
}

TEST(TransformsTests, TestHindiExample) {
  transforms trans(false);
  {
    transform_group trans_group;
    trans_group.emplace_back(std::u32string(U"िह"), std::u32string(U"हि"));
    trans.addGroup(trans_group);
  }
  {
    std::u32string src(U"िह");
    bool res = trans.apply(src);
    EXPECT_EQ(src, std::u32string(U"हि"));
    EXPECT_TRUE(res);
  }
}

TEST(TransformsTests, TestReorderStandaloneElementApi) {
// element API test - not a real element, just here for testing
  element elem_char(U'a', 0xF4500000 | LDML_ELEM_FLAGS_PREBASE | LDML_ELEM_FLAGS_TERTIARY_BASE);  // tertiary -12, primary 80
  std::cout << "elem_char flags" << std::hex << elem_char.get_flags() << std::dec << std::endl;
  // verify element metadata
  ASSERT_FALSE(elem_char.is_uset());
  ASSERT_EQ(elem_char.get_order(), 0x50);
  ASSERT_EQ(elem_char.get_tertiary(), -12);
  ASSERT_TRUE(elem_char.is_prebase());
  ASSERT_TRUE(elem_char.is_tertiary_base());
  // verify element matching
  ASSERT_TRUE(elem_char.matches(U'a'));
  ASSERT_FALSE(elem_char.matches(U'b'));
}

TEST(TransformsTests, TestReordersWithStandaloneNodLanaExample) {
  const std::u32string roasts[] = {
      // from the spec
      U"\u1A21\u1A60\u1A45\u1A6B\u1A76",  // 'ideal'
      U"\u1A21\u1A6B\u1A76\u1A60\u1A45",  // vowel first
      U"\u1A21\u1A6B\u1A60\u1A76\u1A45",  // vowel first, NFC
      U"\u1A21\u1A6B\u1A60\u1A45\u1A76",  // tone after lower
  };
  /** we expect this is what comes out the other side */
  const std::u32string expect = roasts[0];
  // now setup the rules
  const COMP_KMXPLUS_USET_RANGE ranges[]      = {// 0
                                            COMP_KMXPLUS_USET_RANGE(0x1A75, 0x1A79)};
  const COMP_KMXPLUS_USET_USET usets[]        = {{0, 1, 0xFFFFFFFF}};
  const COMP_KMXPLUS_USET_USET &toneMarksUset = usets[0];
  const SimpleUSet toneMarks(&ranges[toneMarksUset.range], toneMarksUset.count);
  // validate that the range [1A75, 1A79] matches
  ASSERT_TRUE(toneMarks.contains(0x1A76));
  ASSERT_FALSE(toneMarks.contains(0x1A60));

  // element test
  {
    element elem_char(U'a', (80 << LDML_ELEM_FLAGS_ORDER_BITSHIFT) | LDML_ELEM_FLAGS_PREBASE);  // tertiary -12, primary 80
    std::cout << "elem_char flags" << std::hex << elem_char.get_flags() << std::dec << std::endl;
    // verify element metadata
    ASSERT_FALSE(elem_char.is_uset());
    ASSERT_EQ(elem_char.get_order(), 0x50);
    ASSERT_EQ(elem_char.get_tertiary(), 0);
    ASSERT_TRUE(elem_char.is_prebase());
    ASSERT_FALSE(elem_char.is_tertiary_base());
    // verify element matching
    ASSERT_TRUE(elem_char.matches(U'a'));
    ASSERT_FALSE(elem_char.matches(U'b'));

    element elem_uset(toneMarks, 0x37F40000);
    // element metadata
    std::cout << "elem_uset flags" << std::hex << elem_uset.get_flags() << std::dec << std::endl;
    ASSERT_TRUE(elem_uset.is_uset());
    std::cout << "order" << (int)elem_uset.get_order() << std::endl;
    ASSERT_EQ(elem_uset.get_order(), -12);
    ASSERT_EQ(elem_uset.get_tertiary(), 55);
    ASSERT_FALSE(elem_uset.is_prebase());
    ASSERT_FALSE(elem_uset.is_tertiary_base());
    // element matching
    ASSERT_FALSE(elem_uset.matches(U'a'));
    ASSERT_TRUE(elem_uset.matches(U'\u1A76'));
    ASSERT_TRUE(elem_uset.matches(U'\u1A75'));

    element_list l;  // '[tones]a'
    l.emplace_back(elem_char);
    l.emplace_back(elem_uset);

    std::cout << __FILE__ << ":" << __LINE__ << " - list test " << std::endl;
    ASSERT_EQ(l.match_end(U"asdfasdf"), 0);                            // no match
    ASSERT_EQ(l.match_end(U"a"), 0);                                   // partial substring, fastpath because it's short
    ASSERT_EQ(l.match_end(U"\u1A76"), 0);                              // partial substring, fastpath because it's short
    ASSERT_EQ(l.match_end(U"a\u1A76"), 2);                             // Match
    ASSERT_EQ(l.match_end(U"a\u1A75"), 2);                             // Match
    ASSERT_EQ(l.match_end(U"SomethingSomethingSomethinga\u1A76"), 2);  // Sub-Match

    // generate sort keys
    std::cout << __FILE__ << ":" << __LINE__ << " - get_sort_key test " << std::endl;
    std::u32string str = U"a\u1A78";
    // get the baseline sortkey
    auto keylist = reorder_sort_key::from(str);
    std::cout << __FILE__ << ":" << __LINE__ << "  baseline sortkey" << std::endl;
    for (auto i = keylist.begin(); i < keylist.end(); i++) {
      i->dump();
    }
    // make sure it matches
    ASSERT_EQ(l.match_end(str), 2);
    // update the keylist with these elements.
    l.update_sort_key(0, keylist);
    std::cout << __FILE__ << ":" << __LINE__ << "  updated sortkey" << std::endl;
    ASSERT_EQ(keylist.size(), 2);
    reorder_weight secondary = 0;
    for (auto i = keylist.begin(); i < keylist.end(); i++) {
      i->dump();
      ASSERT_EQ(i->secondary, secondary);
      ASSERT_EQ(i->quaternary, secondary);
      secondary++;
    }
    std::cout << std::endl;

    // spot check first sortkey
    ASSERT_EQ(keylist.begin()->primary, 80);
    ASSERT_EQ(keylist.begin()->tertiary, 0);
    ASSERT_EQ(keylist.begin()->ch, 0x61);
    std::cout << __FILE__ << ":" << __LINE__ << "  sorted sortkey" << std::endl;

    // now sort them
    std::sort(keylist.begin(), keylist.end());
    for (auto i = keylist.begin(); i < keylist.end(); i++) {
      i->dump();
    }
    std::cout << std::endl;
    // spot check first sort key
    ASSERT_EQ(keylist.begin()->primary, -12);
    ASSERT_EQ(keylist.begin()->tertiary, 55);
    ASSERT_EQ(keylist.begin()->ch, 0x1A78);
  }

  std::cout << __FILE__ << ":" << __LINE__ << " - key test " << std::endl;
  {
    // can we sort an array of reorder sort keys?
    reorder_sort_key keys0[] = {
        {U'\u1A21', 0, 0, 0, 0},
        {U'\u1A6B', 42, 1, 0, 1},
        {U'\u1A76', 55, 2, 0, 2},
        {U'\u1A60', 10, 3, 0, 3},
        {U'\u1A45', 10, 4, 0, 3}};
    // add it to a list
    std::deque<reorder_sort_key> keylist;
    for (size_t i = 0; i < sizeof(keys0) / sizeof(keys0[0]); i++) {
      keylist.emplace_back(keys0[i]);
      // print it out
      keys0[i].dump();
    }
    // now sort it
    std::cout << __FILE__ << ":" << __LINE__ << "  sort" << std::endl;
    std::sort(keylist.begin(), keylist.end());
    std::u32string sorted;
    for (auto i = keylist.begin(); i < keylist.end(); i++) {
      i->dump();
      sorted.append(1, i->ch);
    }
    // did the roast come out?
    ASSERT_EQ(sorted, expect);
  }
  std::cout << "now prepare the reorder elements" << std::endl;
  transforms trans(false);
  {
    reorder_group rg;

    // <reorder from="\u1A60" order="127" />
    element_list e0;
    e0.emplace_back(U'\u1A60', 127 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
    rg.list.emplace_back(e0);

    // <reorder from="\u1A6B" order="42" />
    element_list e1;
    e1.emplace_back(U'\u1A6B', 42 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
    rg.list.emplace_back(e1);

    // <reorder from="[\u1A75-\u1A79]" order="55" />
    element_list e2;
    e2.emplace_back(toneMarks, 55 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
    rg.list.emplace_back(e2);

    // <reorder before="\u1A6B" from="\u1A60\u1A45" order="10" />
    element_list e3;
    e3.emplace_back(U'\u1A60', 10 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
    e3.emplace_back(U'\u1A45', 10 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
    element_list e3before;
    e3before.emplace_back(U'\u1A6B', 0);
    rg.list.emplace_back(e3, e3before);

    // <reorder before="\u1A6B[\u1A75-\u1A79]" from="\u1A60\u1A45" order="10" />
    element_list e4;
    e4.emplace_back(U'\u1A60', 10 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
    e4.emplace_back(U'\u1A45', 10 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
    element_list e4before;
    e4before.emplace_back(U'\u1A6B', 0);
    e4before.emplace_back(toneMarks, 0);
    rg.list.emplace_back(e4, e4before);

    // <reorder before="\u1A6B" from="\u1A60[\u1A75-\u1A79]\u1A45" order="10 55 10" />
    element_list e5;
    e5.emplace_back(U'\u1A60', 10 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
    e5.emplace_back(toneMarks, 55 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
    e5.emplace_back(U'\u1A45', 10 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
    element_list e5before;
    e5before.emplace_back(U'\u1A6B', 0);
    rg.list.emplace_back(e5, e5before);

    trans.addGroup(rg);
  }

  // now actually test it
  std::cout << __FILE__ << ":" << __LINE__ << " - back to nod-Lana " << std::endl;
  // TODO-LDML: move this into test code perhaps
  for (size_t r = 0; r < sizeof(roasts) / sizeof(roasts[0]); r++) {
    const auto &roast = roasts[r];
    std::cout << __FILE__ << ":" << __LINE__ << " - trying roast #" << r << "=" << roast << std::endl;
    // try apply with string
    {
      std::cout << "- try apply(text, output)" << std::endl;
      std::u32string text = roast;
      std::u32string output;
      size_t len = trans.apply(text, output);
      if (len == 0) {
        std::cout << " (did not apply)" << std::endl;
      } else {
        std::cout << " applied, matchLen= " << len << std::endl;
        text.resize(text.size() - len);  // shrink
        text.append(output);
        std::cout << " = " << text << std::endl;
      }
      ASSERT_EQ(text, expect);
    }
    // try all-at-once
    {
      std::cout << "- try apply(text)" << std::endl;
      std::u32string text = roast;
      if (!trans.apply(text)) {
        std::cout << " (did not apply)" << std::endl;
      } else if (text == roast) {
        std::cout << " (suboptimal: apply returned true but made no change)" << std::endl;
      } else {
        std::cout << " changed to " << text;
      }
      ASSERT_EQ(text, expect);
      std::cout << " matched (converting all at once)!" << std::endl;
    }
    // simulate typing this one char at a time;
    {
      std::cout << "- try key-at-a-time" << std::endl;
      std::u32string text;
      for (auto ch = roast.begin(); ch < roast.end(); ch++) {
        // append the string
        text.append(1, *ch);
        std::cout << "-: " << text << std::endl;
        if (!trans.apply(text)) {
          std::cout << " (did not apply)" << std::endl;
        }
      }
      // now the moment of truth
      ASSERT_EQ(text, expect);
      std::cout << " matched! (converting char at a time)" << std::endl;
      std::cout << std::endl;
    }
  }
  // special test
  {
    std::cout << __FILE__ << ":" << __LINE__ << " - special test " << std::endl;
    const std::u32string expect = U"\u1A21\u1A60\u1A45";  // this string shouldn't mutate at all.
    {
      std::u32string text = expect;
      trans.apply(text);
      ASSERT_EQ(text, expect);
    }
    {
      // try submatch
      std::u32string text = expect;
      std::u32string output;
      size_t len = trans.apply(text, output);
      ASSERT_EQ(output, U"");
      ASSERT_EQ(len, 0);
    }
  }
}

// this test case is also in XML form under 'k_201_*'
TEST(TransformsTests, TestReorderEsk) {
  // now setup the rules
  // rules are a little bit simplified, having only the vowel 'a'

  std::cout << "now prepare the reorder elements" << std::endl;
  transforms trans(false);
  {
    reorder_group rg;

    // <reorder from="a" order="0" tertiaryBase="true" />
    {
      element_list e;
      e.emplace_back(U'a', (0 << LDML_ELEM_FLAGS_ORDER_BITSHIFT) | LDML_ELEM_FLAGS_TERTIARY_BASE);
      rg.list.emplace_back(e);
    }

    // <reorder from="[\u{0332}]" tertiary="1" />
    {
      element_list e;
      e.emplace_back(0x0332, (1 << LDML_ELEM_FLAGS_TERTIARY_BITSHIFT));
      rg.list.emplace_back(e);
    }
    // <reorder from="[\u{0305}\u{0302}]" tertiary="2" />
    {
      element_list e;
      e.emplace_back(0x305, (2 << LDML_ELEM_FLAGS_TERTIARY_BITSHIFT));
      // (should be a unicodeset, but for simplicity we're dropping the U+302)
      // e.emplace_back(0x302, (2 << LDML_ELEM_FLAGS_TERTIARY_BITSHIFT));
      rg.list.emplace_back(e);
    }
    // <reorder from="x" order="1" />
    {
      element_list e;
      e.emplace_back(U'x', (1 << LDML_ELEM_FLAGS_ORDER_BITSHIFT));
      rg.list.emplace_back(e);
    }
    // <reorder from="y" order="2" />
    {
      element_list e;
      e.emplace_back(U'y', (2 << LDML_ELEM_FLAGS_ORDER_BITSHIFT));
      rg.list.emplace_back(e);
    }
    // <reorder from="z" order="3" />
    {
      element_list e;
      e.emplace_back(U'z', (3 << LDML_ELEM_FLAGS_ORDER_BITSHIFT));
      rg.list.emplace_back(e);
    }

    trans.addGroup(rg);
  }

  // now actually test it
  std::cout << __FILE__ << ":" << __LINE__ << " - cases " << std::endl;
  const std::u32string orig_expect[] = {
      // 1short
      U"ax\u0305",  // orig
      U"a\u0305x",  // expect

      // 2longer
      U"az\u0305x\u0332",  // orig
      U"a\u0332\u0305xz",  // expect
  };
  // TODO-LDML: move this into test code perhaps
  for (size_t r = 0; r < sizeof(orig_expect) / sizeof(orig_expect[0]); r += 2) {
    const auto &orig   = orig_expect[r + 0];
    const auto &expect = orig_expect[r + 1];
    std::cout << __FILE__ << ":" << __LINE__ << " - trying str #" << r + 1 << "=" << orig << std::endl;
    // try apply with string
    {
      std::cout << "- try apply(text, output)" << std::endl;
      std::u32string text = orig;
      std::u32string output;
      size_t len = trans.apply(text, output);
      if (len == 0) {
        std::cout << " (did not apply)" << std::endl;
      } else {
        std::cout << " applied, matchLen= " << len << std::endl;
        text.resize(text.size() - len);  // shrink
        text.append(output);
        std::cout << " = " << text << std::endl;
      }
      ASSERT_EQ(text, expect);
    }
    // try all-at-once
    {
      std::cout << "- try apply(text)" << std::endl;
      std::u32string text = orig;
      if (!trans.apply(text)) {
        std::cout << " (did not apply)" << std::endl;
      } else if (text == orig) {
        std::cout << " (suboptimal: apply returned true but made no change)" << std::endl;
      } else {
        std::cout << " changed to " << text;
      }
      ASSERT_EQ(text, expect);
      std::cout << " matched (converting all at once)!" << std::endl;
    }
    // simulate typing this one char at a time;
    {
      std::cout << "- try key-at-a-time" << std::endl;
      std::u32string text;
      for (auto ch = orig.begin(); ch < orig.end(); ch++) {
        // append the string
        text.append(1, *ch);
        std::cout << "-: " << text << std::endl;
        if (!trans.apply(text)) {
          std::cout << " (did not apply)" << std::endl;
        }
      }
      // now the moment of truth
      ASSERT_EQ(text, expect);
      std::cout << " matched! (converting char at a time)" << std::endl;
      std::cout << std::endl;
    }
  }
}

TEST(TransformsTests, TestMap) {
  std::deque<std::u32string> list;
  ASSERT_EQ(km::core::util::km_regex::findIndex(U"Does Not Exist", list), -1);

  list.emplace_back(U"0th");
  list.emplace_back(U"First");
  list.emplace_back(U"Second");

  ASSERT_EQ(km::core::util::km_regex::findIndex(U"First", list), 1);
  ASSERT_EQ(km::core::util::km_regex::findIndex(U"0th", list), 0);
  ASSERT_EQ(km::core::util::km_regex::findIndex(U"Second", list), 2);
  ASSERT_EQ(km::core::util::km_regex::findIndex(U"Nowhere", list), -1);
}

// Strutils

TEST(TransformsTests, TestStrutilsBasicTest0) {
  const std::u32string src = U"abc";
  const std::u32string dst = remove_markers(src);
  ASSERT_EQ(dst, src);  // unchanged
}

TEST(TransformsTests, TestStrutilsBasicTest) {
  marker_map map;
  const std::u32string src = U"abc";
  const std::u32string dst = remove_markers(src, map);
  ASSERT_EQ(dst, src);  // unchanged
  ASSERT_EQ(count_markers(map), 0);
}

TEST(TransformsTests, TestStrutilsMarkerTest) {
  marker_map map;
  const std::u32string src    = U"6\U0000ffff\U00000008\U00000001e";
  const std::u32string dst    = remove_markers(src, map);
  const std::u32string expect = U"6e";
  ASSERT_EQ(dst, expect);
  marker_map expm = {{U'e', 0x1L}};
  assert_marker_map_equal(map, expm);  // marker 1 @ e
  ASSERT_EQ(count_markers(map), 1);
}

TEST(TransformsTests, TestStrutilsBad0) {
  marker_map map;
  const std::u32string src    = U"6\U0000ffff\U00000008";  // missing trailing marker #
  const std::u32string dst    = remove_markers(src, map);
  const std::u32string expect = src;
  ASSERT_EQ(dst, expect);
  ASSERT_EQ(count_markers(map), 0);
}

TEST(TransformsTests, TestStrutilsBad1) {
  marker_map map;
  const std::u32string src    = U"6\U0000ffffq";  // missing sentinel subtype
  const std::u32string dst    = remove_markers(src, map);
  const std::u32string expect = src;  // 'q' removed
  ASSERT_EQ(dst, expect);
  ASSERT_EQ(count_markers(map), 0);
}

TEST(TransformsTests, TestStrutilsBad1B) {
  marker_map map;
  const std::u32string src    = U"6\U0000ffff";  // missing code
  const std::u32string dst    = remove_markers(src, map);
  const std::u32string expect = src;
  ASSERT_EQ(dst, expect);
  ASSERT_EQ(count_markers(map), 0);
}

TEST(TransformsTests, TestStrutilsBad1C) {
  marker_map map;
  const std::u32string src    = U"6\U0000ffffzz";  // missing code
  const std::u32string dst    = remove_markers(src, map);
  const std::u32string expect = src;
  ASSERT_EQ(dst, expect);
  ASSERT_EQ(count_markers(map), 0);
}

TEST(TransformsTests, TestStrutilsMarkerEnd) {
  marker_map map;
  const std::u32string src    = U"6\U0000ffff\U00000008\U00000001";
  const std::u32string dst    = remove_markers(src, map);
  const std::u32string expect = U"6";
  ASSERT_EQ(dst, expect);
  marker_map expm({{MARKER_BEFORE_EOT, 0x1L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(count_markers(map), 1);
}

TEST(TransformsTests, TestStrutilsComplex) {
  marker_map map;
  const std::u32string src =
      U"6\U0000ffff\U00000008\U00000001e\U0000ffff\U00000008\U00000002\U00000320\U0000ffff\U00000008\U00000003\U00000300"
      U"\U0000ffff\U00000008\U00000004";
  const std::u32string dst    = remove_markers(src, map);
  const std::u32string expect = U"6e\U00000320\U00000300";
  ASSERT_EQ(dst, expect);
  marker_map expm({{U'e', 0x1L}, {0x0320, 0x2L}, {0x0300, 0x3L}, {MARKER_BEFORE_EOT, 0x4L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(count_markers(map), 4);
}

// PrependHex

TEST(TransformsTests, TestStrutilsPrependHexQuad1) {
  std::u32string dst;
  prepend_hex_quad(dst, 0x0001);
  ASSERT_EQ(dst, U"0001");
}

TEST(TransformsTests, TestStrutilsPrependHexQuadCAFE) {
  std::u32string dst;
  prepend_hex_quad(dst, 0xCAFE);
  ASSERT_EQ(dst, U"CAFE");
}

TEST(TransformsTests, TestStrutilsPrependHexQuadFFFF) {
  std::u32string dst;
  prepend_hex_quad(dst, 0xFFFF);
  ASSERT_EQ(dst, U"FFFF");
}

TEST(TransformsTests, TestStrutilsParseHexQuad) {
  ASSERT_EQ(parse_hex_quad(U"0001"), 0x0001);
  ASSERT_EQ(parse_hex_quad(U"CAFE"), 0xCAFE);
  ASSERT_EQ(parse_hex_quad(U"D00d"), 0xD00D);
  ASSERT_EQ(parse_hex_quad(U"FFFF"), 0xFFFF);
  ASSERT_EQ(parse_hex_quad(U"zzzz"), 0);  // err
}

TEST(TransformsTests, TestNormalizeNoop) {
  marker_map map;
  const std::u32string src    = U"6e\U00000320\U00000300";  // already NFD
  const std::u32string expect = src;
  std::u32string dst          = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map));
  ASSERT_EQ(dst, expect);
  ASSERT_EQ(count_markers(map), 0);
}

TEST(TransformsTests, TestNormalizeMedium) {
  marker_map map;
  const std::u32string src    = U"6e\U00000300\U00000320";  // swapped
  const std::u32string expect = U"6e\U00000320\U00000300";  // correct NFD
  std::u32string dst          = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map));
  ASSERT_EQ(dst, expect);
  ASSERT_EQ(count_markers(map), 0);
}

TEST(TransformsTests, TestNormalizeNoopWithMarkers)   {
  marker_map map;
  const std::u32string src =
      U"6\U0000ffff\U00000008\U00000001e\U0000ffff\U00000008\U00000002\U00000320\U0000ffff\U00000008\U00000003\U00000300"
      U"\U0000ffff\U00000008\U00000004";
  const std::u32string expect = src;
  std::u32string dst          = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map));
  ASSERT_EQ(dst, expect);
  marker_map expm({{U'e', 0x1L}, {0x320, 0x2L}, {0x300, 0x3L}, {MARKER_BEFORE_EOT, 0x4L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(count_markers(map), 4);
}

TEST(TransformsTests, TestNormalizeComplex)  {
  marker_map map;
  const std::u32string src =  // already in order: 320+300
      U"6\U0000ffff\U00000008\U00000001e\U0000ffff\U00000008\U00000002\U00000320\U0000ffff\U00000008\U00000003\U00000300"
      U"\U0000ffff\U00000008\U00000004";
  const std::u32string expect = src;
  std::u32string dst          = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map));
  ASSERT_EQ(dst, expect);
  marker_map expm({{U'e', 0x1L}, {0x320, 0x2L}, {0x300, 0x3L}, {MARKER_BEFORE_EOT, 0x4L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(count_markers(map), 4);
}

TEST(TransformsTests, TestNormalizeComplex2)   {
  marker_map map;
  const std::u32string src =  // out of order, 300-320
      U"6\U0000ffff\U00000008\U00000001e\U0000ffff\U00000008\U00000002\U00000300\U0000ffff\U00000008\U00000003\U00000320"
      U"\U0000ffff\U00000008\U00000004";
  const std::u32string expect =
      U"6\U0000ffff\U00000008\U00000001e\U0000ffff\U00000008\U00000003\U00000320\U0000ffff\U00000008\U00000002\U00000300"
      U"\U0000ffff\U00000008\U00000004";
  std::u32string dst = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map));
  if (dst != expect) {
    std::cout << "dst: " << Debug_UnicodeString(dst) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect) << std::endl;
  }
  ASSERT_EQ(dst, expect);
  marker_map expm({{U'e', 0x1L},  {0x300, 0x2L}, {0x320, 0x3L}, {MARKER_BEFORE_EOT, 0x4L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(count_markers(map), 4);
}

TEST(TransformsTests, TestNormalizeComplex4A) {
  // u"4è\U0000ffff\u0008\U00000001̠"
  marker_map map;
  const std::u32string src    = U"4e\u0300\uFFFF\u0008\u0001\u0320";
  const std::u32string expect = U"4e\uFFFF\u0008\u0001\u0320\u0300";
  std::u32string dst          = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map));
  if (dst != expect) {
    std::cout << "dst: " << Debug_UnicodeString(dst) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect) << std::endl;
  }
  ASSERT_EQ(dst, expect);
  marker_map expm({{0x320, 0x1L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(count_markers(map), 1);
}

TEST(TransformsTests, TestNormalizeComplex9C) {
  // from tests
  marker_map map;
  const std::u32string src    = U"9ce\u0300\uFFFF\u0008\u0002\u0320\uFFFF\u0008\u0001";
  const std::u32string expect = U"9ce\uFFFF\u0008\u0002\u0320\u0300\uFFFF\u0008\u0001";
  std::u32string dst          = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map));
  if (dst != expect) {
    std::cout << "dst: " << Debug_UnicodeString(dst) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect) << std::endl;
  }
  ASSERT_EQ(dst, expect);
  marker_map expm({{0x320, 0x2L}, {MARKER_BEFORE_EOT, 0x1L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(count_markers(map), 2);
}

TEST(TransformsTests, TestNormalizeComplex9CRegex) {
  // from tests - regex edition
  marker_map map;
  const std::u32string src    = U"9ce\u0300\\uffff\\u0008\\u0002\u0320\\uffff\\u0008\\u0001";
  const std::u32string expect = U"9ce\\uffff\\u0008\\u0002\u0320\u0300\\uffff\\u0008\\u0001";
  std::u32string dst          = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map, regex_sentinel));
  if (dst != expect) {
    std::cout << "dst: " << Debug_UnicodeString(dst) << std::endl;
    std::cout << "     " << dst << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect) << std::endl;
    std::cout << "     " << expect << std::endl;
  }
  ASSERT_EQ(dst, expect);
  marker_map expm({{0x320, 0x2L}, {MARKER_BEFORE_EOT, 0x1L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(count_markers(map), 2);
}

TEST(TransformsTests, TestNormalizeComplexMarkerAny) {
  // from tests - regex edition
  marker_map map;
  const std::u32string src    = U"9ce\u0300\\uffff\\u0008[\\u0001-\\ud7fe]\u0320\\uffff\\u0008\\u0001";
  const std::u32string expect = U"9ce\\uffff\\u0008[\\u0001-\\ud7fe]\u0320\u0300\\uffff\\u0008\\u0001";
  std::u32string dst          = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map, regex_sentinel));
  if (dst != expect) {
    std::cout << "dst: " << Debug_UnicodeString(dst) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect) << std::endl;
  }
  ASSERT_EQ(dst, expect);
  marker_map expm({{0x320, LDML_MARKER_ANY_INDEX}, {MARKER_BEFORE_EOT, 0x1L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(count_markers(map), 2);
}

TEST(TransformsTests, TestNormalizeComplex10StackOf2x2) {
  // from tests
  marker_map map;
  const std::u32string src    = U"9ce\u0300\uFFFF\u0008\u0002\uFFFF\u0008\u0002\u0320";
  const std::u32string expect = U"9ce\uFFFF\u0008\u0002\uFFFF\u0008\u0002\u0320\u0300";
  std::u32string dst          = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map));
  if (dst != expect) {
    std::cout << "dst: " << Debug_UnicodeString(dst) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect) << std::endl;
  }
  ASSERT_EQ(dst, expect);
  marker_map expm({{0x320, 0x2L}, {0x320, 0x2L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(count_markers(map), 2);
}

TEST(TransformsTests, TestNormalizeComplex10StackOf2x1x2) {
  // from tests
  marker_map map;
  const std::u32string src    = U"9ce\u0300\uFFFF\u0008\u0002\uFFFF\u0008\u0001\uFFFF\u0008\u0003\u0320";
  const std::u32string expect = U"9ce\uFFFF\u0008\u0002\uFFFF\u0008\u0001\uFFFF\u0008\u0003\u0320\u0300";
  std::u32string dst          = src;
  ASSERT_TRUE(normalize_nfd_markers_segment(dst, map));
  if (dst != expect) {
    std::cout << "dst: " << Debug_UnicodeString(dst) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect) << std::endl;
  }
  ASSERT_EQ(dst, expect);
  marker_map expm({{0x320, 0x2L}, {0x320, 0x1L}, {0x320, 0x3L}});
  assert_marker_map_equal(map, expm);
}

TEST(TransformsTests, TestNormalizeDupChar) {
  marker_map map;
  const std::u32string src    = U"a\uFFFF\u0008\u0001\u0300e\uFFFF\u0008\u0002\u0300";
  const std::u32string dst    = remove_markers(src, map);
  const std::u32string expect = U"a\u0300e\u0300";  // U+0300 twice! This should be removed in 2 segments
  ASSERT_EQ(dst, expect);
  marker_map expm({{0x300, 0x1L}, {0x300, 0x2L}});
  assert_marker_map_equal(map, expm);
}

TEST(TransformsTests, TestNormalizeTwoSegmentMarkers) {
  marker_map map;
  // e\m{1}`\m{2}_E\m{3}`\m{4}_
  const std::u32string src =
      U"e\uFFFF\u0008\u0001\u0300\uFFFF\u0008\u0002\u0320E\uFFFF\u0008\u0003\u0300\uFFFF\u0008\u0004\u0320";
  // e\m{2}_\m{1}`E\m{4}_\m{3}`
  const std::u32string expect_rem =
      U"e\u0300\u0320E\u0300\u0320";
  const std::u32string expect_nfd =
      U"e\uFFFF\u0008\u0002\u0320\uFFFF\u0008\u0001\u0300E\uFFFF\u0008\u0004\u0320\uFFFF\u0008\u0003\u0300";
  auto dst_rem = remove_markers(src, &map); // note: this is bigger than a single segment. so it is a degenerate test case.
  marker_map expm({{0x300, 0x1L}, {0x320, 0x2L}, {0x300, 0x3L}, {0x320, 0x4L}});
  assert_marker_map_equal(map, expm);
  ASSERT_EQ(dst_rem, expect_rem);
  std::u32string dst_nfd = src;
  ASSERT_TRUE(normalize_nfd_markers(dst_nfd));
  if (dst_nfd != expect_nfd) {
    std::cout << "dst: " << Debug_UnicodeString(dst_nfd) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect_nfd) << std::endl;
  }
    ASSERT_EQ(dst_nfd, expect_nfd);
}

TEST(TransformsTests, TestNormalizeMarkerBeforeNFC) {
  marker_map map;
  // KA \m O -> KA \m E AA
  const std::u32string src        = U"\u0995\uFFFF\u0008\u0001\u09CB";
  const std::u32string expect_rem = U"\u0995\u09CB";
  const std::u32string expect_nfd = U"\u0995\uFFFF\u0008\u0001\u09C7\u09BE";
  auto dst_rem                    = remove_markers(src, &map);
  marker_map expm({{0x09C7, 0x1L}});
  ASSERT_EQ(dst_rem, expect_rem);
  std::u32string dst_nfd = src;
  ASSERT_TRUE(normalize_nfd_markers(dst_nfd));
  if (dst_nfd != expect_nfd) {
    std::cout << "dst: " << Debug_UnicodeString(dst_nfd) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect_nfd) << std::endl;
  }
  ASSERT_EQ(dst_nfd, expect_nfd);
  assert_marker_map_equal(map, expm);
}

TEST(TransformsTests, TestNormalizeMarkerBeforeNFC2) {
  marker_map map;
  const std::u32string src        = U"\u0995\u09BE\uFFFF\u0008\u0001\u09C7";
  const std::u32string expect_rem = U"\u0995\u09BE\u09C7";
  const std::u32string expect_nfd = src; // does not get reordered
  auto dst_rem                    = remove_markers(src, &map);
  marker_map expm({{0x09C7, 0x1L}});
  ASSERT_EQ(dst_rem, expect_rem);
  std::u32string dst_nfd = src;
  ASSERT_TRUE(normalize_nfd_markers(dst_nfd));
  if (dst_nfd != expect_nfd) {
    std::cout << "dst: " << Debug_UnicodeString(dst_nfd) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect_nfd) << std::endl;
  }
  ASSERT_EQ(dst_nfd, expect_nfd);
  assert_marker_map_equal(map, expm);
}

TEST(TransformsTests, TestNormalizeMarkerBeforeNFC3) {
  marker_map map;
  const std::u32string src        = U"\u0995\u09BE\uFFFF\u0008\u0001\u09C7";
  const std::u32string expect_rem = U"\u0995\u09BE\u09C7";
  const std::u32string expect_nfd = src; // does not get reordered
  auto dst_rem                    = remove_markers(src, &map);
  marker_map expm({{0x09C7, 0x1L}});
  ASSERT_EQ(dst_rem, expect_rem);
  std::u32string dst_nfd = src;
  ASSERT_TRUE(normalize_nfd_markers(dst_nfd));
  if (dst_nfd != expect_nfd) {
    std::cout << "dst: " << Debug_UnicodeString(dst_nfd) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect_nfd) << std::endl;
  }
  ASSERT_EQ(dst_nfd, expect_nfd);
  assert_marker_map_equal(map, expm);
}

TEST(TransformsTests, TestNormalizeMarkerBeforeGreek) {
  marker_map map;
  const std::u32string src        = U"\u03B5\uFFFF\u0008\u0001\u0344";
  const std::u32string expect_rem = U"\u03B5\u0344";
  const std::u32string expect_nfd = U"\u03B5\uFFFF\u0008\u0001\u0308\u0301";
  auto dst_rem                    = remove_markers(src, &map);
  marker_map expm({{0x0308, 0x1L}});
  ASSERT_EQ(dst_rem, expect_rem);
  std::u32string dst_nfd = src;
  ASSERT_TRUE(normalize_nfd_markers(dst_nfd));
  if (dst_nfd != expect_nfd) {
    std::cout << "dst: " << Debug_UnicodeString(dst_nfd) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect_nfd) << std::endl;
  }
  ASSERT_EQ(dst_nfd, expect_nfd);
  assert_marker_map_equal(map, expm);
}

// doubled markers tests

TEST(TransformsTests, TestNormalizeDoubledMarker1) {
  marker_map map;
  const std::u32string src        = U"e\uffff\u0008\u0001\u0300\u0320\u0300";
  const std::u32string expect_rem = U"e\u0300\u0320\u0300";
  const std::u32string expect_nfd = U"e\u0320\uffff\u0008\u0001\u0300\u0300";
  auto dst_rem                    = remove_markers(src, &map);
  marker_map expm({{0x0300, 0x1L}});
  ASSERT_EQ(dst_rem, expect_rem);
  std::u32string dst_nfd = src;
  ASSERT_TRUE(normalize_nfd_markers(dst_nfd));
  if (dst_nfd != expect_nfd) {
    std::cout << "dst: " << Debug_UnicodeString(dst_nfd) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect_nfd) << std::endl;
  }
  ASSERT_EQ(dst_nfd, expect_nfd);
  assert_marker_map_equal(map, expm);
}

TEST(TransformsTests, TestNormalizeDoubledUnchangedMarker) {
  marker_map map;
  const std::u32string src        = U"e\u0320\uffff\u0008\u0001\u0300\u0300";
  const std::u32string expect_rem = U"e\u0320\u0300\u0300";
  const std::u32string expect_nfd = src;
  auto dst_rem                    = remove_markers(src, &map);
  marker_map expm({{0x300, 0x1L}});
  ASSERT_EQ(dst_rem, expect_rem);
  std::u32string dst_nfd = src;
  ASSERT_TRUE(normalize_nfd_markers(dst_nfd));
  if (dst_nfd != expect_nfd) {
    std::cout << "dst: " << Debug_UnicodeString(dst_nfd) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect_nfd) << std::endl;
  }
  ASSERT_EQ(dst_nfd, expect_nfd);
  assert_marker_map_equal(map, expm);
}

TEST(TransformsTests, TestNormalizeMarkerBeforeDoubleGreek) {
  marker_map map;
  const std::u32string src        = U"\u03B5\uFFFF\u0008\u0001\u0344\uFFFF\u0008\u0002\u0344\uFFFF\u0008\u0003";
  const std::u32string expect_rem = U"\u03B5\u0344\u0344";
  const std::u32string expect_nfd = U"\u03B5\uFFFF\u0008\u0001\u0308\u0301\uFFFF\u0008\u0002\u0308\u0301\uFFFF\u0008\u0003";
  auto dst_rem                    = remove_markers(src, &map);
  marker_map expm({{0x0308, 0x1L},{0x0308, 0x2L},{MARKER_BEFORE_EOT, 0x3L}});
  ASSERT_EQ(dst_rem, expect_rem);
  std::u32string dst_nfd = src;
  ASSERT_TRUE(normalize_nfd_markers(dst_nfd));
  if (dst_nfd != expect_nfd) {
    std::cout << "dst: " << Debug_UnicodeString(dst_nfd) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect_nfd) << std::endl;
  }
  ASSERT_EQ(dst_nfd, expect_nfd);
  assert_marker_map_equal(map, expm);
}

void TEST_NFD_PLAIN(std::u32string x, std::u32string y) {
  marker_map map;
  const std::u32string src        = x;
  const std::u32string expect_nfd = y;
  std::u32string dst_nfd          = src;
  ASSERT_TRUE(normalize_nfd_markers(dst_nfd));
  if (dst_nfd != expect_nfd) {
    std::cout << "dst: " << Debug_UnicodeString(dst_nfd) << std::endl;
    std::cout << "exp: " << Debug_UnicodeString(expect_nfd) << std::endl;
  }
  ASSERT_EQ(dst_nfd, expect_nfd);
}

TEST(TransformsTests, TestNormalizeNFDDoubleMarker) {
  // double marker - in front of second, no change
  ASSERT_NO_FATAL_FAILURE(TEST_NFD_PLAIN(U"e\u0320\u0300\uffff\u0008\u0001\u0300", U"e\u0320\u0300\uffff\u0008\u0001\u0300"));
}

TEST(TransformsTests, TestNormalizeNFDDoubleMarkerWithSegmentReordering) {
  // double marker - in front of second, with segment reordering
  ASSERT_NO_FATAL_FAILURE(TEST_NFD_PLAIN(U"e\u0300\u0320\uffff\u0008\u0001\u0300", U"e\u0320\u0300\uffff\u0008\u0001\u0300"));
}
TEST(TransformsTests, TestNormalizeNFDDoubleMarkerAlternatePattern) {
  // double marker - alternate pattern with reordering needed
  ASSERT_NO_FATAL_FAILURE(TEST_NFD_PLAIN(U"e\u0300\uffff\u0008\u0001\u0300\u0320", U"e\u0320\u0300\uffff\u0008\u0001\u0300"));
}

TEST(TransformsTests, TestNormalizeNFDTripleDiacriticAndMarker) {
  // triple diacritic + marker - reordering needed
  ASSERT_NO_FATAL_FAILURE(TEST_NFD_PLAIN(U"e\u0300\uffff\u0008\u0001\u0300\u0320\u0300", U"e\u0320\u0300\uffff\u0008\u0001\u0300\u0300"));
}

/* tests for the util_regex.hpp functions */

TEST(TransformsTests, TestUtilRegexNull) {
  km::core::util::km_regex r;
  ASSERT_TRUE(!r.valid()); // not valid because of an empty string
}

TEST(TransformsTests, TestUtilRegexSimple) {
  km::core::util::km_regex r(U"ion");
  ASSERT_TRUE(r.valid());
  const std::u32string to(U"ivity");
  const std::deque<std::u32string> fromList;
  const std::deque<std::u32string> toList;
  std::u32string output;
  auto apply0 = r.apply(U"not present", output, to, fromList, toList);
  ASSERT_EQ(apply0, 0); // not found

  const std::u32string input(U"action");
  auto apply1 = r.apply(input, output, to, fromList, toList);
  ASSERT_EQ(apply1, 3); // matched last 3 codepoints
  std::u32string expect(U"ivity");
  ASSERT_EQ(output, expect);
}

TEST(TransformsTests, TestUtilRegexWide) {
  km::core::util::km_regex r(U"e𐒻");
  ASSERT_TRUE(r.valid());
  const std::u32string to(U"𐓏");
  const std::deque<std::u32string> fromList;
  const std::deque<std::u32string> toList;
  std::u32string output;
  const std::u32string input(U":e𐒻");
  auto apply1 = r.apply(input, output, to, fromList, toList);
  ASSERT_EQ(apply1, 2); // matched last 2 codepoints
  std::u32string expect(U"𐓏");
  ASSERT_EQ(output, expect);
}

TEST(TransformsTests, TestUtilRegexSimpleMap) {
  km::core::util::km_regex r(U"(A|B|C)");
  ASSERT_TRUE(r.valid());
  const std::u32string to(U"$[1:alpha2]"); // ignored
  std::deque<std::u32string> fromList;
  fromList.emplace_back(U"A");
  fromList.emplace_back(U"B");
  fromList.emplace_back(U"C");
  std::deque<std::u32string> toList;
  toList.emplace_back(U"N");
  toList.emplace_back(U"O");
  toList.emplace_back(U"P");
  std::u32string output;
  auto apply0 = r.apply(U"not present", output, to, fromList, toList);
  ASSERT_EQ(apply0, 0); // not found

  const std::u32string input(U"WHOA");
  auto apply1 = r.apply(input, output, to, fromList, toList);
  ASSERT_EQ(apply1, 1); // matched last 1 codepoint
  std::u32string expect(U"N");
  ASSERT_EQ(output, expect);
}

TEST(TransformsTests, TestUtilRegexWideMap) {
  km::core::util::km_regex r(U"(𐒷|𐒻|𐓏𐓏|x)");
  ASSERT_TRUE(r.valid());
  const std::u32string to(U"$[1:alpha2]"); // ignored
  std::deque<std::u32string> fromList;
  fromList.emplace_back(U"𐒷");
  fromList.emplace_back(U"𐒻");
  fromList.emplace_back(U"𐓏𐓏");
  fromList.emplace_back(U"x");
  std::deque<std::u32string> toList;
  toList.emplace_back(U"x");
  toList.emplace_back(U"𐒷");
  toList.emplace_back(U"𐒻");
  toList.emplace_back(U"𐓏");
  std::u32string output;
  auto apply0 = r.apply(U"not present", output, to, fromList, toList);
  ASSERT_EQ(apply0, 0); // not found

  ASSERT_EQ(r.apply(U"WHO𐓏𐒷", output, to, fromList, toList), 1);
  ASSERT_EQ(output, U"x");

  ASSERT_EQ(r.apply(U"WHO𐓏x", output, to, fromList, toList), 1);
  ASSERT_EQ(output, U"𐓏");

  ASSERT_EQ(r.apply(U"WHO𐓏𐓏", output, to, fromList, toList), 2); // 2 codepoints
  ASSERT_EQ(output, U"𐒻");
}
