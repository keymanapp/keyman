#include "../../../src/ldml/ldml_transforms.hpp"
#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include <iostream>
#include <string>
#include <test_assert.h>
#include "test_color.h"

// TODO-LDML: normal asserts wern't working, so using some hacks.
// #include "ldml_test_utils.hpp"
// #include <test_assert.h>
// #include "debuglog.h"

#ifndef zassert_string_equal
#define zassert_string_equal(actual, expected)                                                                               \
  {                                                                                                                          \
    if (actual != expected) {                                                                                                \
      std::wcerr << __FILE__ << ":" << __LINE__ << ": " << console_color::fg(console_color::BRIGHT_RED) << "got: " << km::core::kmx::Debug_UnicodeString(actual, 0) \
                 << " expected " << km::core::kmx::Debug_UnicodeString(expected, 1) << console_color::reset() << std::endl;                                         \
      return EXIT_FAILURE;                                                                                                   \
    }                                                                                                                        \
  }
#endif

#ifndef zassert_equal
#define zassert_equal(actual, expected)                                                                             \
  {                                                                                                                          \
    if (actual != expected) {                                                                                                \
      std::wcerr << __FILE__ << ":" << __LINE__ << ": " << console_color::fg(console_color::BRIGHT_RED) << "got: " << actual \
                 << " expected " << expected << console_color::reset() << std::endl;                                         \
      return EXIT_FAILURE;                                                                                                   \
    }                                                                                                                        \
  }
#endif

// needed for streaming operators
#include "utfcodec.hpp"

using namespace km::core::ldml;
using namespace km::core::kmx;

// using km::core::kmx::u16cmp;

int
test_transforms() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  std::cout << __FILE__ << ":" << __LINE__ << " - basic " << std::endl;
  {
    // start with one
    transform_entry te(std::u32string(U"e\\^"), std::u32string(U"E"));  // keep it simple
    // OK now make a group do it
    transforms tr;
    transform_group st;

    st.push_back(te);

    tr.addGroup(st);

    // see if we can match the same
    {
      std::u32string src(U"barQ^");
      bool res = tr.apply(src);
      zassert_equal(res, false);
      zassert_string_equal(src, std::u32string(U"barQ^"));  // no change
    }

    {
      std::u32string src(U"fooe^");
      bool res = tr.apply(src);
      zassert_equal(res, true);
      zassert_string_equal(src, std::u32string(U"fooE"));
    }
  }

  std::cout << __FILE__ << ":" << __LINE__ << " - more complex " << std::endl;

  {
    transforms tr;

    // setup
    {
      transform_group st;
      st.emplace_back(std::u32string(U"za"), std::u32string(U"c"));
      st.emplace_back(std::u32string(U"a"), std::u32string(U"bb"));
      tr.addGroup(st);
    }
    {
      transform_group st;
      st.emplace_back(std::u32string(U"bb"), std::u32string(U"ccc"));
      tr.addGroup(st);
    }
    {
      transform_group st;
      st.emplace_back(std::u32string(U"cc"), std::u32string(U"d"));
      tr.addGroup(st);
    }
    {
      transform_group st;
      st.emplace_back(std::u32string(U"tcd"), std::u32string(U"e"));
      tr.addGroup(st);
    }

    // now test

    // see if we can match the same
    {
      std::u32string src(U"ta");
      bool res = tr.apply(src);
      // pipe (|) symbol shows where the 'output' is delineated
      // t|a --> t|bb --> t|ccc --> t|cd --> |e
      zassert_string_equal(src, std::u32string(U"e"));
      zassert_equal(res, true);
    }
    {
      std::u32string src(U"qza");
      bool res = tr.apply(src);
      // pipe (|) symbol shows where the 'output' is delineated
      // q|za -> q|c
      zassert_string_equal(src, std::u32string(U"qc"));
      zassert_equal(res, true);
    }
    {
      std::u32string src(U"qa");
      bool res = tr.apply(src);
      zassert_string_equal(src, std::u32string(U"qcd"));
      zassert_equal(res, true);
    }
    {
      std::u32string src(U"tb");
      bool res = tr.apply(src);
      zassert_string_equal(src, std::u32string(U"tb"));
      zassert_equal(res, false);
    }
  }

  std::cout << __FILE__ << ":" << __LINE__ << " - hindi example " << std::endl;

  {
    transforms tr;
    {
      transform_group st;
      st.emplace_back(std::u32string(U"िह"), std::u32string(U"हि"));
      tr.addGroup(st);
    }
    {
      std::u32string src(U"िह");
      bool res = tr.apply(src);
      zassert_string_equal(src, std::u32string(U"हि"));
      zassert_equal(res, true);
    }
  }

  return EXIT_SUCCESS;
}

int
test_reorder_standalone() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  std::cout << __FILE__ << ":" << __LINE__ << " - element API test " << std::endl;
  // element API test - not a real element, just here for testing
  {
    element es(U'a', 0xF4500000 | LDML_ELEM_FLAGS_PREBASE | LDML_ELEM_FLAGS_TERTIARY_BASE);  // tertiary -12, primary 80
    std::cout << "es flags" << std::hex << es.get_flags() << std::dec << std::endl;
    // verify element metadata
    assert_equal(es.is_uset(), false);
    assert_equal(es.get_order(), 0x50);
    assert_equal(es.get_tertiary(), -12);
    assert_equal(es.is_prebase(), true);
    assert_equal(es.is_tertiary_base(), true);
    // verify element matching
    assert_equal(es.matches(U'a'), true);
    assert_equal(es.matches(U'b'), false);
  }

  std::cout << __FILE__ << ":" << __LINE__ << " - nod-Lana " << std::endl;
  {
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
    assert_equal(toneMarks.contains(0x1A76), true);
    assert_equal(toneMarks.contains(0x1A60), false);

    std::cout << __FILE__ << ":" << __LINE__ << " - element API test " << std::endl;
    // element test
    {
      element es(U'a', (80 << LDML_ELEM_FLAGS_ORDER_BITSHIFT)  | LDML_ELEM_FLAGS_PREBASE);  // tertiary -12, primary 80
      std::cout << "es flags" << std::hex << es.get_flags() << std::dec << std::endl;
      // verify element metadata
      assert_equal(es.is_uset(), false);
      assert_equal(es.get_order(), 0x50);
      assert_equal(es.get_tertiary(), 0);
      assert_equal(es.is_prebase(), true);
      assert_equal(es.is_tertiary_base(), false);
      // verify element matching
      assert_equal(es.matches(U'a'), true);
      assert_equal(es.matches(U'b'), false);

      element eu(toneMarks, 0x37F40000);
      // element metadata
      std::cout << "eu flags" << std::hex << eu.get_flags() << std::dec << std::endl;
      assert_equal(eu.is_uset(), true);
      std::cout << "order" << (int)eu.get_order() << std::endl;
      assert_equal(eu.get_order(), -12);
      assert_equal(eu.get_tertiary(), 55);
      assert_equal(eu.is_prebase(), false);
      assert_equal(eu.is_tertiary_base(), false);
      // element matching
      assert_equal(eu.matches(U'a'), false);
      assert_equal(eu.matches(U'\u1A76'), true);
      assert_equal(eu.matches(U'\u1A75'), true);

      element_list l;  // '[tones]a'
      l.emplace_back(es);
      l.emplace_back(eu);

      std::cout << __FILE__ << ":" << __LINE__ << " - list test " << std::endl;
      assert_equal(l.match_end(U"asdfasdf"), 0);                            // no match
      assert_equal(l.match_end(U"a"), 0);                                   // partial substring, fastpath because it's short
      assert_equal(l.match_end(U"\u1A76"), 0);                              // partial substring, fastpath because it's short
      assert_equal(l.match_end(U"a\u1A76"), 2);                             // Match
      assert_equal(l.match_end(U"a\u1A75"), 2);                             // Match
      assert_equal(l.match_end(U"SomethingSomethingSomethinga\u1A76"), 2);  // Sub-Match

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
      assert_equal(l.match_end(str), 2);
      // update the keylist with these elements.
      l.update_sort_key(0, keylist);
      std::cout << __FILE__ << ":" << __LINE__ << "  updated sortkey" << std::endl;
      assert_equal(keylist.size(), 2);
      size_t secondary = 0;
      for (auto i = keylist.begin(); i < keylist.end(); i++) {
        i->dump();
        assert_equal(i->secondary, secondary);
        assert_equal(i->quaternary, secondary);
        secondary++;
      }
      std::cout << std::endl;

      // spot check first sortkey
      assert_equal(keylist.begin()->primary, 80);
      assert_equal(keylist.begin()->tertiary, 0);
      assert_equal(keylist.begin()->ch, 0x61);
      std::cout << __FILE__ << ":" << __LINE__ << "  sorted sortkey" << std::endl;

      // now sort them
      std::sort(keylist.begin(), keylist.end());
      for (auto i = keylist.begin(); i < keylist.end(); i++) {
        i->dump();
      }
      std::cout << std::endl;
      // spot check first sort key
      assert_equal(keylist.begin()->primary, -12);
      assert_equal(keylist.begin()->tertiary, 55);
      assert_equal(keylist.begin()->ch, 0x1A78);
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
      zassert_string_equal(sorted, expect);
    }
    std::cout << "now prepare the reorder elements" << std::endl;
    transforms tr;
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

      tr.addGroup(rg);
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
          size_t len = tr.apply(text, output);
          if (len == 0) {
            std::cout << " (did not apply)" << std::endl;
          } else {
            std::cout << " applied, matchLen= " << len << std::endl;
            text.resize(text.size()-len); // shrink
            text.append(output);
            std::cout << " = " << text << std::endl;
          }
          zassert_string_equal(text, expect);
      }
      // try all-at-once
      {
        std::cout << "- try apply(text)" << std::endl;
        std::u32string text = roast;
        if (!tr.apply(text)) {
          std::cout << " (did not apply)" << std::endl;
        } else if (text == roast) {
          std::cout << " (suboptimal: apply returned true but made no change)" << std::endl;
        } else {
          std::cout << " changed to " << text;
        }
        zassert_string_equal(text, expect);
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
          if (!tr.apply(text)) {
            std::cout << " (did not apply)" << std::endl;
          }
        }
        // now the moment of truth
        zassert_string_equal(text, expect);
        std::cout << " matched! (converting char at a time)" << std::endl;
        std::cout << std::endl;
      }
    }
    // special test
    {
      std::cout << __FILE__ << ":" << __LINE__ << " - special test " << std::endl;
      const std::u32string expect = U"\u1A21\u1A60\u1A45"; // this string shouldn't mutate at all.
      {
        std::u32string text = expect;
        tr.apply(text);
        zassert_string_equal(text, expect);
      }
      {
        // try submatch
        std::u32string text = expect;
        std::u32string output;
        size_t len = tr.apply(text, output);
        zassert_string_equal(output, U"");
        assert_equal(len, 0);
      }
    }
  }
  return EXIT_SUCCESS;
}


// this test case is also in XML form under 'k_201_*'
int
test_reorder_esk() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  std::cout << __FILE__ << ":" << __LINE__ << " - k_201_reorder_esk (tertiary reordering) " << std::endl;
  {
    // now setup the rules
    // rules are a little bit simplified, having only the vowel 'a'

    std::cout << "now prepare the reorder elements" << std::endl;
    transforms tr;
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

      tr.addGroup(rg);
    }

    // now actually test it
    std::cout << __FILE__ << ":" << __LINE__ << " - cases " << std::endl;
    const std::u32string orig_expect[] = {
      // 1short
      U"ax\u0305",          // orig
      U"a\u0305x",          // expect

      // 2longer
      U"az\u0305x\u0332",   // orig
      U"a\u0332\u0305xz",   // expect
    };
    // TODO-LDML: move this into test code perhaps
    for (size_t r = 0; r < sizeof(orig_expect) / sizeof(orig_expect[0]); r+= 2) {
      const auto &orig   = orig_expect[r + 0];
      const auto &expect = orig_expect[r + 1];
      std::cout << __FILE__ << ":" << __LINE__ << " - trying str #" << r+1 << "=" << orig << std::endl;
      // try apply with string
      {
          std::cout << "- try apply(text, output)" << std::endl;
          std::u32string text = orig;
          std::u32string output;
          size_t len = tr.apply(text, output);
          if (len == 0) {
            std::cout << " (did not apply)" << std::endl;
          } else {
            std::cout << " applied, matchLen= " << len << std::endl;
            text.resize(text.size()-len); // shrink
            text.append(output);
            std::cout << " = " << text << std::endl;
          }
          zassert_string_equal(text, expect);
      }
      // try all-at-once
      {
        std::cout << "- try apply(text)" << std::endl;
        std::u32string text = orig;
        if (!tr.apply(text)) {
          std::cout << " (did not apply)" << std::endl;
        } else if (text == orig) {
          std::cout << " (suboptimal: apply returned true but made no change)" << std::endl;
        } else {
          std::cout << " changed to " << text;
        }
        zassert_string_equal(text, expect);
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
          if (!tr.apply(text)) {
            std::cout << " (did not apply)" << std::endl;
          }
        }
        // now the moment of truth
        zassert_string_equal(text, expect);
        std::cout << " matched! (converting char at a time)" << std::endl;
        std::cout << std::endl;
      }
    }
  }
  return EXIT_SUCCESS;
}

int test_map() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  std::cout << __FILE__ << ":" << __LINE__ << "  transform_entry::findIndex" << std::endl;
  {
    std::deque<std::u32string> list;
    assert_equal(transform_entry::findIndex(U"Does Not Exist", list), -1);

    list.emplace_back(U"0th");
    list.emplace_back(U"First");
    list.emplace_back(U"Second");

    assert_equal(transform_entry::findIndex(U"First", list), 1);
    assert_equal(transform_entry::findIndex(U"0th", list), 0);
    assert_equal(transform_entry::findIndex(U"Second", list), 2);
    assert_equal(transform_entry::findIndex(U"Nowhere", list), -1);
  }

  return EXIT_SUCCESS;
}

int test_strutils() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  std::cout << __FILE__ << ":" << __LINE__ << " * remove_markers" << std::endl;


  {
    std::cout << __FILE__ << ":" << __LINE__ << "   - basic test0" << std::endl;
    const std::u32string src = U"abc";
    const std::u32string dst = remove_markers(src);
    zassert_string_equal(dst, src); // unchanged
  }
  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - basic test" << std::endl;
    const std::u32string src = U"abc";
    const std::u32string dst = remove_markers(src, map);
    zassert_string_equal(dst, src); // unchanged
    assert_equal(map.size(), 0);
  }
  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - marker test" << std::endl;
    const std::u32string src = U"6\U0000ffff\U00000008\U00000001e";
    const std::u32string dst = remove_markers(src, map);
    const std::u32string expect = U"6e";
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 1);
    assert_equal(map[U'e'], 0x1L); // marker 1 @ e
  }
  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - bad0" << std::endl;
    const std::u32string src = U"6\U0000ffff\U00000008"; // missing trailing marker #
    const std::u32string dst = remove_markers(src, map);
    const std::u32string expect = U"6";
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 0);
  }
  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - bad1" << std::endl;
    const std::u32string src = U"6\U0000ffffq"; // missing code
    const std::u32string dst = remove_markers(src, map);
    const std::u32string expect = U"6q";
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 0);
  }
  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - bad1" << std::endl;
    const std::u32string src = U"6\U0000ffff"; // missing code
    const std::u32string dst = remove_markers(src, map);
    const std::u32string expect = U"6";
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 0);
  }
  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - marker end test" << std::endl;
    const std::u32string src = U"6\U0000ffff\U00000008\U00000001";
    const std::u32string dst = remove_markers(src, map);
    const std::u32string expect = U"6";
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 1);
    assert_equal(map[MARKER_BEFORE_EOT], 0x1L); // marker 1 @ e
  }
  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - complex test" << std::endl;
    const std::u32string src = U"6\U0000ffff\U00000008\U00000001e\U0000ffff\U00000008\U00000002\U00000320\U0000ffff\U00000008\U00000003\U00000300\U0000ffff\U00000008\U00000004";
    const std::u32string dst = remove_markers(src, map);
    const std::u32string expect = U"6e\U00000320\U00000300";
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 4);
    assert_equal(map[U'e'], 0x1L);
    assert_equal(map[0x0320], 0x2L);
    assert_equal(map[0x0300], 0x3L);
    assert_equal(map[MARKER_BEFORE_EOT], 0x4L);
  }
  return EXIT_SUCCESS;
}

int test_normalize() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  std::cout << __FILE__ << ":" << __LINE__ << " * normalize_nfd_markers" << std::endl;


  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - noop test" << std::endl;
    const std::u32string src    = U"6e\U00000320\U00000300";  // already NFD
    const std::u32string expect = src;
    std::u32string dst          = src;
    assert(normalize_nfd_markers(dst, map));
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 0);
  }
  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - medium test" << std::endl;
    const std::u32string src    = U"6e\U00000300\U00000320";  // swapped
    const std::u32string expect = U"6e\U00000320\U00000300";  // correct NFD
    std::u32string dst          = src;
    assert(normalize_nfd_markers(dst, map));
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 0);
  }

  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - noop test w markers" << std::endl;
    const std::u32string src =
        U"6\U0000ffff\U00000008\U00000001e\U0000ffff\U00000008\U00000002\U00000320\U0000ffff\U00000008\U00000003\U00000300"
        U"\U0000ffff\U00000008\U00000004";
    const std::u32string expect = src;
    std::u32string dst = src;
    assert(normalize_nfd_markers(dst, map));
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 4);
    assert_equal(map[U'e'], 0x1L);
    assert_equal(map[0x0320], 0x2L);
    assert_equal(map[0x0300], 0x3L);
    assert_equal(map[MARKER_BEFORE_EOT], 0x4L);
  }

  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - complex test" << std::endl;
    const std::u32string src = // already in order: 320+300
        U"6\U0000ffff\U00000008\U00000001e\U0000ffff\U00000008\U00000002\U00000320\U0000ffff\U00000008\U00000003\U00000300\U0000ffff\U00000008\U00000004";
    const std::u32string expect = src;
    std::u32string dst = src;
    assert(normalize_nfd_markers(dst, map));
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 4);
    assert_equal(map[U'e'], 0x1L);
    assert_equal(map[0x0320], 0x2L);
    assert_equal(map[0x0300], 0x3L);
    assert_equal(map[MARKER_BEFORE_EOT], 0x4L);

  }
  {
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - complex test2" << std::endl;
    const std::u32string src = // out of order, 300-320
        U"6\U0000ffff\U00000008\U00000001e\U0000ffff\U00000008\U00000002\U00000300\U0000ffff\U00000008\U00000003\U00000320\U0000ffff\U00000008\U00000004";
    const std::u32string expect =
        U"6\U0000ffff\U00000008\U00000001e\U0000ffff\U00000008\U00000003\U00000320\U0000ffff\U00000008\U00000002\U00000300\U0000ffff\U00000008\U00000004";
    std::u32string dst = src;
    assert(normalize_nfd_markers(dst, map));
    if (dst != expect) {
      std::cout << "dst: " << Debug_UnicodeString(dst) << std::endl;
      std::cout << "exp: " << Debug_UnicodeString(expect) << std::endl;
    }
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 4);
    assert_equal(map[U'e'], 0x1L);
    assert_equal(map[0x0320], 0x3L);
    assert_equal(map[0x0300], 0x2L);
    assert_equal(map[MARKER_BEFORE_EOT], 0x4L);

  }

  {
    // u"4è\U0000ffff\b\U00000001̠"
    marker_map map;
    std::cout << __FILE__ << ":" << __LINE__ << "   - complex test 4a" << std::endl;
    const std::u32string src    = U"4e\u0300\uFFFF\b\u0001\u0320";
    const std::u32string expect = U"4e\uFFFF\b\u0001\u0320\u0300";
    std::u32string dst = src;
    assert(normalize_nfd_markers(dst, map));
    if (dst != expect) {
      std::cout << "dst: " << Debug_UnicodeString(dst) << std::endl;
      std::cout << "exp: " << Debug_UnicodeString(expect) << std::endl;
    }
    zassert_string_equal(dst, expect);
    assert_equal(map.size(), 1);
    assert_equal(map[0x0320], 0x1L);
  }

  return EXIT_SUCCESS;
}


int
main(int argc, const char *argv[]) {
  int rc = EXIT_SUCCESS;

  bool arg_color = false;

  int first_arg = 1;

  if (first_arg < argc) {
    arg_color = std::string(argv[first_arg]) == "--color";
    if(arg_color) {
      first_arg++;
    }
  }

  console_color::enabled = console_color::isaterminal() || arg_color;

  if (test_transforms() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (test_reorder_standalone() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (test_reorder_esk() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (test_map() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (test_strutils() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (test_normalize() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (rc == EXIT_FAILURE) {
    std::cout << "== FAILURE" << std::endl;
  } else {
    std::cout << "== PASS" << std::endl;
  }

  return rc;
}
