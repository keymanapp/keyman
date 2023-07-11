#include "../../../src/ldml/ldml_transforms.hpp"
#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include <iostream>
#include <string>
#include <test_assert.h>

// TODO-LDML: normal asserts wern't working, so using some hacks.
// #include "ldml_test_utils.hpp"
// #include <test_assert.h>
// #include "debuglog.h"

#ifndef zassert_string_equal
#define zassert_string_equal(actual, expected)                                 \
  {                                                                            \
    if (actual != expected) {                                                  \
      std::cerr << __FILE__ << ":" << __LINE__ << ": "                         \
                << "got: " << actual << " expected " << expected << std::endl; \
      return EXIT_FAILURE;                                                     \
    }                                                                          \
  }
#endif

#ifndef zassert_equal
#define zassert_equal(actual, expected) zassert_string_equal(actual, expected)
#endif

// needed for streaming operators
#include "utfcodec.hpp"

using namespace km::kbp::ldml;
using namespace km::kbp::kmx;

// using km::kbp::kmx::u16cmp;

int
test_transforms() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  std::cout << __FILE__ << ":" << __LINE__ << " - basic " << std::endl;
  {
    // start with one
    transform_entry te(std::u16string(u"e^"), std::u16string(u"E"));  // keep it simple
    // OK now make a group do it
    transforms tr;
    transform_group st;

    st.push_back(te);

    tr.addGroup(st);

    // see if we can match the same
    {
      std::u16string src(u"barQ^");
      bool res = tr.apply(src);
      zassert_equal(res, false);
      zassert_string_equal(src, std::u16string(u"barQ^"));  // no change
    }

    {
      std::u16string src(u"fooe^");
      bool res = tr.apply(src);
      zassert_equal(res, true);
      zassert_string_equal(src, std::u16string(u"fooE"));
    }
  }

  std::cout << __FILE__ << ":" << __LINE__ << " - more complex " << std::endl;

  {
    transforms tr;

    // setup
    {
      transform_group st;
      st.emplace_back(std::u16string(u"za"), std::u16string(u"c"));
      st.emplace_back(std::u16string(u"a"), std::u16string(u"bb"));
      tr.addGroup(st);
    }
    {
      transform_group st;
      st.emplace_back(std::u16string(u"bb"), std::u16string(u"ccc"));
      tr.addGroup(st);
    }
    {
      transform_group st;
      st.emplace_back(std::u16string(u"cc"), std::u16string(u"d"));
      tr.addGroup(st);
    }
    {
      transform_group st;
      st.emplace_back(std::u16string(u"tcd"), std::u16string(u"e"));
      tr.addGroup(st);
    }

    // now test

    // see if we can match the same
    {
      std::u16string src(u"ta");
      bool res = tr.apply(src);
      // pipe (|) symbol shows where the 'output' is delineated
      // t|a --> t|bb --> t|ccc --> t|cd --> |e
      zassert_string_equal(src, std::u16string(u"e"));
      zassert_equal(res, true);
    }
    {
      std::u16string src(u"qza");
      bool res = tr.apply(src);
      // pipe (|) symbol shows where the 'output' is delineated
      // q|za -> q|c
      zassert_string_equal(src, std::u16string(u"qc"));
      zassert_equal(res, true);
    }
    {
      std::u16string src(u"qa");
      bool res = tr.apply(src);
      zassert_string_equal(src, std::u16string(u"qcd"));
      zassert_equal(res, true);
    }
    {
      std::u16string src(u"tb");
      bool res = tr.apply(src);
      zassert_string_equal(src, std::u16string(u"tb"));
      zassert_equal(res, false);
    }
  }

  std::cout << __FILE__ << ":" << __LINE__ << " - hindi example " << std::endl;

  {
    transforms tr;
    {
      transform_group st;
      st.emplace_back(std::u16string(u"िह"), std::u16string(u"हि"));
      tr.addGroup(st);
    }
    {
      std::u16string src(u"िह");
      bool res = tr.apply(src);
      zassert_string_equal(src, std::u16string(u"हि"));
      zassert_equal(res, true);
    }
  }

  return EXIT_SUCCESS;
}

int
test_reorder_standalone() {
  std::cout << "== " << __FUNCTION__ << std::endl;

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
                                              {0x1A75, 0x1A79}};
    const COMP_KMXPLUS_USET_USET usets[]        = {{0, 1, 0xFFFFFFFF}};
    const COMP_KMXPLUS_USET_USET &toneMarksUset = usets[0];
    const USet toneMarks(&ranges[toneMarksUset.range], toneMarksUset.count);
    // validate that the range [1A75, 1A79] matches
    assert_equal(toneMarks.contains(0x1A76), true);
    assert_equal(toneMarks.contains(0x1A60), false);

    std::cout << __FILE__ << ":" << __LINE__ << " - element test " << std::endl;
    // element test
    {
      element es(U'a', 0xF4500000 | LDML_ELEM_FLAGS_PREBASE | LDML_ELEM_FLAGS_TERTIARY_BASE); // tertiary -12, primary 80
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

      element_list l; // '[tones]a'
      l.emplace_back(es);
      l.emplace_back(eu);

      std::cout << __FILE__ << ":" << __LINE__ << " - list test " << std::endl;
      assert_equal(l.match_end(U"asdfasdf"), 0); // no match
      assert_equal(l.match_end(U"a"), 0);        // partial substring, fastpath because it's short
      assert_equal(l.match_end(U"\u1A76"), 0);   // partial substring, fastpath because it's short
      assert_equal(l.match_end(U"a\u1A76"), 2);  // Match
      assert_equal(l.match_end(U"a\u1A75"), 2);  // Match
      assert_equal(l.match_end(U"SomethingSomethingSomethinga\u1A76"), 2);  // Sub-Match

      // generate sort keys
      std::cout << __FILE__ << ":" << __LINE__ << " - get_sort_key test " << std::endl;
      auto keylist = l.get_sort_key(U"a\u1A7A");
      assert_equal(keylist.size(), 2);
      size_t secondary = 0;
      for (auto i = keylist.begin(); i < keylist.end(); i++) {
        std::cout << " U+" << std::hex << i->ch << std::dec << " (" << (int)i->primary << "," << (int)i->secondary
          << "," << (int)i->tertiary << "," << (int)i->quaternary << ")";
          assert_equal(i->secondary, secondary);
          assert_equal(i->quaternary, secondary);
          secondary++;
      }
      std::cout << std::endl;

      // spot check first sortkey
      assert_equal(keylist.begin()->primary, 80);
      assert_equal(keylist.begin()->tertiary, -12);
      assert_equal(keylist.begin()->ch, 0x61);

      // now sort them
      std::sort(keylist.begin(), keylist.end());
      for (auto i = keylist.begin(); i < keylist.end(); i++) {
        std::cout << " U+" << std::hex << i->ch << std::dec << " (" << (int)i->primary << "," << (int)i->secondary
          << "," << (int)i->tertiary << "," << (int)i->quaternary << ")";
      }
      std::cout << std::endl;
      // spot check first sort key
      assert_equal(keylist.begin()->primary, -12);
      assert_equal(keylist.begin()->tertiary, 55);
      assert_equal(keylist.begin()->ch, 0x1A7A);
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
      for (size_t i = 0; i < sizeof(keys0)/sizeof(keys0[0]); i++) {
        keylist.emplace_back(keys0[i]);
      }
      // now sort it
      std::sort(keylist.begin(), keylist.end());
      std::u32string sorted;
      std::cout << " sorted: ";
      for (auto i = keylist.begin(); i < keylist.end(); i++) {
        std::cout << " U+" << std::hex << i->ch << std::dec << " (" << (int)i->primary << "," << (int)i->secondary
          << "," << (int)i->tertiary << "," << (int)i->quaternary << ")";
        sorted.append(1, i->ch);
      }
      std::cout << std::endl;
      // did the roast come out?
      zassert_string_equal(sorted, expect);
    }
    std::cout << "now prepare the reorder elements" << std::endl;
    transforms tr;
    {
      reorder_group rg;

      // <reorder from="\u1A60" order="127" />
      element_list e0;
      e0.emplace_back(U'\u1A6B', 127 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
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
      e3.emplace_back(U'\u1A60', 55 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      e3.emplace_back(U'\u1A45', 55 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
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
    for (size_t r = 0; r < sizeof(roasts)/sizeof(roasts[0]); r++) {
      std::cout << __FILE__ << ":" << __LINE__ << " - trying roast #" << r << std::endl;
      const auto &roast = roasts[r];
      // simulate typing this one char at a time;
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
      std::cout << " matched!" << std::endl;
      std::cout << std::endl;
    }
  }
  return EXIT_SUCCESS;
}

int
main(int argc, const char *argv[]) {
  int rc = EXIT_SUCCESS;

  if (test_transforms() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (test_reorder_standalone() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (rc == EXIT_FAILURE) {
    std::cout << "== FAILURE" << std::endl;
  } else {
    std::cout << "== PASS" << std::endl;
  }

  return rc;
}
