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
    const std::u16string roasts[] = {
        // from the spec
        u"\u1A21\u1A60\u1A45\u1A6B\u1A76",  // 'ideal'
        u"\u1A21\u1A6B\u1A76\u1A60\u1A45",  // vowel first
        u"\u1A21\u1A6B\u1A60\u1A76\u1A45",  // vowel first, NFC
        u"\u1A21\u1A6B\u1A60\u1A45\u1A76",  // tone after lower
    };
    /** we expect this is what comes out the other side */
    const std::u16string expect = roasts[0];
    // now setup the rules
    const COMP_KMXPLUS_USET_RANGE ranges[]      = {// 0
                                              {0x1A75, 0x1A79}};
    const COMP_KMXPLUS_USET_USET usets[]        = {{0, 1, 0xFFFFFFFF}};
    const COMP_KMXPLUS_USET_USET &toneMarksUset = usets[0];
    const USet toneMarks(&ranges[toneMarksUset.range], toneMarksUset.count);
    // validate
    assert_equal(toneMarks.contains(0x1A76), true);
    assert_equal(toneMarks.contains(0x1A60), false);

    // element test
    {
      element es(u"a", 123 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      std::cout << "es flags" << std::hex << es.get_flags() << std::dec << std::endl;
      assert_equal(es.is_uset(), false);
      assert_equal(es.get_order(), 123);
      element eu(toneMarks, 43 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      std::cout << "eu flags" << std::hex << eu.get_flags() << std::dec << std::endl;
      assert_equal(eu.is_uset(), true);
      std::cout << "order" << eu.get_order() << std::endl;
      assert_equal(eu.get_order(), 43);
    }

    transforms tr;
    {
      reorder_group rg;

      // <reorder from="\u1A60" order="127" />
      element_list e0;
      e0.emplace_back(u"\u1A6B", 127 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      rg.list.emplace_back(e0);

      // <reorder from="\u1A6B" order="42" />
      element_list e1;
      e1.emplace_back(u"\u1A6B", 42 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      rg.list.emplace_back(e1);

      // <reorder from="[\u1A75-\u1A79]" order="55" />
      element_list e2;
      e2.emplace_back(toneMarks, 55 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      rg.list.emplace_back(e2);
      // <reorder before="\u1A6B" from="\u1A60\u1A45" order="10" />
      element_list e3;
      e3.emplace_back(u"\u1A60", 55 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      e3.emplace_back(u"\u1A45", 55 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      element_list e3before;
      e3before.emplace_back(u"\u1A6B", 0);
      rg.list.emplace_back(e3, e3before);

      // <reorder before="\u1A6B[\u1A75-\u1A79]" from="\u1A60\u1A45" order="10" />
      element_list e4;
      e4.emplace_back(u"\u1A60", 10 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      e4.emplace_back(u"\u1A45", 10 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      element_list e4before;
      e4before.emplace_back(u"\u1A6B", 0);
      e4before.emplace_back(toneMarks, 0);
      rg.list.emplace_back(e4, e4before);
      // <reorder before="\u1A6B" from="\u1A60[\u1A75-\u1A79]\u1A45" order="10 55 10" />
      element_list e5;
      e5.emplace_back(u"\u1A60", 10 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      e5.emplace_back(toneMarks, 55 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      e5.emplace_back(u"\u1A45", 10 << LDML_ELEM_FLAGS_ORDER_BITSHIFT);
      element_list e5before;
      e5before.emplace_back(u"\u1A6B", 0);
      rg.list.emplace_back(e5, e5before);

      tr.addGroup(rg);
    }

    // now actually test it
    // TODO-LDML: move this into test code perhaps
    for (int r = 0; r < std::size(roasts); r++) {
      std::cout << __FILE__ << ":" << __LINE__ << " - trying roast #" << r << std::endl;
      const auto &roast = roasts[r];
      // simulate typing this one char at a time;
      std::u16string text;
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
