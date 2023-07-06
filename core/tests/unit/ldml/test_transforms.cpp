#include <test_assert.h>
#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include "../../../src/ldml/ldml_transforms.hpp"
#include <iostream>
#include <string>

// TODO-LDML: normal asserts wern't working, so using some hacks.
//#include "ldml_test_utils.hpp"
// #include <test_assert.h>
//#include "debuglog.h"

#ifndef zassert_string_equal
#define zassert_string_equal(actual, expected) { if (actual != expected) { std::cerr << __FILE__ << ":" << __LINE__ << ": " << "got: " << actual << " expected " << expected << std::endl; return EXIT_FAILURE; }}
#endif

#ifndef zassert_equal
#define zassert_equal(actual, expected) zassert_string_equal(actual,expected)
#endif

// needed for streaming operators
#include "utfcodec.hpp"


using namespace km::kbp::ldml;
using namespace km::kbp::kmx;

//using km::kbp::kmx::u16cmp;

int test_transforms() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  std::cout << __FILE__ << ":" << __LINE__ << " - basic " << std::endl;
  {
    // start with one
    transform_entry te(std::u16string(u"e^"), std::u16string(u"E")); // keep it simple
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
      zassert_string_equal(src, std::u16string(u"barQ^")); // no change
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

int test_reorder_standalone() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  std::cout << __FILE__ << ":" << __LINE__ << " - nod-Lana " << std::endl;
  {
    const std::u16string roasts[] = {
      // from the spec
      u"\u1A21\u1A60\u1A45\u1A6B\u1A76", // 'ideal'
      u"\u1A21\u1A6B\u1A76\u1A60\u1A45", // vowel first
      u"\u1A21\u1A6B\u1A60\u1A76\u1A45", // vowel first, NFC
      u"\u1A21\u1A6B\u1A60\u1A45\u1A76", // tone after lower
    };
    const std::u16string expect = roasts[0];
    /*
          <!-- from the spec: nod-Lana for ᩅᩫ᩶  -->
      <reorder from="\u1A60" order="127" />
      <reorder from="\u1A6B" order="42" />
      <reorder from="[\u1A75-\u1A79]" order="55" />
      <reorder before="\u1A6B" from="\u1A60\u1A45" order="10" />
      <reorder before="\u1A6B[\u1A75-\u1A79]" from="\u1A60\u1A45" order="10" />
      <reorder before="\u1A6B" from="\u1A60[\u1A75-\u1A79]\u1A45" order="10 55 10" />
    */
   // now setup the rules
    const COMP_KMXPLUS_USET_RANGE ranges[] = {
      // 0
      {0x1A75, 0x1A79}
    };
    const COMP_KMXPLUS_USET_USET usets[] = {
      {
        0, 1, 0xFFFFFFFF
      }
    };
    const COMP_KMXPLUS_USET_USET &toneMarksUset = usets[0];
    const USet toneMarks(&ranges[toneMarksUset.range], toneMarksUset.count);
    // validate
    assert_equal(toneMarks.contains(0x1A76), true);
    assert_equal(toneMarks.contains(0x1A60), false);
    // TODO-LDML: build the transforms group.
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
