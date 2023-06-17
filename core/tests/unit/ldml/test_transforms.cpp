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

    tr.addTransformGroup(st);

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
      st.emplace_back(std::u16string(u"a"), std::u16string(u"bb"));
      tr.addTransformGroup(st);
    }
    {
      transform_group st;
      st.emplace_back(std::u16string(u"bb"), std::u16string(u"ccc"));
      tr.addTransformGroup(st);
    }
    {
      transform_group st;
      st.emplace_back(std::u16string(u"cc"), std::u16string(u"d"));
      tr.addTransformGroup(st);
    }
    {
      transform_group st;
      st.emplace_back(std::u16string(u"tcd"), std::u16string(u"e"));
      tr.addTransformGroup(st);
    }

    // now test

    // see if we can match the same
    {
      std::u16string src(u"ta");
      bool res = tr.apply(src);
      // ta --> tbb --> tccc --> tcd --> e
      zassert_string_equal(src, std::u16string(u"e"));
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

  return EXIT_SUCCESS;
}

int
main(int argc, const char *argv[]) {
  int rc = EXIT_SUCCESS;

  if (test_transforms() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }


  if (rc == EXIT_FAILURE) {
    std::cout << "== FAILURE" << std::endl;
  } else {
    std::cout << "== PASS" << std::endl;
  }

  return rc;
}
