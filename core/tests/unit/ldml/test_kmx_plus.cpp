#include <test_assert.h>
#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include "../../../src/ldml/ldml_vkeys.hpp"
#include <iostream>
#include "ldml_test_utils.hpp"

// needed for streaming operators
#include "utfcodec.hpp"


using namespace km::kbp::kmx;

int test_COMP_KMXPLUS_KEYS_KEY() {
  std::cout << "== " << __FUNCTION__ << std::endl;

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
  assert_equal(s0.length(), 1);
  assert_equal(s0.at(0), 0x0127);
  assert(s0 == std::u16string(u"ħ"));

  std::u16string s1 = e[1].get_to_string();
  assert_equal(s1.length(), 2);
  assert_equal(s1.at(0), 0xD83D);
  assert_equal(s1.at(1), 0xDE40);
  assert(s1 == std::u16string(u"🙀"));

  // now, elems. Parallel.
  std::u16string es0 = elems[0].get_element_string();
  assert_equal(es0.length(), 1);
  assert_equal(es0.at(0), 0x0127);
  assert(es0 == std::u16string(u"ħ"));

  std::u16string es1 = elems[1].get_element_string();
  assert_equal(es1.length(), 2);
  assert_equal(es1.at(0), 0xD83D);
  assert_equal(es1.at(1), 0xDE40);
  assert(es1 == std::u16string(u"🙀"));

  return EXIT_SUCCESS;
}

int test_ldml_vkeys() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  km::kbp::ldml::vkeys vk;

#define ADD_VKEY(k, m) { \
  const char* str = k "-" #m ; \
  PKMX_WCHAR wstr = km::kbp::kmx::strtowstr(const_cast<PKMX_CHAR>(str)); \
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
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_A"), 0), u"K_A-0");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_A"), LCTRLFLAG), u"K_A-LCTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_A"), RCTRLFLAG), u"K_A-RCTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_A"), LALTFLAG), u"K_A-LALTFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_A"), RALTFLAG), u"K_A-RALTFLAG");

  // now try either-side keys :should get the same result with either or both
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), LCTRLFLAG), u"K_B-K_CTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), RCTRLFLAG), u"K_B-K_CTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), LCTRLFLAG|RCTRLFLAG), u"K_B-K_CTRLFLAG");

  assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), LALTFLAG), u"K_B-K_ALTFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), RALTFLAG), u"K_B-K_ALTFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_B"), LALTFLAG|RALTFLAG), u"K_B-K_ALTFLAG");

  // OOOkay now try BOTH side
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_C"), LCTRLFLAG|LALTFLAG), u"K_C-K_ALTFLAG|K_CTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_C"), LCTRLFLAG|RALTFLAG), u"K_C-K_ALTFLAG|K_CTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_C"), RCTRLFLAG|LALTFLAG), u"K_C-K_ALTFLAG|K_CTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_C"), RCTRLFLAG|RALTFLAG), u"K_C-K_ALTFLAG|K_CTRLFLAG");

  // OOOkay now try either alt
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_D"), LCTRLFLAG|LALTFLAG), u"K_D-LALTFLAG|K_CTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_D"), LCTRLFLAG|RALTFLAG), u"K_D-RALTFLAG|K_CTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_D"), RCTRLFLAG|LALTFLAG), u"K_D-LALTFLAG|K_CTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_D"), RCTRLFLAG|RALTFLAG), u"K_D-RALTFLAG|K_CTRLFLAG");

  // OOOkay now try either ctrl
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_E"), LCTRLFLAG|LALTFLAG), u"K_E-K_ALTFLAG|LCTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_E"), LCTRLFLAG|RALTFLAG), u"K_E-K_ALTFLAG|LCTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_E"), RCTRLFLAG|LALTFLAG), u"K_E-K_ALTFLAG|RCTRLFLAG");
  assert_equal(vk.lookup(km::tests::get_vk(
    "K_E"), RCTRLFLAG|RALTFLAG), u"K_E-K_ALTFLAG|RCTRLFLAG");

  return EXIT_SUCCESS;
}

int test_uset() {
  std::cout << "== " << __FUNCTION__ << std::endl;

  const COMP_KMXPLUS_USET_RANGE r[] = {
    {0x61, 0x7a}, // [a-z]
    {0x127, 0x127} // [ħ]
  };

  USet u0(&r[0], 2);
  assert_equal(u0.contains(0x62), true); // b
  assert_equal(u0.contains(0x41), false); // A
  assert_equal(u0.contains(0x127), true); // ħ

  USet uempty;
  assert_equal(uempty.contains(0x62), false);
  assert_equal(uempty.contains(0x127), false);

  return EXIT_SUCCESS;
}

int
main(int argc, const char *argv[]) {
  int rc = EXIT_SUCCESS;

  if (test_COMP_KMXPLUS_KEYS_KEY() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (test_ldml_vkeys() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (test_uset() != EXIT_SUCCESS) {
    rc = EXIT_FAILURE;
  }

  if (rc == EXIT_FAILURE) {
    std::cout << "== FAILURE" << std::endl;
  } else {
    std::cout << "== PASS" << std::endl;
  }

  return rc;
}
