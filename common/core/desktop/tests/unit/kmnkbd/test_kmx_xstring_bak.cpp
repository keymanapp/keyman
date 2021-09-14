/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Debugger API unit tests
 */

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <string>
//#include <keyman/keyboardprocessor.h>
//#include "path.hpp"
//#include "state.hpp"
//#include "kmx/kmx_base.h"
#include "../../../src/kmx/kmx_xstring.h"

//#include "action_items.hpp"
//#include "debug_items.hpp"

#include "../test_assert.h"

using namespace km::kbp::kmx;



void test_incxstr() {


  //PKMX_WCHAR pp;
  //PKMX_WCHAR qq;
  **********************
  PKMX_WCHAR pp = (PKMX_WCHAR) u"1234";
  PKMX_WCHAR qq = incxstr(pp);
  assert(qq == pp+1);
 /*  **********************  
  pp = (PKMX_WCHAR) u"0065";
  qq = incxstr(pp);
  assert(qq == pp+1);
  **********************

  
 
    char16_t wcs[] = u"zß水🍌"; // or "z\u00df\u6c34\U0001f34c"
    size_t wcs_sz = sizeof wcs / sizeof *wcs;
    printf("%zu UTF-16 code units: [ ", wcs_sz);
    for (size_t n = 0; n < wcs_sz; ++n) printf("%#x ", wcs[n]);
    printf("]\n");

*/


/*

  PKMX_WCHAR p = (PKMX_WCHAR) u"1234";
  PKMX_WCHAR q = incxstr(p);

  pp = (PKMX_WCHAR) u"1200";
  qq = incxstr(pp);

  PKMX_WCHAR p = (PKMX_WCHAR) u"1234";
  PKMX_WCHAR q = incxstr(p);

*/

  assert(q == p+1);
  //assert(q == p);

  //PKMX_WCHAR pp = (PKMX_WCHAR) u"1F600";
  //PKMX_WCHAR qq = incxstr(pp);
  //assert(qq == pp+2);
  //assert(qq == pp);
}

constexpr const auto help_str = "\
debug_api [--color] <SOURCE_PATH>|--print-sizeof\n\
\n\
  --color         Force color output\n\
  --print-sizeof  Emit structure sizes for interop debug\n\
  SOURCE_PATH     Path where debug_api.cpp is found; kmx files are\n\
                  located relative to this path.\n";

int error_args() {
  std::cerr << "debug_api: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int main(int argc, char *argv []) {
  // TODO: choose/create a keyboard which has some rules
  // Global setup
  //std::string path(argv[1]);

  if(argc < 2) {
   return error_args();
 }

  auto arg_color = std::string(argv[1]) == "--color";
  if(arg_color && argc < 3) {
    return error_args();
  }
  console_color::enabled = console_color::isaterminal() || arg_color;

  //arg_path = argv[arg_color ? 2 : 1];

  test_incxstr();

  // Destroy them
  //teardown();


  return 0;
}
