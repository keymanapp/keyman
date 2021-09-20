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
#include "../../../src/kmx/kmx_xstring.h"
#include "../test_assert.h"

using namespace km::kbp::kmx;
using namespace std;

void test_incxstr() {

  PKMX_WCHAR p;   // pointer input to incxstr()
  PKMX_WCHAR q;   // pointer output to incxstr()

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- NULL, SURROGATE PAIRS, NON_UC_SENTINEL, ONE CHARACTER ---------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------
                             
  // --- Test for empty string ------------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) u"\0";
  q = incxstr(p);
  assert(q == p);
 
  // --- Test for character ---------------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) u"\U00001234";   
  q = incxstr(p);
  assert(q == p+1);

  // --- Test for surrogate pair ----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) u"\U0001F609";
  q = incxstr(p);
  assert(q == p+2);

    // --- Test for one <control> -----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U00000012";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF only -------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF";
  q = incxstr(p);
  assert(q == p + 1);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITHOUT \0 ----------------------------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + CODE_ANY ----------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000001\U00000001";
  q = incxstr(p);
  assert(q == p + 3);
  // 
  // --- Test for FFFF +CODE_NOTANY ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000012\U00000001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_INDEX --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000002\U00000002\U00000001";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_USE ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000005\U00000001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_DEADKEY ------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000008\U00000001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF  CODE_EXTENDED -------------------------------------------------------------------------------------------------------- 
  p = (PKMX_WCHAR)u"\U0000FFFF\U0000000A\U00000001\U00000002\U00000003\U00000004\U00000005\U00000006\U00000010";
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +CODE_CLEARCONTEXT ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U0000000E\U00000001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_CALL ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U0000000F\U00000001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_CONTEXTEX ---------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000011\U00000001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_IFOPT -------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000014\U00000002\U00000002\U00000001";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_IFSYSTEMSTORE ------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000017\U00000002\U00000002\U00000001";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_SETOPT ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000013\U00000002\U00000001";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_SETSYSTEMRESTORE ---------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000018\U00000002\U00000001";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_RESETOPT -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000016\U00000001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_SAVEOPT -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000015\U00000001";
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF +default ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000004";
  q = incxstr(p);
  assert(q == p + 2);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITH \0 AT DIFFERENT POSITIONS --------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------
                        
  // --- Test for FFFF + control (earlier p+1) with \0 after first position --------------- unit test failed with old version of incxstr() -----
  p = (PKMX_WCHAR)u"\U0000FFFF\0\U00000008\U00000001";
  q = incxstr(p);
  assert(q == p+1);

  // --- Test for FFFF +control (earlier p+1) with \0 after second position --------- unit test failed with old version of incxstr() -----
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000008\0\U00000001";
  q = incxstr(p);
  assert(q == p+2);

  // --- Test for FFFF +control (earlier p+1) with \0 after third position ----- unit test failed with old version of incxstr() -----
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000008\U00000001\0";
  q = incxstr(p);
  assert(q == p+3)

  // --- Test for FFFF +control (earlier p+2) with \0 after fourth position ----- unit test failed with old version of incxstr() ----
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000002\U00000001\U00000001\0";
  q = incxstr(p);
  assert(q == p+4);
 
  // --- Test for FFFF +control (earlier p+3) with \0 after fifth  position ----- unit test failed with old version of incxstr() ---------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000014\U00000001\U00000001\U00000001\0";
  q = incxstr(p);
  assert(q == p+5);
  
  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 6.  position  ----- unit test failed with old version of incxstr() ----- 
  p = (PKMX_WCHAR)u"\U0000FFFF\U0000000A\U00000001\U00000002\U00000003\U00000004\0\U00000005\U00000006\U00000007\U00000010";
  q = incxstr(p);
  assert(q == p + 6);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 7.  position  ----- unit test failed with old version of incxstr() 
  p = (PKMX_WCHAR)u"\U0000FFFF\U0000000A\U00000001\U00000002\U00000003\U00000004\U00000005\0\U00000006\U00000007\U00000010";
  q = incxstr(p);
  assert(q == p + 7);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 8.  position  ----- unit test failed with old version of incxstr() ----------
  p = (PKMX_WCHAR)u"\U0000FFFF\U0000000A\U00000001\U00000002\U00000003\U00000004\U00000005\U00000006\0\U00000007\U00000010";
  q = incxstr(p);
  assert(q == p + 8);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 9.  position  ----- unit test failed with old version of incxstr() ---
  p = (PKMX_WCHAR)u"\U0000FFFF\U0000000A\U00000001\U00000002\U00000003\U00000004\U00000005\U00000006\U00000007\0\U00000010";
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 10.  position  ----- unit test failed with old version of incxstr() -----------
  p = (PKMX_WCHAR)u"\U0000FFFF\U0000000A\U00000001\U00000002\U00000003\U00000004\U00000005\U00000006\U00000007\U00000010\0";
  q = incxstr(p);
  assert(q == p + 10);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL, INCOMPLETE & UNUSUAL SEQUENCES--------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + \0 --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\0";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one character ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000062";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +one <control> -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000004";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + one <control> + character -------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0000FFFF\U00000004\U00000062";
  q = incxstr(p);
  assert(q == p + 2);

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

  auto arg_color = argc > 1 && std::string(argv[1]) == "--color";
  if(arg_color && argc < 3) {
    return error_args();
  }
  console_color::enabled = console_color::isaterminal() || arg_color;


  test_incxstr();

  return 0;
}
