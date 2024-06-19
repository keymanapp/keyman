/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - KMX Extended String unit tests
 */

#ifndef KM_CORE_LIBRARY
#define KM_CORE_LIBRARY
#endif
#ifndef USE_CHAR16_T
#define USE_CHAR16_T
#endif

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <string>
#include "../../../src/kmx/kmx_xstring.h"
#include <kmx_file.h>
#include <test_assert.h>

using namespace km::core::kmx;
using namespace std;

#define U_1F609_WINKING_FACE u"\U0001F609"

PKMX_WCHAR find_ptr_to_last_character(PKMX_WCHAR p_first) {
  auto length = std::u16string(p_first).length();
  if (length > 0)
    return p_first + length - 1;
  else
    return p_first;
}


void test_decxstr() {

    PKMX_WCHAR p_start; // pointer start of input
    PKMX_WCHAR p;       // pointer end of input
    PKMX_WCHAR q;       // pointer output

    // -------------------------------------------------------------------------------------------------------------------------------------------------------
    //  UC_SENTINEL followed by valid CODE_xx with sufficient & insufficient number of parameters
    // -------------------------------------------------------------------------------------------------------------------------------------------------------

    p_start = (PKMX_WCHAR) C_CODE_ANY(u"\u0002");
    p = p_start + 2; // \u0002 in the middle of the otherwise valid UC_SENTINEL CODE_ANY sequence
    q = decxstr(p, p_start);
    assert(q == p - 1);

    p_start = (PKMX_WCHAR)u"abc" C_CODE_ANY(u"\u0001");
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p-1));

    p_start = (PKMX_WCHAR) C_CODE_ANY(u"\u0001");
    p = p_start + 3; // nul, i.e. at the end of the valid UC_SENTINEL CODE_ANY sequence
    q = decxstr(p, p_start);
    assert(q == p - 3);

    p_start = (PKMX_WCHAR)u"abc" C_CODE_ANY(u"\u0001") u"\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p-3));

    p_start = (PKMX_WCHAR)u"abc" C_CODE_ANY(u"\u0001") U_1F609_WINKING_FACE;
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p-1));

    p_start = (PKMX_WCHAR)u"abc" C_CODE_ANY(u"\u0001") U_1F609_WINKING_FACE;
    p       = find_ptr_to_last_character(p_start)-1;
    q       = decxstr(p, p_start);
    assert(q == (p-3));

    // -------------------------------------------------------------------------------------------------------------------------------------------------------
    // even more tests: check for use UC_SENTINEL with F000
    // -------------------------------------------------------------------------------------------------------------------------------------------------------

    p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL u"\uF000";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p-1));

    p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL u"\uF000d";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL u"\uF000\u0001";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL u"\uF000\u0002";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL u"\uF000\u0001\u0001";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL u"\uF000\u0002\u0001";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL u"\uF000\u0001\u0001d";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL u"\uF000\u0002\u0001\u0001";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL u"\uF000\u0002\u0001\u0001d";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    // -------------------------------------------------------------------------------------------------------------------------------------------------------
    // more tests: check if we might end up left of pstart
    // -------------------------------------------------------------------------------------------------------------------------------------------------------

    p_start = (PKMX_WCHAR)u"\u0001";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (NULL ));

    p_start = (PKMX_WCHAR)u"\u0001\u0001";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"\u0001\u0001\u0001";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR) U_UC_SENTINEL U_CODE_ANY;
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    // Note: this test puts the pointer into the middle of a valid `UC_SENTINEL CODE_ANY <store>`
    // so we should expect it to not be properly understood.
    p_start = (PKMX_WCHAR) C_CODE_ANY(u"\u0001");
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR) C_CODE_ANY(u"\u0001") u"\u0001";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 3));

    p_start = (PKMX_WCHAR)u"\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (NULL));

    p_start = (PKMX_WCHAR)u"\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"\u0014\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"\u0014\u0014\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"\u0014\u0014\u0014\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"\u0014\u0014\u0014\u0014\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR) U_UC_SENTINEL u"\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    // 0x14 = CODE_IFOPT which has 3 parameters, so this is an invalid, so
    // go back only one char
    p_start = (PKMX_WCHAR) U_UC_SENTINEL u"\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR) U_UC_SENTINEL u"\u0014\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR) C_CODE_IFOPT(u"\u0014", u"\u0014", u"\u0014");
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR) C_CODE_IFOPT(u"\u0014", u"\u0014", u"\u0014") u"\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 5));

    p_start = (PKMX_WCHAR) C_CODE_IFOPT(u"\u0014", u"\u0014", u"\u0014") u"\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    // -------------------------------------------------------------------------------------------------------------------------------------------------------
    // -- differences in pointer movement for new decxstr ----------------------------------------------------------------------------------------------------
    // ----- OLD version of decxstr: 0x0A not used  => return ( p - 1) , 0x0C not used => return ( p - 1) , 0x0E = 1  => return ( p - 3)
    // ----- NEW version of decxstr: Values set in in CODE__SIZE[]: 0x0A = 2 => return ( p - 4) , 0x0C =1 => return ( p - 3) , 0x0E = 0  => return ( p - 1)
    // -------------------------------------------------------------------------------------------------------------------------------------------------------

     // runs OK with NEW version of decxstr (with CODE_EXTENDED pointer moves 3 ( 4 altogether)
     p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL U_CODE_EXTENDED u"\u0001" u"\u0001" u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 4) );

     //runs OK with NEW version of decxstr (with CODE_SWITCH pointer moves 2 ( 3 altogether)
     p_start = (PKMX_WCHAR)u"abc" C_CODE_SWITCH(u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 3) );

     // runs OK with NEW version of decxstr (with CODE_CLEARCONTEXT pointer moves 0 ( 1 altogether)
     p_start = (PKMX_WCHAR)u"abc" C_CODE_CLEARCONTEXT() u"\u0001d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     // runs OK with OLD version and NEW version of decxstr
     p_start = (PKMX_WCHAR)u"abc" C_CODE_CLEARCONTEXT() u"\u0001";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 2) );

     // ---------------------------------------------------------------------------------------
     // ---- character
     // ---------------------------------------------------------------------------------------

     p_start = (PKMX_WCHAR)u"abcd";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);

     assert(q == (p - 1) );

    // ---------------------------------------------------------------------------------------
    // ---- p <= pstart
    // ---------------------------------------------------------------------------------------

     p_start = (PKMX_WCHAR)u"abc";
     p       = find_ptr_to_last_character(p_start)-5;
     q       = decxstr(p, p_start);
     assert(q == (NULL) );

     p_start = (PKMX_WCHAR)u"a";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (NULL) );

    // ---------------------------------------------------------------------------------------
    // ----  p= UC_SENTINEL_EXTENDED
    // ---------------------------------------------------------------------------------------

     p_start = (PKMX_WCHAR)u"abc" C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" U_CODE_EXTENDEDEND);
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\u0007" u"\u0008" U_CODE_EXTENDEDEND);
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" U_CODE_EXTENDEDEND) u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 8) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\u0007" u"\u0008" U_CODE_EXTENDEDEND) u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 11) );

    // ---------------------------------------------------------------------------------------
    // ---- Surrogate Pair
    // ---------------------------------------------------------------------------------------

     p_start = (PKMX_WCHAR)u"abc" U_1F609_WINKING_FACE;
     p       = find_ptr_to_last_character(p_start)-1;
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc" U_1F609_WINKING_FACE;
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc" U_1F609_WINKING_FACE u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL U_1F609_WINKING_FACE;
     p       = find_ptr_to_last_character(p_start)-1;
     q       = decxstr(p, p_start);
     assert(q == (p-1));

     p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL U_1F609_WINKING_FACE;
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p-1));

     p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL U_1F609_WINKING_FACE u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p-2));
    // ---------------------------------------------------------------------------------------
    // ---- CODE_
    // ---------------------------------------------------------------------------------------

     for (auto code = 1 /* CODE_ANY */; code <= CODE_LASTCODE; code++) {
       if (code == UC_SENTINEL_EXTENDEDEND) {
         // We won't try and test that here as it won't assert in the
         // same way
         continue;
       }
       auto size = CODE__SIZE[code];
       std::u16string str(u"abc" U_UC_SENTINEL);
       str.append(1, code);
       str.append(size < 0 ? 0 : size, u'\u0001');

       p_start = (PKMX_WCHAR) str.c_str();
       p       = find_ptr_to_last_character(p_start);
       q       = decxstr(p, p_start);
       assert(q == (p - 1)); // this is in the middle of the sequence so it should always go back a single unit

       str.append(u"a");
       p_start = (PKMX_WCHAR)str.c_str();
       p       = find_ptr_to_last_character(p_start);
       q       = decxstr(p, p_start);
       if (size < 0) {
         assert(q == (p - 1));  // this is in the middle of the sequence so it should always go back a single unit
       } else {
         assert(q == (p - size - 2)); /* UC_SENTINEL + code + (CODE__SIZE = number of params) */
       }
     }

    // ---------------------------------------------------------------------------------------
    // ---- CODE_ followed by letter
    // ---------------------------------------------------------------------------------------

     p_start = (PKMX_WCHAR)u"abc" C_CODE_ANY(u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_INDEX(u"\u0001", u"\u0001d");
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 4) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_CONTEXT() u"\u0001d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_NUL() u"\u0001d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_USE(u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_RETURN() u"\u0001d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_BEEP() u"\u0001d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_DEADKEY(u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_CALL(u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_CONTEXTEX(u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_NOTANY(u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_SETOPT(u"\u0001", u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 4) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_IFOPT(u"\u0001", u"\u0001", u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 5) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_SAVEOPT(u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_RESETOPT(u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_IFSYSTEMSTORE(u"\u0001", u"\u0001", u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 5) );

     p_start = (PKMX_WCHAR)u"abc" C_CODE_SETSYSTEMSTORE(u"\u0001", u"\u0001") u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 4) );

    // ---------------------------------------------------------------------------------------
    // ---- other
    // ---------------------------------------------------------------------------------------

     p_start = (PKMX_WCHAR)u"abc" C_CODE_INDEX(u"d", u"e") u"f\u0001";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc" U_UC_SENTINEL;
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     // pointer in the middle of a surrogate pair, so beware!
     p_start = (PKMX_WCHAR) U_UC_SENTINEL U_1F609_WINKING_FACE;
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"\u0014d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR) U_UC_SENTINEL;
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == NULL );

     p_start = (PKMX_WCHAR) U_UC_SENTINEL u"d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR) U_UC_SENTINEL U_UC_SENTINEL;
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == p - 1 );
}



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
  p = (PKMX_WCHAR) u"\u1234";
  q = incxstr(p);
  assert(q == p+1);

  // --- Test for surrogate pair ----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p+2);

  // --- Test for one <control> -----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\u0012";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF only -------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_UC_SENTINEL;
  q = incxstr(p);
  assert(q == p + 1);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITHOUT \0 ----------------------------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF +CODE_INDEX --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_INDEX(u"\u0002", u"\u0001");
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_USE ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_USE(u"\u0001");
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_DEADKEY ------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_DEADKEY(u"\u0001");
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF  CODE_EXTENDED --------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" U_CODE_EXTENDEDEND);
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +CODE_CLEARCONTEXT ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CLEARCONTEXT() u"\u0001";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +CODE_CALL ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CALL(u"\u0001");
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_CONTEXTEX ---------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CONTEXTEX(u"\u0001");
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_IFOPT -------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_IFOPT(u"\u0002", u"\u0002", u"\u0001");
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_IFSYSTEMSTORE ------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_IFSYSTEMSTORE(u"\u0002", u"\u0002", u"\u0001");
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_SETOPT ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SETOPT(u"\u0002", u"\u0001");
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_SETSYSTEMRESTORE ---------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SETSYSTEMSTORE(u"\u0002", u"\u0001");
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_RESETOPT -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_RESETOPT(u"\u0001");
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_SAVEOPT -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SAVEOPT(u"\u0001");
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF +default ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NUL();
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + CODE_ANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_ANY(u"\u0001");
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + CODE_CONTEXT -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CONTEXT() u"\u0001";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_RETURN -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_RETURN() u"\u0001";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_BEEP -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_BEEP() u"\u0001";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_SWITCH -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SWITCH(u"\u0001");
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + NOTANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NOTANY(u"\u0001");
  q = incxstr(p);
  assert(q == p + 3 );

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITH \0 AT DIFFERENT POSITIONS --------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + control (earlier p+1) with \0 after first position --------------- unit test failed with old version of incxstr() -----
  p = (PKMX_WCHAR) U_UC_SENTINEL u"\0\u0008\u0001";
  q = incxstr(p);
  assert(q == p+1);

  // --- Test for FFFF +control (earlier p+1) with \0 after second position --------- unit test failed with old version of incxstr() -----
  p = (PKMX_WCHAR) C_CODE_DEADKEY(u"\0") u"\u0001";
  q = incxstr(p);
  assert(q == p+2);

  // --- Test for FFFF +control (earlier p+1) with \0 after third position ----- unit test failed with old version of incxstr() -----
  p = (PKMX_WCHAR) C_CODE_DEADKEY(u"\u0001") u"\0";
  q = incxstr(p);
  assert(q == p+3)

  // --- Test for FFFF +control (earlier p+2) with \0 after fourth position ----- unit test failed with old version of incxstr() ----
  p = (PKMX_WCHAR) C_CODE_INDEX(u"\u0001", u"\u0001") u"\0";
  q = incxstr(p);
  assert(q == p+4);

  // --- Test for FFFF +control (earlier p+3) with \0 after fifth  position ----- unit test failed with old version of incxstr() ---------
  p = (PKMX_WCHAR) C_CODE_IFOPT(u"\u0001", u"\u0001", u"\u0001") u"\0";
  q = incxstr(p);
  assert(q == p+5);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 6.  position  ----- unit test failed with old version of incxstr() -----
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\0" u"\u0005" u"\u0006" u"\u0007" U_CODE_EXTENDEDEND);
  q = incxstr(p);
  assert(q == p + 6);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 7.  position  ----- unit test failed with old version of incxstr()
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\0" u"\u0006" u"\u0007" U_CODE_EXTENDEDEND);
  q = incxstr(p);
  assert(q == p + 7);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 8.  position  ----- unit test failed with old version of incxstr() ----------
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\0" u"\u0007" U_CODE_EXTENDEDEND);
  q = incxstr(p);
  assert(q == p + 8);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 9.  position  ----- unit test failed with old version of incxstr() ---
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\u0007" u"\0" U_CODE_EXTENDEDEND);
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 10.  position  ----- unit test failed with old version of incxstr() -----------
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\u0007" U_CODE_EXTENDEDEND) u"\0";
  q = incxstr(p);
  assert(q == p + 10);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL, INCOMPLETE & UNUSUAL SEQUENCES--------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + \0 --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_UC_SENTINEL u"\0";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one character ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_UC_SENTINEL u"\u0062";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one <control> -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NUL();
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + one <control> + character -------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NUL() u"\u0062";
  q = incxstr(p);
  assert(q == p + 2);

//-------------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------------------------------------
//
  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- NULL, SURROGATE PAIRS, NON_UC_SENTINEL, ONE CHARACTER  WITH \u1234\u2468 At END
  // ---------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for empty string
  //---------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) u"\0\u1234\u2468";
  q = incxstr(p);
  assert(q == p);

  // --- Test for character
  // ---------------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\u1234\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);;

  // --- Test for surrogate pair
  // ----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_1F609_WINKING_FACE u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for one <control>
  // -----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\u0012\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF only
  // -------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_UC_SENTINEL u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITHOUT \0
  // ----------------------------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF +CODE_INDEX
  // --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_INDEX(u"\u0002", u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_USE
  // ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_USE(u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_DEADKEY
  // ------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_DEADKEY(u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF  CODE_EXTENDED
  // --------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" U_CODE_EXTENDEDEND) u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +CODE_CLEARCONTEXT
  // ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CLEARCONTEXT() u"\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +CODE_CALL
  // ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CALL(u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_CONTEXTEX
  // ---------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CONTEXTEX(u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_IFOPT
  // -------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_IFOPT(u"\u0002", u"\u0002", u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_IFSYSTEMSTORE
  // ------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_IFSYSTEMSTORE(u"\u0002", u"\u0002", u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_SETOPT
  // ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SETOPT(u"\u0002", u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_SETSYSTEMRESTORE
  // ---------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SETSYSTEMSTORE(u"\u0002", u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_RESETOPT
  // -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_RESETOPT(u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_SAVEOPT
  // -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SAVEOPT(u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +default
  // ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NUL() u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + CODE_ANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_ANY(u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + CODE_CONTEXT -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CONTEXT() u"\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_RETURN -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_RETURN() u"\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_BEEP -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_BEEP() u"\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_SWITCH -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SWITCH(u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + NOTANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NOTANY(u"\u0001") u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3 );

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITH \0 AT DIFFERENT POSITIONS
  // --------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + control (earlier p+1) with \0 after first position --------------- unit test failed with old version of
  // incxstr() -----
  p = (PKMX_WCHAR) U_UC_SENTINEL u"\0\u0008\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +control (earlier p+1) with \0 after second position --------- unit test failed with old version of
  // incxstr() -----
  p = (PKMX_WCHAR) C_CODE_DEADKEY(u"\0") u"\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +control (earlier p+1) with \0 after third position ----- unit test failed with old version of incxstr()
  // -----
  p = (PKMX_WCHAR) C_CODE_DEADKEY(u"\u0001") u"\0\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3)

  // --- Test for FFFF +control (earlier p+2) with \0 after fourth position ----- unit test failed with old version of
  // incxstr() ----
  p = (PKMX_WCHAR) C_CODE_INDEX(u"\u0001", u"\u0001") u"\0\u1234\u2468";
  q     = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +control (earlier p+3) with \0 after fifth  position ----- unit test failed with old version of incxstr()
  // ---------
  p = (PKMX_WCHAR) C_CODE_IFOPT(u"\u0001", u"\u0001", u"\u0001") u"\0\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 6.  position  ----- unit test failed with old
  // version of incxstr() -----
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\0" u"\u0005" u"\u0006" u"\u0007" U_CODE_EXTENDEDEND) u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 6);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 7.  position  ----- unit test failed with old
  // version of incxstr()
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\0" u"\u0006" u"\u0007" U_CODE_EXTENDEDEND) u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 7);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 8.  position  ----- unit test failed with old
  // version of incxstr() ----------
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\0" u"\u0007" U_CODE_EXTENDEDEND) u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 8);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 9.  position  ----- unit test failed with old
  // version of incxstr() ---
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\u0007" u"\0" U_CODE_EXTENDEDEND) u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 10.  position  ----- unit test failed with old
  // version of incxstr() -----------
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\u0007" U_CODE_EXTENDEDEND) u"\0\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 10);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL, INCOMPLETE & UNUSUAL SEQUENCES--------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + \0
  // --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_UC_SENTINEL u"\0\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one character
  // ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_UC_SENTINEL u"\u0062\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one <control>
  // -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NUL() u"\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + one <control> + character
  // -------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NUL() u"\u0062\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

//-------------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------------------------------------

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- NULL, SURROGATE PAIRS, NON_UC_SENTINEL, ONE CHARACTER  WITH SURROGATE PAIR \U0001F609 At END
  // ---------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for empty string
  //------------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\0" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p);

  // --- Test for character
  // ---------------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\u1234" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for surrogate pair
  // ----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_1F609_WINKING_FACE U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for one <control>
  // -----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\u0012" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF only
  // -------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_UC_SENTINEL U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 1);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITHOUT \0
  // ----------------------------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF +CODE_INDEX
  // --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_INDEX(u"\u0002", u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_USE
  // ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_USE(u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_DEADKEY
  // ------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_DEADKEY(u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF  CODE_EXTENDED
  // --------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" U_CODE_EXTENDEDEND) U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +CODE_CLEARCONTEXT
  // ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CLEARCONTEXT() u"\u0001" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +CODE_CALL
  // ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CALL(u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_CONTEXTEX
  // ---------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CONTEXTEX(u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_IFOPT
  // -------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_IFOPT(u"\u0002", u"\u0002", u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_IFSYSTEMSTORE
  // ------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_IFSYSTEMSTORE(u"\u0002", u"\u0002", u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_SETOPT
  // ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SETOPT(u"\u0002", u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_SETSYSTEMRESTORE
  // ---------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SETSYSTEMSTORE(u"\u0002", u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_RESETOPT
  // -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_RESETOPT(u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_SAVEOPT
  // -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SAVEOPT(u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +default
  // ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NUL() U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + CODE_ANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_ANY(u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + CODE_CONTEXT -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_CONTEXT() u"\u0001" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_RETURN -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_RETURN() u"\u0001" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_BEEP -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_BEEP() u"\u0001" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_SWITCH -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_SWITCH(u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + NOTANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NOTANY(u"\u0001") U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 3 );

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITH \0 AT DIFFERENT POSITIONS
  // --------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + control (earlier p+1) with \0 after first position --------------- unit test failed
  // with old version of incxstr() -----
  p = (PKMX_WCHAR) U_UC_SENTINEL u"\0\u0008\u0001" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +control (earlier p+1) with \0 after second position --------- unit test failed with
  // old version of incxstr() -----
  p = (PKMX_WCHAR) C_CODE_DEADKEY(u"\0") u"\u0001" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +control (earlier p+1) with \0 after third position ----- unit test failed with old
  // version of incxstr() -----
  p = (PKMX_WCHAR) C_CODE_DEADKEY(u"\u0001") u"\0" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 3)

  // --- Test for FFFF +control (earlier p+2) with \0 after fourth position ----- unit test failed with
  // old version of incxstr() ----
  p = (PKMX_WCHAR) C_CODE_INDEX(u"\u0001", u"\u0001") u"\0" U_1F609_WINKING_FACE;
  q     = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +control (earlier p+3) with \0 after fifth  position ----- unit test failed with old
  // version of incxstr() ---------
  p = (PKMX_WCHAR) C_CODE_IFOPT(u"\u0001", u"\u0001", u"\u0001") u"\0" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 6.  position  ----- unit test
  // failed with old version of incxstr() -----
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\0" u"\u0005" u"\u0006" u"\u0007" U_CODE_EXTENDEDEND) U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 6);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 7.  position  ----- unit test
  // failed with old version of incxstr()
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\0" u"\u0006" u"\u0007" U_CODE_EXTENDEDEND) U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 7);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 8.  position  ----- unit test
  // failed with old version of incxstr() ----------
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\0" u"\u0007" U_CODE_EXTENDEDEND) U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 8);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 9.  position  ----- unit test
  // failed with old version of incxstr() ---
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\u0007" u"\0" U_CODE_EXTENDEDEND) U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 10.  position  ----- unit test
  // failed with old version of incxstr() -----------
  p = (PKMX_WCHAR) C_CODE_EXTENDED(u"\u0001" u"\u0002" u"\u0003" u"\u0004" u"\u0005" u"\u0006" u"\u0007" U_CODE_EXTENDEDEND) u"\0" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 10);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL, INCOMPLETE & UNUSUAL SEQUENCES--------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + \0
  // --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_UC_SENTINEL u"\0" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one character
  // ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) U_UC_SENTINEL u"\u0062" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one <control>
  // -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NUL() U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + one <control> + character
  // -------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) C_CODE_NUL() u"\u0062" U_1F609_WINKING_FACE;
  q = incxstr(p);
  assert(q == p + 2);
}

void test_xstrlen() {
  assert_equal(xstrlen((PKMX_WCHAR)u""), 0);
  assert_equal(xstrlen((PKMX_WCHAR)u"1"), 1);
  assert_equal(xstrlen((PKMX_WCHAR)u"1234567890"), 10);
  assert_equal(xstrlen((PKMX_WCHAR) C_CODE_DEADKEY(u"\u0001") u"a"), 2);
  assert_equal(xstrlen((PKMX_WCHAR) C_CODE_IFOPT(u"\u0001", u"\u0002", u"\u0003") u"a"), 2);
  assert_equal(xstrlen((PKMX_WCHAR) C_CODE_IFSYSTEMSTORE(u"\u0001", u"\u0002", u"\u0003") u"a"), 2);
  assert_equal(xstrlen((PKMX_WCHAR) U_1F609_WINKING_FACE u"a"), 2);
}

void
test_xstrlen_ignoreifopt() {
  assert_equal(xstrlen_ignoreifopt((PKMX_WCHAR)u""), 0);
  assert_equal(xstrlen_ignoreifopt((PKMX_WCHAR)u"1"), 1);
  assert_equal(xstrlen_ignoreifopt((PKMX_WCHAR)u"1234567890"), 10);
  assert_equal(xstrlen_ignoreifopt((PKMX_WCHAR) C_CODE_DEADKEY(u"\u0001") u"a"), 2);
  assert_equal(xstrlen_ignoreifopt((PKMX_WCHAR) C_CODE_IFOPT(u"\u0001", u"\u0002", u"\u0003") u"a"), 1);
  assert_equal(xstrlen_ignoreifopt((PKMX_WCHAR) C_CODE_IFSYSTEMSTORE(u"\u0001", u"\u0002", u"\u0003") u"a"), 1);
  assert_equal(xstrlen_ignoreifopt((PKMX_WCHAR) U_1F609_WINKING_FACE u"a"), 2);
}

void
test_utf32() {
  std::cout << "== " << __FUNCTION__ << std::endl;
  const KMX_DWORD u295 =     0x0127; // ħ

  assert(Uni_IsBMP(u295));

  const char16_t hmaqtugha = Uni_UTF32BMPToUTF16(u295);
  assert(hmaqtugha == 0x0127);

  char16_single c0;
  int l0 = Utf32CharToUtf16(u295, c0);
  assert(l0 == 1);
  assert(c0.ch[0] == 0x0127);
  assert(c0.ch[1] == 0);
  std::u16string s0 = std::u16string(c0.ch, l0);
  assert(s0 == std::u16string(u"ħ"));
  assert_equal(s0.at(0), 0x0127);

  const KMX_DWORD scat = 0x0001F640; // 🙀
  assert(!Uni_IsBMP(scat));
  char16_single c1;
  int l1 = Utf32CharToUtf16(scat, c1);
  assert(l1 == 2);
  assert_equal(c1.ch[0], 0xD83D);
  assert_equal(c1.ch[1], 0xDE40);
  assert_equal(c1.ch[2], 0);
  std::u16string s1 = std::u16string(c1.ch, l1);
  assert_equal(s1.at(0), 0xD83D);
  assert_equal(s1.at(1), 0xDE40);
  assert(s1 == std::u16string(u"🙀"));
}

void
test_u16string_to_u32string() {
  std::cout << "== " << __FUNCTION__ << std::endl;
  // normal cases
  {
    const std::u32string str = u16string_to_u32string(u"");
    assert_equal(str.length(), 0);
  }
  {
    const std::u32string str = u16string_to_u32string(u"e");
    assert_equal(str.length(), 1);
    assert_equal(str.at(0), 0x0065);
  }
  {
    const std::u32string str = u16string_to_u32string(u"🙀");
    assert_equal(str.length(), 1);
    assert_equal(str.at(0), 0x0001F640);
  }
  {
    const std::u32string str = u16string_to_u32string(u"Ω🙀");
    assert_equal(str.length(), 2);
    assert_equal(str.at(0), u'Ω');
    assert_equal(str.at(1), 0x0001F640);
  }

  // error cases
  {
    std::u16string half_cat;
    half_cat.push_back(0xD83D);  // mismatched lead surrogate
    const std::u32string str = u16string_to_u32string(half_cat);
    assert_equal(str.length(), 1);
    assert_equal(str.at(0), 0xFFFD);
  }
  {
    std::u16string half_cat;
    half_cat.push_back(0xD83D);  // mismatched lead surrogate
    half_cat.push_back(u'Ω');    // with following text
    const std::u32string str = u16string_to_u32string(half_cat);
    assert_equal(str.length(), 2);
    assert_equal(str.at(0), 0xFFFD);
    assert_equal(str.at(1), u'Ω');
  }
  {
    std::u16string half_cat;
    half_cat.push_back(0xDE40);  // mismatched trail surrogate
    const std::u32string str = u16string_to_u32string(half_cat);
    assert_equal(str.length(), 1);
    assert_equal(str.at(0), 0xFFFD);
  }
  {
      std::u16string no_cat;
      no_cat.push_back(0xDE40);
      no_cat.push_back(0xD83D);
      const std::u32string str = u16string_to_u32string(no_cat);
      assert_equal(str.length(), 2);
      assert_equal(str.at(0), 0xFFFD);
      assert_equal(str.at(1), 0xFFFD);
  }
}


void
test_u32string_to_u16string() {
  std::cout << "== " << __FUNCTION__ << std::endl;
  // normal cases
  {
    const auto str = u32string_to_u16string(U"");
    assert_equal(str.length(), 0);
  }
  {
    const auto str = u32string_to_u16string(U"e");
    assert_equal(str.length(), 1);
    assert_equal(str.at(0), 0x0065);
  }
  {
    const auto str = u32string_to_u16string(U"🙀");
    assert_equal(str.length(), 2);
    assert_equal(str.at(0), 0xD83D);
    assert_equal(str.at(1), 0xDE40);
  }
  {
    const auto str = u32string_to_u16string(U"Ω🙀");
    assert_equal(str.length(), 3);
    assert_equal(str.at(0), u'Ω');
    assert_equal(str.at(1), 0xD83D);
    assert_equal(str.at(2), 0xDE40);
  }
}

void test_is_valid() {
  std::cout << "== " << __FUNCTION__ << std::endl;
  // valid
  assert_equal(Uni_IsValid(0x0000), true);
  assert_equal(Uni_IsValid(0x0127), true);
  assert_equal(Uni_IsValid(U'🙀'), true);

  // invalid
  assert_equal(Uni_IsValid(0xDECAFBAD), false); // out of range
  assert_equal(Uni_IsValid(0x566D4128), false);
  assert_equal(Uni_IsValid(0xFFFF), false); // nonchar
  assert_equal(Uni_IsValid(0xFFFE), false); // nonchar
  assert_equal(Uni_IsValid(0x10FFFF), false); // nonchar
  assert_equal(Uni_IsValid(0x10FFFE), false); // nonchar
  assert_equal(Uni_IsValid(0x01FFFF), false); // nonchar
  assert_equal(Uni_IsValid(0x01FFFE), false); // nonchar
  assert_equal(Uni_IsValid(0x02FFFF), false); // nonchar
  assert_equal(Uni_IsValid(0x02FFFE), false); // nonchar
  assert_equal(Uni_IsValid(0xFDD1), false); // nonchar
  assert_equal(Uni_IsValid(0xFDD0), false); // nonchar


 // positive range test
  assert_equal(Uni_IsValid(0x100000, 0x10FFFD), true);
  assert_equal(Uni_IsValid(0x10, 0x20), true);
  assert_equal(Uni_IsValid(0x100000, 0x10FFFD), true);

  // all valid ranges in BMP
  assert_equal(Uni_IsValid(0x0000, 0xD7FF), true);
  assert_equal(Uni_IsValid(0xD800, 0xDFFF), false);
  assert_equal(Uni_IsValid(0xE000, 0xFDCF), true);
  assert_equal(Uni_IsValid(0xFDD0, 0xFDEF), false);
  assert_equal(Uni_IsValid(0xFDF0, 0xFDFF), true);
  assert_equal(Uni_IsValid(0xFDF0, 0xFFFD), true);

  // negative range test
  assert_equal(Uni_IsValid(0, 0x10FFFF), false);         // ends with nonchar
  assert_equal(Uni_IsValid(0, 0x10FFFD), false);         // contains lots o' nonchars
  assert_equal(Uni_IsValid(0x20, 0x10), false);          // swapped
  assert_equal(Uni_IsValid(0xFDEF, 0xFDF0), false);      // just outside range
  assert_equal(Uni_IsValid(0x0000, 0x010000), false);    // crosses noncharacter plane boundary and other stuff
  assert_equal(Uni_IsValid(0x010000, 0x020000), false);  // crosses noncharacter plane boundary
  assert_equal(Uni_IsValid(0x0000, 0xFFFF), false);      // crosses other BMP prohibited and plane boundary
  assert_equal(Uni_IsValid(0x0000, 0xFFFD), false);      // crosses other BMP prohibited
  assert_equal(Uni_IsValid(0x0000, 0xE000), false);      // crosses surrogate space
  assert_equal(Uni_IsValid(0x0000, 0x20FFFF), false);      // out of bounds
  assert_equal(Uni_IsValid(0x10FFFD, 0x20FFFF), false);      // out of bounds

 }

constexpr const auto help_str = u"\
test_kmx_xstring [--color]\n\
\n\
  --color         Force color output\n";

int error_args() {
  std::cerr << u"test_kmx_xstring: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int main(int argc, char *argv []) {

  auto arg_color = argc > 1 && std::string(argv[1]) == "--color";
  console_color::enabled = console_color::isaterminal() || arg_color;

  test_incxstr();
  test_decxstr();
  test_xstrlen();
  test_xstrlen_ignoreifopt();
  test_utf32();
  test_u16string_to_u32string();
  test_is_valid();

  return 0;
}
