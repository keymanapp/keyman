/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - KMX Extended String unit tests
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
  p = (PKMX_WCHAR) u"\u1234";
  q = incxstr(p);
  assert(q == p+1);

  // --- Test for surrogate pair ----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR) u"\U0001F609";
  q = incxstr(p);
  assert(q == p+2);

  // --- Test for one <control> -----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\u0012";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF only -------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF";
  q = incxstr(p);
  assert(q == p + 1);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITHOUT \0 ----------------------------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF +CODE_INDEX --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0002\u0002\u0001";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_USE ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0005\u0001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_DEADKEY ------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0008\u0001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF  CODE_EXTENDED -------------------------------------------------------------------------------------------------------- 
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0010";
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +CODE_CLEARCONTEXT ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000E\u0001";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +CODE_CALL ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000F\u0001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_CONTEXTEX ---------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0011\u0001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_IFOPT -------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0014\u0002\u0002\u0001";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_IFSYSTEMSTORE ------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0017\u0002\u0002\u0001";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_SETOPT ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0013\u0002\u0001";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_SETSYSTEMRESTORE ---------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0018\u0002\u0001";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_RESETOPT -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0016\u0001";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_SAVEOPT -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0015\u0001";
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF +default ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0004";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + CODE_ANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0001\u0001";
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + CODE_CONTEXT -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0003\u0001";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_RETURN -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0006\u0001";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_BEEP -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0007\u0001";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_SWITCH -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000C\u0001";
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + NOTANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0012\u0001";
  q = incxstr(p);
  assert(q == p + 3 );

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITH \0 AT DIFFERENT POSITIONS --------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------
                        
  // --- Test for FFFF + control (earlier p+1) with \0 after first position --------------- unit test failed with old version of incxstr() -----
  p = (PKMX_WCHAR)u"\uFFFF\0\u0008\u0001";
  q = incxstr(p);
  assert(q == p+1);

  // --- Test for FFFF +control (earlier p+1) with \0 after second position --------- unit test failed with old version of incxstr() -----
  p = (PKMX_WCHAR)u"\uFFFF\u0008\0\u0001";
  q = incxstr(p);
  assert(q == p+2);

  // --- Test for FFFF +control (earlier p+1) with \0 after third position ----- unit test failed with old version of incxstr() -----
  p = (PKMX_WCHAR)u"\uFFFF\u0008\u0001\0";
  q = incxstr(p);
  assert(q == p+3)

  // --- Test for FFFF +control (earlier p+2) with \0 after fourth position ----- unit test failed with old version of incxstr() ----
  p = (PKMX_WCHAR)u"\uFFFF\u0002\u0001\u0001\0";
  q = incxstr(p);
  assert(q == p+4);
 
  // --- Test for FFFF +control (earlier p+3) with \0 after fifth  position ----- unit test failed with old version of incxstr() ---------
  p = (PKMX_WCHAR)u"\uFFFF\u0014\u0001\u0001\u0001\0";
  q = incxstr(p);
  assert(q == p+5);
  
  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 6.  position  ----- unit test failed with old version of incxstr() ----- 
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\0\u0005\u0006\u0007\u0010";
  q = incxstr(p);
  assert(q == p + 6);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 7.  position  ----- unit test failed with old version of incxstr() 
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\0\u0006\u0007\u0010";
  q = incxstr(p);
  assert(q == p + 7);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 8.  position  ----- unit test failed with old version of incxstr() ----------
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\0\u0007\u0010";
  q = incxstr(p);
  assert(q == p + 8);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 9.  position  ----- unit test failed with old version of incxstr() ---
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0007\0\u0010";
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 10.  position  ----- unit test failed with old version of incxstr() -----------
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0010\0";
  q = incxstr(p);
  assert(q == p + 10);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL, INCOMPLETE & UNUSUAL SEQUENCES--------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + \0 --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\0";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one character ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0062";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one <control> -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0004";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + one <control> + character -------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0004\u0062";
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
  p = (PKMX_WCHAR)u"\U0001F609\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for one <control>
  // -----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\u0012\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF only
  // -------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITHOUT \0
  // ----------------------------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF +CODE_INDEX
  // --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0002\u0002\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_USE
  // ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0005\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_DEADKEY
  // ------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0008\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF  CODE_EXTENDED
  // --------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0010\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +CODE_CLEARCONTEXT
  // ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000E\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +CODE_CALL
  // ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000F\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_CONTEXTEX
  // ---------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0011\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_IFOPT
  // -------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0014\u0002\u0002\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_IFSYSTEMSTORE
  // ------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0017\u0002\u0002\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_SETOPT
  // ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0013\u0002\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_SETSYSTEMRESTORE
  // ---------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0018\u0002\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_RESETOPT
  // -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0016\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_SAVEOPT
  // -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0015\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +default
  // ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0004\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + CODE_ANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0001\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + CODE_CONTEXT -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0003\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_RETURN -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0006\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_BEEP -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0007\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_SWITCH -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000C\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + NOTANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0012\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3 );

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITH \0 AT DIFFERENT POSITIONS
  // --------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + control (earlier p+1) with \0 after first position --------------- unit test failed with old version of
  // incxstr() -----
  p = (PKMX_WCHAR)u"\uFFFF\0\u0008\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +control (earlier p+1) with \0 after second position --------- unit test failed with old version of
  // incxstr() -----
  p = (PKMX_WCHAR)u"\uFFFF\u0008\0\u0001\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +control (earlier p+1) with \0 after third position ----- unit test failed with old version of incxstr()
  // -----
  p = (PKMX_WCHAR)u"\uFFFF\u0008\u0001\0\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 3)

  // --- Test for FFFF +control (earlier p+2) with \0 after fourth position ----- unit test failed with old version of
  // incxstr() ----
  p = (PKMX_WCHAR)u"\uFFFF\u0002\u0001\u0001\0\u1234\u2468";
  q     = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +control (earlier p+3) with \0 after fifth  position ----- unit test failed with old version of incxstr()
  // ---------
  p = (PKMX_WCHAR)u"\uFFFF\u0014\u0001\u0001\u0001\0\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 6.  position  ----- unit test failed with old
  // version of incxstr() -----
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\0\u0005\u0006\u0007\u0010\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 6);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 7.  position  ----- unit test failed with old
  // version of incxstr()
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\0\u0006\u0007\u0010\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 7);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 8.  position  ----- unit test failed with old
  // version of incxstr() ----------
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\0\u0007\u0010\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 8);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 9.  position  ----- unit test failed with old
  // version of incxstr() ---
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0007\0\u0010\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 10.  position  ----- unit test failed with old
  // version of incxstr() -----------
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0010\0\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 10);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL, INCOMPLETE & UNUSUAL SEQUENCES--------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + \0
  // --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\0\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one character
  // ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0062\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one <control>
  // -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0004\u1234\u2468";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + one <control> + character
  // -------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0004\u0062\u1234\u2468";
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
  p = (PKMX_WCHAR)u"\0\U0001F609";
  q = incxstr(p);
  assert(q == p);

  // --- Test for character
  // ---------------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\u1234\U0001F609";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for surrogate pair
  // ----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\U0001F609\U0001F609";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for one <control>
  // -----------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\u0012\U0001F609";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF only
  // -------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\U0001F609";
  q = incxstr(p);
  assert(q == p + 1);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITHOUT \0
  // ----------------------------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF +CODE_INDEX
  // --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0002\u0002\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_USE
  // ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0005\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_DEADKEY
  // ------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0008\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF  CODE_EXTENDED
  // --------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0010\U0001F609";
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +CODE_CLEARCONTEXT
  // ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000E\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +CODE_CALL
  // ----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000F\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_CONTEXTEX
  // ---------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0011\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_IFOPT
  // -------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0014\u0002\u0002\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_IFSYSTEMSTORE
  // ------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0017\u0002\u0002\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +CODE_SETOPT
  // ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0013\u0002\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_SETSYSTEMRESTORE
  // ---------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0018\u0002\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +CODE_RESETOPT
  // -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0016\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +CODE_SAVEOPT
  // -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0015\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 3);

  // --- Test for FFFF +default
  // ----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0004\U0001F609";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + CODE_ANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0001\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + CODE_CONTEXT -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0003\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_RETURN -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0006\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_BEEP -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0007\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 2 );

  // --- Test for FFFF + CODE_SWITCH -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u000C\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 3 );

  // --- Test for FFFF + NOTANY -----------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0012\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 3 );

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL WITH \0 AT DIFFERENT POSITIONS
  // --------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + control (earlier p+1) with \0 after first position --------------- unit test failed
  // with old version of incxstr() -----
  p = (PKMX_WCHAR)u"\uFFFF\0\u0008\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +control (earlier p+1) with \0 after second position --------- unit test failed with
  // old version of incxstr() -----
  p = (PKMX_WCHAR)u"\uFFFF\u0008\0\u0001\U0001F609";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF +control (earlier p+1) with \0 after third position ----- unit test failed with old
  // version of incxstr() -----
  p = (PKMX_WCHAR)u"\uFFFF\u0008\u0001\0\U0001F609";
  q = incxstr(p);
  assert(q == p + 3)

  // --- Test for FFFF +control (earlier p+2) with \0 after fourth position ----- unit test failed with
  // old version of incxstr() ----
  p = (PKMX_WCHAR)u"\uFFFF\u0002\u0001\u0001\0\U0001F609";
  q     = incxstr(p);
  assert(q == p + 4);

  // --- Test for FFFF +control (earlier p+3) with \0 after fifth  position ----- unit test failed with old
  // version of incxstr() ---------
  p = (PKMX_WCHAR)u"\uFFFF\u0014\u0001\u0001\u0001\0\U0001F609";
  q = incxstr(p);
  assert(q == p + 5);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 6.  position  ----- unit test
  // failed with old version of incxstr() -----
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\0\u0005\u0006\u0007\u0010\U0001F609";
  q = incxstr(p);
  assert(q == p + 6);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 7.  position  ----- unit test
  // failed with old version of incxstr()
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\0\u0006\u0007\u0010\U0001F609";
  q = incxstr(p);
  assert(q == p + 7);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 8.  position  ----- unit test
  // failed with old version of incxstr() ----------
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\0\u0007\u0010\U0001F609";
  q = incxstr(p);
  assert(q == p + 8);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 9.  position  ----- unit test
  // failed with old version of incxstr() ---
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0007\0\u0010\U0001F609";
  q = incxstr(p);
  assert(q == p + 9);

  // --- Test for FFFF +control CODE_EXTENDED ----- (earlier p+n) with \0 after 10.  position  ----- unit test
  // failed with old version of incxstr() -----------
  p = (PKMX_WCHAR)u"\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0010\0\U0001F609";
  q = incxstr(p);
  assert(q == p + 10);

  // --------------------------------------------------------------------------------------------------------------------------------------------------
  // ---- UC_SENTINEL, INCOMPLETE & UNUSUAL SEQUENCES--------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------------------------------------

  // --- Test for FFFF + \0
  // --------------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\0\U0001F609";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one character
  // ------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0062\U0001F609";
  q = incxstr(p);
  assert(q == p + 1);

  // --- Test for FFFF +one <control>
  // -----------------------------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0004\U0001F609";
  q = incxstr(p);
  assert(q == p + 2);

  // --- Test for FFFF + one <control> + character
  // -------------------------------------------------------------------------------------------
  p = (PKMX_WCHAR)u"\uFFFF\u0004\u0062\U0001F609";
  q = incxstr(p);
  assert(q == p + 2);
}

constexpr const auto help_str = "\
test_kmx_xstring [--color]\n\
\n\
  --color         Force color output\n";

int error_args() {
  std::cerr << "test_kmx_xstring: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int main(int argc, char *argv []) {

  auto arg_color = argc > 1 && std::string(argv[1]) == "--color";
  console_color::enabled = console_color::isaterminal() || arg_color;

  test_incxstr();

  return 0;
}
