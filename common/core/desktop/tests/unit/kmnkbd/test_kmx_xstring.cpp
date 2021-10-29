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

PKMX_WCHAR find_ptr_to_last_character(PKMX_WCHAR p_first) {
  int length = wcslen((const wchar_t *)p_first); 
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

    p_start = (PKMX_WCHAR)u"\uFFFF\u0001";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"\uFFFF\u0001\u0001";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 2));

    p_start = (PKMX_WCHAR)u"\uFFFF\u0001\u0001\u0001";
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

    p_start = (PKMX_WCHAR)u"\uFFFF\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"\uFFFF\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p-2 ));

    p_start = (PKMX_WCHAR)u"\uFFFF\u0014\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"\uFFFF\u0014\u0014\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    p_start = (PKMX_WCHAR)u"\uFFFF\u0014\u0014\u0014\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 5));

    p_start = (PKMX_WCHAR)u"\uFFFF\u0014\u0014\u0014\u0014\u0014\u0014";
    p       = find_ptr_to_last_character(p_start);
    q       = decxstr(p, p_start);
    assert(q == (p - 1));

    // -------------------------------------------------------------------------------------------------------------------------------------------------------
    // -- differences in pointer movement for new decxstr ----------------------------------------------------------------------------------------------------
    // ----- OLD version of decxstr: 0x0A not used  => return ( p - 1) , 0x0C not used => return ( p - 1) , 0x0E = 1  => return ( p - 3) 
    // ----- NEW version of decxstr: Values set in in CODE__SIZE[]: 0x0A = 2 => return ( p - 4) , 0x0C =1 => return ( p - 3) , 0x0E = 0  => return ( p - 1)     
    // ------------------------------------------------------------------------------------------------------------------------------------------------------- 

     // runs OK with NEW version of decxstr (with CODE_EXTENDED pointer moves 3 ( 4 altogether) 
     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000A\u0001\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 4) );

     //runs OK with NEW version of decxstr (with CODE_SWITCH pointer moves 2 ( 3 altogether) 
     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000C\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 3) ); 

     // runs OK with NEW version of decxstr (with CODE_CLEARCONTEXT pointer moves 0 ( 1 altogether) 
     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000E\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );
     
     // runs OK with OLD version and NEW version of decxstr
     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000E\u0001";     
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
     
     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000A\u0001\u0002\u0003\u0004\u0010";         
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);     
     assert(q == (p - 1) );
   
     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0010";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);     
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0010d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 8) );
    
     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000A\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0010d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 11) );

    // ---------------------------------------------------------------------------------------
    // ---- Surrogate Pair
    // ---------------------------------------------------------------------------------------
      
     p_start = (PKMX_WCHAR)u"abc\U0001F609";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     p       = find_ptr_to_last_character(p_start); 
     assert(q == (p - 1) );
      
     p_start = (PKMX_WCHAR)u"abc\U0001F609d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );
     
    // ---------------------------------------------------------------------------------------
    // ---- CODE_ 
    // ---------------------------------------------------------------------------------------

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0001\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0001\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );
     
     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0002\u0001\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0003\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );   

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0004\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0005\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0006\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0007\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0008\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000A\u0001\u0001";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) ); 

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000C\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) ); 

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000E\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000F\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0011\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0012\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) ); 

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0013\u0001\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0014\u0001\u0001\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0015\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) ); 

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0017\u0001\u0001\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );
     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0016\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0018\u0001\u0001";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) ); 

    // ---------------------------------------------------------------------------------------
    // ---- CODE_ followed by letter
    // ---------------------------------------------------------------------------------------

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0001\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0002\u0001\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 4) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0003\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0004\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0005\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0006\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0007\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0008\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 3) ); 

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u000F\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 3) ); 

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0011\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0012\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0013\u0001\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 4) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0014\u0001\u0001\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 5) ); 

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0015\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0016\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0017\u0001\u0001\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 5) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0018\u0001\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 4) );   
     
    // ---------------------------------------------------------------------------------------
    // ---- malformed CODE_  too short & too long
    // ---------------------------------------------------------------------------------------

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0001";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0005d";    
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 2) ); 

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0002d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0002\u0001";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0002\u0001d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0014d";
     p       = find_ptr_to_last_character(p_start); 
     q       = decxstr(p, p_start);
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0014\u0001";
     p       = find_ptr_to_last_character(p_start); 
     q       = decxstr(p, p_start);
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0014\u0001d";
     p       = find_ptr_to_last_character(p_start); 
     q       = decxstr(p, p_start);
     assert(q == (p - 1 ));

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0014\u0001\u0001";
     p       = find_ptr_to_last_character(p_start); 
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0014\u0001\u0001d";
     p       = find_ptr_to_last_character(p_start); 
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0001\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 3) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0002\u0001\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 4) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0014\u0001\u0001\u0001d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 5) );

    // ---------------------------------------------------------------------------------------
    // ---- \0 in sequence
    // ---------------------------------------------------------------------------------------
    
     p_start = (PKMX_WCHAR)u"abc\0";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );
 
     p_start = (PKMX_WCHAR)u"abc\0d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"\0\U0001F609";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == NULL );

     p_start = (PKMX_WCHAR)u"\uFFFF\0\u0014\u0014\u0014d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == NULL );

     p_start = (PKMX_WCHAR)u"\uFFFF\u0014\0\u0014\u0014d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == p - 1 );

     p_start = (PKMX_WCHAR)u"\uFFFF\u0014\u0014\0\u0014d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == p - 2 );

     p_start = (PKMX_WCHAR)u"\uFFFF\u0014\u0014\u0014\0d";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == p - 1 );

     p_start = (PKMX_WCHAR)u"\0";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == NULL );

     p_start = (PKMX_WCHAR)u"d\0";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == NULL );

     p_start = (PKMX_WCHAR)u"\0d";        
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == NULL );

     p_start = (PKMX_WCHAR)u"\uFFFF\0";        
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == NULL );

     p_start = (PKMX_WCHAR)u"\0\uFFFF";        
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == NULL );

     p_start = (PKMX_WCHAR)u"\U0001F609\0";        
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );
     
    // ---------------------------------------------------------------------------------------
    // ---- other 
    // ---------------------------------------------------------------------------------------
    
     p_start = (PKMX_WCHAR)u"abc\uFFFF\u0002def\u0001";
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"abc\uFFFF";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"\uFFFF\U0001F609";        
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == (p - 2) );

     p_start = (PKMX_WCHAR)u"\u0014d";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"\uFFFF";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);
     assert(q == NULL );

     p_start = (PKMX_WCHAR)u"\uFFFFd";     
     p       = find_ptr_to_last_character(p_start);
     q       = decxstr(p, p_start);       
     assert(q == (p - 1) );

     p_start = (PKMX_WCHAR)u"\uFFFF\uFFFF";     
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
  test_decxstr();

  return 0;
}
