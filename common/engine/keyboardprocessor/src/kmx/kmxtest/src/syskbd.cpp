/*
  Name:             syskbd
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Jan 2007

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Jan 2007 - mcdurdin - Don't translate any number pad keys
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    30 Nov 2009 - mcdurdin - I934 - Prep for x64 - change UINT to WORD for vkeys
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    15 Jun 2012 - mcdurdin - I3358 - Media keys generate characters when positional keyboard is active
                    04 Nov 2012 - mcdurdin - I3535 - V9.0 - Merge of I3358 - Media keys generate characters when positional keyboard is active
                    28 Nov 2012 - mcdurdin - I3597 - V9.0 - Move VK_ defines to global header
                    17 Jan 2013 - mcdurdin - I3762 - V9.0 - underlying layout cannot be controlled cleanly via TSF so support translation
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    03 Feb 2015 - mcdurdin - I4582 - V9.0 - Most underlying layout code in Keyman32 is now obsolete and needs to be removed
*/
   // I4169
////////////////////////////////////////////////////////////////////////////
//
// Interfaces for handling windows system keyboards
//
////////////////////////////////////////////////////////////////////////////

#include "pch.h"

extern BOOL KeyboardGivesCtrlRAltForRAlt_NT();

/************************************************************************/ 
/* CharFromVK: Return a character code from a virtual key code          */ 
/************************************************************************/ 


/*
 For some reason, keyboards that use RAlt as a shifter modify it internally to Ctrl+RAlt, which
 just confuses the heck out of keyboards that use RAlt.  So we work around by recognising this and
 adding Ctrl to the tests for these layouts.
*/

BOOL KeyboardGivesCtrlRAltForRAlt()
{
	return KeyboardGivesCtrlRAltForRAlt_NT();
}
