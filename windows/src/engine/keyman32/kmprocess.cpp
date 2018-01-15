/*
  Name:             kmprocess
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    28 Mar 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Remove NoSetShift flag
                    04 Jan 2007 - mcdurdin - Add CODE_NOTANY
                    22 Jan 2007 - mcdurdin - Fix for K_NPENTER
                    19 Jun 2007 - mcdurdin - I890 - Flush end-of-textstore deadkeys with non-character keystrokes
                    13 Jul 2007 - mcdurdin - I934 - Prep for x64
                    23 Aug 2007 - mcdurdin - I994 - Fix TSF keystrokes out of order
                    05 Nov 2007 - mcdurdin - I1133 - Rule regression - Mismatched parentheses (fix for .239 fix)
                    05 Nov 2007 - mcdurdin - I1130 - Speed up keystrokes by avoiding unnecessary debug string building
                    14 Jun 2008 - mcdurdin - I1389 - Supplementary chars on Vista default to a single BKSP
                    16 Jan 2009 - mcdurdin - I1622 - Fix deadkeys not working correctly in stores
                    27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    30 Nov 2009 - mcdurdin - I2156 - Remove invalid CODE_CONTEXT test
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    15 Jun 2010 - mcdurdin - I2424 - Use AIWin2000Unicode unless disabled
                    24 Jun 2010 - mcdurdin - I2436 - Add U+0020 to context manually when not matched
                    29 Jun 2010 - mcdurdin - I2439 - If keyboard fails to load, IsKeyboardUnicode will crash
                    25 Mar 2011 - mcdurdin - I2590 - Keyman Engine crashes if if() statement is not first character of rule
                    05 Sep 2011 - mcdurdin - I3054 - WM_UNICHAR support regression
                    17 Aug 2012 - mcdurdin - I3432 - V9.0 - Add support for &platform and &baselayout to Keyman Engine
                    27 Aug 2012 - mcdurdin - I3439 - V9.0 - Refactor xstring support in C++ code
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys Created
                    02 Dec 2011 - mcdurdin - I3162 - Potential buffer overflow for keyboards with complex contexts
                    02 Dec 2011 - mcdurdin - I3158 - Keyman Engine can crash other applications when multiple OEM products are started, with a _tcscpy assertion
                    04 Nov 2012 - mcdurdin - I3536 - V9.0 - Merge of I3162 - Potential buffer overflow for keyboards with complex contexts
                    04 Nov 2012 - mcdurdin - I3524 - V9.0 - Merge of I3158 - Keyman Engine can crash other applications when multiple OEM products are started, with a _tcscpy assertion
                    17 Nov 2012 - mcdurdin - I3573 - V9.0 - Don't recreate the TSF AppIntegration with each keystroke
                    17 Nov 2012 - mcdurdin - I3577 - V9.0 - Keyman becomes responsible for all key outputs when using TSF, even those it wouldn't normally eat
                    01 Dec 2012 - mcdurdin - I3617 - V9.0 - Keyboard hook obsolete, strip out code
                    10 Jan 2014 - mcdurdin - I4024 - V9.0 - Shift states not working with default keys in V9
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    24 Apr 2014 - mcdurdin - I4196 - V9.0 - wm_kmmoreposting must be refactored for TIP work as it is not sequential
                    01 May 2014 - mcdurdin - I4128 - V9.0 - Shift states still not working with unprocessed keys in V9
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    26 Jun 2014 - mcdurdin - I4290 - Keys that have rules but are not matched due to context do not generate output
                    14 Nov 2014 - mcdurdin - I4516 - V9.0 - Language hotkeys associated with non-primary keyboards do not trigger language change
                    27 Jan 2015 - mcdurdin - I4575 - V9.0 - Support output of TAB and ENTER for unmatched key events
                    27 Jan 2015 - mcdurdin - I4575 - V9.0 - Support output of TAB and ENTER for unmatched key events
                    03 Feb 2015 - mcdurdin - I4582 - V9.0 - Most underlying layout code in Keyman32 is now obsolete and needs to be removed
                    04 Feb 2015 - mcdurdin - I4585 - V9.0 - Keyman Engine appears to ignore space in context when no rule for it
                    06 Feb 2015 - mcdurdin - I4583 - V9.0 - Remove altgr lookup test from keyman32 and put it into the registry
                    23 Jun 2015 - mcdurdin - I4033 - V9.0 - Enable SHIFT+ESC to pause debugger with TSF
                    03 Aug 2015 - mcdurdin - I4826 - Navigation keys don't work in debugger
                    03 Aug 2015 - mcdurdin - I4827 - Key down debug events are not always processed correctly
                    09 Aug 2015 - mcdurdin - I4845 - Debugger does not permit Delete key
                    10 Aug 2015 - mcdurdin - I4838 - Backspace and Delete don't work in the keyboard debugger
                    28 Mar 2016 - mcdurdin - I4933 - Compat issue with Firefox 42 and IE and Keyman 9 TSF
*/
#include "keyman64.h"   // I4575
#include "AbstractKeymanRuleProcessor.h"


/*
*	BOOL ProcessHook();
*
*	Parameters:	none
*
*	Returns:	TRUE if keystroke should be eaten
*
*   Called by:  FilterFunc
*
*	ProcessHook organizes the messages and gives them to the appropriate routines to
*	process, and checks the state of Windows for the keyboard handling.
*/

AbstractKeymanRuleProcessor *GetCurrentKeymanRuleProcessor() {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return NULL;
  if (!_td->lpActiveKeyboard) return NULL;
  return _td->lpActiveKeyboard->ruleProcessor;
}

BOOL ProcessHook()
{
  AbstractKeymanRuleProcessor *krp = GetCurrentKeymanRuleProcessor();
  if (!krp) return FALSE;

  KeymanRuleEvent event;
  KeymanRuleActionList actions;

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;
  event.vk = _td->state.vkey;
  event.shiftState = Globals::get_ShiftState();
  BOOL r = krp->ProcessEvent(&event, &actions);

  return r;
}



PWSTR strtowstr(PSTR in)
{
    PWSTR result;
    size_t len;

    mbstowcs_s(&len, NULL, 0, in, strlen(in));
    result = new WCHAR[len+1];
    mbstowcs_s(&len, result, len, in, strlen(in));
    result[len] = 0;
    return result;
}


PSTR wstrtostr(PWSTR in)
{
    PSTR result;
    size_t len;

    wcstombs_s(&len, NULL, 0, in, wcslen(in));
    result = new CHAR[len+1];
    wcstombs_s(&len, result, len, in, wcslen(in));
    result[len] = 0;
    return result;
}

   // I4220
void SelectApplicationIntegration()   // I4287
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;
  if(!_td->app) {
    _td->app = new AITIP;
  }
}
