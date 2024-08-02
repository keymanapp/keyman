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
#include "pch.h"   // I4575

BOOL fOutputKeystroke;

/**
 *  Process the key stroke using the core processor
 *
 * @param   _td   Pointer to KEYMAN64THREADDATA
 * @return  BOOL  Return TRUE if keystroke event is passed successfully
 */

static BOOL
Process_Event_Core(PKEYMAN64THREADDATA _td) {
  WCHAR application_context[MAXCONTEXT];
  if (_td->app->ReadContext(application_context)) {
    km_core_context_status result;
    result = km_core_state_context_set_if_needed(_td->lpActiveKeyboard->lpCoreKeyboardState, reinterpret_cast<const km_core_cu *>(application_context));
    if (result == KM_CORE_CONTEXT_STATUS_ERROR || result == KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT) {
      SendDebugMessageFormat("km_core_state_context_set_if_needed returned [%d]", result);
    }
  }

  SendDebugMessageFormat(
      "vkey[%d] ShiftState[%d] isDown[%d]", _td->state.vkey,
      static_cast<uint16_t>(Globals::get_ShiftState() & (KM_CORE_MODIFIER_MASK_ALL | KM_CORE_MODIFIER_MASK_CAPS)), (uint8_t)_td->state.isDown);
  //  Mask the bits supported according to `km_core_modifier_state` enum, update the mask if this enum is expanded.
  if (KM_CORE_STATUS_OK != km_core_process_event(
    _td->lpActiveKeyboard->lpCoreKeyboardState, _td->state.vkey,
    static_cast<uint16_t>(Globals::get_ShiftState() & (KM_CORE_MODIFIER_MASK_ALL | KM_CORE_MODIFIER_MASK_CAPS)), (uint8_t)_td->state.isDown, KM_CORE_EVENT_FLAG_DEFAULT)) {
    SendDebugMessageFormat("CoreProcessEvent Result:False %d ", FALSE);
    return FALSE;
  }
  return TRUE;
}

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

BOOL ProcessHook()
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

  fOutputKeystroke = FALSE;  // TODO: 5442 no longer needs to be global once we use core processor

  if (ShouldDebug()) {
    if(!_td->lpActiveKeyboard || !_td->lpActiveKeyboard->lpCoreKeyboardState) {
      SendDebugMessageFormat("Key %s: %s Context <unavailable>",
        _td->state.isDown ? "pressed" : "released",
        Debug_VirtualKey(_td->state.vkey));
    } else {
      km_core_cu* debug_context = km_core_state_context_debug(
        _td->lpActiveKeyboard->lpCoreKeyboardState,
        KM_CORE_DEBUG_CONTEXT_CACHED
      );
      SendDebugMessageFormatW(L"Key %s: %hs Context '%s'",
        _td->state.isDown ? L"pressed" : L"released",
        Debug_VirtualKey(_td->state.vkey), debug_context);
      km_core_cu_dispose(debug_context);
    }
  }

  // For applications not using the TSF kmtip calls this function twice for each keystroke,
  // first to determine if we are doing processing work (TIPFUpdateable == FALSE),
  // if we say yes it will call a second time to actually do the work.
  // We call the core process event only once and use the core's queued actions
  // on the second pass.
  // For the TSF in most cases kmtip (except OnPreservedKey) will not call the non-updateable test parse.
  // Therfore the core process event will need to be called before processing the actions.

  // CoreProcessEventRun would be a sufficient test however testing TIPFUpdateable defines
  // the status of the keystroke processing more precisely.
  if (!_td->TIPFUpdateable || !_td->CoreProcessEventRun) {
    if (!Process_Event_Core(_td)) {
      return FALSE;
    }
  }

  if (!_td->TIPFUpdateable) {
    ProcessActionsNonUpdatableParse(&fOutputKeystroke);
  } else {
    ProcessActions(&fOutputKeystroke);
  }

  if (fOutputKeystroke && !_td->app->IsQueueEmpty()) {
    //
    // #2759: The keyboard has requested that the default output
    // should be emitted. With .kmn, this is done with 'use(final)'
    // where 'group(final) using keys' is an empty group. Typical
    // use case is to allow ENTER, TAB, etc, to be emitted as
    // normal but to do some preprocessing first. A classic example
    // is final sigma for Greek which changes only when the word is
    // 'complete'.
    //
    // We have some events in the queue, so we need to
    // block the default keystroke, emit those characters, and
    // then synthesize the original keystroke
    //
    SendDebugMessageFormat("%d events in queue and default output requested. [IsLegacy:%d, IsUpdateable:%d]",
      _td->app->GetQueueSize(), _td->app->IsLegacy(), _td->TIPFUpdateable);

    if (_td->app->IsLegacy()) {
      _td->app->QueueAction(QIT_VSHIFTDOWN, Globals::get_ShiftState());
      _td->app->QueueAction(QIT_VKEYDOWN, _td->state.vkey);
      _td->app->QueueAction(QIT_VKEYUP, _td->state.vkey);
      _td->app->QueueAction(QIT_VSHIFTUP, Globals::get_ShiftState());
      fOutputKeystroke = FALSE;
    }
		else if (!_td->TIPFUpdateable) {
      //
      // #2759: kmtip calls this function twice for each keystroke, first to
      // determine if we are doing processing work (IsUpdateable() == FALSE),
      // then to actually do the work. We want to say, "yes we are doing
      // processing work", which we currently do by returning TRUE
      // (that is, setting fOutputKeystroke to FALSE), and then for the second
      // pass, we will do the work and then output the keytroke. Tricky.
      //
      fOutputKeystroke = FALSE;
    }
  }

	if(*Globals::hwndIM() == 0 || *Globals::hwndIMAlways())
	{
		_td->app->SetCurrentShiftState(Globals::get_ShiftState());
		_td->app->SendActions();   // I4196
	}

	return !fOutputKeystroke;
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

PSTR wstrtostr(PCWSTR in)
{
    PSTR result;
    int len;

    len = WideCharToMultiByte(CP_ACP, 0, in, -1, NULL, 0, NULL, NULL);
    result = new CHAR[len+1];
    WideCharToMultiByte(CP_ACP, 0, in, -1, result, len, NULL, NULL);
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
