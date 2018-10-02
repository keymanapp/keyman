/*
  Name:             kmhook_getmessage
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Visual keyboard updating
                    14 Sep 2006 - mcdurdin - Add wxm_keymanchar message for character map and other tools
                    04 Dec 2006 - mcdurdin - Add support for mosewheel on character map even when not focused
                    22 Jan 2007 - mcdurdin - Fix for K_NPENTER
                    16 May 2007 - mcdurdin - I819 - Test for Keyman functioning
                    30 May 2007 - mcdurdin - I864 - Log exceptions in hook procedures
                    13 Jul 2007 - mcdurdin - I934 - Prep for x64
                    27 Mar 2008 - mcdurdin - I1337 - move mousewheel handling before PM_REMOVE to avoid mousing both osk and active app
                    16 Jan 2009 - mcdurdin - Minor performance improvements around debugging
                    27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    30 Jan 2009 - mcdurdin - I1835 - Improve refresh performance
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    11 Dec 2009 - mcdurdin - I1455 - keyboard activation per thread
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    29 Mar 2010 - mcdurdin - I1089 - One keyboard - all apps
                    06 Apr 2010 - mcdurdin - I2271 - Select Keyboard tidy up
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    04 May 2010 - mcdurdin - I2297 - Keyboard and focus switching reliability
                    10 May 2010 - mcdurdin - Enable exception logging
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    24 Jun 2010 - mcdurdin - I2435 - Keyman Engine stays attached to a number of apps when Keyman shuts down - due to local keyboardhook attachment
                    29 Jun 2010 - mcdurdin - I2435 - Make Keyman exit more reliably by sending rather than posting KM_EXIT
                    29 Jun 2010 - mcdurdin - I2435 - Prevent Keyman from re-initialising when it is shutting down
                    18 Feb 2011 - mcdurdin - I2712 - Font helper not supporting SMP
                    12 May 2011 - mcdurdin - I2908 - Fix double strike issue
                    19 Jul 2011 - mcdurdin - I2999 - If switch for all apps is off, Keyman icon does not reflect active keyboard when application switched
                    17 Aug 2012 - mcdurdin - I3432 - V9.0 - Add support for &platform and &baselayout to Keyman Engine
                    21 Feb 2012 - mcdurdin - I3253 - Wrong hook handle passed to CallNextHookEx
                    03 Jul 2012 - mcdurdin - I3385 - Keyman Engine thinks it is in Keyman Desktop Light mode in parent process when starting
                    04 Nov 2012 - mcdurdin - I3532 - V9.0 - Merge of I3253 - Wrong hook handle passed to CallNextHookEx
                    04 Nov 2012 - mcdurdin - I3533 - V9.0 - Merge of I3385 - Keyman Engine thinks it is in Keyman Desktop Light mode in parent process when starting
                    20 Nov 2012 - mcdurdin - I3583 - V9.0 - Remove AITIP::NotifyKeyboardChange as obsolete
                    28 Nov 2012 - mcdurdin - I3594 - V9.0 - Remove old SelectKeyboard code and related messages
                    01 Dec 2012 - mcdurdin - I3617 - V9.0 - Keyboard hook obsolete, strip out code
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    07 Nov 2013 - mcdurdin - I3949 - V9.0 - Keyboard selection and notification needs consolidation
                    24 Apr 2014 - mcdurdin - I4196 - V9.0 - wm_kmmoreposting must be refactored for TIP work as it is not sequential
                    16 Jun 2014 - mcdurdin - I4271 - V9.0 - Switch language for all applications is not working
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    25 Sep 2014 - mcdurdin - I4412 - V9.0 - Character Map needs to insert characters using SendInput
                    29 Mar 2015 - mcdurdin - I4642 - V9.0 - In Logos, generated backspace receives a scan of 0x00 instead of 0xFF
                    22 Apr 2015 - mcdurdin - I4674 - V9.0 - Hotkeys do not always work consistently
                    09 Aug 2015 - mcdurdin - I4793 - Race condition with preserved keys and modifiers
                    28 Mar 2016 - mcdurdin - I4933 - Compat issue with Firefox 42 and IE and Keyman 9 TSF
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/
   // I3583   // I4287
#include "keyman64.h"

void ProcessWMKeymanControlInternal(HWND hwnd, WPARAM wParam, LPARAM lParam);
void ProcessWMKeyman(HWND hwnd, WPARAM wParam, LPARAM lParam);
void GetCapsAndNumlockState();
BOOL IsDevEnvCtrlTabSequence(BOOL isPMRemove, LPMSG mp);

/*
BOOL SysTabCtrl(HWND hwnd)
{
	char buf[64];
	GetClassName(hwnd, buf, 64);
	return !strcmpi(buf, "SysTabControl32");
}
*/

BOOL IsMessageEquivalent(LPMSG m1, LPMSG m2)
{
	return m1->hwnd == m2->hwnd &&
		m1->lParam == m2->lParam &&
		m1->message == m2->message &&
		m1->time == m2->time &&
		m1->pt.x == m2->pt.x &&
		m1->pt.y == m2->pt.y &&
		m1->wParam == m2->wParam;
}

LRESULT _kmnGetMessageProc(int nCode, WPARAM wParam, LPARAM lParam);

LRESULT CALLBACK kmnGetMessageProc(int nCode, WPARAM wParam, LPARAM lParam)
{
  LRESULT res = 0;

#ifdef _DEBUG_EXCEPTION
  res = _kmnGetMessageProc(nCode,wParam,lParam);
#else
  __try
  {
    res = _kmnGetMessageProc(nCode,wParam,lParam);
  }
  __except(ExceptionMessage("kmnGetMessageProc", GetExceptionInformation()))
  {
  }
#endif

	return res;
}

void ProcessModifierChange(UINT key, BOOL isUp, BOOL isExtended);   // I4793

LRESULT _kmnGetMessageProc(int nCode, WPARAM wParam, LPARAM lParam)
{
	LPMSG mp;

	mp = (LPMSG)lParam;

	// If the message sent should be ignored, just return
  if(nCode < 0) 
    return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td)
  {
    if(!Globals::CheckControllers()) 
      return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);

    if(!Globals_InitThread())
      return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);

    _td = ThreadGlobals();
	  if(!_td)
      return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
  }

  if(!_td->FInitialised)			// Keyman keyboard is being selected for first time in active process
  {
    if(!Globals::CheckControllers()) return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);  // I2435 - Prevent Keyman from re-initialising when it is shutting down  // I3253   // I3532
		if(!InitialiseProcess(mp->hwnd)) return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
  }

	if(mp->message == WM_CHAR || mp->message == WM_SYSCHAR || mp->message == WM_KEYDOWN || mp->message == WM_SYSKEYDOWN || mp->message == WM_KEYUP || mp->message == WM_SYSKEYUP) 
    if(ShouldDebug(sdmMessage)) DebugMessage(mp, wParam);

  if ((mp->message == WM_KEYDOWN || mp->message == WM_SYSKEYDOWN || mp->message == WM_KEYUP || mp->message == WM_SYSKEYUP)) {   // I4642
    BYTE scan = KEYMSG_LPARAM_SCAN(mp->lParam);
    if (scan != 0xFE) {
      _td->LastScanCode = scan;
      _td->LastKey = mp->wParam;
    }

    /*
      Serializes all input events onto the current focused window thread by repeating any externally generated events. This
      is required to ensure that we can get the correct modifier state when we need to push a modifier release/set around
      keystroke output (mostly for the purposes of the backspace event).

      We need to release Alt and Ctrl modifiers (and we do Shift as well for completeness) when emitting Backspace to
      modify context in order to avoid triggering Alt+Backspace -> Undo or Ctrl+Backspace -> 0x7F / Word delete left
      instead of a Char delete left event.

      However it is possible that a modifier release event generated by the user is already in the queue at the time we
      send the input. Since we have no way to detect or prevent this happening, what we do instead is re-inject each
      keystroke into the queue in the focused input thread, which means we can guarantee order of events and sort out
      the modifier state as required.  This looks really messy in the message event queue but turns out to be robust
      in practice.

      You can disable this flag with flag_ShouldSerializeInput and flag_IsDevEnvProcess handles a nuance with devenv.exe.
    */
    
    if (flag_ShouldSerializeInput && _td->lpActiveKeyboard && 
        !IsDevEnvCtrlTabSequence((wParam & PM_REMOVE) == PM_REMOVE, mp)) {
      if (scan < 0xFE && mp->wParam != VK_PACKET && mp->wParam != VK_PROCESSKEY) {
        if ((wParam & PM_REMOVE) == PM_REMOVE) {
          // We only want to send this input once -- with PM_NOREMOVE, we will be
          // processing the same message multiple times
          INPUT input;
          input.type = INPUT_KEYBOARD;
          input.ki.wVk = (WORD)mp->wParam;
          input.ki.wScan = 0xFE;
          input.ki.time = mp->time;
          input.ki.dwExtraInfo = mp->lParam;  // We'll copy this back later
          input.ki.dwFlags =
            (KEYMSG_FLAG_EXTENDED(mp->lParam) ? KEYEVENTF_EXTENDEDKEY : 0) |
            (mp->message == WM_KEYUP || mp->message == WM_SYSKEYUP ? KEYEVENTF_KEYUP : 0);

          SendInput(1, &input, sizeof(INPUT));
        }

        mp->message = wm_keyman_ignore;

        return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
      }
    }

    switch (mp->wParam) {
    case VK_MENU:
    case VK_CONTROL:
    case VK_SHIFT:
      if (scan != 0xFF) {   // I4793
        ProcessModifierChange((UINT)mp->wParam, mp->message == WM_KEYUP || mp->message == WM_SYSKEYUP, KEYMSG_FLAG_EXTENDED(mp->lParam));
      }
      break;
    }

    // 4 Aug 2003 - mcdurdin - Stuff any Keyman wm_key* message with the correct scancode
    if (mp->wParam != VK_BACK) {
      if (scan == 0xFE) {
        mp->lParam = GetMessageExtraInfo();
        SetMessageExtraInfo(0);
      } else if(scan == 0xFF) {
        mp->lParam = (mp->lParam & 0xFF00FFFFL) | (MapVirtualKey((UINT)mp->wParam, 0) << 16);
      }
    }
  }

  // I1337 - move mousewheel message checking before the PM_REMOVE handling - otherwise we can't pre-modify the message
	if(mp->message == WM_MOUSEWHEEL)
	{
		HWND hwndMouseWheel = FindWindow("TfrmVisualKeyboard", NULL);
		if(hwndMouseWheel && GetWindowThreadProcessId(mp->hwnd, NULL) != GetWindowThreadProcessId(hwndMouseWheel, NULL))
		{
			RECT rcMouseWheel;
			POINT pt;
			GetWindowRect(hwndMouseWheel, &rcMouseWheel);
			pt.x = LOWORD(mp->lParam);
			pt.y = HIWORD(mp->lParam);
			if(PtInRect(&rcMouseWheel, pt))
			{
        // if(!_td->state.PreviousNoRemove)  // I2908
				  PostMessage(WindowFromPoint(pt), WM_MOUSEWHEEL, mp->wParam, mp->lParam);
				mp->message = WM_NULL;
				return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
			}
		}
	}

  if((wParam & PM_REMOVE) == 0)  // I2908
  {
    return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
  }

#if 0  // I2908
  if(_td->state.PreviousNoRemove && IsMessageEquivalent(&_td->state.msg, mp))
	{
		/*
		  Was the previous message the same as this one.  If so, don't process it again.
		 */
		_td->state.PreviousNoRemove = (wParam & PM_REMOVE) == 0;
		_td->state.msg = *mp;
		return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
	}
#endif

	_td->state.msg = *mp;
	/*_td->state.PreviousNoRemove = (wParam & PM_REMOVE) == 0; I2908 */

  // 16 April 2005, 7 May 2007 - mcdurdin - Check if Keyman is receiving messages (reimplement from v6.2) */
	if(mp->message == wm_test_keyman_functioning && mp->wParam == TKF_PING)
	{
		PostMessage(mp->hwnd, wm_test_keyman_functioning, TKF_RESPONSE, mp->lParam);
		return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
	}

   // I3594
	if(mp->message == wm_keyman_refresh)
	{
		SendDebugMessageFormat(0, sdmInternat, 0, "GetMessage: wm_keyman_refresh %d tag=%d", mp->wParam, mp->lParam);
		HandleRefresh((int) mp->wParam, (LONG) mp->lParam); 
		return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
	}

	if(mp->message == wm_keyman_control_internal)   // I3933
	{
    SendDebugMessageFormat(0, sdmInternat, 0, "GetMessage: wm_keyman_control_internal hwnd=%x %x %x", mp->hwnd, mp->wParam, mp->lParam);   // I4674
		ProcessWMKeymanControlInternal(mp->hwnd, mp->wParam, mp->lParam);
		return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
	}

	if(mp->message == wm_keyman)   // I3933
	{
    SendDebugMessageFormat(0, sdmInternat, 0, "GetMessage: wm_keyman hwnd=%x %x %x", mp->hwnd, mp->wParam, mp->lParam);   // I4674
		ProcessWMKeyman(mp->hwnd, mp->wParam, mp->lParam);
		return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
	}

	/*
	 Handle WM_UNICHAR messages for RichEdit control -- should we test RichEdit version?
	*/

	if(mp->message == WM_UNICHAR && Addin_ShouldProcessUnichar(mp->hwnd))
	{
		if(Addin_ProcessUnichar(mp->hwnd, (DWORD) mp->wParam)) 
		{
			mp->message = 0;
			return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
		}
	}

	/*
	 Handle wm_keyman_control_internal messages
	*/

	/*
	 If the keyboard needs to be activated, select it: messaging from the main application
	*/

	if(_td->lpActiveKeyboard)
	{
		_td->state.lpkb = _td->lpActiveKeyboard->Keyboard;
	}
   // I4412
	if(mp->message == wm_keymanshift)
	{
		SendDebugMessageFormat(0, sdmInternat, 0, "GetMessage: wm_keymanshift %x %x", mp->wParam, mp->lParam);
    SelectApplicationIntegration();
		if(!_td->app->IsWindowHandled(mp->hwnd)) _td->app->HandleWindow(mp->hwnd);
		if(_td->app->DebugControlled())
		{
			if(mp->wParam == 1) *Globals::ShiftState() = (DWORD) mp->lParam;
			else *Globals::ShiftState() = 0;
		}
		return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
	}

	if(mp->message == WM_ACTIVATEAPP && mp->wParam)
		UpdateKeymanUI();

	if(*Globals::hwndIM() != 0)
	{
		if(mp->message == wm_keymanim_close)
		{
			if(mp->lParam) KMHideIM();
			if(mp->wParam)
			{
				if(_td->app)
				{
					_td->app->SetCurrentShiftState(Globals::get_ShiftState());
					_td->app->SendActions();   // I4196
				}
			}
			return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
		}
	
		if(!*Globals::hwndIMAlways())
		{
			if(mp->message == wm_keymankeydown)
				PostMessage(*Globals::hwndIM(), WM_KEYDOWN, mp->wParam, mp->lParam);
			else if(mp->message == wm_keymankeyup)
				PostMessage(*Globals::hwndIM(), WM_KEYUP, mp->wParam, mp->lParam);
			return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
		}
	}

	/*
	 If no keyboard is active, return
	*/

	if(!_td->lpActiveKeyboard || _td->FInRefreshKeyboards)
	{
		if(_td->app) _td->app->ResetContext();
		return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
	}

	if(mp->message > WM_MOUSEFIRST && mp->message <= WM_MOUSELAST && !IsIMWindow(mp->hwnd)) 
	{
		// > MOUSEFIRST ignores mouse movement
		if(_td->app) _td->app->ResetContext();
		return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
	}

	/*   // I3617
	 Text Services Framework tells us we don't need to continue here
	*/

	return CallNextHookEx(Globals::get_hhookGetMessage(), nCode, wParam, lParam);
}

BOOL CALLBACK KeymanExitEnumProc(HWND hwnd, LPARAM lParam)
{
  UNREFERENCED_PARAMETER(lParam);

  if(GetWindowThreadProcessId(hwnd, NULL) != GetCurrentThreadId())
  {
    // This will be ignored by Keyman if the thread has already shutdown, because no controller windows will be registered
    DWORD_PTR dwResult;
    SendMessageTimeout(hwnd, wm_keyman, KM_EXIT, 0, SMTO_BLOCK, 1000, &dwResult);
  }
  return TRUE;
}

void KeymanExit(HWND hwnd)
{
	UninitialiseProcess(TRUE);
  Globals_UninitThread();
  EnumChildWindows(hwnd, KeymanExitEnumProc, 0);
}


void ProcessWMKeyman(HWND hwnd, WPARAM wParam, LPARAM lParam)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

	SendDebugMessageFormat(hwnd, sdmGlobal, 0, "ProcessWMKeyman wm_keyman: %d %d", wParam, lParam);
	switch(wParam)
	{
	case KM_DISABLEUI:
		_td->KeymanUIDisabled = TRUE;
		UpdateKeymanUI();
		break;
	case KM_ENABLEUI:
		_td->KeymanUIDisabled = FALSE;
		UpdateKeymanUI();
		break;
	case KM_FOCUSCHANGED:
    if(lParam & KMF_WINDOWCHANGED)
    {
      hwnd = GetFocus();

		  if(_td->lpActiveKeyboard) {
			  _td->state.lpkb = _td->lpActiveKeyboard->Keyboard;
		  }
      
      SelectApplicationIntegration();
      if(!_td->app->IsWindowHandled(hwnd)) _td->app->HandleWindow(hwnd);
      _td->state.windowunicode = _td->app->IsUnicode();

      if(IsFocusedThread())
		  {
        if(_td->app) _td->app->ResetQueue();
	  	  GetCapsAndNumlockState();
  		  Addin_FocusChanged(hwnd);
	  	  UpdateActiveWindows();
      }
    }

		break;
  case KM_EXIT:
    KeymanExit(hwnd);
    break;
	}
}


void ProcessWMKeymanControlInternal(HWND hwnd, WPARAM wParam, LPARAM lParam)
{
  UNREFERENCED_PARAMETER(hwnd);
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

	switch(wParam)
	{
  case KMCI_GETACTIVEKEYBOARD:   // I3933
    ReportActiveKeyboard(_td, LOWORD(lParam));   // I3949
    break;

  case KMCI_SELECTKEYBOARD_BACKGROUND:   // I4271
  case KMCI_SELECTKEYBOARD:   // I3933
    SelectKeyboardHKL(_td, (DWORD) lParam, wParam == KMCI_SELECTKEYBOARD);
    break;

  case KMCI_SELECTKEYBOARD_BACKGROUND_TSF:   // I4271
  case KMCI_SELECTKEYBOARD_TSF:   // I3933
    SelectKeyboardTSF(_td, (DWORD) lParam, wParam == KMCI_SELECTKEYBOARD_TSF);
    break;

  case KMCI_SETFOREGROUND:   // I3933
    SetForegroundWindow(hwnd);
    break;
	}
}

void UpdateKeymanUI()
{
	Globals::PostControllers(wm_keyman_control, KMC_CHANGEUISTATE, 2);  // TODO: Fixup constant
}

/*
  GetCapsAndNumlockState:

  Updates the global caps and numlock state when a window is focused, because it
  may have been reset while Keyman was not aware of it
*/
void GetCapsAndNumlockState() {   // I4793
	DWORD n = Globals::get_ShiftState();

	if(GetKeyState(VK_NUMLOCK) & 1) *Globals::ShiftState() |= NUMLOCKFLAG;
	else *Globals::ShiftState() &= ~NUMLOCKFLAG;

	if(GetKeyState(VK_CAPITAL) & 1) *Globals::ShiftState() |= CAPITALFLAG;
	else *Globals::ShiftState() &= ~CAPITALFLAG;

	if(GetKeyState(VK_SHIFT) < 0) *Globals::ShiftState() |= K_SHIFTFLAG;
	else *Globals::ShiftState() &= ~K_SHIFTFLAG;

	if(GetKeyState(VK_RCONTROL) < 0) *Globals::ShiftState() |= RCTRLFLAG;
	else *Globals::ShiftState() &= ~RCTRLFLAG;

	if(GetKeyState(VK_LCONTROL) < 0) *Globals::ShiftState() |= LCTRLFLAG;
	else *Globals::ShiftState() &= ~LCTRLFLAG;

	if(GetKeyState(VK_LMENU) < 0) *Globals::ShiftState() |= LALTFLAG;
	else *Globals::ShiftState() &= ~LALTFLAG;

	if(GetKeyState(VK_RMENU) < 0) *Globals::ShiftState() |= RALTFLAG;
	else *Globals::ShiftState() &= ~RALTFLAG;

	SendDebugMessageFormat(0, sdmInternat, 0, "GetCapsAndNumlockState (Enter:%x Exit:%x)", n, Globals::get_ShiftState());
}

/*
  Update the Keyman shift state based on the key event. This has to be
  done from the GetMessage hook because we don't consistently receive
  modifier events in some applications (e.g. ALT in Firefox) via TSF.
*/
void ProcessModifierChange(UINT key, BOOL isUp, BOOL isExtended) {   // I4793
  UINT flag = 0;

  switch(key) {  
    case VK_SHIFT:   flag = K_SHIFTFLAG; break;
    case VK_MENU:    flag = isExtended ? RALTFLAG : LALTFLAG; break;
    case VK_CONTROL: flag = isExtended ? RCTRLFLAG : LCTRLFLAG; break;
    case VK_CAPITAL: flag = CAPITALFLAG; break;
    case VK_NUMLOCK: flag = NUMLOCKFLAG; break;
  }

  UINT oldShiftState = Globals::get_ShiftState();

  if(isUp) {
    *Globals::ShiftState() &= ~flag; 
  }
  else { 
    *Globals::ShiftState() |= flag;
  }

  SendDebugMessageFormat(0, sdmGlobal, 0, "ProcessModifierChange(%x, %d, %d) -> shift state from %x to %x", 
    key, isUp, isExtended, oldShiftState, Globals::get_ShiftState());
}

BOOL IsFocusedThread()
{
  GUITHREADINFO gti;
  gti.cbSize = sizeof(GUITHREADINFO);
  if(!GetGUIThreadInfo(0, &gti))
  {
    DebugLastError();
    return FALSE;
  }
  return gti.hwndFocus == GetFocus();
	//return GetCurrentThreadId() == GetWindowThreadProcessId(GetForegroundWindow(), NULL);
}

/*
  Compatibility workaround for devenv and Ctrl+Tab - used with flag_ShouldSerializeInput:

  *Visual Studio Ctrl + Tab shows popup as expected but does not switch windows while this is active.This is because
  the final VK_CONTROL key up ends up being posted to the edit window instead of the temporary switch window, presumably
  because the temporary switch window is destroyed in between the key event and the repost of the key event, which
  suggests that Visual Studio is running a hook to capture the Ctrl + Tab sequence but only processing the outcome of it
  back in the main window.Mitigation : difficult.

  Message sequence with ShouldSerializeInput :
  < 000023>	50456	P	WM_KEYDOWN	nVirtKey : VK_CONTROL	cRepeat : 1	ScanCode : 1D	fExtended : 0	fAltDown : 0	fRepeat : 0	fUp : 0
  < 000024 > 50456	P	WM_KEYDOWN	nVirtKey : VK_TAB	cRepeat : 1	ScanCode : 0F	fExtended : 0	fAltDown : 0	fRepeat : 0	fUp : 0
  < 000025 > 0016157A	P	WM_KEYUP	nVirtKey : VK_TAB	cRepeat : 1	ScanCode : 0F	fExtended : 0	fAltDown : 0	fRepeat : 1	fUp : 1
  < 000026 > 0016157A	P	WM_KEYDOWN	nVirtKey : VK_TAB	cRepeat : 1	ScanCode : 0F	fExtended : 0	fAltDown : 0	fRepeat : 0	fUp : 0
  < 000027 > 0016157A	P	WM_KEYUP	nVirtKey : VK_TAB	cRepeat : 1	ScanCode : 0F	fExtended : 0	fAltDown : 0	fRepeat : 1	fUp : 1
  < 000028 > 0016157A	P	WM_KEYUP	nVirtKey : VK_CONTROL	cRepeat : 1	ScanCode : 1D	fExtended : 0	fAltDown : 0	fRepeat : 1	fUp : 1

  Message sequence with ShouldSerializeInput(note last message, Ctrl release) :
    < 000029>	50456	P	WM_KEYDOWN	nVirtKey : VK_CONTROL	cRepeat : 1	ScanCode : 1D	fExtended : 0	fAltDown : 0	fRepeat : 0	fUp : 0
    < 000030 > 50456	P	WM_KEYDOWN	nVirtKey : VK_TAB	cRepeat : 1	ScanCode : 0F	fExtended : 0	fAltDown : 0	fRepeat : 0	fUp : 0
    < 000031 > 181324	P	WM_KEYUP	nVirtKey : VK_TAB	cRepeat : 1	ScanCode : 0F	fExtended : 0	fAltDown : 0	fRepeat : 1	fUp : 1
    < 000032 > 181324	P	WM_KEYDOWN	nVirtKey : VK_TAB	cRepeat : 1	ScanCode : 0F	fExtended : 0	fAltDown : 0	fRepeat : 0	fUp : 0
    < 000033 > 181324	P	WM_KEYUP	nVirtKey : VK_TAB	cRepeat : 1	ScanCode : 0F	fExtended : 0	fAltDown : 0	fRepeat : 1	fUp : 1
    < 000034 > 50456	P	WM_KEYUP	nVirtKey : VK_CONTROL	cRepeat : 1	ScanCode : 1D	fExtended : 0	fAltDown : 0	fRepeat : 1	fUp : 1
*/

BOOL IsDevEnvCtrlTabSequence(BOOL isPMRemove, LPMSG mp) {
  if (!flag_IsDevEnvProcess) {
    return FALSE;
  }

  switch (flag_DevEnv_CtrlTabStarted) {
  case 0:
    if (mp->wParam == VK_CONTROL && mp->message == WM_KEYDOWN) {
      flag_DevEnv_CtrlTabStarted = VK_CONTROL;
    } 
    return FALSE;
  case VK_CONTROL:
    if (mp->wParam == VK_CONTROL && mp->message == WM_KEYDOWN) {
      return TRUE;
    }
    if (mp->wParam == VK_TAB && mp->message == WM_KEYDOWN) {
      flag_DevEnv_CtrlTabStarted = VK_TAB;
      return TRUE;
    }
    return FALSE;
  case VK_TAB:
    if (mp->wParam == VK_CONTROL && mp->message == WM_KEYUP && isPMRemove) {
      flag_DevEnv_CtrlTabStarted = 0;
    }
    return TRUE;
  }

  return FALSE;
}