/*
  Name:             AIWin2000Unicode
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      22 Jan 2007

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          22 Jan 2007 - mcdurdin - Fix for K_NPENTER
                    13 Jul 2007 - mcdurdin - I934 - Prep fox x64
                    23 Aug 2007 - mcdurdin - I719 - Fix Alt+LeftShift and Word interactions
                    14 Jun 2008 - mcdurdin - I1389 - Supp chars on Vista+ default to single BKSP
                    20 Jul 2008 - mcdurdin - I1546 - Language switch not working with some keyboard IDs
                    16 Jan 2009 - mcdurdin - I1512 - SendInput support
                    27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    15 Jun 2010 - mcdurdin - I2426 - Remove incorrectl KEYMAN_CHARFLAG
                    24 Jun 2010 - mcdurdin - I2436 - Add space to context for AIWin2000Unicode when not matched
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys Created
                    18 Feb 2012 - mcdurdin - I3242 - Arrow keys do not function correctly in Internet Explorer
                    26 Feb 2012 - mcdurdin - I2787 - Reset keyboard state for a key when key down is sent to avoid confusion for Chrome
                    04 Nov 2012 - mcdurdin - I3527 - V9.0 - Merge of I3242 - Arrow keys do not function correctly in Internet Explorer
                    04 Nov 2012 - mcdurdin - I3528 - V9.0 - Merge of I2787 - Reset keyboard state for a key when key down is sent to avoid confusion for Chrome
                    17 Dec 2013 - mcdurdin - I4006 - V9.0 - Remove old aiDefault code
                    24 Apr 2014 - mcdurdin - I4196 - V9.0 - wm_kmmoreposting must be refactored for TIP work as it is not sequential
                    01 May 2014 - mcdurdin - I4128 - V9.0 - Shift states still not working with unprocessed keys in V9
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    13 Aug 2014 - mcdurdin - I4370 - Deadkeys are still not working in Winword TIP mode
                    14 Aug 2014 - mcdurdin - I4378 - V9.0 - Rapid typing in legacy mode breaks Keyman input
                    13 Oct 2014 - mcdurdin - I4452 - V9.0 - Chinese keyboard is not working correctly
                    31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
                    09 Aug 2015 - mcdurdin - I4844 - Tidy up PostDummyKeyEvent calls
*/
#include "pch.h"   // I4128   // I4287
#include "serialkeyeventclient.h"

AIWin2000Unicode::AIWin2000Unicode() {
}
AIWin2000Unicode::~AIWin2000Unicode(){}

/* Information functions */


BOOL AIWin2000Unicode::CanHandleWindow(HWND ahwnd)
{
  UNREFERENCED_PARAMETER(ahwnd);
	return TRUE;
}

BOOL AIWin2000Unicode::HandleWindow(HWND ahwnd)
{
	if(hwnd != ahwnd)
	{
		hwnd = ahwnd;
    ResetContext();
	}
	return TRUE;
}

BOOL AIWin2000Unicode::IsWindowHandled(HWND ahwnd)
{
	return (hwnd == ahwnd);
}

BOOL AIWin2000Unicode::IsUnicode()
{
  BOOL Result = IsWindowUnicode(hwnd);
  SendDebugMessageFormat("IsWindowUnicode=%s", Result ? "Yes" : "No");
	return Result;
}

/* Context functions */

BOOL AIWin2000Unicode::ReadContext(PWSTR buf) {
  UNREFERENCED_PARAMETER(buf);
  // We cannot read any context from legacy apps, so we return a
  // failure here -- telling Core to maintain its own cached
  // context.
  return FALSE;
}

void AIWin2000Unicode::ResetContext()
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (_td && _td->lpActiveKeyboard && _td->lpActiveKeyboard->lpCoreKeyboardState) {
    km_core_state_context_clear(_td->lpActiveKeyboard->lpCoreKeyboardState);
  }
}

BYTE SavedKbdState[256];

BOOL AIWin2000Unicode::SendActions()   // I4196
{
	if(*Globals::hwndIM()) {
    PostMessage(*Globals::hwndIM(), wm_keymanim_contextchanged, 0, 0);
  }
	return PostKeys();
}

// I1512 - SendInput with VK_PACKET for greater robustness

BOOL AIWin2000Unicode::PostKeys()
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) {
    return FALSE;
  }

  if(QueueSize == 0)
	{
		return TRUE;
	}

  if (_td->CustomPostKeyCallback != nullptr) {
    BOOL res = _td->CustomPostKeyCallback(Queue, QueueSize);
    QueueSize = 0;
    return res;
  }

  int n = 0;
	/* 6.0.153.0: Fix repeat state for virtual keys */

  LPINPUT pInputs = new INPUT[QueueSize*100]; // TODO: Tidy this up. Horrid doing a junky alloc like this each event
  int i = 0;

  for(; n < QueueSize; n++)
  {
	  switch(Queue[n].ItemType) {
	  case QIT_VKEYDOWN:
		  if((Queue[n].dwData & QVK_KEYMASK) == 0x05) Queue[n].dwData = (Queue[n].dwData & QVK_FLAGMASK) | VK_RETURN; // I649  // I3438

		  /* 6.0.153.0: Fix repeat state for virtual keys */

      if((Queue[n].dwData & QVK_KEYMASK) <= VK__MAX)  // I3438
      {
        pInputs[i].type = INPUT_KEYBOARD;
      pInputs[i].ki.wVk = (WORD)Queue[n].dwData & 0xFF;  // I3242 - noted as unclean but was not cause of the issue  // I3438   // I3527

        pInputs[i].ki.wScan = SCAN_FLAG_KEYMAN_KEY_EVENT;
        pInputs[i].ki.dwFlags = ((Queue[n].dwData & QVK_EXTENDED) ? KEYEVENTF_EXTENDEDKEY : 0);  // I3438

        pInputs[i].ki.time = 0;
        pInputs[i++].ki.dwExtraInfo = 0; //KEYEVENT_EXTRAINFO_KEYMAN;   // I4370   // I4378
      }

      //// I2787 - Reset keyboard state so Chrome doesn't get confused with VK_RETURN   // I3528
      {
        BYTE keystate[256];
        GetKeyboardState(keystate);
        if(keystate[Queue[n].dwData & 0xFF] & 0x80)
        {
          keystate[Queue[n].dwData & 0xFF] = 0;
          SetKeyboardState(keystate);
        }
      }
      //// I2787 - END CHROME TEST   // I3528

		  break;
	  case QIT_VKEYUP:
		  if((Queue[n].dwData & QVK_KEYMASK) == 0x05) Queue[n].dwData = (Queue[n].dwData & QVK_FLAGMASK) | VK_RETURN; // I649  // I3438

      if((Queue[n].dwData & QVK_KEYMASK) <= VK__MAX)  // I3438
      {
        pInputs[i].type = INPUT_KEYBOARD;
        pInputs[i].ki.wVk = (WORD) Queue[n].dwData & 0xFF;  // I3242 - noted as unclean but was not cause of the issue  // I3438   // I3527
        pInputs[i].ki.wScan = SCAN_FLAG_KEYMAN_KEY_EVENT;
        pInputs[i].ki.dwFlags = KEYEVENTF_KEYUP | ((Queue[n].dwData & QVK_EXTENDED) ? KEYEVENTF_EXTENDEDKEY : 0);  // I3438
        pInputs[i].ki.time = 0;
        pInputs[i++].ki.dwExtraInfo = 0; //KEYEVENT_EXTRAINFO_KEYMAN;   // I4370   // I4378
      }

		  break;
	  case QIT_CHAR:
      pInputs[i].type = INPUT_KEYBOARD;
      pInputs[i].ki.wVk = 0;
      pInputs[i].ki.wScan = (WORD) Queue[n].dwData;
      pInputs[i].ki.dwFlags = KEYEVENTF_UNICODE;
      pInputs[i].ki.time = 0;
      pInputs[i++].ki.dwExtraInfo = 0; //KEYEVENT_EXTRAINFO_KEYMAN;   // I4370   // I4378

      pInputs[i].type = INPUT_KEYBOARD;
      pInputs[i].ki.wVk = 0;
      pInputs[i].ki.wScan = (WORD) Queue[n].dwData;
      pInputs[i].ki.dwFlags = KEYEVENTF_KEYUP | KEYEVENTF_UNICODE;
      pInputs[i].ki.time = 0;
      pInputs[i++].ki.dwExtraInfo = 0; //KEYEVENT_EXTRAINFO_KEYMAN;   // I4370   // I4378

      break;
	  case QIT_DEADKEY:
		  break;
	  case QIT_BELL:
		  MessageBeep(MB_ICONASTERISK); //(UINT) -1);
		  break;
	  case QIT_BACK:
		  if(Queue[n].dwData & BK_DEADKEY) break;

      pInputs[i].type = INPUT_KEYBOARD;
      pInputs[i].ki.wVk = VK_BACK;
      pInputs[i].ki.wScan = SCAN_FLAG_KEYMAN_KEY_EVENT;
      pInputs[i].ki.dwFlags = ((Queue[n].dwData & 0x0100) ? KEYEVENTF_EXTENDEDKEY : 0);
      pInputs[i].ki.time = 0;
      pInputs[i++].ki.dwExtraInfo = 0; //KEYEVENT_EXTRAINFO_KEYMAN;   // I4370   // I4378

      pInputs[i].type = INPUT_KEYBOARD;
      pInputs[i].ki.wVk = VK_BACK;
      pInputs[i].ki.wScan = SCAN_FLAG_KEYMAN_KEY_EVENT;
      pInputs[i].ki.dwFlags = KEYEVENTF_KEYUP | ((Queue[n].dwData & 0x0100) ? KEYEVENTF_EXTENDEDKEY : 0);
      pInputs[i].ki.time = 0;
      pInputs[i++].ki.dwExtraInfo = 0; //KEYEVENT_EXTRAINFO_KEYMAN;   // I4370   // I4378

      break;
	  }
  }

	QueueSize = 0;

  if(ShouldDebug()) {
    for(int j = 0; j < i; j++ ) {   // I4548
      SendDebugMessageFormat("sending input [i=%d, input[%d]=vk:%s scan:%x flags:%x", i, j, Debug_VirtualKey(pInputs[j].ki.wVk), pInputs[j].ki.wScan, pInputs[j].ki.dwFlags);
    }
  }

  SetLastError(0);

  if(i > 0) {   // I4452
    _td->pSerialKeyEventClient->SignalServer(pInputs, i);
  }

  SendDebugMessageFormat("sending input finished");

  delete[] pInputs;

  return TRUE;
}
