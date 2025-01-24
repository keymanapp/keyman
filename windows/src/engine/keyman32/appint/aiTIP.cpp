/*
  Name:             aiTIP
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      19 Jun 2007

  Modified Date:    23 Feb 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          19 Jun 2007 - mcdurdin - I890 - Fix deadkeys in TSF
                    13 Jul 2007 - mcdurdin - I934 - Prep for x64
                    27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    07 Sep 2009 - mcdurdin - I2096 - TSF addin should not use fake 0x88 message for langbar icon updates
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    24 Jun 2010 - mcdurdin - I2436 - Add space to context for AIWin2000Unicode when not matched
                    07 Nov 2012 - mcdurdin - I3549 - V9.0 - x64 version of kmtip addin
                    17 Nov 2012 - mcdurdin - I3572 - V9.0 - Keyman Engine should avoid processing VK_PACKET
                    17 Nov 2012 - mcdurdin - I3573 - V9.0 - Don't recreate the TSF AppIntegration with each keystroke
                    17 Nov 2012 - mcdurdin - I3574 - V9.0 - TIP AppIntegration should inherit from AIWin2000Unicode AppInt in order to support legacy fallback for transitory TSF contexts
                    17 Nov 2012 - mcdurdin - I3575 - V9.0 - context must be cached for legacy mode in TSF as rules are processed twice
                    20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    20 Nov 2012 - mcdurdin - I3583 - V9.0 - Remove AITIP::NotifyKeyboardChange as obsolete
                    28 Nov 2012 - mcdurdin - I3594 - V9.0 - Remove old SelectKeyboard code and related messages
                    28 Nov 2012 - mcdurdin - I3588 - V9.0 - Use preserved keys in TSF to handle key combinations in Keyman
                    28 Nov 2012 - mcdurdin - I3589 - V9.0 - Handle key state for KeyUp events in TSF
                    11 Aug 2013 - mcdurdin - I3775 - V9.0 - Fail I3762 - AltGr combinations are not matched on French base layout
                    11 Nov 2013 - mcdurdin - I3961 - V9.0 - Clean up communication between keyman32 and keyman
                    24 Apr 2014 - mcdurdin - I4196 - V9.0 - wm_kmmoreposting must be refactored for TIP work as it is not sequential
                    02 May 2014 - mcdurdin - I4223 - V9.0 - Moving with cursor keys does not reset context
                    10 Jun 2014 - mcdurdin - I4262 - V9.0 - TSF deadkeys do not function correctly
                    16 Jun 2014 - mcdurdin - I4272 - V9.0 - TIP only outputs first 127 characters of a rule result
                    16 Jun 2014 - mcdurdin - I4266 - V9.0 - Deadkeys only work in first 61 characters of document with TIP
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    26 Jun 2014 - mcdurdin - I4290 - Keys that have rules but are not matched due to context do not generate output
                    03 Aug 2014 - mcdurdin - I4278 - V9.0 - PQR test in dead key test fails in TIP
                    13 Aug 2014 - mcdurdin - I4370 - Deadkeys are still not working in Winword TIP mode
                    13 Aug 2014 - mcdurdin - I4370 - Deadkeys are still not working in Winword TIP mode
                    31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
                    12 Jan 2015 - mcdurdin - I4562 - V9.0 - Forced output of virtual keys no longer works
                    03 Feb 2015 - mcdurdin - I4582 - V9.0 - Most underlying layout code in Keyman32 is now obsolete and needs to be removed
                    04 Feb 2015 - mcdurdin - I4585 - V9.0 - Keyman Engine appears to ignore space in context when no rule for it
                    29 Mar 2015 - mcdurdin - I4642 - V9.0 - In Logos, generated backspace receives a scan of 0x00 instead of 0xFF
                    09 Aug 2015 - mcdurdin - I4793 - Race condition with preserved keys and modifiers
                    09 Aug 2015 - mcdurdin - I4844 - Tidy up PostDummyKeyEvent calls
                    23 Feb 2016 - mcdurdin - I4978 - Keyboard options do not apply correctly because they are set twice accidentally
*/
#include "pch.h"   // I3583   // I4287

extern const int VKContextReset[256];

DWORD TSFShiftToShift(LPARAM shift);   // I3588

extern "C" __declspec(dllexport) BOOL WINAPI TIPIsKeymanRunning() {
  return Globals::get_Keyman_Initialised() && !Globals::get_Keyman_Shutdown();
}

extern "C" __declspec(dllexport) BOOL WINAPI TIPActivateKeyboard(GUID *profile) {   // I3581
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;
  if(profile != NULL) {
    for(int i = 0; i < _td->nKeyboards; i++) {
      for(int j = 0; j < _td->lpKeyboards[i].nProfiles; j++) {
        if(IsEqualGUID(_td->lpKeyboards[i].Profiles[j].Guid, *profile)) {
          SelectKeyboard(_td->lpKeyboards[i].KeymanID);   // I3594
          ReportActiveKeyboard(PC_UPDATE);   // I3961
          return TRUE;
        }
      }
    }
  }

  SendDebugMessage("Could not find profile");
  SelectKeyboard(KEYMANID_NONKEYMAN);   // I3594
  ReportActiveKeyboard(PC_UPDATE);   // I3961
  return FALSE;
}

extern "C" __declspec(dllexport) BOOL WINAPI TIPActivateEx(BOOL FActivate) {  // I3581
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;
  if(!FActivate)
    SelectKeyboard(KEYMANID_NONKEYMAN);   // I3594
  Globals::PostMasterController(wm_keyman_control, KMC_SETFOCUSINFO, 0);   // I3961
	return TRUE;
}


void ProcessToggleChange(UINT key) {   // I4793
  UINT flag = 0;

  switch(key) {
    case VK_CAPITAL: flag = CAPITALFLAG; break;
    case VK_NUMLOCK: flag = NUMLOCKFLAG; break;
    default: return;
  }

  if (GetKeyState(key) & 1) {
    SendDebugMessageFormat("Setting %s", flag == CAPITALFLAG ? "CAPITALFLAG" : "NUMLOCKFLAG");
    *Globals::ShiftState() |= flag;
  }
  else {
    SendDebugMessageFormat("Clearing %s", flag == CAPITALFLAG ? "CAPITALFLAG" : "NUMLOCKFLAG");
    *Globals::ShiftState() &= ~flag;
  }
}

BOOL TIPProcessKeyInternal(
  PKEYMAN64THREADDATA _td,
  WPARAM wParam,
  LPARAM lParam,
  BOOL Updateable,
  BOOL Preserved) {
  if(!_td) {
    return FALSE;
  }

  WORD keyFlags = HIWORD(lParam);
  BOOL isUp = keyFlags & KF_UP ? TRUE : FALSE;
  BOOL extended = keyFlags & KF_EXTENDED ? TRUE : FALSE;
  BYTE scan = keyFlags & 0xFF;

  SendDebugEntry();
  SendDebugMessageFormat("VirtualKey=%s lParam=%x   IsUp=%d Extended=%d Updateable=%d Preserved=%d",
    Debug_VirtualKey((WORD) wParam), lParam, isUp, extended, Updateable, Preserved);

  if(_td->LastKey == wParam && scan == 0) {   // I4642
    scan = _td->LastScanCode;
    SendDebugMessageFormat("Scan code was zero so using cached scan code %x", scan);
  }

  if(scan == SCAN_FLAG_KEYMAN_KEY_EVENT) {   // I4370
    if (wParam == VK_CAPITAL && !isUp) {
      // Must also record toggle state change when Keyman has generated
      // a Caps Lock event
      ProcessToggleChange((UINT)wParam);   // I4793
    }
    SendDebugMessageFormat("Virtual Key was generated by Keyman [Scan=0xFF]");
    return_SendDebugExit(FALSE);
  }

	if(!_td->lpActiveKeyboard) {
		SendDebugMessageFormat("Fail -- lpActiveKeyboard = NULL");
	  return_SendDebugExit(FALSE);
	}

  if(!_td->app) {
    _td->app = new AITIP;
	  if(!_td->app)	{
		  SendDebugMessageFormat("Fail -- could not allocate app");
		  return_SendDebugExit(FALSE);
	  }
  }

  DWORD LocalShiftState = Globals::get_ShiftState();
  // Only the modifer flag 'f_ShiftState' is changed before sending the key stroke to the
  // core processor. The core processor has the keyboard Caps Lock stores and will
  // queue an action 'KM_CORE_IT_CAPSLOCK'. In processing the action the Windows engine will synthesise keystrokes
  // to ensure caps lock is in the correct state.
  if (!Preserved) {
    switch (wParam) {
    case VK_MENU:
    case VK_CONTROL:
      ProcessModifierChange((UINT)wParam, isUp, extended);
      return_SendDebugExit(FALSE);
    case VK_NUMLOCK:
      if (!isUp)
        ProcessToggleChange((UINT)wParam);  // I4793
      return_SendDebugExit(FALSE);
    case VK_CAPITAL:
      if (!isUp)
        ProcessToggleChange((UINT)wParam);  // I4793
      break;
    case VK_SHIFT:
      ProcessModifierChange((UINT)wParam, isUp, extended);
      break;
    }
  } else {
    // Mask out Ctrl, Shift and Alt and include new modifiers   // I4548
    DWORD NewShiftState = TSFShiftToShift(lParam);  // I3588
    SendDebugMessageFormat(
        "TSFShiftToShift start with %x, include %x", LocalShiftState, NewShiftState);
    *Globals::ShiftState() = (LocalShiftState & K_NOTMODIFIERFLAG) | NewShiftState;  // I3588
  }

	_td->TIPFUpdateable = Updateable;
  _td->TIPFPreserved = Preserved;   // I4290

	_td->state.vkey = (WORD) wParam;
  _td->state.isDown = !isUp;

  _td->state.lpCoreKb = _td->lpActiveKeyboard->lpCoreKeyboard;

  _td->state.windowunicode = TRUE;

  _td->state.charCode = CharFromVK(&_td->state.vkey, Globals::get_ShiftState());   // I4582

  BOOL res = ProcessHook();

	_td->TIPProcessOutput = NULL;
	_td->TIPGetContext = NULL;
  _td->TIPGetContextEx = NULL;

	SendDebugMessageFormat("Success, res=%d", res);

	if(!res && Updateable) {   // I4562
    //if(wParam <= 0xFF && VKContextReset[wParam]) _td->app->ResetContext();  // I3438   // I4223
		SendDebugMessageFormat("key not matched: %s", Debug_VirtualKey((WORD) wParam));
  }

  if(Preserved) {
    // Restore shift state
    // TODO: Is this necessary? See keybd_shift_reset() instead and I5394. Some more
    // formal mapping of the modifier state machine would be helpful
    SendDebugMessageFormat("restoring shift state from %x to %x", Globals::get_ShiftState(), LocalShiftState);
    *Globals::ShiftState() = LocalShiftState;
  }

	return_SendDebugExit(res);

}

extern "C" __declspec(dllexport) BOOL WINAPI TIPProcessKey(
  WPARAM wParam,
  LPARAM lParam,   // I3589   // I3588
	PKEYMANPROCESSOUTPUTFUNC outfunc,
  PKEYMANGETCONTEXTFUNC ctfunc,
  BOOL Updateable,
  BOOL Preserved) {

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) {
    return FALSE;
  }

  _td->TIPProcessOutput = outfunc;
	_td->TIPGetContext = ctfunc;
  BOOL res = TIPProcessKeyInternal(_td, wParam, lParam, Updateable, Preserved);
  if (res == FALSE) {
    _td->TIPProcessOutput = NULL;
	  _td->TIPGetContext = NULL;
  }
  return res;

}

// This includes the PKEYMANGETCONTEXTISSELECTEDFUNC to report if text is selected
extern "C" __declspec(dllexport) BOOL WINAPI TIPProcessKeyEx(
  WPARAM wParam,
  LPARAM lParam,   // I3589   // I3588
	PKEYMANPROCESSOUTPUTFUNC outfunc,
  PKEYMANGETCONTEXTISSELECTEDFUNC ctfunc,
  BOOL Updateable,
  BOOL Preserved) {

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) {
    return FALSE;
  }

  _td->TIPProcessOutput = outfunc;
	_td->TIPGetContextEx = ctfunc;
  BOOL res = TIPProcessKeyInternal(_td, wParam, lParam, Updateable, Preserved);
  if (res == FALSE) {
    _td->TIPProcessOutput = NULL;
	  _td->TIPGetContextEx = NULL;
  }
  return res;

}

/* AI constructor and destructor */

AITIP::AITIP() {
  ::AIWin2000Unicode();   // I3574

  useLegacy = FALSE;
}

AITIP::~AITIP() {
}

/* Information functions */

BOOL AITIP::CanHandleWindow(HWND ahwnd) {
  UNREFERENCED_PARAMETER(ahwnd);
  return TRUE;   // I3574
}

BOOL AITIP::IsUnicode() {
	return TRUE;
}

//#define DEBUG_MERGECONTEXT

#ifdef DEBUG_MERGECONTEXT
char *debugstr(PWSTR buf) {
	WCHAR *p;
	char *bufout = new char[30*7];
	char *q;
	for(p = buf, q = bufout; *p && (p-buf < 30); p++)
	{
		wsprintf(q, "U+%4.4X ", *p); q = strchr(q, 0);
	}
	//WideCharToMultiByte(CP_ACP, 0, buf, -1, bufout, 128, NULL, NULL);
	return bufout;
}
#endif

/* Context functions */

BOOL AITIP::ReadContext(PWSTR buf) {
  if (buf == nullptr) {
    return FALSE;
  }

  PKEYMAN64THREADDATA _td = ThreadGlobals();

  if(!_td) {
    return FALSE;
  }

  if (_td->TIPGetContextEx && (*_td->TIPGetContextEx)(MAXCONTEXT - 1, buf, &isTextSelected) == S_OK) {  // I3575   // I4262
    if(ShouldDebug()) {
      SendDebugMessageFormat("full context [Updateable=%d] %s", _td->TIPFUpdateable, Debug_UnicodeString(buf));
    }
    useLegacy = FALSE;   // I3575
    return TRUE;
  } else if (_td->TIPGetContext && (*_td->TIPGetContext)(MAXCONTEXT - 1, buf) == S_OK) {  // I3575   // I4262
    if(ShouldDebug()) {
      SendDebugMessageFormat("full context [Updateable=%d] %s", _td->TIPFUpdateable, Debug_UnicodeString(buf));
    }
    useLegacy = FALSE;   // I3575
    return TRUE;
  }	else {
    SendDebugMessageFormat("transitory context, so use buffered context [Updateable=%d]", _td->TIPFUpdateable);
    useLegacy = TRUE;   // I3575
    return FALSE;
  }
}


/* Output actions */

BOOL AITIP::SendActions() {  // I4196
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

  if(!_td->TIPFUpdateable) {  // I3575
    QueueSize = 0;
    return TRUE;
  }

  if(useLegacy) {  // I3575
    return AIWin2000Unicode::SendActions();   // I3575   // I4196
  }

	if(*Globals::hwndIM()) {
    PostMessage(*Globals::hwndIM(), wm_keymanim_contextchanged, 0, 0);
  }

	return PostKeys();
}

BOOL AITIP::PostKeys() {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) {
    return FALSE;
  }

	if(QueueSize == 0) {
		return TRUE;
	}

  int bk=0;
	WCHAR *OutBuf = new WCHAR[QueueSize+1], *p = OutBuf;   // I4272

  *OutBuf = 0;

	for(int n = 0; n < QueueSize; n++) {
		switch(Queue[n].ItemType) {
		case QIT_CHAR:
			*p++ = (WCHAR) Queue[n].dwData;
			*p = 0;
			break;
		case QIT_BELL:
			MessageBeep(MB_ICONASTERISK);
			break;
		case QIT_BACK:
			if(Queue[n].dwData & BK_DEADKEY) break;
			if(p > OutBuf) {
				*(--p) = 0;
			} else {
			  bk++;
      }
      if (Queue[n].dwData & BK_SURROGATE) {
        if (p > OutBuf) {
          *(--p) = 0;
        }
        else {
          bk++;
        }
      }
      break;
		}
	}

	if(_td->TIPProcessOutput && (bk > 0 || OutBuf[0])) {
  	SendDebugMessageFormat("output bk=%d outcount=%d", bk, (INT_PTR)(p-OutBuf));
		(*_td->TIPProcessOutput)(bk, OutBuf, (int)(INT_PTR)(p - OutBuf));
  } else {
    SendDebugMessageFormat("no output");
  }

  delete[] OutBuf;   // I4272

	QueueSize = 0;
	return TRUE;
}

/* Utility functions */

DWORD TSFShiftToShift(LPARAM shift)   // I3588
{
  DWORD res = 0;
  if(shift & TF_MOD_ALT) res |= LALTFLAG;
  if(shift & TF_MOD_CONTROL) res |= LCTRLFLAG;
  if(shift & TF_MOD_SHIFT) res |= K_SHIFTFLAG;
  if(shift & TF_MOD_LALT) res |= LALTFLAG;
  if(shift & TF_MOD_RALT) res |= RALTFLAG;
  if((shift & (TF_MOD_RALT|TF_MOD_LCONTROL)) == (TF_MOD_RALT|TF_MOD_LCONTROL)) res |= RALTFLAG;   // I3775
  else if(shift & TF_MOD_LCONTROL) res |= LCTRLFLAG;
  if(shift & TF_MOD_RCONTROL) res |= RCTRLFLAG;
  return res;
}
