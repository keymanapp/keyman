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

  SendDebugMessage(0, sdmAIDefault, 0, "TIPActivateKeyboard: Could not find profile");
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


/*
  Update the Keyman toggle state based on the key event
*/
void ProcessToggleChange(UINT key) {   // I4793
  UINT flag = 0;

  switch(key) {
    case VK_CAPITAL: flag = CAPITALFLAG; break;
    case VK_NUMLOCK: flag = NUMLOCKFLAG; break;
    default: return;
  }

  if (GetKeyState(key) & 1) {
    SendDebugMessageFormat(0, sdmAIDefault, 0, "ProcessToggleChange: Setting %s", flag == CAPITALFLAG ? "CAPITALFLAG" : "NUMLOCKFLAG");
    *Globals::ShiftState() |= flag;
  }
  else {
    SendDebugMessageFormat(0, sdmAIDefault, 0, "ProcessToggleChange: Clearing %s", flag == CAPITALFLAG ? "CAPITALFLAG" : "NUMLOCKFLAG");
    *Globals::ShiftState() &= ~flag;
  }
}

extern "C" __declspec(dllexport) BOOL WINAPI TIPProcessKey(WPARAM wParam, LPARAM lParam,   // I3589   // I3588
	PKEYMANPROCESSOUTPUTFUNC outfunc, PKEYMANGETCONTEXTFUNC ctfunc, BOOL Updateable, BOOL Preserved) {

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) {
    return FALSE;
  }

  WORD keyFlags = HIWORD(lParam);
  BOOL isUp = keyFlags & KF_UP ? TRUE : FALSE;
  BOOL extended = keyFlags & KF_EXTENDED ? TRUE : FALSE;
  BYTE scan = keyFlags & 0xFF;

  SendDebugMessageFormat(0, sdmAIDefault, 0, "TIPProcessKey: Enter VirtualKey=%s lParam=%x   IsUp=%d Extended=%d Updateable=%d Preserved=%d", Debug_VirtualKey((WORD) wParam), lParam, isUp, extended, Updateable, Preserved);

  if(_td->LastKey == wParam && scan == 0) {   // I4642
    scan = _td->LastScanCode;
    SendDebugMessageFormat(0, sdmAIDefault, 0, "TIPProcessKey: Scan code was zero so using cached scan code %x", scan);
  }

  if(scan == SCAN_FLAG_KEYMAN_KEY_EVENT) {   // I4370
    if (wParam == VK_CAPITAL && !isUp) {
      // Must also record toggle state change when Keyman has generated
      // a Caps Lock event
      ProcessToggleChange((UINT)wParam);   // I4793
    }
    SendDebugMessageFormat(0, sdmAIDefault, 0, "TIPProcessKey: Virtual Key was generated by Keyman [Scan=0xFF]");
    return FALSE;
  }

	if(!_td->lpActiveKeyboard) {
		SendDebugMessageFormat(0, sdmAIDefault, 0, "TIPProcessKey: Fail -- lpActiveKeyboard = NULL");
	  return FALSE;
	}

  if(!_td->app) {
    _td->app = new AITIP;
	  if(!_td->app)	{
		  SendDebugMessageFormat(0, sdmAIDefault, 0, "TIPProcessKey: Fail -- could not allocate app");
		  return FALSE;
	  }
  }

  DWORD LocalShiftState = Globals::get_ShiftState();

  if(!Preserved) {
    switch(wParam) {
    case VK_CAPITAL:
      if(!isUp) ProcessToggleChange((UINT) wParam);   // I4793
      if (!Updateable) {
        // We only want to process the Caps Lock key event once --
        // in the first pass (!Updateable).
        KeyCapsLockPress(isUp);   // I4548
      }
      return FALSE;
    case VK_SHIFT:
      if (!Updateable) {
        // We only want to process the Shift key event once --
        // in the first pass (!Updateable).
        KeyShiftPress(isUp);   // I4548
      }
      // Fall through
    case VK_MENU:
    case VK_CONTROL:
      ProcessModifierChange((UINT) wParam, isUp, extended);
      return FALSE;

    case VK_NUMLOCK:
      if(!isUp) ProcessToggleChange((UINT) wParam);   // I4793
      return FALSE;
    }
    if(isUp) {
      return FALSE; // return value ignored in this case; we only needed it for testing anyway
    }
  } else {
    // Mask out Ctrl, Shift and Alt and include new modifiers   // I4548
    DWORD NewShiftState = TSFShiftToShift(lParam);   // I3588
    SendDebugMessageFormat(0, sdmGlobal, 0, "TIPProcessKey: TSFShiftToShift start with %x, include %x", LocalShiftState, NewShiftState);
    *Globals::ShiftState() = (LocalShiftState & K_NOTMODIFIERFLAG) | NewShiftState;   // I3588
  }

	_td->TIPFUpdateable = Updateable;
  _td->TIPFPreserved = Preserved;   // I4290

	_td->state.msg.hwnd = GetFocus();
	_td->state.msg.lParam = 0;
	_td->state.msg.message = wm_keymankeydown;
	_td->state.vkey = (WORD) wParam;
  /// TODO: 5011 set the modifier _td->state.modifer
   /// TODO: 5011 set the correct active keyboard.
	_td->state.lpkb = _td->lpActiveKeyboard->Keyboard;

  _td->state.windowunicode = TRUE;
	_td->state.startgroup = &_td->state.lpkb->dpGroupArray[_td->state.lpkb->StartGroup[BEGIN_UNICODE]];

	_td->state.NoMatches = TRUE;
	_td->state.LoopTimes = 0;
	_td->state.StopOutput = FALSE;

  _td->state.charCode = CharFromVK(&_td->state.vkey, Globals::get_ShiftState());   // I4582

	_td->TIPProcessOutput = outfunc;
	_td->TIPGetContext = ctfunc;

  AppContextWithStores *savedContext = NULL;   // I4370   // I4978
  if(!Updateable) {   // I4370
    savedContext = new AppContextWithStores(_td->lpActiveKeyboard->Keyboard->cxStoreArray);   // I4978
    _td->app->SaveContext(savedContext);
  }

	BOOL res = ProcessHook();

  if(!Updateable) {   // I4370
    if(res) {   // I4585
      // Reset the context if match found
      _td->app->RestoreContext(savedContext);
    }
    delete savedContext;
    savedContext = NULL;
  }

	_td->TIPProcessOutput = NULL;
	_td->TIPGetContext = NULL;

	SendDebugMessageFormat(0, sdmAIDefault, 0, "TIPProcessKey: Success, res=%d", res);

	if(!res && Updateable) {   // I4562
    //if(wParam <= 0xFF && VKContextReset[wParam]) _td->app->ResetContext();  // I3438   // I4223
		SendDebugMessageFormat(0, sdmAIDefault, 0, "TIPProcessKey: key not matched: %s", Debug_VirtualKey((WORD) wParam));
  }

  if(Preserved) {
    // Restore shift state
    // TODO: Is this necessary? See keybd_shift_reset() instead and I5394. Some more
    // formal mapping of the modifier state machine would be helpful
    SendDebugMessageFormat(0, sdmAIDefault, 0, "TIPProcessKey: restoring shift state from %x to %x", Globals::get_ShiftState(), LocalShiftState);
    *Globals::ShiftState() = LocalShiftState;
  }

	return res;
}

/* AI constructor and destructor */

AITIP::AITIP() {
  ::AIWin2000Unicode();   // I3574

  FIsDebugControlWindow = FALSE;
  useLegacy = FALSE;

	WM_KEYMANDEBUG_CANDEBUG         = RegisterWindowMessage("WM_KEYMANDEBUG_CANDEBUG");
	WM_KEYMANDEBUG_GETUNICODESTATUS = RegisterWindowMessage("WM_KEYMANDEBUG_GETUNICODESTATUS");
	WM_KEYMANDEBUG_GETCONTEXT       = RegisterWindowMessage("WM_KEYMANDEBUG_GETCONTEXT");
	WM_KEYMANDEBUG_ACTION           = RegisterWindowMessage("WM_KEYMANDEBUG_ACTION");
	WM_KEYMANDEBUG_RULEMATCH        = RegisterWindowMessage("WM_KEYMANDEBUG_RULEMATCH");
}

AITIP::~AITIP() {
}

/* Information functions */

BOOL AITIP::CanHandleWindow(HWND ahwnd) {
  UNREFERENCED_PARAMETER(ahwnd);
  return TRUE;   // I3574
}

BOOL AITIP::HandleWindow(HWND ahwnd) {
  FIsDebugControlWindow = IsDebugControlWindow(ahwnd);
  return AIWin2000Unicode::HandleWindow(ahwnd);   // I3574
}

BOOL AITIP::IsWindowHandled(HWND ahwnd) {
  return AIWin2000Unicode::IsWindowHandled(ahwnd);   // I3574
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

void AITIP::MergeContextWithCache(PWSTR buf, AppContext *local_context) {   // I4262
  WCHAR tmpbuf[MAXCONTEXT], contextExDeadkeys[MAXCONTEXT];
  local_context->Get(tmpbuf, MAXCONTEXT-1);

  int n = 0;
  PWSTR p = tmpbuf, q = contextExDeadkeys, r = buf;   // I4266
  while(*p) {
    if(*p == UC_SENTINEL) {
      p += 2; // We know the only UC_SENTINEL CODE in the context is CODE_DEADKEY, which has only 1 parameter: UC_SENTINEL CODE_DEADKEY <deadkey_id>
      n++;
    } else {
      *q++ = *p;
    }
    p++;
  }
  *q = 0;

  if(n > 0 && wcslen(buf) > wcslen(contextExDeadkeys)) {   // I4266
    r += wcslen(buf) - wcslen(contextExDeadkeys);
  }

  // We have to cut off the context comparison from the left by #deadkeys matched to ensure we are comparing like with like,
  // at least when tmpbuf len=MAXCONTEXT-1 at entry.

#ifdef DEBUG_MERGECONTEXT
  char *mc1 = debugstr(buf), *mc2 = debugstr(contextExDeadkeys), *mc3 = debugstr(tmpbuf);

  SendDebugMessageFormat(0, sdmAIDefault, 0, "AITIP::MergeContextWithCache TIP:'%s' Context:'%s' DKContext:'%s'",
      mc1, mc2, mc3);

  delete mc1;
  delete mc2;
  delete mc3;
#endif

  if(wcscmp(r, contextExDeadkeys) != 0) {
    // context has changed, reset context
#ifdef DEBUG_MERGECONTEXT
    SendDebugMessageFormat(0, sdmAIDefault, 0, "AITIP::MergeContextWithCache --> load context from app (losing deadkeys)");
#endif
    local_context->Set(buf);
  } else {
#ifdef DEBUG_MERGECONTEXT
    SendDebugMessageFormat(0, sdmAIDefault, 0, "AITIP::MergeContextWithCache --> loading cached context");
#endif
    wcscpy_s(buf, MAXCONTEXT, tmpbuf);
  }
}

void AITIP::ReadContext() {
  if(DebugControlled()) {
    Debug_FillContextBuffer();
    return;
  }

	WCHAR buf[MAXCONTEXT];
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

	if(_td->TIPGetContext && (*_td->TIPGetContext)(MAXCONTEXT-1, buf) == S_OK) {   // I3575   // I4262
    if(ShouldDebug(sdmKeyboard)) {
      SendDebugMessageFormat(0, sdmAIDefault, 0, "AITIP::ReadContext: full context [Updateable=%d] %s", _td->TIPFUpdateable, Debug_UnicodeString(buf));
    }
    useLegacy = FALSE;   // I3575

    // If the text content of the context is identical, inject the deadkeys
    // Otherwise, reset the cachedContext to match buf, no deadkeys

    MergeContextWithCache(buf, context);

    if(ShouldDebug(sdmKeyboard)) {
      SendDebugMessageFormat(0, sdmAIDefault, 0, "AITIP::ReadContext: after merge [Updateable=%d] %s", _td->TIPFUpdateable, Debug_UnicodeString(buf));
    }

    context->Set(buf);
  }	else {
    SendDebugMessageFormat(0, sdmAIDefault, 0, "AITIP::ReadContext: transitory context, so use buffered context [Updateable=%d]", _td->TIPFUpdateable);
    useLegacy = TRUE;   // I3575
  }
}

AppContextWithStores::AppContextWithStores(int nKeyboardOptions) : AppContext() {   // I4978
  this->nKeyboardOptions = nKeyboardOptions;
  KeyboardOptions = new INTKEYBOARDOPTIONS[nKeyboardOptions];
  memset(KeyboardOptions, 0, sizeof(INTKEYBOARDOPTIONS) * nKeyboardOptions);
}

AppContextWithStores::~AppContextWithStores() {   // I4978
  for(DWORD i = 0; i < nKeyboardOptions; i++) {
    if(KeyboardOptions[i].Value) delete KeyboardOptions[i].Value;
  }
  delete KeyboardOptions;
}

void AITIP::SaveContext(AppContextWithStores *savedContext) {   // I4370   // I4978
  savedContext->CopyFrom(context);

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td || !_td->lpActiveKeyboard || !_td->lpActiveKeyboard->Keyboard) return;

  assert(savedContext->nKeyboardOptions == _td->lpActiveKeyboard->Keyboard->cxStoreArray);

  for(DWORD i = 0; i < savedContext->nKeyboardOptions; i++) {   // I4978
    if(_td->lpActiveKeyboard->KeyboardOptions[i].Value != NULL) {
      savedContext->KeyboardOptions[i].Value = new WCHAR[wcslen(_td->lpActiveKeyboard->KeyboardOptions[i].Value)+1];
      wcscpy_s(savedContext->KeyboardOptions[i].Value, wcslen(_td->lpActiveKeyboard->KeyboardOptions[i].Value)+1, _td->lpActiveKeyboard->KeyboardOptions[i].Value);
    }
  }
}

void AITIP::RestoreContext(AppContextWithStores *savedContext) {   // I4370   // I4978
  context->CopyFrom(savedContext);

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td || !_td->lpActiveKeyboard || !_td->lpActiveKeyboard->Keyboard) return;
  LPINTKEYBOARDINFO kp = _td->lpActiveKeyboard;

  assert(savedContext->nKeyboardOptions == kp->Keyboard->cxStoreArray);

  for(DWORD i = 0; i < savedContext->nKeyboardOptions; i++) {   // I4978
    if(kp->KeyboardOptions[i].Value == NULL && savedContext->KeyboardOptions[i].Value != NULL) {
      // Restore the previously saved value as it was reset
      kp->KeyboardOptions[i].OriginalStore = kp->Keyboard->dpStoreArray[i].dpString;
      kp->Keyboard->dpStoreArray[i].dpString = kp->KeyboardOptions[i].Value = savedContext->KeyboardOptions[i].Value;
      savedContext->KeyboardOptions[i].Value = NULL;
    } else if(kp->KeyboardOptions[i].Value != NULL && savedContext->KeyboardOptions[i].Value == NULL) {
      // Clear the newly saved value back to the default
      delete kp->KeyboardOptions[i].Value;
      kp->KeyboardOptions[i].Value = NULL;
      kp->Keyboard->dpStoreArray[i].dpString = kp->KeyboardOptions[i].OriginalStore;
    } else if(kp->KeyboardOptions[i].Value != NULL && savedContext->KeyboardOptions[i].Value != NULL &&
        wcscmp(kp->KeyboardOptions[i].Value, savedContext->KeyboardOptions[i].Value) != 0) {
      // Restore the previously saved value as it was changed
      delete kp->KeyboardOptions[i].Value;
      kp->Keyboard->dpStoreArray[i].dpString = kp->KeyboardOptions[i].Value = savedContext->KeyboardOptions[i].Value;
      savedContext->KeyboardOptions[i].Value = NULL;
    }
  }
}

/* Output actions */

BOOL AITIP::QueueAction(int ItemType, DWORD dwData) {
	if(DebugControlled()) {
	  switch(ItemType) {
	  case QIT_VKEYDOWN:
      if((dwData & QVK_KEYMASK) <= VK__MAX && VKContextReset[(BYTE) dwData]) context->Reset();  // I3438   // I4370
		  break;

	  case QIT_DEADKEY:
		  context->Add(UC_SENTINEL);   // I4370
		  context->Add(CODE_DEADKEY);   // I4370
		  context->Add((WORD) dwData);   // I4370
		  break;

	  case QIT_CHAR:
		  context->Add((WORD) dwData);   // I4370
		  break;

	  case QIT_BACK:
		  if(dwData & BK_BACKSPACE)
			  while(context->CharIsDeadkey()) context->Delete();   // I4370
		  context->Delete();   // I4370
		  if(dwData & BK_BACKSPACE)
			  while(context->CharIsDeadkey()) context->Delete();   // I4370
		  break;
	  }

		SendMessage(GetDebugControlWindow(), WM_KEYMANDEBUG_ACTION, ItemType, dwData);
    return TRUE;
  }

  return AIWin2000Unicode::QueueAction(ItemType, dwData);   // I3575
}

BOOL AITIP::SendActions() {  // I4196
  if(DebugControlled()) {
    return TRUE;
  }

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
  	SendDebugMessageFormat(0, sdmAIDefault, 0, "AITIP::PostKeys: output bk=%d outcount=%d", bk, (INT_PTR)(p-OutBuf));
		(*_td->TIPProcessOutput)(bk, OutBuf, (int)(INT_PTR)(p - OutBuf));
  } else {
    SendDebugMessageFormat(0, sdmAIDefault, 0, "AITIP::PostKeys: no output");
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

/* Debug Integration */

BOOL AITIP::IsDebugControlWindow(HWND hwnd)
{
	static int WM_KEYMANDEBUG_CANDEBUG = RegisterWindowMessage("WM_KEYMANDEBUG_CANDEBUG");
  DWORD_PTR dwResult;
	SendMessageTimeout(hwnd, WM_KEYMANDEBUG_CANDEBUG, 0, 0, SMTO_BLOCK, 50, &dwResult);
  return dwResult != 0;
}

HWND AITIP::GetDebugControlWindow()
{
	if(!FIsDebugControlWindow) return NULL;
	return hwnd;
}

BOOL AITIP::DebugControlled()
{
	return FIsDebugControlWindow;
}

void AITIP::Debug_FillContextBuffer()
{
	WCHAR buf[MAXCONTEXT];
	if(DebugControlled() &&
			SendMessage(GetDebugControlWindow(), WM_KEYMANDEBUG_GETCONTEXT, MAXCONTEXT, (LPARAM) buf))
	{
		context->Set(buf);    // I4370
		SendDebugMessageFormat(0, sdmKeyboard, 0, "AIDebugger::FillContextBuffer(%ls)", buf);
	}
	else
	{
		context->Reset();   // I4370
		SendDebugMessageFormat(0, sdmKeyboard, 0, "AIDebugger::FillContextBuffer()-Reset");
	}
}

#define MAXSTOREOFFSETS	20

struct AIDEBUGINFO
{
	int cbSize;
	int ItemType;
	PWSTR Context, Output;
	LPKEY Rule;
	LPGROUP Group;
	DWORD_PTR Flags;
	WORD StoreOffsets[MAXSTOREOFFSETS*2+1];	// pairs--store, char position, terminated by 0xFFFF
};

void FillStoreOffsets(AIDEBUGINFO *di)
{
	int i, n;
	PWSTR p;

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

	for(i = n = 0, p = di->Rule->dpContext; *p; p = incxstr(p), i++)
	{
		if(*p == UC_SENTINEL && (*(p+1) == CODE_ANY || *(p+1) == CODE_NOTANY))
		{
			di->StoreOffsets[n++] = *(p+2) - 1;
			di->StoreOffsets[n++] = _td->IndexStack[i];
		}
		if(*p == UC_SENTINEL && *(p+1) == CODE_INDEX)
		{
			di->StoreOffsets[n++] = *(p+2) - 1;
			di->StoreOffsets[n++] = _td->IndexStack[*(p+3) - 1];
		}
		if(n == MAXSTOREOFFSETS*2) break;
	}

	if(n < MAXSTOREOFFSETS*2 - 1)
		for(p = di->Rule->dpOutput; *p; p = incxstr(p))
		{
			if(*p == UC_SENTINEL && *(p+1) == CODE_INDEX)
			{
				di->StoreOffsets[n++] = *(p+2) - 1;
				di->StoreOffsets[n++] = _td->IndexStack[*(p+3) - 1];
			}
			if(n == MAXSTOREOFFSETS*2) break;
		}
	di->StoreOffsets[n] = 0xFFFF;
}

BOOL AITIP::QueueDebugInformation(int ItemType, LPGROUP Group, LPKEY Rule, PWSTR fcontext, PWSTR foutput, DWORD_PTR dwExtraFlags)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return TRUE;
  if(!_td->ForceFileName[0]) return TRUE;

	SendDebugMessageFormat(0, sdmAIDefault, 0, "AIDebugger::QueueDebugInformation ItemType=%d", ItemType);
	AIDEBUGINFO di;
	di.cbSize = sizeof(AIDEBUGINFO);
	di.ItemType = ItemType;		// int
	di.Context = fcontext;		// PWSTR
	di.Rule = Rule;				// LPKEY
	di.Group = Group;			// LPGROUP
	di.Output = foutput;		// PWSTR
	di.Flags = dwExtraFlags;	// DWORD

	if(di.Rule) FillStoreOffsets(&di);

	// data required
	// keystroke
	// context for rule
	// if rule, then output of rule
	// match positions for all stores in rule

	if(DebugControlled())
		SendMessage(GetDebugControlWindow(), WM_KEYMANDEBUG_RULEMATCH, ItemType, (LPARAM) &di);

	return TRUE;
}

typedef BOOL(WINAPI *PREFRESHPRESERVEDKEYSFUNC)(BOOL Activating);

void RefreshPreservedKeys(BOOL Activating) {
#ifdef _WIN64
  HMODULE hModule = GetModuleHandle("kmtip64");
#else
  HMODULE hModule = GetModuleHandle("kmtip");
#endif
  if (hModule != NULL) {
    PREFRESHPRESERVEDKEYSFUNC pRefreshPreservedKeys = (PREFRESHPRESERVEDKEYSFUNC)GetProcAddress(hModule, "RefreshPreservedKeys");
    if (pRefreshPreservedKeys) {
      pRefreshPreservedKeys(Activating);
    }
  }
}
