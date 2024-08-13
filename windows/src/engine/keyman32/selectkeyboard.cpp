/*
  Name:             selectkeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      6 Apr 2010

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          06 Apr 2010 - mcdurdin - I2271 - Select Keyboard tidy up
                    19 Apr 2010 - mcdurdin - I2297 - Use global cache to remember last active keyboard for all apps
                    19 Apr 2010 - mcdurdin - I2297 - Rewrite SelectKeyboard to reduce complexity and increase stability
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    04 May 2010 - mcdurdin - I2297 - Rewrite cache code to share across all apps using file mapping
                    15 Jun 2010 - mcdurdin - I2425 - Vista security incorrectly applied to WinXP
                    29 Jun 2010 - mcdurdin - I2441 - SelectKeyboard cache was not zero-initialised at startup
                    30 Nov 2010 - mcdurdin - I2543 - Support switching to TSF TIPs
                    30 Nov 2010 - mcdurdin - I2545 - Fix switching to default Windows keyboard from lang switch window
                    17 Dec 2010 - mcdurdin - I2572 - Keyman keyboard switch has to go twice, also I2557
                    18 Mar 2011 - mcdurdin - I2779 - HKLs with high bit set do not get selected correctly
                    19 Aug 2011 - mcdurdin - I3040 - keyman32.dll can crash if advapi32.dll is not loaded when first called from a process
                    17 Oct 2011 - mcdurdin - I3109 - Language Switcher does not show active HKL until first language switch after keyman started
                    02 Dec 2011 - mcdurdin - I3159 - When multiple OEM products installed, hotkeys can activate keyboards for non-running products
                    23 Dec 2011 - mcdurdin - I3191 - Corruption of keyboard selection cache between x86 and x64
                    24 Jan 2012 - mcdurdin - I3143 - Report more useful information when Keyman fails to start
                    06 Feb 2012 - mcdurdin - I3227 - When switching keyboards and typing rapidly, Windows/Keyman keyboard links can get out of sync
                    04 Nov 2012 - mcdurdin - I3537 - V9.0 - Merge of I3159 - When multiple OEM products installed, hotkeys can activate keyboards for non-running products
                    04 Nov 2012 - mcdurdin - I3523 - V9.0 - Merge of I3143 - Report more useful information when Keyman fails to start
                    04 Nov 2012 - mcdurdin - I3526 - V9.0 - Merge of I3191 - Corruption of keyboard selection cache between x86 and x64
                    04 Nov 2012 - mcdurdin - I3538 - V9.0 - Merge of I3227 - When switching keyboards and typing rapidly, Windows/Keyman keyboard links can get out of sync
                    28 Nov 2012 - mcdurdin - I3594 - V9.0 - Remove old SelectKeyboard code and related messages
                    07 Nov 2013 - mcdurdin - I3949 - V9.0 - Keyboard selection and notification needs consolidation
                    21 Feb 2014 - mcdurdin - I4066 - V9.0 - Keyman D_esktop needs to run in a lockdown mode when expired so developers can still use it
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    16 Jun 2014 - mcdurdin - I4271 - V9.0 - Switch language for all applications is not working
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    23 Jun 2014 - mcdurdin - I4277 - V9.0 - Switch from Keyman to Keyman keyboard causes loop in global language switch
                    23 Jun 2014 - mcdurdin - I4285 - V9.0 - Profile change needs debug data for tracing source profile change thread
                    14 May 2015 - mcdurdin - I4714 - V9.0 - Keyboard and language hotkeys don't always work
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/
#include "pch.h"   // I5136
#include "accctrl.h"
#include "aclapi.h"
#include "sddl.h"
#include "SharedBuffers.h"

// I3594  // I4220

BOOL SelectKeyboard(DWORD KeymanID)
{
  int i;
  HWND hwnd = GetFocus();

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;

  SendDebugEntry();
  SendDebugMessageFormat("Current:(HKL=%x KeymanID=%x %s) New:(ID=%x)", //lpActiveKeyboard=%s ActiveKeymanID: %x sk: %x KeymanID: %d",
    GetKeyboardLayout(0),
    _td->ActiveKeymanID,
    _td->lpActiveKeyboard == NULL ? "NULL" : _td->lpActiveKeyboard->Name,
    //_td->NextKeyboardLayout,
    KeymanID);

  __try
  {

    KMHideIM();

    if (_td->lpActiveKeyboard) DeactivateDLLs(_td->lpActiveKeyboard);
    _td->lpActiveKeyboard = NULL;
    _td->ActiveKeymanID = KEYMANID_NONKEYMAN;

    //SendDebugMessageFormat("nKeyboards=%d", nKeyboards);

    for (i = 0; i < _td->nKeyboards; i++)
    {
      if (_td->lpKeyboards[i].KeymanID == KeymanID)
      {
        if (!_td->lpKeyboards[i].lpCoreKeyboard && !LoadlpKeyboard(i))
        {
          SendDebugMessageFormat("Unable to load");
          return TRUE;
        }

        _td->lpActiveKeyboard = &_td->lpKeyboards[i];
        _td->ActiveKeymanID = _td->lpActiveKeyboard->KeymanID;

        SendDebugMessageFormat("NewKeymanID: %x", _td->ActiveKeymanID);

        if (_td->app) _td->app->ResetContext();
        SelectApplicationIntegration();   // I4287
        if (_td->app && !_td->app->IsWindowHandled(hwnd)) _td->app->HandleWindow(hwnd);
        _td->state.windowunicode = !_td->app || _td->app->IsUnicode();

        ActivateDLLs(_td->lpActiveKeyboard);

        if (KM_CORE_STATUS_OK !=
            km_core_event(_td->lpActiveKeyboard->lpCoreKeyboardState, KM_CORE_EVENT_KEYBOARD_ACTIVATED, nullptr)) {
          SendDebugMessageFormat("km_core_event Failed Result: %d ", FALSE);
        } else {
          ProcessActionsExternalEvent();
        }
        return TRUE;
      }
      if (IsFocusedThread())
      {
        SendDebugMessageFormat("Keyboard Not Found");
      }
    }
  }

    __finally
    {
      SendDebugMessageFormat("EXIT Current:(HKL=%x KeymanID=%x %s) New:(ID=%x)", //lpActiveKeyboard=%s ActiveKeymanID: %x sk: %x KeymanID: %d",
        GetKeyboardLayout(0),
        _td->ActiveKeymanID,
        _td->lpActiveKeyboard == NULL ? "NULL" : _td->lpActiveKeyboard->Name,
        KeymanID);
      SendDebugExit();
    }
    return TRUE;
}

BOOL SelectKeyboardTSF(DWORD dwIdentity, BOOL foreground)   // I3933   // I3949   // I4271
{
  SendDebugEntry();
  if (!foreground && IsFocusedThread()) {
    return_SendDebugExit(FALSE);   // I4277
  }

  ISharedBufferManager *sbm = GetThreadSharedBufferManager();
  if (!sbm) {
    return_SendDebugExit(FALSE);
  }

  SelectKeyboardBuffer skb;
  if(!sbm->ReadSelectKeyboardBuffer(dwIdentity, &skb)) {
    return_SendDebugExit(FALSE);
  }

  TSFINTERFACES tsf = { NULL };

  if(!OpenTSF(&tsf)) {
    return_SendDebugExit(FALSE);
  }

  BOOL bResult = FALSE;

  SendDebugMessageFormat("Selecting language identity=%d %x [%d]", dwIdentity, skb.LangID, foreground);
  HRESULT hr = tsf.pInputProcessorProfiles->ChangeCurrentLanguage(skb.LangID);
  if(SUCCEEDED(hr)) {
    SendDebugMessageFormat("Activating profile");
    hr = tsf.pInputProcessorProfileMgr->ActivateProfile(TF_PROFILETYPE_INPUTPROCESSOR, skb.LangID, skb.CLSID, skb.GUIDProfile, NULL, TF_IPPMF_DONTCARECURRENTINPUTLANGUAGE);   // I4714
    bResult = SUCCEEDED(hr);
  }

  CloseTSF(&tsf);

  SendDebugMessageFormat("Exiting with %x", hr);
  return_SendDebugExit(bResult);
}

void SelectKeyboardHKL(DWORD hkl, BOOL foreground) {   // I3933   // I3949   // I4271
  if(!foreground && IsFocusedThread()) {
    return;   // I4277
  }

  SendDebugEntry();

  TSFINTERFACES tsf = { NULL };
  if(OpenTSF(&tsf)) {
    LANGID langid = HKLToLanguageID(ForceKeymanIDToHKL(hkl)); // I3191
    SendDebugMessageFormat("Activating language %x [%d]", langid, foreground);
    HRESULT hr = tsf.pInputProcessorProfiles->ChangeCurrentLanguage(langid);
    if(SUCCEEDED(hr)) {
      SendDebugMessageFormat("Activating keyboard layout %x for %x", hkl, langid);
      hr = tsf.pInputProcessorProfileMgr->ActivateProfile(TF_PROFILETYPE_KEYBOARDLAYOUT, langid, CLSID_NULL, GUID_NULL, ForceKeymanIDToHKL(hkl), TF_IPPMF_DONTCARECURRENTINPUTLANGUAGE); // I3191   // I4714
    }
    SendDebugMessageFormat("Exiting with %x", hr);
    CloseTSF(&tsf);
  }
  SendDebugExit();
}

void PrepareLanguageSwitchString(UINT langid, HKL hkl, char *str) {
  wsprintf(str, "%d|%d|%d", (int) GetCurrentThreadId(), langid, PtrToInt(hkl));   // I4285
}

void PrepareLanguageSwitchString(UINT langid, GUID clsid, GUID guidProfile, char *str) {
  WCHAR clsidstr[40], profilestr[40];
  StringFromGUID2(clsid, clsidstr, _countof(clsidstr));
  StringFromGUID2(guidProfile, profilestr, _countof(profilestr));
  wsprintf(str, "%d|%d|%ls|%ls", (int) GetCurrentThreadId(), langid, clsidstr, profilestr);   // I4285
}

void ReportActiveKeyboard(WORD wCommand) {   // I3933   // I3949
  if (!IsFocusedThread()) return;

  TSFINTERFACES tsf = { NULL };

  if (!OpenTSF(&tsf)) return;

  TF_INPUTPROCESSORPROFILE profile;
  if(SUCCEEDED(tsf.pInputProcessorProfileMgr->GetActiveProfile(GUID_TFCAT_TIP_KEYBOARD, &profile))) {
    ReportKeyboardChanged(wCommand, profile.dwProfileType, profile.langid, profile.hkl, profile.clsid, profile.guidProfile);
  }
  CloseTSF(&tsf);
}

BOOL ReportKeyboardChanged(WORD wCommand, DWORD dwProfileType, UINT langid, HKL hkl, GUID clsid, GUID guidProfile) {
  char str[128];
  //OutputDebugString("  ReportKeyboardChanged: ");
  if (dwProfileType == TF_PROFILETYPE_KEYBOARDLAYOUT) {
    PrepareLanguageSwitchString(langid, hkl, str);
  }
  else if (dwProfileType == TF_PROFILETYPE_INPUTPROCESSOR) {
    PrepareLanguageSwitchString(langid, clsid, guidProfile, str);
  }
  else {
    return FALSE;
  }
  //OutputDebugStringA(str);
  //OutputDebugString("\n");
  WORD wAtom = GlobalAddAtom(str);  // TODO: stop abusing atoms and use a rolling shared memory buffer with index
  Globals::PostMasterController(wm_keyman_control, KMC_PROFILECHANGED, MAKELONG(wCommand, wAtom));
  return TRUE;
}
