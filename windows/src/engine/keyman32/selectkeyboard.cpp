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
                    21 Feb 2014 - mcdurdin - I4066 - V9.0 - Keyman Desktop needs to run in a lockdown mode when expired so developers can still use it
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    16 Jun 2014 - mcdurdin - I4271 - V9.0 - Switch language for all applications is not working
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    23 Jun 2014 - mcdurdin - I4277 - V9.0 - Switch from Keyman to Keyman keyboard causes loop in global language switch
                    23 Jun 2014 - mcdurdin - I4285 - V9.0 - Profile change needs debug data for tracing source profile change thread
                    14 May 2015 - mcdurdin - I4714 - V9.0 - Keyboard and language hotkeys don't always work
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/
#include "keyman64.h"   // I5136
#include "accctrl.h"
#include "aclapi.h"
#include "sddl.h"

// I3594  // I4220

BOOL SelectKeyboard(DWORD KeymanID)
{
	int i;
  HWND hwnd = GetFocus();

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

	SendDebugMessageFormat(hwnd,sdmGlobal,0,"ENTER SelectKeyboard-------------------------------------------");
  SendDebugMessageFormat(hwnd,sdmGlobal,0,"ENTER SelectKeyboard: Current:(HKL=%x KeymanID=%x %s) New:(ID=%x)", //lpActiveKeyboard=%s ActiveKeymanID: %x sk: %x KeymanID: %d", 
    GetKeyboardLayout(0), 
    _td->ActiveKeymanID,
    _td->lpActiveKeyboard == NULL ? "NULL" : _td->lpActiveKeyboard->Name,
    //_td->NextKeyboardLayout,
    KeymanID);

  __try
	{
		if(_td->ForceFileName[0])
		{
			SendDebugMessageFormat(hwnd,sdmGlobal,0,"SelectKeyboard: Ignored due to ForceFile");
			return FALSE;	// Keyboard file is force-loaded
		}

		KMHideIM();

		if(_td->lpActiveKeyboard) DeactivateDLLs(_td->lpActiveKeyboard);
		_td->lpActiveKeyboard = NULL;
		_td->ActiveKeymanID = KEYMANID_NONKEYMAN;

		//SendDebugMessageFormat(hwnd,sdmGlobal,0,"SelectKeyboard: nKeyboards=%d", nKeyboards);

		for(i = 0; i < _td->nKeyboards; i++)
		{
			if(_td->lpKeyboards[i].KeymanID == KeymanID)
			{
				if(!_td->lpKeyboards[i].Keyboard && !LoadlpKeyboard(i))
				{
					SendDebugMessageFormat(hwnd,sdmGlobal,0,"SelectKeyboard: Unable to load");
					return TRUE;
				}

				_td->lpActiveKeyboard = &_td->lpKeyboards[i];
				_td->ActiveKeymanID = _td->lpActiveKeyboard->KeymanID;

				SendDebugMessageFormat(hwnd,sdmGlobal,0,"SelectKeyboard: NewKeymanID: %x", _td->ActiveKeymanID);

				if(_td->app) _td->app->ResetContext(); 
				ResetCapsLock();

        SelectApplicationIntegration();   // I4287
				if(!_td->app->IsWindowHandled(hwnd)) _td->app->HandleWindow(hwnd);
				_td->state.windowunicode = _td->app->IsUnicode();

				ActivateDLLs(_td->lpActiveKeyboard);

				return TRUE;
			}
		}

    if(IsFocusedThread())
    {
      SendDebugMessageFormat(hwnd,sdmGlobal,0,"SelectKeyboard: Keyboard Not Found");
		}
	}
	__finally
	{
    SendDebugMessageFormat(hwnd,sdmGlobal,0,"EXIT SelectKeyboard: Current:(HKL=%x KeymanID=%x %s) New:(ID=%x)", //lpActiveKeyboard=%s ActiveKeymanID: %x sk: %x KeymanID: %d", 
      GetKeyboardLayout(0), 
      _td->ActiveKeymanID,
      _td->lpActiveKeyboard == NULL ? "NULL" : _td->lpActiveKeyboard->Name,
      KeymanID);
	  SendDebugMessageFormat(hwnd,sdmGlobal,0,"EXIT SelectKeyboard-------------------------------------------");
	}
	return TRUE;
}

BOOL SelectKeyboardTSF(PKEYMAN64THREADDATA _td, DWORD KeymanID, BOOL foreground)   // I3933   // I3949   // I4271
{
  WCHAR buf[256];
  
  //atom format: LANGID|CLSID|GUIDPRofile

  if(GlobalGetAtomNameW((ATOM)KeymanID, buf, 255) == 0) {
    // This could happen if the atom is deleted before we get to it, e.g. if the app has hung
    return FALSE; 
  }

  if(foreground) {   // I4271
    GlobalDeleteAtom((ATOM)KeymanID);
  } else if(IsFocusedThread()) {
    return FALSE;   // I4277
  }

  buf[255] = 0; // for safety
  
  LPWSTR pszCLSID = wcschr(buf,L'|');
  if(!pszCLSID) return FALSE;
  *pszCLSID = 0;
  pszCLSID++;
  
  LPWSTR pszGUIDProfile = wcschr(pszCLSID,L'|');
  if(!pszGUIDProfile) return FALSE;
  *pszGUIDProfile = 0;
  pszGUIDProfile++;

  GUID guidProfile;
  IID iidCLSID;
  LANGID langID;

  langID = (LANGID) _wtoi(buf);
  if(IIDFromString(pszCLSID, &iidCLSID) != NOERROR) return FALSE;
  if(CLSIDFromString(pszGUIDProfile, &guidProfile) != NOERROR) return FALSE;

  if(!OpenTSF(_td)) return FALSE;

  BOOL bResult = FALSE;

  SendDebugMessageFormat(0, sdmGlobal, 0, "SelectKeyboardTSF: Selecting language %x [%d]", langID, foreground);
  HRESULT hr = _td->pInputProcessorProfiles->ChangeCurrentLanguage(langID);
  if(SUCCEEDED(hr))
  {
    SendDebugMessageFormat(0, sdmGlobal, 0, "SelectKeyboardTSF: Activating profile %ws %ws for %x", pszCLSID, pszGUIDProfile, langID);
    hr = _td->pInputProcessorProfileMgr->ActivateProfile(TF_PROFILETYPE_INPUTPROCESSOR, langID, iidCLSID, guidProfile, NULL, TF_IPPMF_DONTCARECURRENTINPUTLANGUAGE);   // I4714
    bResult = SUCCEEDED(hr);
  }

  SendDebugMessageFormat(0, sdmGlobal, 0, "SelectKeyboardTSF: Exiting with %x", hr);
  return bResult;
}

void SelectKeyboardHKL(PKEYMAN64THREADDATA _td, DWORD hkl, BOOL foreground) {   // I3933   // I3949   // I4271
    if(!foreground && IsFocusedThread()) {
      return;   // I4277
    }
    if(OpenTSF(_td)) {
        LANGID langid = HKLToLanguageID(ForceKeymanIDToHKL(hkl)); // I3191
        SendDebugMessageFormat(0, sdmGlobal, 0, "SelectKeyboardHKL: Activating language %x [%d]", langid, foreground);
        HRESULT hr = _td->pInputProcessorProfiles->ChangeCurrentLanguage(langid);
        if(SUCCEEDED(hr)) {
          SendDebugMessageFormat(0, sdmGlobal, 0, "SelectKeyboardHKL: Activating keyboard layout %x for %x", hkl, langid);
            hr = _td->pInputProcessorProfileMgr->ActivateProfile(TF_PROFILETYPE_KEYBOARDLAYOUT, langid, CLSID_NULL, GUID_NULL, ForceKeymanIDToHKL(hkl), TF_IPPMF_DONTCARECURRENTINPUTLANGUAGE); // I3191   // I4714
        }
        SendDebugMessageFormat(0, sdmGlobal, 0, "SelectKeyboardHKL: Exiting with %x", hr);
    }
}

void ReportActiveKeyboard(PKEYMAN64THREADDATA _td, WORD wCommand) {   // I3933   // I3949
  if(OpenTSF(_td) && IsFocusedThread()) {   // I4277
    TF_INPUTPROCESSORPROFILE profile;
    if(SUCCEEDED(_td->pInputProcessorProfileMgr->GetActiveProfile(GUID_TFCAT_TIP_KEYBOARD, &profile))) {
      char str[128];
      WCHAR clsidstr[40], profilestr[40];
      if(profile.dwProfileType == TF_PROFILETYPE_KEYBOARDLAYOUT) {
        wsprintf(str, "%d|%d|%d", GetCurrentThreadId(), profile.langid, profile.hkl);   // I4285
      } else if(profile.dwProfileType == TF_PROFILETYPE_INPUTPROCESSOR) {
        StringFromGUID2(profile.clsid, clsidstr, _countof(clsidstr));
        StringFromGUID2(profile.guidProfile, profilestr, _countof(profilestr));
        wsprintf(str, "%d|%d|%ws|%ws", GetCurrentThreadId(), profile.langid, clsidstr, profilestr);   // I4285
      } else {
        return;
      }
      WORD wAtom = GlobalAddAtom(str);
      Globals::PostMasterController(wm_keyman_control, KMC_PROFILECHANGED, MAKELONG(wCommand, wAtom));
    }
  }
}
