/*
  Name:             hotkeys
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
  History:          01 Aug 2006 - mcdurdin - Initial version
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
                    13 Oct 2014 - mcdurdin - I4451 - V9.0 - Language hotkeys are not working
                    14 Nov 2014 - mcdurdin - I4516 - V9.0 - Language hotkeys associated with non-primary keyboards do not trigger language change
                    27 Mar 2015 - mcdurdin - I4641 - V9.0 - Keyman can crash silently on exit due to null hotkeys being addressed
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/
   // I5136
#include "pch.h"

// This file is used only in keyman32.dll; it tracks hotkeys for Keyman keyboard
// switching and events
#ifndef _WIN64

Hotkeys *g_Hotkeys = NULL;   // I4326

Hotkeys *Hotkeys::Instance() {   // I4326
  if(GetCurrentThreadId() != Globals::get_InitialisingThread()) {
    return NULL;  // Will bork on other threads given current usage :)
  }

  if(g_Hotkeys == NULL) {
    g_Hotkeys = new Hotkeys;
    g_Hotkeys->Load();   // I4390
  }

  return g_Hotkeys;
}

void Hotkeys::Unload() {
  if (GetCurrentThreadId() != Globals::get_InitialisingThread()) {
    OutputThreadDebugString("Unexpected: no other thread should be attempting to unload hotkeys");
    return;
  }

  if (g_Hotkeys != NULL) {
    delete g_Hotkeys;
    g_Hotkeys = NULL;
  }
}

void Hotkeys::Reload() {   // I4326   // I4390
  Hotkeys *hotkeys = Hotkeys::Instance();   // I4641
  if(hotkeys == NULL) {
    return;
  }

  hotkeys->Load();   // I4390
}

void Hotkeys::Load() {   // I4390
	SendDebugEntry();
	m_nHotkeys = 0;
	RegistryReadOnly reg(HKEY_CURRENT_USER);

  /* Load interface hotkeys */

	if(reg.OpenKeyReadOnly(REGSZ_KeymanHotkeys)) {
		int n = 0;
		char name[128];
		while(reg.GetValueNames(name, 128, n++)) {
      m_hotkeys[m_nHotkeys].HotkeyType = hktInterface;
			m_hotkeys[m_nHotkeys].HotkeyValue = reg.ReadInteger(name);
      m_hotkeys[m_nHotkeys].Target = atoi(name);

			SendDebugMessageFormat("InterfaceHotkey[%d] = {HotkeyValue: %x, Target: %d}",
        m_nHotkeys,
				m_hotkeys[m_nHotkeys].HotkeyValue,
				m_hotkeys[m_nHotkeys].Target);

      if(++m_nHotkeys >= MAX_HOTKEYS) {
        break;
      }
		}
	}

  reg.CloseKey();

  if(m_nHotkeys >= MAX_HOTKEYS) {
    SendDebugExit();
    return;
  }

  /* Load language hotkeys */   // I4451

  if(reg.OpenKeyReadOnly(REGSZ_KeymanLanguageHotkeys)) {
		int nkey = 0;
		WCHAR valuename[128];
    while(reg.GetValueNames(valuename, 128, nkey++))
		{
      m_hotkeys[m_nHotkeys].HotkeyType = hktLanguage;
      m_hotkeys[m_nHotkeys].hkl = (HKL) wcstoul(valuename, NULL, 16);   // I4516
      if(m_hotkeys[m_nHotkeys].hkl == 0) {
        // Will be a guid
        if(!SUCCEEDED(CLSIDFromString(valuename, &m_hotkeys[m_nHotkeys].profileGUID))) {
          continue;
        }
      }

      WCHAR hkval[64];
      reg.ReadString(valuename, hkval, 64);
      m_hotkeys[m_nHotkeys].HotkeyValue = _wtoi(hkval);

			SendDebugMessageFormat("LanguageHotkey[%d] = {HotkeyValue: %x, hkl: %x, profileGUID: %ws}", m_nHotkeys,
				m_hotkeys[m_nHotkeys].HotkeyValue,
				m_hotkeys[m_nHotkeys].hkl,
        m_hotkeys[m_nHotkeys].hkl == 0 ? valuename : L"");

      if(++m_nHotkeys >= MAX_HOTKEYS) {
        break;
      }
		}
	}

  reg.CloseKey();

	SendDebugExit();
}

Hotkey *Hotkeys::GetHotkey(DWORD hotkey)
{
  for (int i = 0; i < m_nHotkeys; i++) {
    if (m_hotkeys[i].HotkeyValue == hotkey) {
      return &m_hotkeys[i];
    }
  }
	return NULL;
}

#endif
