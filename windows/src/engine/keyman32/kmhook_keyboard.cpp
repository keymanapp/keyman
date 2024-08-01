/*
  Name:             kmhook_keyboard
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
  History:          01 Aug 2006 - mcdurdin - Hotkeys testing
                    14 Sep 2006 - mcdurdin - Initialise process when no messages have been passed
                    30 May 2007 - mcdurdin - I864 - Log exceptions in hook procedures
                    13 Jul 2007 - mcdurdin - I934 - Prep for x64
                    23 Aug 2007 - mcdurdin - I719 - Fix Alt+LeftShift and Word interactions
                    05 Nov 2007 - mcdurdin - I1087 - Add language switching hotkeys for Desktop Pro
                    16 Jan 2009 - mcdurdin - I1512 - Support SendInput
                    09 Mar 2009 - mcdurdin - I1848 - Keyboard hotkeys interfere with Ctrl+Shift and Alt+Shift
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    23 Apr 2009 - mcdurdin - I1940 - Generate a minidump when an exception occurs in a hook
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    11 Dec 2009 - mcdurdin - I934 - x64 - per thread keyboard hooks
                    11 Dec 2009 - mcdurdin - I934 - x64 - test when keyman has shutdown and disconnect
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2356 - Performance - don't clear keyboard hook if new focus is on same thread
                    04 May 2010 - mcdurdin - I2349 - Pause key to signal new debug log file
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    10 May 2010 - mcdurdin - Enable exception logging
                    22 Oct 2010 - mcdurdin - I2522 - Language switch window initial version
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    23 Dec 2011 - mcdurdin - I3191 - Corruption of keyboard selection cache between x86 and x64
                    18 Feb 2012 - mcdurdin - I3242 - Arrow keys do not function correctly in Internet Explorer
                    21 Feb 2012 - mcdurdin - I3250 - Word 2010 sometimes freezes after hotkey pressed
                    23 Mar 2012 - mcdurdin - I3284 - Fix blocking of Ctrl+Shift passthrough to other hooks
                    03 May 2012 - mcdurdin - I3301 - Resolve regression introduced by I3250 with Alt inadvertently activating menus
                    15 Jun 2012 - mcdurdin - I3358 - Media keys generate characters when positional keyboard is active
                    04 Nov 2012 - mcdurdin - I3534 - V9.0 - Merge of I3301 - Resolve regression introduced by I3250 with Alt inadvertently activating menus
                    04 Nov 2012 - mcdurdin - I3527 - V9.0 - Merge of I3242 - Arrow keys do not function correctly in Internet Explorer
                    04 Nov 2012 - mcdurdin - I3535 - V9.0 - Merge of I3358 - Media keys generate characters when positional keyboard is active
                    04 Nov 2012 - mcdurdin - I3529 - V9.0 - Merge of I3284 - Fix blocking of Ctrl+Shift passthrough to other hooks
                    04 Nov 2012 - mcdurdin - I3526 - V9.0 - Merge of I3191 - Corruption of keyboard selection cache between x86 and x64
                    17 Nov 2012 - mcdurdin - I3576 - V9.0 - Keyboard hook should avoid processing any keys when KMTIP is active
                    28 Nov 2012 - mcdurdin - I3596 - V9.0 - Remove keyboard hook
                    28 Nov 2012 - mcdurdin - I3594 - V9.0 - Remove old SelectKeyboard code and related messages
                    01 Dec 2012 - mcdurdin - I3617 - V9.0 - Keyboard hook obsolete, strip out code
                    06 Mar 2014 - mcdurdin - I4124 - V9.0 - Language switch dialog is not working in v9
                    24 Apr 2014 - mcdurdin - I4197 - V9.0 - Avoid interactions with full-screen RDP
                    01 May 2014 - mcdurdin - I4203 - V9.0 - Order that modifiers are depressed changes actions when Keyman is running
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
                    27 Mar 2015 - mcdurdin - I4641 - V9.0 - Keyman can crash silently on exit due to null hotkeys being addressed
                    09 Aug 2015 - mcdurdin - I4844 - Tidy up PostDummyKeyEvent calls
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/
   // I4326
#include "pch.h"
#include <Psapi.h>

// This file is used only in keyman32.dll; it has support functions for hotkeys
// which used to be in a keyboard hook but are now in the low-level keyboard
// hook
// TODO: rename this file
#ifndef _WIN64

   // I3617
BOOL IsLanguageSwitchWindowVisible();
void SendToLanguageSwitchWindow(WPARAM wParam, LPARAM lParam);
BOOL ForegroundWindowIsRDP();   // I4197

BOOL InLanguageSwitchKeySequence = FALSE;
HWND hwndLanguageSwitch = 0;

BOOL ForegroundWindowIsRDP() {   // I4197
  //TODO: Cache result
  DWORD dwProcessId;
  HWND hwnd = GetForegroundWindow();
  if(hwnd == NULL) {
    return FALSE;
  }
  if(GetWindowThreadProcessId(hwnd, &dwProcessId) == 0) {
    return FALSE;
  }

  HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION, false, dwProcessId);
  if(hProcess == NULL) {
    return FALSE;
  }

  char buf[260];
  BOOL bResult = GetProcessImageFileName(hProcess, buf, _countof(buf)) > 0;

  CloseHandle(hProcess);

  if(bResult) {
    char *p = strrchr(buf, '\\');
    bResult = p && _stricmp(p, "\\mstsc.exe") == 0;
  }

  return bResult;
}

BOOL KeyLanguageSwitchPress(WPARAM wParam, BOOL extended, BOOL isUp, DWORD ShiftState)
{
  /* Let's avoid the cascaded remote desktop switching issues */

  if(ForegroundWindowIsRDP()) return FALSE;   // I4197

  /* This is a special-case hotkey for language switching - ALT+LEFT SHIFT */
  Hotkeys *hotkeys = Hotkeys::Instance();   // I4641
  if(!hotkeys) {
    return FALSE;
  }

  Hotkey *hotkey = hotkeys->GetHotkey(HK_ALT | HK_SHIFT);   // I4641
  if(!hotkey)
  {
    hotkey = hotkeys->GetHotkey(HK_CTRL | HK_SHIFT);   // I4641
    if(!hotkey) return FALSE;
  }

  if((hotkey->HotkeyValue & HK_CTRL) && (wParam == VK_CONTROL || wParam == VK_LCONTROL || wParam == VK_RCONTROL) && !isUp && (ShiftState & ~(HK_CTRL|HK_RCTRL_INVALID)) == 0)   // I4124
  {
    InLanguageSwitchKeySequence = TRUE;
    return FALSE;
  }
  else if((hotkey->HotkeyValue & HK_ALT) && (wParam == VK_MENU || wParam == VK_LMENU || wParam == VK_RMENU) && !extended && !isUp && (ShiftState & ~HK_ALT) == 0)   // I4124
  {
    InLanguageSwitchKeySequence = TRUE;
    return FALSE;
  }
  else if(InLanguageSwitchKeySequence)
  {
    if(wParam == VK_SHIFT || wParam == VK_LSHIFT || wParam == VK_RSHIFT)   // I4124
    {
      if(isUp)
      {
        InLanguageSwitchKeySequence = FALSE;
        //ReportActiveKeyboard(_td, PC_UPDATE);
        Globals::PostMasterController(wm_keyman_control, MAKELONG(KMC_INTERFACEHOTKEY, hotkey->Target), 0);
        PostDummyKeyEvent();   // I4124   // I4844
        keybd_event(VK_SHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, 0);    // I4203
        return TRUE;
      }

      return FALSE;
    }
    else
      InLanguageSwitchKeySequence = FALSE;
    return FALSE;
  }

  return FALSE;
}

BOOL IsLanguageSwitchWindowVisible() {
  hwndLanguageSwitch = FindWindow("TfrmLanguageSwitch", NULL);
  if(hwndLanguageSwitch == NULL) return FALSE;
  return IsWindowVisible(hwndLanguageSwitch);
}

void SendToLanguageSwitchWindow(WPARAM wParam, LPARAM lParam) {
  DWORD keyFlags = (DWORD) lParam;   // I4124
  PostMessage(hwndLanguageSwitch, wm_keyman_control, keyFlags & LLKHF_UP ? KMC_KEYUP : KMC_KEYDOWN, wParam);   // I4124
}

int ProcessLanguageSwitchShiftKey(WPARAM wParam, BOOL isUp)
{
  if(!isUp) return 1;

  /* When a ctrl, shift or alt is released we want to know about it, so we can update shift states etc */

  switch(wParam)   // I4548
  {
    case VK_LSHIFT:
    case VK_RSHIFT:
    case VK_SHIFT:
    case VK_CONTROL:
    case VK_LCONTROL:   // I4124
    case VK_RCONTROL:   // I4124
    case VK_MENU:
    case VK_LMENU:   // I4124
    case VK_RMENU:   // I4124
      break;
    default:
      return 1;
  }

  /* Generate a dummy keystroke to block menu activations, etc but let the shift key through */
  PostDummyKeyEvent();  // I3301 - this is imperfect because we don't deal with HC_NOREMOVE.  But good enough?   // I3534   // I4844
  return 0;
}

#endif
