/*
  Name:             k32_lowlevelkeyboardhook
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      3 Aug 2014

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    13 Oct 2014 - mcdurdin - I4451 - V9.0 - Language hotkeys are not working
                    27 Mar 2015 - mcdurdin - I4641 - V9.0 - Keyman can crash silently on exit due to null hotkeys being addressed
                    22 Apr 2015 - mcdurdin - I4674 - V9.0 - Hotkeys do not always work consistently
                    14 May 2015 - mcdurdin - I4714 - V9.0 - Keyboard and language hotkeys don't always work
                    09 Aug 2015 - mcdurdin - I4844 - Tidy up PostDummyKeyEvent calls
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/
// I4326
#include "pch.h"
#include "serialkeyeventserver.h"
#include "kbd.h"	/* DDK kbdlayout */

// This file is used only in keyman32.dll; it implements our low level keyboard hook
// in the main keyman.exe process for hotkeys, serial key event server
#ifndef _WIN64

BOOL ProcessHotkey(UINT vkCode, BOOL isUp, DWORD ShiftState);

LRESULT _kmnLowLevelKeyboardProc(
  _In_  int nCode,
  _In_  WPARAM wParam,
  _In_  LPARAM lParam
);

// Local variables only used by single thread -- low level keyboard proc thread
DWORD FHotkeyShiftState = 0;

LRESULT CALLBACK kmnLowLevelKeyboardProc(
  _In_  int nCode,
  _In_  WPARAM wParam,
  _In_  LPARAM lParam
) {
  LRESULT res = 0;
#ifdef _DEBUG_EXCEPTION
	res = _kmnLowLevelKeyboardProc(nCode,wParam,lParam);
#else
  __try {
	  res = _kmnLowLevelKeyboardProc(nCode,wParam,lParam);
	}
  __except(ExceptionMessage("kmnLowLevelKeyboardProc", GetExceptionInformation())) {
	}
#endif
  return res;
}

BOOL isModifierKey(DWORD vkCode) {
  switch (vkCode) {
    case VK_LCONTROL:
    case VK_RCONTROL:
    case VK_CONTROL:
    case VK_LMENU:
    case VK_RMENU:
    case VK_MENU:
    case VK_LSHIFT:
    case VK_RSHIFT:
    case VK_SHIFT:
      return TRUE;
  }
  return FALSE;
}

BOOL KeyLanguageSwitchPress(WPARAM wParam, BOOL extended, BOOL isUp, DWORD ShiftState);
int ProcessLanguageSwitchShiftKey(WPARAM wParam, BOOL isUp);
BOOL IsLanguageSwitchWindowVisible();
void SendToLanguageSwitchWindow(WPARAM wParam, LPARAM lParam);

LPARAM LLKHFFlagstoWMKeymanKeyEventFlags(PKBDLLHOOKSTRUCT hs) {
  return (hs->scanCode << 16) |
    ((hs->flags & LLKHF_EXTENDED) ? KEYEVENTF_EXTENDEDKEY : 0) |
    ((hs->flags & LLKHF_UP) ? KEYEVENTF_KEYUP : 0);
}

/*
  We don't attempt to serialize input to the console windows because they
  behave somewhat differently to normal windows. For now, this should be
  sufficient. In the future, we may want to find a way to interrogate the
  focused process to find out which window actually has focus for posting
  messages, because we appear to post the messages to the wrong thread
  for console windows.
*/
BOOL IsConsoleWindow(HWND hwnd) {
  static HWND last_hwnd = 0;
  static BOOL last_isConsoleWindow = FALSE;

  if (last_hwnd == hwnd) {
    return last_isConsoleWindow;
  }

  char buf[64];

  last_hwnd = hwnd;
  last_isConsoleWindow = GetClassName(hwnd, buf, 64) && !strcmp(buf, "ConsoleWindowClass");

  return last_isConsoleWindow;
}


/*
 Test for touch panel visibility (#2450). UpdateTouchPanelVisibility is called periodically by
 keyman.exe to refresh the visibility flag.
*/

static BOOL touchPanelVisible;

void WINAPI Keyman_UpdateTouchPanelVisibility(BOOL isVisible) {
  touchPanelVisible = isVisible;
  SendDebugMessageFormat("isVisible=%d", touchPanelVisible);
}

BOOL IsTouchPanelVisible() {
  // Note: GetCurrentInputMessageSource does not work in this context
  // Using IFrameworkInputPaneHandler events only works for a specific window, so not helpful for us.
  return touchPanelVisible;
}

LRESULT _kmnLowLevelKeyboardProc(
  _In_  int nCode,
  _In_  WPARAM wParam,
  _In_  LPARAM lParam
) {

  if(nCode < 0) {
    return CallNextHookEx(Globals::get_hhookLowLevelKeyboardProc(), nCode, wParam, lParam);
  }

  SendDebugEntry();

  LowLevelHookWatchDog::HookIsAlive();

  PKBDLLHOOKSTRUCT hs = (PKBDLLHOOKSTRUCT) lParam;

  BOOL extended = hs->flags & LLKHF_EXTENDED ? TRUE : FALSE;
  BOOL isUp = hs->flags & LLKHF_UP ? TRUE : FALSE;

  SendDebugMessageFormat("wparam: %x  lparam: %x [vk:%s scan:%x flags:%x extra:%x]", wParam, lParam, Debug_VirtualKey((WORD) hs->vkCode), hs->scanCode, hs->flags, hs->dwExtraInfo);   // I4674

  // #5190: Don't cache modifier state because sometimes we won't receive
  // modifier change events (e.g. on lock screen)
  FHotkeyShiftState = 0;
  BOOL AllowRightModifierHotkey = FALSE;
  Hotkeys* hotkeys  = Hotkeys::Instance();
  if (hotkeys) {
    AllowRightModifierHotkey = hotkeys->AllowRightModifierHotkey();
  }

  if (GetKeyState(VK_LCONTROL) < 0) {
    FHotkeyShiftState |= HK_CTRL;
  }

  if (GetKeyState(VK_RCONTROL) < 0) {
    FHotkeyShiftState |= AllowRightModifierHotkey ? HK_CTRL : HK_RCTRL_INVALID;
  }

  if (GetKeyState(VK_LMENU) < 0) {
    FHotkeyShiftState |= HK_ALT;
  }

  if (GetKeyState(VK_RMENU) < 0) {
    FHotkeyShiftState |= AllowRightModifierHotkey ? HK_ALT : HK_RALT_INVALID;
  }

  if (GetKeyState(VK_LSHIFT) < 0) {
    FHotkeyShiftState |= HK_SHIFT;
  }

  if (GetKeyState(VK_RSHIFT) < 0) {
    FHotkeyShiftState |= AllowRightModifierHotkey ? HK_SHIFT : HK_RSHIFT_INVALID;
  }

  if(isModifierKey(hs->vkCode)) {
    PostVisualKeyboardModifierEvent(hs->vkCode, LLKHFFlagstoWMKeymanKeyEventFlags(hs));
  }

  // #7337 Post the modifier state ensuring the serialized queue is in sync
  // Note that the modifier key may be posted again with WM_KEYMAN_KEY_EVENT,
  // later in this function. This is intentional, as the WM_KEYMAN_MODIFIER_EVENT
  // message only updates our internal modifier state, and does not do
  // any additional processing or other serialization of the input queue.
  // #8064 Keyman's own injected modifiers must not feed the cache. The release and restore halves
  // of a batch echo back through this hook; a balanced batch cancels out, but when the user
  // releases the key while the batch is in flight the restore press is applied last and the cache
  // is left holding a modifier nobody holds -- which the reconcile cannot detect, because the same
  // press latched the OS and the two now agree. This post is above the pass-through check at the
  // end of this function on purpose, so that modifier state keeps being tracked when no Keyman
  // keyboard is active; the provenance test therefore has to happen here as well.
  if (isModifierKey(hs->vkCode)) {
    // #8064 Trace the cache-feed decision, not every keystroke: 348b5980 removed the old
    // unconditional per-keystroke trace as noise once the issue was believed resolved, but
    // specs/003-8064-debug-log-adequacy/spec.md (R-6) finds the posting-side modifier trace is
    // one of the few signals that actually helps diagnose a stuck modifier. It comes back here
    // scoped to modifier keys only, and records what the old message didn't: whether the event
    // was filtered as Keyman's own, and whether the post actually reached the server.
    BOOL isKeymanInjected = IsKeymanInjectedKeyEvent(hs->scanCode, hs->dwExtraInfo);
    if (flag_ShouldSerializeInput && !isKeymanInjected) {
      // This post is not an eat -- processing continues below regardless of whether it succeeds --
      // so a failed feed here does not destroy input. It does mean the cache can fall out of sync
      // with what actually happened, which is the class of bug #8064 is, so we still guard against
      // a NULL server window (GetWindow() can be NULL during server startup/shutdown; PostMessage
      // to NULL does not fail, it silently misroutes the message to this thread's own queue) and
      // log the outcome so a stale cache has a cause on record instead of being unexplained later.
      ISerialKeyEventServer *server = ISerialKeyEventServer::GetServer();
      HWND hwndServer = server ? server->GetWindow() : NULL;
      if (hwndServer == NULL) {
        SendDebugMessageFormat("Modifier cache feed skipped, no serializer window [vkCode:%x isUp:%d]", hs->vkCode, isUp);
      } else if (!PostMessage(hwndServer, WM_KEYMAN_MODIFIER_EVENT, hs->vkCode, LLKHFFlagstoWMKeymanKeyEventFlags(hs))) {
        SendDebugMessageFormat("Modifier cache feed failed [vkCode:%x isUp:%d] with error %d", hs->vkCode, isUp, GetLastError());
      } else {
        SendDebugMessageFormat("Modifier cache feed posted [vkCode:%x isUp:%d]", hs->vkCode, isUp);
      }
    } else {
      SendDebugMessageFormat("Modifier cache feed skipped [vkCode:%x isUp:%d flag_ShouldSerializeInput:%d isKeymanInjected:%d]",
        hs->vkCode, isUp, flag_ShouldSerializeInput, isKeymanInjected);
    }
  }

  if(IsLanguageSwitchWindowVisible()) {
    SendDebugMessageFormat("Sending to language switch window %x %x", wParam, lParam);
    SendToLanguageSwitchWindow(hs->vkCode, hs->flags);
    if (ProcessLanguageSwitchShiftKey(hs->vkCode, isUp) == 1) {
      return_SendDebugExit(1);
    }
  }
  else if (KeyLanguageSwitchPress(hs->vkCode, extended, isUp, FHotkeyShiftState)) {
    SendDebugMessageFormat("KeyLanguageSwitchPress [vkCode:%x extended:%x isUp:%d FHotkeyShiftState:%x", hs->vkCode, extended, isUp, FHotkeyShiftState);
    if (ProcessLanguageSwitchShiftKey(hs->vkCode, isUp) == 1) {
      return_SendDebugExit(1);
    }
  }

  if (ProcessHotkey(hs->vkCode, isUp, FHotkeyShiftState)) {
    SendDebugMessageFormat("ProcessHotkey [vkCode:%x isUp:%d FHotkeyShiftState:%x", hs->vkCode, isUp, FHotkeyShiftState);
    return_SendDebugExit(1);
  }

  /*

    Not a registered hotkey, so we will use the serialized input model

  */

  if (hs->dwExtraInfo != 0 ||
      hs->scanCode == SCAN_FLAG_KEYMAN_KEY_EVENT ||
      hs->vkCode == VK_PROCESSKEY ||
      hs->vkCode == VK_PACKET ||
      !isKeymanKeyboardActive) {
    // This key event was generated by Keyman, so pass it through
    // dwExtraInfo is set to 0x4321DCBA by mstsc which does prefiltering. So we ignore for anything where dwExtraInfo!=0 because it
    // probably is not hardware generated and may cause more issues to filter it.
    // We also ignore if a Keyman keyboard is not currently active.
    SendDebugMessageFormat("Pass through [dwExtraInfo:%x scancode:%x vkCode:%x, isKeymanKeyboardActive:%d", hs->dwExtraInfo, hs->scanCode, hs->vkCode, isKeymanKeyboardActive);
    return_SendDebugExit(CallNextHookEx(Globals::get_hhookLowLevelKeyboardProc(), nCode, wParam, lParam));
  }

  if (IsTouchPanelVisible()) {
    // See #2450. The touch panel will close automatically if we reprocess key events
    // So we don't want to reprocess events when it is visible.
    SendDebugMessageFormat("touch panel is visible. Not reprocessing keystrokes");
    return_SendDebugExit(CallNextHookEx(Globals::get_hhookLowLevelKeyboardProc(), nCode, wParam, lParam));
  }

  if (flag_ShouldSerializeInput) {
    GUITHREADINFO gui = { 0 };
    gui.cbSize = sizeof(GUITHREADINFO);
    if (GetGUIThreadInfo(NULL, &gui)) {
      SendDebugMessageFormat("Active=%x Focus=%x Key=%s flags=%x",
        gui.hwndActive, gui.hwndFocus, Debug_VirtualKey((WORD)hs->vkCode), LLKHFFlagstoWMKeymanKeyEventFlags(hs));

      HWND hwnd = gui.hwndFocus ? gui.hwndFocus : gui.hwndActive;
      if (!IsConsoleWindow(hwnd)) {
        // #8064 Only eat this event (return 1, so CallNextHookEx below is never reached) once the
        // handoff to the serializer has actually succeeded. Eating unconditionally and trusting
        // PostMessage destroys the user's real key event whenever the handoff fails -- a NULL
        // server window during startup/shutdown, a full posted-message queue, or a stalled client
        // holding KeymanEngine_KeyMutex while ProcessQueuedKeyEvents waits INFINITE on it -- and
        // for a modifier KEYUP that loss is exactly how #8064 re-asserts: the OS keeps the earlier
        // re-injected KEYDOWN latched, the cache still says down, cache and OS agree, and the
        // clear-only reconcile can never see it again. Falling through to CallNextHookEx instead
        // lets the event through natively: an unserialized keystroke is a far smaller defect than
        // a destroyed one.
        ISerialKeyEventServer *server = ISerialKeyEventServer::GetServer();
        HWND hwndServer = server ? server->GetWindow() : NULL;
        if (hwndServer == NULL) {
          SendDebugMessageFormat("Key event not serialized, no serializer window [vkCode:%x isUp:%d]", hs->vkCode, isUp);
        } else if (!PostMessage(hwndServer, WM_KEYMAN_KEY_EVENT, hs->vkCode, LLKHFFlagstoWMKeymanKeyEventFlags(hs))) {
          SendDebugMessageFormat("Failed to post key event, passing through unserialized [vkCode:%x isUp:%d] with error %d",
            hs->vkCode, isUp, GetLastError());
        } else {
          return_SendDebugExit(1);
        }
      }
      //else SendDebugMessageFormat("console window, not serializing"); // too noisy
    }
    else {
      SendDebugMessageFormat("Failed to get Gui thread info with error %d", GetLastError());
    }
  }

  return_SendDebugExit(CallNextHookEx(Globals::get_hhookLowLevelKeyboardProc(), nCode, wParam, lParam));
}

BOOL ProcessHotkey(UINT vkCode, BOOL isUp, DWORD ShiftState) {

  Hotkeys *hotkeys = Hotkeys::Instance();   // I4641
  if (!hotkeys) {
    return FALSE;
  }

  Hotkey *hotkey = hotkeys->GetHotkey(ShiftState | vkCode);   // I4641
  if (!hotkey) {
    return FALSE;
  }

  if (isUp) {
    return TRUE;
  }

  if (hotkey->HotkeyType == hktInterface) {
    Globals::PostMasterController(wm_keyman_control, MAKELONG(KMC_INTERFACEHOTKEY, hotkey->Target), 0);
  }
  else {
    ReportKeyboardChanged(PC_HOTKEYCHANGE, hotkey->hkl == 0 ? TF_PROFILETYPE_INPUTPROCESSOR : TF_PROFILETYPE_KEYBOARDLAYOUT, 0, hotkey->hkl, GUID_NULL, hotkey->profileGUID);
  }
  /* Generate a dummy keystroke to block menu activations, etc but let the shift key through */
  PostDummyKeyEvent();  // I3301 - this is imperfect because we don't deal with HC_NOREMOVE.  But good enough?   // I3534   // I4844

  return TRUE;
}

#endif
