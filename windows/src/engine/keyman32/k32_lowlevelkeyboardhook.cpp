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

/*
  #8064 What the modifier cache feed did with a modifier event, traced at a volume proportional to
  how interesting the answer is.

  348b5980 removed this hook's previous #8064 tracing as noise, because the hook runs on every
  keystroke, and that reasoning still holds: a line on every Shift press buries the log TRIAGE.md
  sends a responder to read. Silence is not available either -- a feed that has quietly stopped
  feeding is the #8064 class of bug, and a silent log cannot tell a healthy feed from one that never
  ran.

  So what is traced is the DECISION, and a decision the log already implies is dropped. Two rules,
  and between them they keep the property that matters: for any modifier event in the "wparam:"
  trace above, the log still says what the feed did with it.

    - Keyman's own modifiers are announced once and then not again. They interleave with the user's
      on every capital letter, so tracing each one would trace the alternation rather than the
      decision -- and this is the one outcome the log can already answer unaided, since
      IsKeymanInjectedKeyEvent reads nothing but the scan code and dwExtraInfo that the "wparam:"
      line prints for every event.
    - Every other decision is traced when it CHANGES. Its inputs are a process-wide flag read once
      at startup and whether the serializer window is there, so it is near-constant while a user
      types: the first user modifier event of the session names it, and a Shift held across twenty
      keystrokes adds nothing. Each line therefore speaks for every user modifier event after it
      until the next one, and says so, because a responder who reads one "posted" line as one post
      has been misled by the compression.

  The two anomaly decisions are exempt and trace every time: a feed that fails two hundred times is
  a different fault from one that fails once, and neither is ordinary typing. Each also re-arms the
  memo, so the recovery gets its own line and the claim above stays true.

  Both memos are function-local statics, like IsConsoleWindow's above and for the same reason: a
  WH_KEYBOARD_LL proc only ever runs on the thread that installed the hook, so they need no
  synchronisation and are shared with nothing.
*/
enum ModifierCacheFeedDecision {
  FeedNotYetTraced = 0,         // the memo's starting value, never traced
  FeedPosted,                   // handed to the serializer
  FeedSkippedSerializationOff,  // flag_ShouldSerializeInput is off, so nothing consumes the cache
  FeedSkippedKeymanInjected,    // Keyman's own modifier, deliberately not fed (#8064)
  FeedNoServerWindow,           // anomaly: nothing to post to
  FeedPostFailed,               // anomaly: PostMessage refused it
};

static void TraceModifierCacheFeedDecision(ModifierCacheFeedDecision decision, DWORD vkCode, BOOL isUp, DWORD error) {
  // Announced once, for the reason in the second bullet above, and kept out of the memo below so
  // that it cannot make the user's stream look as though it were changing its mind.
  static BOOL reportedKeymanInjected = FALSE;
  if (decision == FeedSkippedKeymanInjected) {
    if (!reportedKeymanInjected) {
      reportedKeymanInjected = TRUE;
      SendDebugMessageFormat("Modifier cache feed skipped, Keyman's own modifier [vkCode:%x isUp:%d]; every later one is skipped too and not traced again", vkCode, isUp);
    }
    return;
  }

  static ModifierCacheFeedDecision lastDecision = FeedNotYetTraced;
  BOOL isAnomaly = decision == FeedNoServerWindow || decision == FeedPostFailed;
  if (decision == lastDecision && !isAnomaly) {
    return;
  }
  lastDecision = decision;

  // Spelled out in full rather than composed from parts: TRIAGE.md sends a responder to grep
  // "Modifier cache feed posted/failed/skipped", so whoever edits either side has to be able to
  // find these strings in the source.
  switch (decision) {
  case FeedPosted:
    SendDebugMessageFormat("Modifier cache feed posted [vkCode:%x isUp:%d], and for every user modifier event after it until the next feed line", vkCode, isUp);
    break;
  case FeedSkippedSerializationOff:
    SendDebugMessageFormat("Modifier cache feed skipped, input serialization is off [vkCode:%x isUp:%d], and for every modifier event after it until the next feed line", vkCode, isUp);
    break;
  case FeedNoServerWindow:
    SendDebugMessageFormat("Modifier cache feed skipped, no serializer window [vkCode:%x isUp:%d]", vkCode, isUp);
    break;
  case FeedPostFailed:
    SendDebugMessageFormat("Modifier cache feed failed [vkCode:%x isUp:%d] with error %d", vkCode, isUp, error);
    break;
  case FeedSkippedKeymanInjected:  // returned above, and never reaches the memo
  case FeedNotYetTraced:           // the memo's starting value, never traced
    break;
  }
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
    // #8064. Provenance travels with the event, because the receiver cannot recover it. The visual
    // keyboard uses this to tell a modifier the USER is holding from one Keyman injected, and the
    // scan code alone cannot answer that for Right Shift -- see
    // KEYMAN_OSK_MODIFIER_FLAG_KEYMAN_INJECTED. Same decision function as the cache feed below, so
    // the two cannot drift apart.
    DWORD oskFlags = (DWORD)LLKHFFlagstoWMKeymanKeyEventFlags(hs);
    if (IsKeymanInjectedKeyEvent(hs->scanCode, hs->dwExtraInfo)) {
      oskFlags |= KEYMAN_OSK_MODIFIER_FLAG_KEYMAN_INJECTED;
    }
    PostVisualKeyboardModifierEvent(hs->vkCode, oskFlags);
  }

  // #7337 Post the modifier state ensuring the serialized queue is in sync
  // Note that the modifier key may be posted again with WM_KEYMAN_KEY_EVENT,
  // later in this function. This is intentional, as the WM_KEYMAN_MODIFIER_EVENT
  // message only updates our internal modifier state, and does not do
  // any additional processing or other serialization of the input queue.
  // #8064 Keyman's own injected modifiers must not feed the cache: a batch's restore press can
  // outlive the user's release, leaving the cache holding a modifier nobody holds -- undetectable
  // by the reconcile, because the OS agrees. Tested here, not at the pass-through check below,
  // which this post precedes on purpose so modifiers are tracked with no Keyman keyboard active.
  if (isModifierKey(hs->vkCode)) {
    // #8064 The decision itself lives in ShouldFeedModifierCache (keybd_shift.cpp) so the suite can
    // reach it; the hook keeps the server lookup, the post and the trace, which need hook state the
    // suite has no way to stand up. How loudly each outcome is said belongs to
    // TraceModifierCacheFeedDecision -- see its comment; the calls below only name which one it was.
    if (!ShouldFeedModifierCache(flag_ShouldSerializeInput, hs->scanCode, hs->dwExtraInfo)) {
      // Which of the two terms refused it, without asking IsKeymanInjectedKeyEvent a second time:
      // the predicate is serializeInput && !injected, so with serialization on, "Keyman's own" is
      // the only way to arrive here. Worth naming apart, because a physical modifier misread as
      // injected would silently stop being tracked, and that is #8064 in miniature.
      TraceModifierCacheFeedDecision(
        flag_ShouldSerializeInput ? FeedSkippedKeymanInjected : FeedSkippedSerializationOff,
        hs->vkCode, isUp, 0);
    } else {
      // Not an eat: processing continues either way, so a failed feed costs sync, not input. Still
      // guarded and traced, because PostMessage to a NULL hwnd does not fail -- it misroutes to
      // this thread's own queue -- and a stale cache is exactly the #8064 class of bug.
      ISerialKeyEventServer *server = ISerialKeyEventServer::GetServer();
      HWND hwndServer = server ? server->GetWindow() : NULL;
      if (hwndServer == NULL) {
        TraceModifierCacheFeedDecision(FeedNoServerWindow, hs->vkCode, isUp, 0);
      } else if (!PostMessage(hwndServer, WM_KEYMAN_MODIFIER_EVENT, hs->vkCode, LLKHFFlagstoWMKeymanKeyEventFlags(hs))) {
        TraceModifierCacheFeedDecision(FeedPostFailed, hs->vkCode, isUp, GetLastError());
      } else {
        TraceModifierCacheFeedDecision(FeedPosted, hs->vkCode, isUp, 0);
      }
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
        // #8064 Only eat the event (return 1) once the handoff has actually succeeded. Eating on
        // trust destroys the user's key event whenever PostMessage fails, and for a modifier KEYUP
        // that is how #8064 re-asserts: the OS stays latched, the cache still says down, and the
        // clear-only reconcile can never see it. Unserialized beats destroyed.
        // The post and the eat decision live in PostKeyEventAndDecideEat (keybd_shift.cpp) so the
        // suite can reach them; this file keeps the server lookup and the logging, which need hook
        // state the suite has no way to stand up.
        ISerialKeyEventServer *server = ISerialKeyEventServer::GetServer();
        HWND hwndServer = server ? server->GetWindow() : NULL;
        if (PostKeyEventAndDecideEat(hwndServer, hs->vkCode, LLKHFFlagstoWMKeymanKeyEventFlags(hs), PostMessage)) {
          return_SendDebugExit(1);
        }
        if (hwndServer == NULL) {
          SendDebugMessageFormat("Key event not serialized, no serializer window [vkCode:%x isUp:%d]", hs->vkCode, isUp);
        } else {
          SendDebugMessageFormat("Failed to post key event, passing through unserialized [vkCode:%x isUp:%d] with error %d",
            hs->vkCode, isUp, GetLastError());
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
