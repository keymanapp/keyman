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
#include <Psapi.h>

// This file is used only in keyman32.dll; it implements our low level keyboard hook
// in the main keyman.exe process for hotkeys, serial key event server
#ifndef _WIN64

static DWORD WINAPI ServerThreadProc(_In_ LPVOID lpParameter);

BOOL KeymanLowLevelKeyboardHook::m_TouchPanelVisible = FALSE;
HHOOK KeymanLowLevelKeyboardHook::m_hhook = 0;
DWORD KeymanLowLevelKeyboardHook::m_idThread = 0;
BOOL KeymanLowLevelKeyboardHook::m_InLanguageSwitchKeySequence = FALSE;
HWND KeymanLowLevelKeyboardHook::m_hwndLanguageSwitch = 0;
Hotkeys* KeymanLowLevelKeyboardHook::m_hotkeys = NULL;
HANDLE KeymanLowLevelKeyboardHook::m_hReloadEvent              = 0;
HANDLE KeymanLowLevelKeyboardHook::m_hResetEvent               = 0;
HANDLE KeymanLowLevelKeyboardHook::m_hThread                   = 0;
HANDLE KeymanLowLevelKeyboardHook::m_hThreadExitEvent          = 0;

/*
 Test for touch panel visibility (#2450). UpdateTouchPanelVisibility is called periodically by
 keyman.exe to refresh the visibility flag.
*/

void WINAPI Keyman_UpdateTouchPanelVisibility(BOOL isVisible) {
  KeymanThreadSafety_assertThread(TV_MAIN);
  KeymanLowLevelKeyboardHook::SetTouchPanelVisible(isVisible);
}

/// PUBLIC METHODS

BOOL KeymanLowLevelKeyboardHook::Startup() {
  KeymanThreadSafety_assertThread(TV_MAIN);

  if (m_hThread != NULL) {
    SendDebugMessage("Startup: Low level hook is already running!");
    return FALSE;
  }

  m_hThreadExitEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
  if (!m_hThreadExitEvent) {
    DebugLastError("CreateEvent");
    return FALSE;
  }

  m_hReloadEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
  if (!m_hReloadEvent) {
    DebugLastError("CreateEvent");
    return FALSE;
  }

  m_hResetEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
  if (!m_hResetEvent) {
    DebugLastError("CreateEvent");
    return FALSE;
  }

  m_hThread = CreateThread(NULL, 0, ServerThreadProc, NULL, 0, &m_idThread);
  if (!m_hThread) {
    DebugLastError("CreateThread");
    return FALSE;
  }

  return TRUE;
}

BOOL KeymanLowLevelKeyboardHook::Shutdown() {
  KeymanThreadSafety_assertThread(TV_MAIN);

  if (m_hThreadExitEvent != NULL) {
    if (!SetEvent(m_hThreadExitEvent)) {
      DebugLastError("SetEvent");
    }

    if (m_hThread != NULL) {
      // Wait for the thread to terminate so we know that we'll not receive
      // additional events after this object is destroyed
      if (WaitForSingleObject(m_hThread, 5000) != WAIT_OBJECT_0) {
        DebugLastError("WaitForSingleObject(m_hThread)");
      }

      if (!CloseHandle(m_hThread)) {
        DebugLastError("CloseHandle(m_hThread)");
      }
      m_hThread = NULL;
    }

    if (!CloseHandle(m_hThreadExitEvent)) {
      DebugLastError("CloseHandle(m_hThreadExitEvent)");
    }
    m_hThreadExitEvent = NULL;
  }

  if (m_hReloadEvent != NULL) {
    if (!CloseHandle(m_hReloadEvent)) {
      DebugLastError("CloseHandle(m_hReloadEvent)");
    }
    m_hReloadEvent = NULL;
  }

  if (m_hResetEvent != NULL) {
    if (!CloseHandle(m_hResetEvent)) {
      DebugLastError("CloseHandle(m_hResetEvent)");
    }
    m_hResetEvent = NULL;
  }

  return TRUE;
}

/**
 * Tell the low level keyboard hook thread to reload its hotkey list, after a
 * configuration change.
 */
void KeymanLowLevelKeyboardHook::RequestReloadHotkeys() {
  KeymanThreadSafety_assertThread(TV_MAIN);
  SetEvent(m_hReloadEvent);
}

/**
 * Tell the low level keyboard hook thread to restart the low level keyboard
 * hook, e.g. if it was silently unloaded. Thread cleanup will report if an
 * error was received unhooking, which would indicate that the silent unload has
 * happened
 */
void KeymanLowLevelKeyboardHook::RequestRestart() {
  KeymanThreadSafety_assertThread(TV_MAIN);
  SetEvent(m_hResetEvent);
}

/**
 * Update the visiblity state of the Windows touch input panel. This is thread
 * safe because the variable is atomically updated. The potential race between
 * showing the touch panel and the next keystroke is considered to be low risk.
 */
void KeymanLowLevelKeyboardHook::SetTouchPanelVisible(BOOL isVisible) {
  KeymanThreadSafety_assertThread(TV_MAIN);
  m_TouchPanelVisible = isVisible;
  SendDebugMessageFormat("m_TouchPanelVisible=%d", m_TouchPanelVisible);
}


//////////////////////////////////////////////////////
// Local thread
//////////////////////////////////////////////////////

/**
 * Stub callback thread procedure
 */
static DWORD WINAPI ServerThreadProc(_In_ LPVOID lpParameter) {
  UNREFERENCED_PARAMETER(lpParameter);
  return KeymanLowLevelKeyboardHook::ThreadMain();
}

/**
 * Stub callback hook procedure
 */
LRESULT CALLBACK kmnLowLevelKeyboardProc(_In_ int nCode, _In_ WPARAM wParam, _In_ LPARAM lParam) {
  LRESULT res = 0;

#ifdef _DEBUG_EXCEPTION
  res = KeymanLowLevelKeyboardHook::HookProc(nCode, wParam, lParam);
#else
  __try {
    res = KeymanLowLevelKeyboardHook::HookProc(nCode, wParam, lParam);
  } __except (ExceptionMessage("kmnLowLevelKeyboardProc", GetExceptionInformation())) {
  }
#endif
  return res;
}


/**
 *   Thread main procedure
 */
DWORD KeymanLowLevelKeyboardHook::ThreadMain() {
  KeymanThreadSafety::setThreadId(TV_LLKH, m_idThread);

  if (!InitThread()) {
    return 1;
  }

  MessageLoop();

  CleanupThread();

  return 0;
}

/**
 * Initialize thread and instanatiate the WH_KEYBOARD_LL hook
 */
BOOL KeymanLowLevelKeyboardHook::InitThread() {
  KeymanThreadSafety_assertThread(TV_LLKH);

  m_hotkeys = new Hotkeys();
  m_hotkeys->Load();

  HINSTANCE hinst = GetModuleHandle(LIBRARY_NAME);
  m_hhook = SetWindowsHookExW(WH_KEYBOARD_LL, (HOOKPROC)kmnLowLevelKeyboardProc, hinst, Globals::get_FSingleThread());  // I4124
  SendDebugMessageFormat("WH_KEYBOARD_LL=%x", m_hhook);

  return m_hhook != NULL;
}

/**
 * Unhook WH_KEYBOARD_LL hook, reporting on failure, and cleanup
 */
void KeymanLowLevelKeyboardHook::CleanupThread() {
  KeymanThreadSafety_assertThread(TV_LLKH);
  if (m_hhook != NULL) {
    if(!UnhookWindowsHookEx(m_hhook)) {  // I4124
      DWORD err=GetLastError();
      WCHAR msg[128];
      wsprintfW(msg, L"Unhook failed with %d", err);
      LogDiagReportToRegistry(__WFUNCTION__, FALSE, msg);
    } else {
      LogDiagReportToRegistry(__WFUNCTION__, TRUE, L"Unhook succeeded");
    }

    m_hhook = NULL;
  }

  delete m_hotkeys;
  m_hotkeys = NULL;
}

/**
 * Main message loop for thread. Terminates on error or when
 * m_hThreadExitEvent is signaled. Sleeps until a window message
 * is received.
 */
void KeymanLowLevelKeyboardHook::MessageLoop() {
  KeymanThreadSafety_assertThread(TV_LLKH);
  HANDLE events[3] = {m_hThreadExitEvent, m_hReloadEvent, m_hResetEvent};
  while (TRUE) {
    switch (MsgWaitForMultipleObjectsEx(3, events, INFINITE, QS_ALLINPUT, 0)) {
    case WAIT_OBJECT_0:      // m_hThreadExitEvent signaled
      // Thread has been signalled, return
      return;
    case WAIT_OBJECT_0 + 1:  // m_hReloadEvent signaled
      m_hotkeys->Load();
      break;
    case WAIT_OBJECT_0 + 2:  // m_hResetEvent signaled
      SendDebugMessage("m_hResetEvent set");
      CleanupThread();
      if(!InitThread()) {
        return;
      }
      break;
    case WAIT_OBJECT_0 + 3:  // Windows message received
      MSG msg;
      while (PeekMessage(&msg, NULL, NULL, NULL, PM_REMOVE)) {
        DispatchMessage(&msg);
      }
      break;
    default:
      DebugLastError("MsgWaitForMultipleObjectsEx");
      return;
    }
  }
}

/**
 * WH_KEYBOARD_LL hook procedure
 */
LRESULT KeymanLowLevelKeyboardHook::HookProc(
  _In_  int nCode,
  _In_  WPARAM wParam,
  _In_  LPARAM lParam
) {
  KeymanThreadSafety_assertThread(TV_LLKH);

  if(nCode < 0) {
    return CallNextHookEx(m_hhook, nCode, wParam, lParam);
  }

  SendDebugEntry();

  LowLevelHookWatchDog::HookIsAlive();

  PKBDLLHOOKSTRUCT const hs = reinterpret_cast<PKBDLLHOOKSTRUCT>(lParam);

  BOOL const extended  = hs->flags & LLKHF_EXTENDED ? TRUE : FALSE;
  BOOL const isUp      = hs->flags & LLKHF_UP ? TRUE : FALSE;

  // #5190: Don't cache modifier state because sometimes we won't receive
  // modifier change events (e.g. on lock screen)
  DWORD const hotkeyShiftState = GetCurrentModifierKeyState();

  SendDebugMessageFormat("message:%x  vkCode:%s (%x)  scanCode:%x  flags:%08.8x (extended=%d isUp=%d) dwExtraInfo:%x  hotkeyShiftState:%x",
    wParam /* wParam is the keyboard message WM_[SYS]KEY(UP|DOWN) */,
    Debug_VirtualKey((WORD) hs->vkCode), hs->vkCode, hs->scanCode, hs->flags, extended, isUp,
    hs->dwExtraInfo, hotkeyShiftState);

  if(IsModifierKey(hs->vkCode)) {
    PostVisualKeyboardModifierEvent(hs->vkCode, LLKHFFlagstoWMKeymanKeyEventFlags(hs));

    // #7337 Post the modifier state ensuring the serialized queue is in sync
    // Note that the modifier key may be posted again with WM_KEYMAN_KEY_EVENT,
    // later in this function. This is intentional, as the WM_KEYMAN_MODIFIER_EVENT
    // message only updates our internal modifier state, and does not do
    // any additional processing or other serialization of the input queue.
    if (flag_ShouldSerializeInput) {
      SendDebugMessage("posting modifier to serial key event server");
      PostMessageToSerialKeyEventServer(WM_KEYMAN_MODIFIER_EVENT, hs->vkCode, LLKHFFlagstoWMKeymanKeyEventFlags(hs));
    }
  }

  // If the foreground app is a remote desktop app (from our list of known apps)
  // then we do not want to do any serialization of input nor do we want our
  // hotkeys to be triggered. Note that updating the OSK and the serialized input
  // server modifier state are required even in this context

  if (ForegroundWindowIsRemoteDesktop()) {
    return_SendDebugExit(CallNextHookEx(m_hhook, nCode, wParam, lParam));
  }

  if(LanguageSwitch_IsWindowVisible()) {
    SendDebugMessage("posting key event to language switch window");
    LanguageSwitch_PostToWindow(hs->vkCode, hs->flags);
    if (LanguageSwitch_ProcessModifierKey(hs->vkCode, isUp) == 1) {
      // Swallow the key event
      return_SendDebugExit(1);
    }
  }
  else if (LanguageSwitch_KeyPress(hs->vkCode, extended, isUp, hotkeyShiftState)) {
    SendDebugMessage("language switch hotkey pressed");
    if (LanguageSwitch_ProcessModifierKey(hs->vkCode, isUp) == 1) {
      // Swallow the key event
      return_SendDebugExit(1);
    }
  }

  if (ProcessHotkey(hs->vkCode, isUp, hotkeyShiftState)) {
    SendDebugMessage("general hotkey pressed");
    // Swallow the key event
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
    SendDebugMessageFormat("pass through; isKeymanKeyboardActive:%d", isKeymanKeyboardActive);
    return_SendDebugExit(CallNextHookEx(m_hhook, nCode, wParam, lParam));
  }

  if (m_TouchPanelVisible) {
    // Note: GetCurrentInputMessageSource does not work in this context
    // Using IFrameworkInputPaneHandler events only works for a specific window, so not helpful for us.

    // See #2450. The touch panel will close automatically if we reprocess key events
    // So we don't want to reprocess events when it is visible.
    SendDebugMessageFormat("touch panel is visible. Not reprocessing keystrokes");
    return_SendDebugExit(CallNextHookEx(m_hhook, nCode, wParam, lParam));
  }

  if (flag_ShouldSerializeInput) {
    GUITHREADINFO gui = { 0 };
    gui.cbSize = sizeof(GUITHREADINFO);
    if (GetGUIThreadInfo(NULL, &gui)) {
      SendDebugMessageFormat("hwndActive=%x hwndFocus=%x", gui.hwndActive, gui.hwndFocus);

      HWND hwnd = gui.hwndFocus ? gui.hwndFocus : gui.hwndActive;
      if (!IsConsoleWindow(hwnd)) {
        if(PostMessageToSerialKeyEventServer(WM_KEYMAN_KEY_EVENT, hs->vkCode, LLKHFFlagstoWMKeymanKeyEventFlags(hs))) {
          // Swallow the key event
          return_SendDebugExit(1);
        }
      }
      //else SendDebugMessageFormat("console window, not serializing"); // too noisy
    } else {
      SendDebugMessageFormat("Failed to get Gui thread info with error %d", GetLastError());
    }
  }

  return_SendDebugExit(CallNextHookEx(m_hhook, nCode, wParam, lParam));
}

/// PRIVATE METHODS

DWORD KeymanLowLevelKeyboardHook::GetCurrentModifierKeyState() {
  KeymanThreadSafety_assertThread(TV_LLKH);

  DWORD hotkeyShiftState = 0;
  BOOL const allowRightModifierHotkey = m_hotkeys->AllowRightModifierHotkey();

  if (GetKeyState(VK_LCONTROL) < 0) {
    hotkeyShiftState |= HK_CTRL;
  }

  if (GetKeyState(VK_RCONTROL) < 0) {
    hotkeyShiftState |= allowRightModifierHotkey ? HK_CTRL : HK_RCTRL_INVALID;
  }

  if (GetKeyState(VK_LMENU) < 0) {
    hotkeyShiftState |= HK_ALT;
  }

  if (GetKeyState(VK_RMENU) < 0) {
    hotkeyShiftState |= allowRightModifierHotkey ? HK_ALT : HK_RALT_INVALID;
  }

  if (GetKeyState(VK_LSHIFT) < 0) {
    hotkeyShiftState |= HK_SHIFT;
  }

  if (GetKeyState(VK_RSHIFT) < 0) {
    hotkeyShiftState |= allowRightModifierHotkey ? HK_SHIFT : HK_RSHIFT_INVALID;
  }

  return hotkeyShiftState;
}

BOOL KeymanLowLevelKeyboardHook::PostMessageToSerialKeyEventServer(UINT msg, WPARAM wParam, LPARAM lParam) {
  KeymanThreadSafety_assertThread(TV_LLKH);

  // Thread safety: we rely on ISerialKeyEventServer to be running; this
  // contract is fulfilled in Keyman_Initialise.

  ISerialKeyEventServer const* server = ISerialKeyEventServer::GetServer();
  if (!server) {
    SendDebugMessage("SerialKeyEventServer is unexpectedly NULL");
    return FALSE;
  }

  HWND hwnd = server->GetWindow();
  if (!hwnd) {
    SendDebugMessage("SerialKeyEventServer->GetWindow() is unexpectedly NULL");
    return FALSE;
  }

  if(!PostMessage(hwnd, msg, wParam, lParam)) {
    DebugLastError("PostMessage");
    return FALSE;
  }

  return TRUE;
}


BOOL KeymanLowLevelKeyboardHook::ProcessHotkey(UINT vkCode, BOOL isUp, DWORD ShiftState) {
  KeymanThreadSafety_assertThread(TV_LLKH);

  Hotkey *hotkey = m_hotkeys->GetHotkey(ShiftState | vkCode);   // I4641
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

// ---------------------------------------------------------------------------
// Utility functions
// ---------------------------------------------------------------------------

BOOL KeymanLowLevelKeyboardHook::IsModifierKey(DWORD vkCode) {
  KeymanThreadSafety_assertThread(TV_ANY);

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

LPARAM KeymanLowLevelKeyboardHook::LLKHFFlagstoWMKeymanKeyEventFlags(PKBDLLHOOKSTRUCT hs) {
  KeymanThreadSafety_assertThread(TV_ANY);

  return (hs->scanCode << 16) |
    ((hs->flags & LLKHF_EXTENDED) ? KEYEVENTF_EXTENDEDKEY : 0) |
    ((hs->flags & LLKHF_UP) ? KEYEVENTF_KEYUP : 0);
}

/**
 * Check if the window handle is for a console window
 *
 * We don't attempt to serialize input to the console windows because they
 * behave somewhat differently to normal windows. For now, this should be
 * sufficient. In the future, we may want to find a way to interrogate the
 * focused process to find out which window actually has focus for posting
 * messages, because we appear to post the messages to the wrong thread for
 * console windows.
 */
BOOL KeymanLowLevelKeyboardHook::IsConsoleWindow(HWND hwnd) {
  KeymanThreadSafety_assertThread(TV_LLKH);

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

/**
 * Check if the current foreground window is a recognized remote desktop window,
 * e.g. RDP Client, Hyper-V Virtual Machine Connection. Caches the result to
 * avoid repeated process querying.
 */
BOOL KeymanLowLevelKeyboardHook::ForegroundWindowIsRemoteDesktop() {  // I4197
  KeymanThreadSafety_assertThread(TV_LLKH);

  DWORD dwProcessId;
  HWND hwnd = GetForegroundWindow();
  if (hwnd == NULL) {
    return FALSE;
  }

  if (GetWindowThreadProcessId(hwnd, &dwProcessId) == 0) {
    return FALSE;
  }

  static DWORD dwLastProcessId = 0;
  static BOOL bLastResult = FALSE;

  if(dwLastProcessId == dwProcessId) {
    return bLastResult;
  }

  dwLastProcessId = dwProcessId;

  HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION, false, dwProcessId);
  if (hProcess == NULL) {
    return bLastResult = FALSE;
  }

  char buf[260];
  BOOL bResult = GetProcessImageFileName(hProcess, buf, _countof(buf)) > 0;

  CloseHandle(hProcess);

  if (bResult) {
    char* p = strrchr(buf, '\\');
    if(p) {
      bResult =
        // This will only work for non-web-based clients
        // TODO: add vnc clients, others? this
        _stricmp(p, "\\mstsc.exe") == 0 ||    // mstsc - Microsoft Remote Desktop Client
        _stricmp(p, "\\vmconnect.exe") == 0;  // vmconnect - Microsoft Virtual Machine Connection
    }
  }

  return bLastResult = bResult;
}

// ---------------------------------------------------------------------------
// Language Switch Menu
// ---------------------------------------------------------------------------

BOOL KeymanLowLevelKeyboardHook::LanguageSwitch_KeyPress(WPARAM wParam, BOOL extended, BOOL isUp, DWORD ShiftState) {
  KeymanThreadSafety_assertThread(TV_LLKH);

  /* This is a special-case hotkey for language switching - ALT+LEFT SHIFT */

  Hotkey* hotkey = m_hotkeys->GetHotkey(HK_ALT | HK_SHIFT);  // I4641
  if (!hotkey) {
    hotkey = m_hotkeys->GetHotkey(HK_CTRL | HK_SHIFT);  // I4641
    if (!hotkey)
      return FALSE;
  }

  if ((hotkey->HotkeyValue & HK_CTRL) && (wParam == VK_CONTROL || wParam == VK_LCONTROL || wParam == VK_RCONTROL) && !isUp &&
      (ShiftState & ~(HK_CTRL | HK_RCTRL_INVALID)) == 0)  // I4124
  {
    m_InLanguageSwitchKeySequence = TRUE;
    return FALSE;
  } else if (
      (hotkey->HotkeyValue & HK_ALT) && (wParam == VK_MENU || wParam == VK_LMENU || wParam == VK_RMENU) && !extended && !isUp &&
      (ShiftState & ~HK_ALT) == 0)  // I4124
  {
    m_InLanguageSwitchKeySequence = TRUE;
    return FALSE;
  } else if (m_InLanguageSwitchKeySequence) {
    if (wParam == VK_SHIFT || wParam == VK_LSHIFT || wParam == VK_RSHIFT)  // I4124
    {
      if (isUp) {
        m_InLanguageSwitchKeySequence = FALSE;
        // ReportActiveKeyboard(_td, PC_UPDATE);
        Globals::PostMasterController(wm_keyman_control, MAKELONG(KMC_INTERFACEHOTKEY, hotkey->Target), 0);
        PostDummyKeyEvent();                                                    // I4124   // I4844
        keybd_event(VK_SHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, 0);  // I4203
        return TRUE;
      }

      return FALSE;
    } else
      m_InLanguageSwitchKeySequence = FALSE;
    return FALSE;
  }

  return FALSE;
}

BOOL KeymanLowLevelKeyboardHook::LanguageSwitch_IsWindowVisible() {
  KeymanThreadSafety_assertThread(TV_LLKH);

  m_hwndLanguageSwitch = FindWindow("TfrmLanguageSwitch", NULL);
  if (m_hwndLanguageSwitch == NULL)
    return FALSE;
  return IsWindowVisible(m_hwndLanguageSwitch);
}

void KeymanLowLevelKeyboardHook::LanguageSwitch_PostToWindow(WPARAM wParam, LPARAM lParam) {
  KeymanThreadSafety_assertThread(TV_LLKH);

  DWORD keyFlags = (DWORD)lParam;                                                                             // I4124
  PostMessage(m_hwndLanguageSwitch, wm_keyman_control, keyFlags & LLKHF_UP ? KMC_KEYUP : KMC_KEYDOWN, wParam);  // I4124
}

int KeymanLowLevelKeyboardHook::LanguageSwitch_ProcessModifierKey(WPARAM wParam, BOOL isUp) {
  KeymanThreadSafety_assertThread(TV_LLKH);

  if (!isUp)
    return 1;

  /* When a ctrl, shift or alt is released we want to know about it, so we can update shift states etc */

  switch (wParam)  // I4548
  {
  case VK_LSHIFT:
  case VK_RSHIFT:
  case VK_SHIFT:
  case VK_CONTROL:
  case VK_LCONTROL:  // I4124
  case VK_RCONTROL:  // I4124
  case VK_MENU:
  case VK_LMENU:  // I4124
  case VK_RMENU:  // I4124
    break;
  default:
    return 1;
  }

  /* Generate a dummy keystroke to block menu activations, etc but let the shift key through */
  PostDummyKeyEvent();  // I3301 - this is imperfect because we don't deal with HC_NOREMOVE.  But good enough?   // I3534   //
                        // I4844
  return 0;
}


#endif
