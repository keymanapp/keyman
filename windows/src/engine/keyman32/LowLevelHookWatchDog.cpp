/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-11-17
 *
 * Handle situations where Windows silently uninstalls our low level keyboard
 * hook, and dynamically reinstall it. The hook can be uninstalled when
 * keyman.exe becomes unresponsive for more than 200msec (default timeout) --
 * this can be due to something Keyman is doing, but it could also happen during
 * high system load. The hook will only be uninstalled if a key is pressed while
 * Keyman is unresponsive.
 *
 * Per Windows documentation:
 * https://learn.microsoft.com/en-us/windows/win32/winmsg/lowlevelkeyboardproc#remarks
 *
 * This works by tracking last event times for both the WH_KEYBOARD_LL hook and
 * the WH_GETMESSAGE hook. These two hooks both receive key events, but the
 * WH_GETMESSAGE hook runs in the focused thread context, whereas WH_KEYBOARD_LL
 * runs in keyman.exe main thread context. The WH_KEYBOARD_LL hook receives the
 * key event first.
 *
 * This (static) class is used in the keyman.exe main thread context and the
 * HookIsAlive function in the Low Level Keyboard Hook thread context.
 *
 * Notification of HookIsAlive is a straightforward call from the low level
 * keyboard hook procedure, but the KeyEventReceivedInGetMessageProc function
 * must be signalled across processes. We have chosen to do this with a posted
 * message to the master controller, which is then handled by
 * UfrmKeyman7Main.pas, and passed through Keyman_WatchDogKeyEvent.
 *
 * The message handler was implemented in UfrmKeyman7Main.pas rather than in
 * kmhook_getmessage.cpp, because it appears that kmnGetMessageProc does not see
 * messages that were posted by it or child functions (this is by observation,
 * not documentation -- I was not able to find documentation on this, but
 * surmise it is to prevent deadlock/infinite loop scenarios, which could easily
 * lockup Windows entirely).
 *
 * No key data is passed in the event, only the information that a key was
 * pressed.
 */

#include "pch.h"

#ifndef _WIN64

/**
 * minimum number of milliseconds between the last LowLevel and GetMessage
 * events before we assume the low level hook has been uninstalled, and we need
 * to reinstall it.
 */
#define WATCHDOG_THRESHOLD  5000

// LastLowLevelEventTick is updated in TV_LLKH and checked in TV_MAIN so all
// access must be interlocked; as it is 64 bit, this guarantees atomicity
static volatile __declspec(align(8)) ULONGLONG LastLowLevelEventTick = 0;

// LastGetMessageEventTick is only ever checked or updated in TV_MAIN (no
// interlocked access required)
static ULONGLONG LastGetMessageEventTick = 0;

void LowLevelHookWatchDog::HookIsAlive() {
  KeymanThreadSafety_assertThread(TV_LLKH);
  InterlockedExchange64(reinterpret_cast<volatile LONG64*>(&LastLowLevelEventTick), GetTickCount64());
}

void LowLevelHookWatchDog::KeyEventReceivedInGetMessageProc() {
  KeymanThreadSafety_assertThread(TV_MAIN);
  LastGetMessageEventTick = GetTickCount64();
  ULONGLONG l_LastLowLevelEventTick = InterlockedExchangeAdd64(reinterpret_cast<volatile LONG64*>(&LastLowLevelEventTick), 0);

  // This is a good place to check if we are still alive -- shortly after each
  // keystroke event in the GetMessage hook, as this means at worst we'll have
  // one or two keystrokes where Keyman must recover
  if(!CheckIfHookIsAlive(l_LastLowLevelEventTick)) {
    ReinstallHook(l_LastLowLevelEventTick);
  }
}

bool LowLevelHookWatchDog::CheckIfHookIsAlive(ULONGLONG l_LastLowLevelEventTick) {
  KeymanThreadSafety_assertThread(TV_MAIN);
  if (LastGetMessageEventTick < l_LastLowLevelEventTick) {
    // this shouldn't be possible but rather safe than sorry
    return true;
  }

  return LastGetMessageEventTick - l_LastLowLevelEventTick < WATCHDOG_THRESHOLD;
}

void LowLevelHookWatchDog::ReinstallHook(ULONGLONG l_LastLowLevelEventTick) {
  KeymanThreadSafety_assertThread(TV_MAIN);

  //keyman32.cpp:
  SendDebugMessageFormat(
    "Watchdog threshold exceeded at %llu msec (last LL=%llu last GM=%llu)",
    LastGetMessageEventTick - l_LastLowLevelEventTick,
    l_LastLowLevelEventTick,
    LastGetMessageEventTick
  );

  // The hook will be restarted when it receives the event

  // NOTE: RequestRestart is currently disabled for #17004 and will be
  // re-enabled if reporting shows 1404 errors from attempting to unhook.

  // KeymanLowLevelKeyboardHook::RequestRestart();

  Globals::PostMasterController(wm_keyman_control, MAKELONG(KMC_WATCHDOG_HOOK_REINSTALL, WHR_TIMING), (LPARAM)(LastGetMessageEventTick - l_LastLowLevelEventTick));

  // We should assume the hook is alive at this point to avoid repeated events
  HookIsAlive();
}

extern "C" void __declspec(dllexport) WINAPI Keyman_WatchDogKeyEvent() {
  SendDebugMessageFormat("Keyman_WatchDogKeyEvent");
  LowLevelHookWatchDog::KeyEventReceivedInGetMessageProc();
}

#endif
