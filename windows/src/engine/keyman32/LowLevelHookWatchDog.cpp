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
 * This (static) class is only used in the keyman.exe main thread context.
 *
 * Notification of HookIsAlive is a straightforward call from the low level
 * keyboard hook procedure, but the KeyEventReceivedInGetMessageProc function
 * must be signalled across processes. We have chosen to do this with a posted
 * message to the master controller, which is then handled by
 * UfrmKeyman7Main.pas, and passed through Keyman_WatchDogKeyEvent.
 *
 * The message handler was implemented in UfrmKeyman7Main.pas rather than in
 * kmhook_getmessage.cpp, because it appears that kmnGetMessageProc does not see
 * messages that were posted by it or child functions (this is by observation, not
 * documentation -- I was not able to find documentation on this, but surmise it
 * is to prevent deadlock/infinite loop scenarios, which could easily lockup
 * Windows entirely).
 *
 * No key data is passed in the event, only the information that a key was
 * pressed.
 */

#include "pch.h"

/**
 * minimum number of milliseconds between the last LowLevel and GetMessage
 * events before we assume the low level hook has been uninstalled, and we need
 * to reinstall it.
 */
#define WATCHDOG_THRESHOLD  1000

static ULONGLONG LastLowLevelEventTick = 0;
static ULONGLONG LastGetMessageEventTick = 0;

void LowLevelHookWatchDog::HookIsAlive() {
  // ULONGLONG Previous = LastLowLevelEventTick;
  LastLowLevelEventTick = GetTickCount64();
  // SendDebugMessageFormat("LowLevelHookWatchDog::HookIsAlive currentLL=%llu currentGM=%llu (lastLL=%llu)", LastLowLevelEventTick, LastGetMessageEventTick, Previous);
}

void LowLevelHookWatchDog::KeyEventReceivedInGetMessageProc() {
  // ULONGLONG Previous = LastGetMessageEventTick;
  LastGetMessageEventTick = GetTickCount64();
  // SendDebugMessageFormat("LowLevelHookWatchDog::KeyEventReceivedInGetMessageProc currentLL=%llu currentGM=%llu (lastGM=%llu)", LastLowLevelEventTick, LastGetMessageEventTick, Previous);

  // This is a good place to check if we are still alive -- shortly after each
  // keystroke event in the GetMessage hook, as this means at worst we'll have
  // one or two keystrokes where Keyman must recover
  if(!CheckIfHookIsAlive()) {
    ReinstallHook();
  }
}

bool LowLevelHookWatchDog::CheckIfHookIsAlive() {
  if(LastGetMessageEventTick < LastLowLevelEventTick) {
    // this shouldn't be possible but rather safe than sorry
    return true;
  }

  return LastGetMessageEventTick - LastLowLevelEventTick < WATCHDOG_THRESHOLD;
}

void LowLevelHookWatchDog::ReinstallHook() {
  //keyman32.cpp:
  SendDebugMessageFormat(
    "Attempting to reinstall hook because watchdog threshold exceeded at %llu msec (last LL=%llu last GM=%llu)",
    LastGetMessageEventTick - LastLowLevelEventTick,
    LastLowLevelEventTick,
    LastGetMessageEventTick
  );

  if(!RestartLowLevelHook()) {
    SendDebugMessage("Attempt to reinstall low level hook may have failed, see previous log messages");
  } else {
    SendDebugMessage("Attempt to reinstall low level hook succeeded");
  }

  Globals::PostMasterController(wm_keyman_control, MAKELONG(KMC_WATCHDOG_HOOK_REINSTALL, WHR_TIMING), (LPARAM)(LastGetMessageEventTick - LastLowLevelEventTick));

  // We should assume the hook is alive at this point to avoid repeated resets
  HookIsAlive();
}

#ifndef _WIN64
extern "C" void __declspec(dllexport) WINAPI Keyman_WatchDogKeyEvent() {
  SendDebugMessageFormat("Keyman_WatchDogKeyEvent");
  LowLevelHookWatchDog::KeyEventReceivedInGetMessageProc();
}
#endif