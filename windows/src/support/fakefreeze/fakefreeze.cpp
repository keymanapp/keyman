/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-11-17
 *
 * Tell keyman.exe to pause for 5 seconds to force Windows to silently uninstall
 * the low level keyboard hook, per Windows documentation at
 * https://learn.microsoft.com/en-us/windows/win32/winmsg/lowlevelkeyboardproc#remarks
 *
 * See LowLevelHookWatchDog.cpp for more detail
 */
#include <windows.h>
#include <iostream>
#include <stdlib.h>

#define KMC_WATCHDOG_FAKEFREEZE 20

int main() {
  std::cout << "Posting a freeze message to Keyman master controller\n";
  UINT wm_keyman_control = RegisterWindowMessage(L"WM_KEYMAN_CONTROL");
  HWND hwnd = FindWindow(L"TfrmKeyman7Main", NULL);
  if (hwnd == NULL) {
    std::cout << "Keyman master controller window not found\n";
    return 1;
  }

  if (!PostMessage(hwnd, wm_keyman_control, KMC_WATCHDOG_FAKEFREEZE, 0)) {
    std::cout << "Error calling Keyman KMC_WATCHDOG_FAKEFREEZE\n";
  }
  std::cout << "Sleeping 5 seconds...\n";

  for (int i = 1; i <= 5; i++) {
    Sleep(1000);
    std::cout << "..." << i << "\n";
  }
  std::cout << "Keyman should be responsive again now\n";

  return 0;
}
