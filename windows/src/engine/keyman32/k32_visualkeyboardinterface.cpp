/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-10-08
 *
 * Interface between keyman32.dll and the visual keyboard module hosted in
 * keyman.exe
 */
#include "pch.h"

// These messages should correspond to the UfrmOSKOnScreenKeyboard section in
// UserMessages.pas. Only messages used in keyman32 are currently defined here.
#define WM_KEYMAN_OSK_MODIFIER_EVENT  (WM_USER + 141)

/**
 * Tell the visual keyboard module that modifier keys on the physical keyboard
 * have been pressed or released, in order to update the display of the visual
 * keyboard. This is sent from the low level keyboard hook, so is an in-process
 * single thread event.
 *
 * This takes the chiral VK_LCONTROL / VK_RCONTROL / VK_LMENU / VK_RMENU virtual
 * key codes.
 *
 * This function exists primarily to support workaround of the Windows hack of
 * generating LCtrl+RAlt when AltGr is pressed on European layouts. The visual
 * keyboard module includes special handling for the custom scan code of 0x21D
 * for the fudged LCtrl key.
 *
 * @param vkCode virtual key code of the modifier key, chiral for Alt and Ctrl
 * @param flags  as follows:
 *    KEYEVENTF_EXTENDEDKEY   0x00000001  - extended bit is set on the scan code
 *    KEYEVENTF_KEYUP         0x00000002  - key is being released
 *    scan code mask          0x0FFF0000  - 12 bits for scan code
 *    all other bits reserved
 */
void PostVisualKeyboardModifierEvent(UINT vkCode, DWORD flags) {
  PostMessageToVisualKeyboardWindowIfExists(WM_KEYMAN_OSK_MODIFIER_EVENT, vkCode, flags);
}

/**
 * Find the handle to the Visual Keyboard window in keyman.exe, if it is open,
 * and returns NULL if not found.
 *
 * @return HWND   Handle to the Visual Keyboard window
 */
HWND GetVisualKeyboardWindow() {
  return FindWindow("TfrmVisualKeyboard", NULL);
}

/**
 * Post a message to the Visual Keyboard window in keyman.exe, if it is open.
 *
 * @param msg     Windows message
 * @param wParam  WPARAM
 * @param lParam  LPARAM
 * @return BOOL   FALSE if window does exist or on PostMessage failure
 */
BOOL PostMessageToVisualKeyboardWindowIfExists(UINT msg, WPARAM wParam, LPARAM lParam) {
  HWND hwnd = GetVisualKeyboardWindow();
  if(hwnd == NULL) {
    return FALSE;
  }

  return PostMessage(hwnd, msg, wParam, lParam);
}
