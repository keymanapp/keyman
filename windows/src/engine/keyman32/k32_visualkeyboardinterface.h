/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-10-08
 *
 * Interface between keyman32.dll and the visual keyboard module hosted in
 * keyman.exe
 */

 #pragma once

/**
 * #8064. Set in the flags of WM_KEYMAN_OSK_MODIFIER_EVENT when the event was injected by Keyman
 * itself, as decided by IsKeymanInjectedKeyEvent.
 *
 * The visual keyboard cannot make that decision from the scan code, and the reason is Right Shift.
 * do_keybd_event (keybd_shift.cpp) overwrites SCAN_FLAG_KEYMAN_KEY_EVENT with SCANCODE_RSHIFT for
 * VK_RSHIFT, because 0x36 is what tells the receiving app which Shift it was -- so an injected Right
 * Shift is byte-identical at the hook to a physical one. That is the same gap ShouldFeedModifierCache
 * needs its second, dwExtraInfo arm for, and dwExtraInfo is not carried in this message.
 *
 * So the decision is made here, where both inputs are in hand, and only the answer is sent.
 *
 * Value chosen clear of the KEYEVENTF_ bits that share this field (EXTENDEDKEY 0x1, KEYUP 0x2,
 * UNICODE 0x4, SCANCODE 0x8) and of the 0x0FFF0000 scan code mask.
 *
 * The consumer is the OSK half, PR #16527 (issue #16462), which redeclares this value as a
 * literal in UfrmOSKOnScreenKeyboard.pas and reads it in UpdateUserHeldModifiers. It is not in
 * this tree: that PR is gated on this one landing first, and until it does, this flag is set
 * and nothing reads it. That is the intended order -- the Delphi side compiles standalone, so
 * merging it first would build clean while nothing ever set the bit, and every injected
 * modifier would be attributed to the user.
 */
#define KEYMAN_OSK_MODIFIER_FLAG_KEYMAN_INJECTED 0x00000010

HWND GetVisualKeyboardWindow();
void PostVisualKeyboardModifierEvent(UINT vkCode, DWORD flags);
BOOL PostMessageToVisualKeyboardWindowIfExists(UINT msg, WPARAM wParam, LPARAM lParam);
