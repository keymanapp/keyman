/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-10-08
 *
 * Interface between keyman32.dll and the visual keyboard module hosted in
 * keyman.exe
 */

 #pragma once

HWND GetVisualKeyboardWindow();
void PostVisualKeyboardModifierEvent(UINT vkCode, DWORD flags);
BOOL PostMessageToVisualKeyboardWindowIfExists(UINT msg, WPARAM wParam, LPARAM lParam);
