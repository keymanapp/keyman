#pragma once

#ifndef _WIN64
class KeymanLowLevelKeyboardHook {
public:
  static BOOL Startup();
  static BOOL Shutdown();
  static BOOL Restart();
  static void ReloadHotkeys();
  static void SetTouchPanelVisible(BOOL isVisible);

  // Callbacks
  static LRESULT HookProc(_In_ int nCode, _In_ WPARAM wParam, _In_ LPARAM lParam);
  static DWORD ThreadMain();

private:
  static HHOOK m_hhook;
  static DWORD m_idThread;
  static HANDLE m_hThread;
  static BOOL m_InLanguageSwitchKeySequence;
  static HWND m_hwndLanguageSwitch;
  static Hotkeys* m_hotkeys;

  static BOOL m_TouchPanelVisible;

  static HANDLE m_hReloadEvent;
  static HANDLE m_hThreadExitEvent;

  // SerialKeyEventServer interface
  static BOOL PostMessageToSerialKeyEventServer(UINT msg, WPARAM wParam, LPARAM lParam);

  // State utilities
  static DWORD GetCurrentModifierKeyState();
  static BOOL ForegroundWindowIsRDP();

  // Pure utilities
  static BOOL IsConsoleWindow(HWND hwnd);
  static BOOL IsModifierKey(DWORD vkCode);
  static LPARAM LLKHFFlagstoWMKeymanKeyEventFlags(PKBDLLHOOKSTRUCT hs);

  // Handle hotkeys
  static BOOL ProcessHotkey(UINT vkCode, BOOL isUp, DWORD ShiftState);

  // Language switch window
  static BOOL LanguageSwitch_KeyPress(WPARAM wParam, BOOL extended, BOOL isUp, DWORD ShiftState);
  static int LanguageSwitch_ProcessModifierKey(WPARAM wParam, BOOL isUp);
  static BOOL LanguageSwitch_IsWindowVisible();
  static void LanguageSwitch_PostToWindow(WPARAM wParam, LPARAM lParam);

  // Thread and lifecycle
  static BOOL InitThread();
  static void MessageLoop();
  static void CleanupThread();
};
#endif
