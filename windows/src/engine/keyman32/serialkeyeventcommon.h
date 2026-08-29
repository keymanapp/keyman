#pragma once

#include <windows.h>

// We permit up to 256 input events in a single transaction
// This allows roughly 120 characters to be output from a single
// Keyman rule, less a bit of space for modifier shenanigans
#define MAX_KEYEVENT_INPUTS 256

// Length of the KeymanModifierVks table (keymanengine.h).
#define KEYMAN_MODIFIER_VK_COUNT 6

// We need to reserve space for up to KEYMAN_MODIFIER_VK_COUNT modifier key events + 2 prefix
// key events at the end of the buffer in order to make sure that we can reset the modifier
// state at the end of the output. This value depends on keybd_shift behaviour.
#define MAX_KEYEVENT_INPUTS_MODIFIERS (KEYMAN_MODIFIER_VK_COUNT + 2)

#define KEYEVENT_WINDOW_CLASS "Keyman_KeyEventConsumerWnd"

#define GLOBAL_FILE_MAPPING_NAME "KeymanEngine_KeyEvent_FileMapping"
#define GLOBAL_KEY_EVENT_NAME "KeymanEngine_KeyEvent"
#define GLOBAL_KEY_MUTEX_NAME "KeymanEngine_KeyMutex"

/**
WM_USER private messages -- used only for communication 
between low level keyboard hook and serial key event server
*/
#define WM_KEYMAN_KEY_EVENT (WM_USER + 1)
#define WM_KEYMAN_MODIFIER_EVENT (WM_USER + 2)
/**
  The INPUT structure and the KEYBDINPUT structure both vary in size between x86 and x64
  because of the presence of the ULONG_PTR member dwExtraInfo. Thus we need to maintain an
  equal sized structure between the two platforms for shared memory, and copy into INPUT
  structures before sending the input.
*/
struct CSDINPUT {
  WORD      wVk;
  WORD      wScan;
  DWORD     dwFlags;
  DWORD     time;
  ULONGLONG extraInfo;
};

struct SerialKeyEventSharedData {
  DWORD nInputs;
  CSDINPUT inputs[MAX_KEYEVENT_INPUTS];
};

// Live modifier-state reader. A function pointer, not a mock: gmock is not linked into
// keyman32.tests.vcxproj, so the tests bind a file-local stub.
typedef SHORT (WINAPI *PGETASYNCKEYSTATE)(int vKey);

// Defined in keybd_shift.cpp. Outside any _WIN64 guard on purpose, so both architectures and the
// gtest project can reach it.
int PrepareInjectedInputBatch(
  LPINPUT pInputs,
  LPBYTE const kbd,
  const SerialKeyEventSharedData *pSharedData,
  PGETASYNCKEYSTATE pfnGetAsyncKeyState);


