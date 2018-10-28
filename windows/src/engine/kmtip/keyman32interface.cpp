#include "pch.h"
#include "kmtip.h"

typedef BOOL (WINAPI *PKEYMAN_WRITEDEBUGEVENT)(char *file, int line, PWCHAR msg);
typedef void (WINAPI *PKEYMAN_SETFOCUSRECEIVED)(HWND hwndActive, HWND hwndPrevious);

//TODO: LoadLibrary on startup, FreeLibrary on shutdown
HMODULE Keyman32Interface::GetHandle() {
#ifdef _WIN64
  return GetModuleHandle("keyman64.dll");
#else
  return GetModuleHandle("keyman32.dll");
#endif
}

void Keyman32Interface::SetFocus(HWND hwndActive, HWND hwndPrevious) {
  HMODULE hKeyman = GetHandle();
  if (hKeyman) {
    PKEYMAN_SETFOCUSRECEIVED pSetFocusReceived = (PKEYMAN_SETFOCUSRECEIVED)GetProcAddress(hKeyman, "TIPSetFocusReceived");
    if (pSetFocusReceived) {
      (*pSetFocusReceived)(hwndActive, hwndPrevious);
    }
    else {
      DebugLastError(L"SetFocusReceived");
    }
  }
  // No point trying to debug last error because it depends on GetHandle() working
}

void Keyman32Interface::WriteDebugEvent(char *file, int line, PWCHAR msg) {
  HMODULE hKeyman = GetHandle();
  if (hKeyman) {
    PKEYMAN_WRITEDEBUGEVENT pWriteDebugEvent = (PKEYMAN_WRITEDEBUGEVENT)GetProcAddress(hKeyman, "Keyman_WriteDebugEvent");
    if (pWriteDebugEvent) {
      (*pWriteDebugEvent)(file, line, msg);
    }
    // Don't DebugLastError here because it depends on WriteDebugEvent...
  }
}
