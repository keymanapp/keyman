#include "pch.h"
#include "kmtip.h"

typedef BOOL (WINAPI *PKEYMAN_WRITEDEBUGEVENT)(char *file, int line, PWCHAR msg);

//TODO: LoadLibrary on startup, FreeLibrary on shutdown
HMODULE Keyman32Interface::GetHandle() {
#ifdef _WIN64
  return GetModuleHandle("keyman64.dll");
#else
  return GetModuleHandle("keyman32.dll");
#endif
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
