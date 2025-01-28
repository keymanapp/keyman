#include "pch.h"
#include "kmtip.h"

extern "C" {
  BOOL WINAPI Keyman_WriteDebugEvent2W(PWCHAR file, int line, PWCHAR function, PWCHAR msg);
  void WINAPI Keyman_SendDebugEntry(PWCHAR file, int line, PWCHAR function);
  void WINAPI Keyman_SendDebugExit(PWCHAR file, int line, PWCHAR function);

  BOOL WINAPI TIPActivateEx(BOOL fActivate);
  BOOL WINAPI TIPActivateKeyboard(GUID *profile);

  BOOL WINAPI TIPProcessKey(WPARAM wParam, LPARAM lParam, PKEYMANPROCESSOUTPUTFUNC outfunc,
    PKEYMANGETCONTEXTFUNC ctfunc, BOOL Updateable, BOOL Preserved);

  BOOL WINAPI TIPProcessKeyEx(WPARAM wParam, LPARAM lParam, PKEYMANPROCESSOUTPUTFUNC outfunc,
    PKEYMANGETCONTEXTISSELECTEDFUNC ctfunc, BOOL Updateable, BOOL Preserved);

  BOOL WINAPI GetKeyboardPreservedKeys(PreservedKey **pPreservedKeys, size_t *cPreservedKeys);

  BOOL WINAPI TIPIsKeymanRunning();
};

void Keyman32Interface::WriteDebugEntry(PWCHAR file, int line, PWCHAR function) {
  ::Keyman_SendDebugEntry(file, line, function);
}

void Keyman32Interface::WriteDebugExit(PWCHAR file, int line, PWCHAR function) {
  ::Keyman_SendDebugExit(file, line, function);
}

void Keyman32Interface::WriteDebugEvent(PWCHAR file, int line, PWCHAR function, PWCHAR msg) {
  ::Keyman_WriteDebugEvent2W(file, line, function, msg);
  // Don't DebugLastError here because it depends on WriteDebugEvent...
}

BOOL Keyman32Interface::TIPActivateEx(BOOL fActivate) {
  return ::TIPActivateEx(fActivate);
}

BOOL Keyman32Interface::TIPActivateKeyboard(GUID *profile) {
  return ::TIPActivateKeyboard(profile);
}

BOOL Keyman32Interface::TIPProcessKey(WPARAM wParam, LPARAM lParam, PKEYMANPROCESSOUTPUTFUNC outfunc,
    PKEYMANGETCONTEXTFUNC ctfunc, BOOL Updateable, BOOL Preserved) {
  return ::TIPProcessKey(wParam, lParam, outfunc, ctfunc, Updateable, Preserved);
}

BOOL Keyman32Interface::TIPProcessKeyEx(WPARAM wParam, LPARAM lParam, PKEYMANPROCESSOUTPUTFUNC outfunc,
    PKEYMANGETCONTEXTISSELECTEDFUNC ctfunc, BOOL Updateable, BOOL Preserved) {
  return ::TIPProcessKeyEx(wParam, lParam, outfunc, ctfunc, Updateable, Preserved);
}

BOOL Keyman32Interface::GetKeyboardPreservedKeys(PreservedKey **pPreservedKeys, size_t *cPreservedKeys) {
  return ::GetKeyboardPreservedKeys(pPreservedKeys, cPreservedKeys);
}

BOOL Keyman32Interface::TIPIsKeymanRunning() {
  return ::TIPIsKeymanRunning();
}
