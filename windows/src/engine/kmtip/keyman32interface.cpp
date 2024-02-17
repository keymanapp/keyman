#include "pch.h"
#include "kmtip.h"

extern "C" {
  BOOL WINAPI Keyman_WriteDebugEventW(PWCHAR file, int line, PWCHAR msg);

  BOOL WINAPI TIPActivateEx(BOOL fActivate);
  BOOL WINAPI TIPActivateKeyboard(GUID *profile);

  BOOL WINAPI TIPProcessKey(WPARAM wParam, LPARAM lParam, PKEYMANPROCESSOUTPUTFUNC outfunc,
    PKEYMANGETCONTEXTFUNC ctfunc, BOOL Updateable, BOOL Preserved);

  BOOL WINAPI GetKeyboardPreservedKeys(PreservedKey **pPreservedKeys, size_t *cPreservedKeys);

  BOOL WINAPI TIPIsKeymanRunning();
};

void Keyman32Interface::WriteDebugEvent(PWCHAR file, int line, PWCHAR msg) {
  ::Keyman_WriteDebugEventW(file, line, msg);
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

BOOL Keyman32Interface::GetKeyboardPreservedKeys(PreservedKey **pPreservedKeys, size_t *cPreservedKeys) {
  return ::GetKeyboardPreservedKeys(pPreservedKeys, cPreservedKeys);
}

BOOL Keyman32Interface::TIPIsKeymanRunning() {
  return ::TIPIsKeymanRunning();
}
