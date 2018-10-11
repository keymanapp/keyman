#include "pch.h"
#include <compfile.h>
#include <compiler.h>
#include <comperr.h>
#include <kmcmpdll.h>

BOOL CheckKeyboardFinalVersion(PFILE_KEYBOARD fk) {
  char buf[128];

  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    if (fk->version <= 0) {
      fk->version = VERSION_60; // minimum version that we can be safe with
    }

    wsprintf(buf, "The compiler has assigned a minimum engine version of %d.%d based on features used in this keyboard", (fk->version & 0xFF00) >> 8, fk->version & 0xFF);
    AddCompileString(buf);
  }

  return TRUE;
}

BOOL VerifyKeyboardVersion(PFILE_KEYBOARD fk, DWORD ver) {
  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    fk->version = max(fk->version, ver);
    return TRUE;
  }

  return fk->version >= ver;
}
