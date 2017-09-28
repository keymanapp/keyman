
#include <ctype.h>
#include <stdio.h>

#include <string.h>
#include <windows.h>

// Keyman includes

#include <keyman64.h>
#include <compfile.h>
#include <compiler.h>
#include <comperr.h>

BOOL AddCompileString(LPSTR buf);
BOOL AddCompileMessage(DWORD msg);

BOOL CheckKeyboardFinalVersion(PFILE_KEYBOARD fk) {
  char buf[128];

  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    wsprintf(buf, "The compiler has assigned a minimum engine version of %x.%x based on features used in this keyboard", (fk->version & 0xFF00) >> 8, fk->version & 0xFF);
    AddCompileString(buf);
  }

  return TRUE;
}

BOOL VerifyKeyboardVersionF(PFILE_KEYBOARD fk, DWORD ver) {
  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    fk->version = max(fk->version, ver);
    return TRUE;
  }

  return fk->version >= ver;
}
