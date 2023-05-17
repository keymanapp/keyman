#include "pch.h"
#include "compfile.h"
#include <kmn_compiler_errors.h>
#include "kmcmplib.h"
#include "versioning.h"

KMX_BOOL kmcmp::CheckKeyboardFinalVersion(PFILE_KEYBOARD fk) {
  KMX_CHAR buf[128];

  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    if (fk->version <= 0) {
      fk->version = VERSION_60; // minimum version that we can be safe with
    }

    snprintf(buf, 128, "The compiler has assigned a minimum engine version of %d.%d based on features used in this keyboard", (int)((fk->version & 0xFF00) >> 8), (int)(fk->version & 0xFF));
    AddCompileWarning(buf);
  }

  return TRUE;
}

// Note: max is not a standard c api function or macro
#ifndef max
#define max(a,b)            (((a) > (b)) ? (a) : (b))
#endif

KMX_BOOL VerifyKeyboardVersion(PFILE_KEYBOARD fk, KMX_DWORD ver) {
  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    fk->version = max(fk->version, ver);
    return TRUE;
  }

  return fk->version >= ver;
}
