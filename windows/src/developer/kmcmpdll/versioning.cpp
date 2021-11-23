#include "pch.h"
//#include <compfile.h>

#include "../../../../developer/kmcompx/include/Compfile.h"
#include <compiler.h>
#include <comperr.h>
#include <kmcmpdll.h>

KMX_BOOL CheckKeyboardFinalVersion(PFILE_KEYBOARD fk) {
  KMX_CHAR buf[128];

  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    if (fk->version <= 0) {
      fk->version = VERSION_60; // minimum version that we can be safe with
    }

    wsprintf(buf, "The compiler has assigned a minimum engine version of %d.%d based on features used in this keyboard", (int)((fk->version & 0xFF00) >> 8), (int)(fk->version & 0xFF));
    AddCompileString(buf);
  }

  return TRUE;
}

KMX_BOOL VerifyKeyboardVersion(PFILE_KEYBOARD fk, KMX_DWORD ver) {
  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    fk->version = max(fk->version, ver);
    return TRUE;
  }

  return fk->version >= ver;
}

//***************************************************************************
KMX_BOOL KMX_CheckKeyboardFinalVersion(PKMX_FILE_KEYBOARD fk) {
  KMX_CHAR buf[128];

  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    if (fk->version <= 0) {
      fk->version = VERSION_60; // minimum version that we can be safe with
    }

    wsprintf(buf, "The compiler has assigned a minimum engine version of %d.%d based on features used in this keyboard", (int)((fk->version & 0xFF00) >> 8), (int)(fk->version & 0xFF));
    AddCompileString(buf);
  }

  return TRUE;
}

KMX_BOOL KMX_VerifyKeyboardVersion(PKMX_FILE_KEYBOARD fk, KMX_DWORD ver) {
  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    fk->version = max(fk->version, ver);
    return TRUE;
  }

  return fk->version >= ver;
}

