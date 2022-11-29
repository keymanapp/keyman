#pragma once

#include <Compfile.h>

#define VERIFY_KEYBOARD_VERSION(fk, ver, err) { \
  if(!VerifyKeyboardVersion((fk), (ver))) \
    return (err); \
}

KMX_BOOL CheckKeyboardFinalVersion(PFILE_KEYBOARD fk);
KMX_BOOL VerifyKeyboardVersion(PFILE_KEYBOARD fk, KMX_DWORD ver);
