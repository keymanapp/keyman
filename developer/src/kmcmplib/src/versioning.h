#pragma once

#include "compfile.h"
#include "../../../../common/include/km_types.h"

#define VERIFY_KEYBOARD_VERSION(fk, ver, err) { \
  if(!VerifyKeyboardVersion((fk), (ver))) \
    return (err); \
}

namespace kmcmp {
  KMX_BOOL CheckKeyboardFinalVersion(PFILE_KEYBOARD fk);
}

KMX_BOOL VerifyKeyboardVersion(PFILE_KEYBOARD fk, KMX_DWORD ver);
