#pragma once

#include "compfile.h"
#include "../../../../common/include/km_types.h"

namespace kmcmp {
  KMX_BOOL CheckKeyboardFinalVersion(PFILE_KEYBOARD fk);
}

KMX_BOOL VerifyKeyboardVersion(PFILE_KEYBOARD fk, KMX_DWORD ver);
