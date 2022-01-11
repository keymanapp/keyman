#pragma once

#include <Windows.h>
//#include <Compfile.h>
#include "../../../../developer/kmcompx/include/kmcompx.h"
#include "../../../../developer/kmcompx/include/Compfile.h"

#define VERIFY_KEYBOARD_VERSION(fk, ver, err) { \
  if(!VerifyKeyboardVersion((fk), (ver))) \
    return (err); \
}


KMX_BOOL CheckKeyboardFinalVersion(PFILE_KEYBOARD fk);
KMX_BOOL VerifyKeyboardVersion(PFILE_KEYBOARD fk, KMX_DWORD ver);

//---------------------------


#define KMX_VERIFY_KEYBOARD_VERSION(fk, ver, err) { \
  if(!KMX_VerifyKeyboardVersion((fk), (ver))) \
    return (err); \
}

KMX_BOOL KMX_CheckKeyboardFinalVersion(PKMX_FILE_KEYBOARD fk);
KMX_BOOL KMX_VerifyKeyboardVersion(PKMX_FILE_KEYBOARD fk, KMX_DWORD ver);
