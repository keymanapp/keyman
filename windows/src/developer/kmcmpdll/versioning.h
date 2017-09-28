#pragma once

#include <Windows.h>
#include <Compfile.h>

#define VerifyKeyboardVersion(fk, ver, err) { \
  if(!VerifyKeyboardVersionF((fk), (ver))) \
    return (err); \
}

BOOL CheckKeyboardFinalVersion(PFILE_KEYBOARD fk);
BOOL VerifyKeyboardVersionF(PFILE_KEYBOARD fk, DWORD ver);