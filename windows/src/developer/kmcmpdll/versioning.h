#pragma once

#include <Windows.h>
#include <Compfile.h>

#define VERIFY_KEYBOARD_VERSION(fk, ver, err) { \
  if(!VerifyKeyboardVersion((fk), (ver))) \
    return (err); \
}

BOOL CheckKeyboardFinalVersion(PFILE_KEYBOARD fk);
BOOL VerifyKeyboardVersion(PFILE_KEYBOARD fk, DWORD ver);