#include "pch.h"
#include "compfile.h"
#include <kmn_compiler_errors.h>
#include "kmcmplib.h"
#include "versioning.h"

KMX_BOOL kmcmp::CheckKeyboardFinalVersion(PFILE_KEYBOARD fk) {
  if (fk->dwFlags & KF_AUTOMATICVERSION) {
    if (fk->version <= 0) {
      fk->version = VERSION_60; // minimum version that we can be safe with
    }

    if(kmcmp::CompileTarget != CKF_KEYMANWEB) {
      // Note: the KeymanWeb compiler is responsible for reporting minimum
      // version for the web targets
      AddCompileError(KmnCompilerMessages::INFO_MinimumCoreEngineVersion, {
        /* majorVersion */ std::to_string((fk->version & 0xFF00) >> 8),
        /* minorVersion */ std::to_string(fk->version & 0xFF)
      });
    }
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
