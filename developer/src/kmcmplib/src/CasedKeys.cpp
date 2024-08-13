
#include "pch.h"

#include "compfile.h"
#include "kmn_compiler_errors.h"
#include "../../../../common/windows/cpp/include/vkeys.h"
#include "kmcmplib.h"

#include "CharToKeyConversion.h"
#include "kmx_u16.h"
#include "xstring.h"

namespace kmcmp {
  extern KMX_BOOL FMnemonicLayout; // TODO: these globals should be consolidated one day
}

bool resizeKeyArray(PFILE_GROUP gp, int increment = 1);

KMX_DWORD ExpandCapsRule(PFILE_GROUP gp, PFILE_KEY kpp, PFILE_STORE sp);

KMX_DWORD VerifyCasedKeys(PFILE_STORE sp) {
  assert(sp != NULL);

  if (kmcmp::FMnemonicLayout) {
    // The &CasedKeys system store is not supported for
    // mnemonic layouts in 14.0
    return KmnCompilerMessages::ERROR_CasedKeysNotSupportedWithMnemonicLayout;
  }

  // We will rewrite this store with virtual keys

  PKMX_WCHAR p = sp->dpString;
  PKMX_WCHAR buf = new KMX_WCHAR[u16len(p) * 5 + 1];  // extended keys are 5 units long, so this is the max length
  PKMX_WCHAR q = buf;

  while (*p) {
    KMX_UINT key = 0, shift = 0;
    if (*p != UC_SENTINEL) {
      if (!kmcmp::MapUSCharToVK(*p, &key, &shift)) {
        return KmnCompilerMessages::ERROR_CasedKeysMustContainOnlyVirtualKeys;
      }
      if (shift & K_SHIFTFLAG) {
        return KmnCompilerMessages::ERROR_CasedKeysMustNotIncludeShiftStates;
      }
    }
    else {
      if (*(p + 1) != CODE_EXTENDED) {
        return KmnCompilerMessages::ERROR_CasedKeysMustContainOnlyVirtualKeys;
      }
      shift = *(p + 2);
      key = *(p + 3);
      if (shift != ISVIRTUALKEY) {
        return KmnCompilerMessages::ERROR_CasedKeysMustNotIncludeShiftStates;
      }
    }
    *q++ = UC_SENTINEL;
    *q++ = CODE_EXTENDED;
    *q++ = shift;
    *q++ = key;
    *q++ = UC_SENTINEL_EXTENDEDEND;
    *q = 0;

    p = incxstr(p);
  }

  delete[] sp->dpString;
  sp->dpString = buf;

  return STATUS_Success;
}

KMX_DWORD ExpandCapsRulesForGroup(PFILE_KEYBOARD fk, PFILE_GROUP gp) {
  assert(fk != NULL);
  assert(gp != NULL);

  if (kmcmp::FMnemonicLayout) {
    // The &CasedKeys system store is not supported for
    // mnemonic layouts in 14.0
    return STATUS_Success;
  }

  PFILE_STORE sp = FindSystemStore(fk, TSS_CASEDKEYS);
  if (!sp) {
    // If there is no &CasedKeys system store, then we do not
    // process the key
    return STATUS_Success;
  }

  KMX_DWORD msg;
  // ExpandCapsRule may add extra rules at the end of gp->dpKeyArray,
  // reallocating it, so we (a) cache the original length, and (b)
  // dereference the array every call
  int cxKeyArray = gp->cxKeyArray;
  for (int i = 0; i < cxKeyArray; i++) {
    if ((msg = ExpandCapsRule(gp, &gp->dpKeyArray[i], sp)) != STATUS_Success) {
      return msg;
    }
  }
  return STATUS_Success;
}

KMX_DWORD ExpandCapsRule(PFILE_GROUP gp, PFILE_KEY kpp, PFILE_STORE sp) {
  KMX_UINT key = kpp->Key;
  KMX_UINT shift = kpp->ShiftFlags;

  if (shift == 0) {
    // Convert US key cap to a virtual key
    if (!kmcmp::MapUSCharToVK(kpp->Key, &key, &shift)) {
      return STATUS_Success;
    }
  }

  if (shift & (CAPITALFLAG | NOTCAPITALFLAG)) {
    // Don't attempt expansion if either Caps Lock flag is specified in the key rule
    return STATUS_Success;
  }

  PKMX_WCHAR p = sp->dpString;
  for (; *p; p = incxstr(p)) {
    // We've already verified that the store contains only virtual keys in VerifyCasedKeys
    if (*(p + 3) == key) {
      break;
    }
  }

  if (!*p) {
    // This key is not modified by Caps Lock
    return STATUS_Success;
  }

  // This key is modified by Caps Lock, so we need to duplicate this rule
  int offset = (int)(kpp - gp->dpKeyArray);
  if(!resizeKeyArray(gp)) {
    return KmnCompilerMessages::FATAL_CannotAllocateMemory;
  }
  kpp = &gp->dpKeyArray[offset];
  gp->cxKeyArray++;

  PFILE_KEY k = &gp->dpKeyArray[gp->cxKeyArray - 1];
  k->dpContext = new KMX_WCHAR[u16len(kpp->dpContext) + 1];
  k->dpOutput  = new KMX_WCHAR[u16len(kpp->dpOutput) + 1];
  u16cpy(k->dpContext, kpp->dpContext );  // copy the context.
  u16cpy(k->dpOutput, kpp->dpOutput );    // copy the output.

  k->Key = key;
  k->Line = kpp->Line;
  // Add the CAPITAL FLAG, invert shift flag for the rule
  k->ShiftFlags = (shift ^ K_SHIFTFLAG) | CAPITALFLAG;
  kpp->Key = key;
  kpp->ShiftFlags = shift | NOTCAPITALFLAG;

  return STATUS_Success;
}
