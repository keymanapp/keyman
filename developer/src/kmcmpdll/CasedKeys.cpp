
#include "pch.h"

#include <compfile.h>
#include <kmn_compiler_errors.h>
#include "../../../common/windows/cpp/include/vkeys.h"
#include <kmcmpdll.h>

#include "CharToKeyConversion.h"

extern BOOL FMnemonicLayout; // TODO: these globals should be consolidated one day

DWORD ExpandCapsRule(PFILE_GROUP gp, PFILE_KEY kpp, PFILE_STORE sp);

DWORD VerifyCasedKeys(PFILE_STORE sp) {
  assert(sp != NULL);

  if (FMnemonicLayout) {
    // The &CasedKeys system store is not supported for
    // mnemonic layouts in 14.0
    return CERR_CasedKeysNotSupportedWithMnemonicLayout;
  }

  // We will rewrite this store with virtual keys

  PWSTR p = sp->dpString;
  PWSTR buf = new WCHAR[wcslen(p) * 5 + 1];  // extended keys are 5 units long, so this is the max length
  PWSTR q = buf;

  while (*p) {
    UINT key = 0, shift = 0;
    if (*p != UC_SENTINEL) {
      if (!MapUSCharToVK(*p, &key, &shift)) {
        return CERR_CasedKeysMustContainOnlyVirtualKeys;
      }
      if (shift & K_SHIFTFLAG) {
        return CERR_CasedKeysMustNotIncludeShiftStates;
      }
    }
    else {
      if (*(p + 1) != CODE_EXTENDED) {
        return CERR_CasedKeysMustContainOnlyVirtualKeys;
      }
      shift = *(p + 2);
      key = *(p + 3);
      if (shift != ISVIRTUALKEY) {
        return CERR_CasedKeysMustNotIncludeShiftStates;
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

  return CERR_None;
}

DWORD ExpandCapsRulesForGroup(PFILE_KEYBOARD fk, PFILE_GROUP gp) {
  assert(fk != NULL);
  assert(gp != NULL);

  if (FMnemonicLayout) {
    // The &CasedKeys system store is not supported for
    // mnemonic layouts in 14.0
    return CERR_None;
  }

  PFILE_STORE sp = FindSystemStore(fk, TSS_CASEDKEYS);
  if (!sp) {
    // If there is no &CasedKeys system store, then we do not
    // process the key
    return CERR_None;
  }

  DWORD msg;
  // ExpandCapsRule may add extra rules at the end of gp->dpKeyArray,
  // reallocating it, so we (a) cache the original length, and (b)
  // dereference the array every call
  int cxKeyArray = gp->cxKeyArray;
  for (int i = 0; i < cxKeyArray; i++) {
    if ((msg = ExpandCapsRule(gp, &gp->dpKeyArray[i], sp)) != CERR_None) {
      return msg;
    }
  }
  return CERR_None;
}

DWORD ExpandCapsRule(PFILE_GROUP gp, PFILE_KEY kpp, PFILE_STORE sp) {
  UINT key = kpp->Key;
  UINT shift = kpp->ShiftFlags;

  if (shift == 0) {
    // Convert US key cap to a virtual key
    if (!MapUSCharToVK(kpp->Key, &key, &shift)) {
      return CERR_None;
    }
  }

  if (shift & (CAPITALFLAG | NOTCAPITALFLAG)) {
    // Don't attempt expansion if either Caps Lock flag is specified in the key rule
    return CERR_None;
  }

  PWSTR p = sp->dpString;
  for (; *p; p = incxstr(p)) {
    // We've already verified that the store contains only virtual keys in VerifyCasedKeys
    if (*(p + 3) == key) {
      break;
    }
  }

  if (!*p) {
    // This key is not modified by Caps Lock
    return CERR_None;
  }

  // This key is modified by Caps Lock, so we need to duplicate this rule
  PFILE_KEY k = new FILE_KEY[gp->cxKeyArray + 1];
  if (!k) return CERR_CannotAllocateMemory;
  memcpy(k, gp->dpKeyArray, gp->cxKeyArray * sizeof(FILE_KEY));

  kpp = &k[(INT_PTR)(kpp - gp->dpKeyArray)];

  delete gp->dpKeyArray;
  gp->dpKeyArray = k;
  gp->cxKeyArray++;

  k = &k[gp->cxKeyArray - 1];
  k->dpContext = new WCHAR[wcslen(kpp->dpContext) + 1];
  k->dpOutput = new WCHAR[wcslen(kpp->dpOutput) + 1];
  wcscpy_s(k->dpContext, wcslen(kpp->dpContext) + 1, kpp->dpContext);	// copy the context.
  wcscpy_s(k->dpOutput, wcslen(kpp->dpOutput) + 1, kpp->dpOutput);		// copy the output.
  k->Key = key;
  k->Line = kpp->Line;
  // Add the CAPITAL FLAG, invert shift flag for the rule
  k->ShiftFlags = shift ^ K_SHIFTFLAG | CAPITALFLAG;
  kpp->Key = key;
  kpp->ShiftFlags = shift | NOTCAPITALFLAG;

  return CERR_None;
}
