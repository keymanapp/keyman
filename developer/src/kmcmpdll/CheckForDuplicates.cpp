
#include "pch.h"

#include <compfile.h>
#include <kmn_compiler_errors.h>
#include <kmcmpdll.h>

#include "CheckForDuplicates.h"


DWORD CheckForDuplicateGroup(PFILE_KEYBOARD fk, PFILE_GROUP gp) noexcept {
  DWORD i;
  PFILE_GROUP gp0 = fk->dpGroupArray;
  for (i = 0; i < fk->cxGroupArray; i++, gp0++) {
    if (gp0 == gp) {
      continue;
    }
    if (_wcsicmp(gp0->szName, gp->szName) == 0) {
      wsprintf(ErrExtra, "Group '%ls' declared on line %d", gp0->szName, gp0->Line);
      return CERR_DuplicateGroup;
    }
  }
  return CERR_None;
}

DWORD CheckForDuplicateStore(PFILE_KEYBOARD fk, PFILE_STORE sp) noexcept {
  if (!sp->szName[0]) {
    // Stores with zero length names are reserved system stores.
    // They cannot be defined in user code. This is not an issue.
    return CERR_None;
  }
  DWORD i;
  PFILE_STORE sp0 = fk->dpStoreArray;
  for (i = 0; i < fk->cxStoreArray; i++, sp0++) {
    if (sp0 == sp) {
      continue;
    }
    if (_wcsicmp(sp0->szName, sp->szName) == 0) {
      wsprintf(ErrExtra, "Store '%ls' declared on line %d", sp0->szName, sp0->line);
      return CERR_DuplicateStore;
    }
  }
  return CERR_None;
}
