
#include "pch.h"

#include "compfile.h"
#include <kmn_compiler_errors.h>
#include "kmcmplib.h"
#include <xstring.h>
#include "kmx_u16.h"
#include <kmcompx.h>

#include "CheckForDuplicates.h"

KMX_DWORD CheckForDuplicateGroup(PFILE_KEYBOARD fk, PFILE_GROUP gp) noexcept {
  KMX_DWORD i;
  PFILE_GROUP gp0 = fk->dpGroupArray;
  for (i = 0; i < fk->cxGroupArray; i++, gp0++) {
    if (gp0 == gp) {
      continue;
    }
    if (u16icmp(gp0->szName, gp->szName) == 0) {
      u16sprintf(ErrExtraW, 256, L" Group '%ls' declared on line %d", u16fmt(gp0->szName).c_str(), gp0->Line);
      strcpy(ErrExtraLIB, string_from_u16string(ErrExtraW).c_str());
      return CERR_DuplicateGroup;
    }
  }
  return CERR_None;
}

KMX_DWORD CheckForDuplicateStore(PFILE_KEYBOARD fk, PFILE_STORE sp) noexcept {
  if (!sp->szName[0]) {
    // Stores with zero length names are reserved system stores.
    // They cannot be defined in user code. This is not an issue.
    return CERR_None;
  }
  KMX_DWORD i;
  PFILE_STORE sp0 = fk->dpStoreArray;
  for (i = 0; i < fk->cxStoreArray; i++, sp0++) {
    if (sp0 == sp) {
      continue;
    }
    if (u16icmp(sp0->szName, sp->szName) == 0) {
      u16sprintf(ErrExtraW,256, L" Store '%ls' declared on line %d", u16fmt(sp0->szName).c_str(), sp0->line);
      strcpy(ErrExtraLIB, string_from_u16string(ErrExtraW).c_str());
      return CERR_DuplicateStore;
    }
  }
  return CERR_None;
}
