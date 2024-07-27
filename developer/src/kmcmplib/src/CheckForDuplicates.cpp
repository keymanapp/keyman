
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
      snprintf(ErrExtraLIB, ERR_EXTRA_LIB_LEN, " Group '%s' declared on line %d", string_from_u16string(gp->szName).c_str(), gp0->Line);
      return CERR_DuplicateGroup;
    }
  }
  return STATUS_Success;
}

KMX_DWORD CheckForDuplicateStore(PFILE_KEYBOARD fk, PFILE_STORE sp) noexcept {
  if (!sp->szName[0]) {
    // Stores with zero length names are reserved system stores.
    // They cannot be defined in user code. This is not an issue.
    return STATUS_Success;
  }
  KMX_DWORD i;
  PFILE_STORE sp0 = fk->dpStoreArray;
  for (i = 0; i < fk->cxStoreArray; i++, sp0++) {
    if (sp0 == sp) {
      continue;
    }
    if (u16icmp(sp0->szName, sp->szName) == 0) {
      snprintf(ErrExtraLIB, ERR_EXTRA_LIB_LEN, " Store '%s' declared on line %d", string_from_u16string(sp0->szName).c_str(), sp0->line);
      return CERR_DuplicateStore;
    }
  }
  return STATUS_Success;
}
