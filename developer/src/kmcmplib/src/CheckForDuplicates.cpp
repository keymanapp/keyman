
#include "pch.h"

#include "compfile.h"
#include <kmn_compiler_errors.h>
#include "kmcmplib.h"
#include <xstring.h>
#include "kmx_u16.h"
#include <kmcompx.h>

#include "CheckForDuplicates.h"

KMX_BOOL CheckForDuplicateGroup(PFILE_KEYBOARD fk, PFILE_GROUP gp) noexcept {
  KMX_DWORD i;
  PFILE_GROUP gp0 = fk->dpGroupArray;
  for (i = 0; i < fk->cxGroupArray; i++, gp0++) {
    if (gp0 == gp) {
      continue;
    }
    if (u16icmp(gp0->szName, gp->szName) == 0) {
      ReportCompilerMessage(KmnCompilerMessages::ERROR_DuplicateGroup, {
        /*groupName*/  string_from_u16string(gp->szName),
        /*lineNumber*/ std::to_string(gp0->Line)
      });
      return FALSE;
    }
  }
  return TRUE;
}

KMX_BOOL CheckForDuplicateStore(PFILE_KEYBOARD fk, PFILE_STORE sp) noexcept {
  if (!sp->szName[0]) {
    // Stores with zero length names are reserved system stores.
    // They cannot be defined in user code. This is not an issue.
    return TRUE;
  }
  KMX_DWORD i;
  PFILE_STORE sp0 = fk->dpStoreArray;
  for (i = 0; i < fk->cxStoreArray; i++, sp0++) {
    if (sp0 == sp) {
      continue;
    }
    if (u16icmp(sp0->szName, sp->szName) == 0) {
      ReportCompilerMessage(KmnCompilerMessages::ERROR_DuplicateStore, {
        /*storeName*/  string_from_u16string(sp0->szName),
        /*lineNumber*/ std::to_string(sp0->line)
      });
      return FALSE;
    }
  }
  return TRUE;
}
