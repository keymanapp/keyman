
#include "pch.h"

#include <compfile.h>
#include <comperr.h>
#include <kmcmpdll.h>
#include <xstring.h>
#include "kmx_u16.h"
#include <kmcompx.h>

#include "CheckForDuplicates.h"


KMX_DWORD CheckForDuplicateGroup(PFILE_KEYBOARD fk, PFILE_GROUP gp) noexcept {
  KMX_DWORD i;
  KMX_WCHAR kmcmp_ErrExtra[256];
  PFILE_GROUP gp0 = fk->dpGroupArray;
  for (i = 0; i < fk->cxGroupArray; i++, gp0++) {
    if (gp0 == gp) {
      continue;
    }
    if (u16icmp(gp0->szName, gp->szName) == 0) {
      u16sprintf(kmcmp_ErrExtra, _countof(kmcmp_ErrExtra), L"Group '%ls' declared on line %d", u16fmt(gp0->szName).c_str(), gp0->Line);
      return CERR_DuplicateGroup;
    }
  }
  return CERR_None;
}

KMX_DWORD CheckForDuplicateStore(PFILE_KEYBOARD fk, PFILE_STORE sp) noexcept {
  KMX_WCHAR kmcmp_ErrExtra[256];
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
        u16sprintf(kmcmp_ErrExtra, _countof(kmcmp_ErrExtra), L"Store '%ls' declared on line %d", u16fmt(sp0->szName).c_str(), sp0->line);
      return CERR_DuplicateStore;
    }
  }
  return CERR_None;
}
