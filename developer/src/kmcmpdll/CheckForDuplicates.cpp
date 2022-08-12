#pragma once

#include "pch.h"

#include "Compfile.h"         // _S2 #include <Compfile.h>
#include "comperr.h"          // _S2 #include <comperr.h>
#include "kmcmpdll.h"         // _S2 #include <kmcmpdll.h>
//#include "../../../common/windows/cpp/include/xstring.h"
#include "xstring.h"
#include "kmx_u16.h"

#include <kmcompx.h>

#include <CheckForDuplicates.h>


KMX_DWORD CheckForDuplicateGroup(PFILE_KEYBOARD fk, PFILE_GROUP gp) noexcept {
  KMX_DWORD i;
  PFILE_GROUP gp0 = fk->dpGroupArray;
  for (i = 0; i < fk->cxGroupArray; i++, gp0++) {
    if (gp0 == gp) {
      continue;
    }
    if (u16icmp(gp0->szName, gp->szName) == 0)       // _S2 if (_wcsicmp(gp0->szName, gp->szName) == 0)
    {
      // _ S2    wsprintf(ErrExtra, "Group '%ls' declared on line %d", gp0->szName, gp0->Line);
          PKMX_WCHAR p_ErrExtra ;

          KMX_WCHAR T1[250]  = u"Group '";
			    KMX_WCHAR T3[250]  = u"' declared on line ";
          u16ncat(T1, gp0->szName, xstrlen(T1));
          u16ncat(T1, T3, xstrlen(T1));
          u16printf(&p_ErrExtra, 'd', 0x0020, createIntVector( (int)gp0->Line), T1);

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
    if (u16icmp(sp0->szName, sp->szName) == 0)   // _S2 if (_wcsicmp(sp0->szName, sp->szName) == 0)
     {
      
          PKMX_WCHAR ErrExtra ;
      // _ S2    wsprintf(ErrExtra, "Store '%ls' declared on line %d", sp0->szName, sp0->line);
      /*
          PKMX_WCHAR p_ErrExtra ;
          KMX_WCHAR T1[250]  = u"Store '";
			    KMX_WCHAR T3[250]  = u"' declared on line ";
          u16ncat(T1, sp0->szName, xstrlen(T1));
          u16ncat(T1, T3, xstrlen(T1));
          u16printf(&p_ErrExtra, 'd', 0x0020, createIntVector( (int)sp0->line), T1);
*/
          u16sprintf(ErrExtra, 1024, L"Store '%ls' declared on line %d", u16fmt(sp0->szName), sp0->line);


      return CERR_DuplicateStore;
    }
  }
  return CERR_None;
}
