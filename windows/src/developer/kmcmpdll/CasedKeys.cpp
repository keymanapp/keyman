
#include "../../../../developer/kmcompx/include/kmcompx.h"        // added S


#include "../../../../developer/kmcompx/include/pch.h"            // added S
//#include "pch.h"                                                // removed S

#include "../../../../developer/kmcompx/include/compfile.h"       // added S
//#include <compfile.h>                                           // removed S

#include "../../../../developer/kmcompx/include/compiler.h"       // added S
//#include <compiler.h>                                           // removed S

#include "../../../../developer/kmcompx/include/comperr.h"        // added S
//#include <comperr.h>                                            // removed S

#include "../../../../developer/kmcompx/include/vkeys.h"          // added S
//#include <vkeys.h>                                              // removed S

#include "../../../../developer/kmcompx/include/kmcmpdll.h"       // added S
//#include <kmcmpdll.h>                                           // removed S

#include "../../../../developer/kmcompx/include/CharToKeyConversion.h"      // added S
//#include "CharToKeyConversion.h"                                          // removed S
// for u16cpy
#include "../../../../developer/kmcompx/include/kmx_xstring.h"      // added S
#include <string>


//using namespace km::kbp;
//using namespace kmx;
extern KMX_BOOL FMnemonicLayout; // TODO: these globals should be consolidated one day

KMX_DWORD ExpandCapsRule(PFILE_GROUP gp, PFILE_KEY kpp, PFILE_STORE sp);

KMX_DWORD VerifyCasedKeys(PFILE_STORE sp) {
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
    KMX_UINT key = 0, shift = 0;
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

KMX_DWORD ExpandCapsRulesForGroup(PFILE_KEYBOARD fk, PFILE_GROUP gp) {
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

  KMX_DWORD msg;
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

KMX_DWORD ExpandCapsRule(PFILE_GROUP gp, PFILE_KEY kpp, PFILE_STORE sp) {
  KMX_UINT key = kpp->Key;
  KMX_UINT shift = kpp->ShiftFlags;

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

//*****************************************************************************************************
//*****************************************************************************************************
//*****************************************************************************************************

extern KMX_BOOL KMX_FMnemonicLayout; // TODO: these globals should be consolidated one day

KMX_DWORD KMX_ExpandCapsRule(PKMX_FILE_GROUP gp, PKMX_FILE_KEY kpp, PKMX_FILE_STORE sp);

KMX_DWORD KMX_VerifyCasedKeys(PKMX_FILE_STORE sp) {
  assert(sp != NULL);

  if (FMnemonicLayout) {
    // The &CasedKeys system store is not supported for
    // mnemonic layouts in 14.0
    return CERR_CasedKeysNotSupportedWithMnemonicLayout;
  }

  // We will rewrite this store with virtual keys

  PKMX_WSTR p = sp->dpString;
  PKMX_WSTR buf = new KMX_WCHAR[std::u16string(p).length() * 5 + 1];  // extended keys are 5 units long, so this is the max length
  PKMX_WSTR q = buf;

  while (*p) {
    KMX_UINT key = 0, shift = 0;
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

KMX_DWORD KMX_ExpandCapsRulesForGroup(PKMX_FILE_KEYBOARD fk, PKMX_FILE_GROUP gp) {
  assert(fk != NULL);
  assert(gp != NULL);

  if (FMnemonicLayout) {
    // The &CasedKeys system store is not supported for
    // mnemonic layouts in 14.0
    return CERR_None;
  }

  PKMX_FILE_STORE sp = KMX_FindSystemStore(fk, TSS_CASEDKEYS);
  if (!sp) {
    // If there is no &CasedKeys system store, then we do not
    // process the key
    return CERR_None;
  }

  KMX_DWORD msg;
  // ExpandCapsRule may add extra rules at the end of gp->dpKeyArray,
  // reallocating it, so we (a) cache the original length, and (b)
  // dereference the array every call
  int cxKeyArray = gp->cxKeyArray;
  for (int i = 0; i < cxKeyArray; i++) {
    if ((msg = KMX_ExpandCapsRule(gp, &gp->dpKeyArray[i], sp)) != CERR_None) {
      return msg;
    }
  } 
 
  return CERR_None;
 
}

KMX_DWORD KMX_ExpandCapsRule(PKMX_FILE_GROUP gp, PKMX_FILE_KEY kpp, PKMX_FILE_STORE sp) {
  KMX_UINT key = kpp->Key;
  KMX_UINT shift = kpp->ShiftFlags;

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

  PKMX_WSTR p = sp->dpString;
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
  PKMX_FILE_KEY k = new KMX_FILE_KEY[gp->cxKeyArray + 1];
  if (!k) return CERR_CannotAllocateMemory;
  memcpy(k, gp->dpKeyArray, gp->cxKeyArray * sizeof(FILE_KEY));

  kpp = &k[(INT_PKMX)(kpp - gp->dpKeyArray)];

  delete gp->dpKeyArray;
  gp->dpKeyArray = k;
  gp->cxKeyArray++;

  k = &k[gp->cxKeyArray - 1];
  k->dpContext = new KMX_WCHAR[std::u16string(kpp->dpContext).length() + 1];
  k->dpOutput  = new KMX_WCHAR[std::u16string(kpp->dpOutput).length() + 1];
  
  // copy the output.
  km::kbp::kmx::KMX_u16cpy(k->dpContext, kpp->dpContext );
  km::kbp::kmx::KMX_u16cpy(k->dpOutput, kpp->dpOutput );
  
  k->Key = key;
  k->Line = kpp->Line;
  // Add the CAPITAL FLAG, invert shift flag for the rule
  k->ShiftFlags = shift ^ K_SHIFTFLAG | CAPITALFLAG;
  kpp->Key = key;
  kpp->ShiftFlags = shift | NOTCAPITALFLAG;

  return CERR_None;
}

//*****************************************************************************

void TestSab(){
//incxstr:  ************************************************************
// A) can use PKMX_WCHAR incxstr(PKMX_WCHAR p) of C:\Projects\keyman\keyman\windows\src\global\vc
// A) can use PKMX_WCHAR incxstr(PKMX_WCHAR p) of C:\Projects\keyman\keyman\windows\src\test\manual-tests\mnemonic-to-positional\m-to-p\m-to-p
// B) can use PWSTR      incxstr(PWSTR p)      of C:\Projects\keyman\keyman\windows\src\test\manual-tests\mnemonic-to-positional\m-to-p\m-to-p

// new should work without win -> kmx
// A)
// there is PKMX_WCHAR KMX_incxstr	(	PKMX_WCHAR 	p	)	
// PKMX_WCHAR == PKMX_WSTR
     //PKMX_FILE_STORE sp;
     //PKMX_WSTR p = sp->dpString;
     //PKMX_WCHAR q = sp->dpString;
     //q = incxstr(q);     // takes win/src/global/vc-> created overloaded fun there
     //q = incxstr(p);     // takes win/src/global/vc-> created overloaded fun there

// B)
// old with win ( for already created tests)
     //PWSTR pp ;
     //pp = incxstr(pp);


// use u16cpy ************************************************************
// Z1   can use  km_kbp_cp     const km_kbp_cp *km::kbp::kmx::   u16cpy(km_kbp_cp *dst, const km_kbp_cp *src)   
      //   of C:\Projects\keyman\keyman\common\core\desktop\src\kmx\kmx_xstring.h
// Z1 can use  km_kbp_cp       const km_kbp_cp *km::kbp::kmx::   u16cpy(km_kbp_cp *dst, const km_kbp_cp *src)   
      //    of C:\Projects\keyman\keyman\developer\kmcompx\include\X\kmx_xstring.h
// Z2 can use KMX_WCHAR     const KMX_WCHAR *km::kbp::kmx::KMX_u16cpy(KMX_WCHAR *dst, const KMX_WCHAR *src) 
      //     of C:\Projects\keyman\keyman\developer\kmcompx\include\X\kmx_xstring.h
// Z3 can use KMX_WCHAR     const KMX_WCHAR *km::kbp::kmx::     u16cpy(KMX_WCHAR *dst, const KMX_WCHAR *src) 
      //     of C:\Projects\keyman\keyman\developer\kmcompx\include\X\kmx_xstring.h
// Z3   can use  km_kbp_cp     const km_kbp_cp *km::kbp::kmx::   u16cpy(km_kbp_cp *dst, const km_kbp_cp *src)   
      //   of C:\Projects\keyman\keyman\common\core\desktop\src\kmx\kmx_xstring.h
// Z4 can use KMX_WCHAR     const KMX_WCHAR *km::kbp::kmx::KMX_u16cpy(KMX_WCHAR *dst, const KMX_WCHAR *src) 
      //     of C:\Projects\keyman\keyman\developer\kmcompx\include\X\kmx_xstring.h

  // use like that:
      // km::kbp::kmx::KMX_u16cpy(k->dpContext, kpp->dpContext );
  /*
  PKMX_FILE_KEY kpp;
  PKMX_FILE_GROUP gp;
  PKMX_FILE_KEY k = new KMX_FILE_KEY[gp->cxKeyArray + 1];
  delete gp->dpKeyArray;
  gp->dpKeyArray = k;
  gp->cxKeyArray++;

  k = &k[gp->cxKeyArray - 1];
  k->dpContext = new KMX_WCHAR[std::u16string(kpp->dpContext).length() + 1];
  k->dpOutput  = new KMX_WCHAR[std::u16string(kpp->dpOutput).length() + 1];
    
  km::kbp::kmx::KMX_u16cpy(k->dpContext, kpp->dpContext );
*/
}