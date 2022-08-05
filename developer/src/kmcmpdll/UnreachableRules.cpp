
#include "pch.h"
#include "Compfile.h"         // _S2 #include <Compfile.h>
#include "comperr.h"          // _S2 #include <comperr.h>
#include "../../../common/windows/cpp/include/vkeys.h"
#include "kmcmpdll.h"         // _S2 #include <kmcmpdll.h>

#include <unordered_map>
#include <unordered_set>
#include <string>
#include <sstream>

#include "UnreachableRules.h"

std::wstring MakeHashKeyFromFileKey(PFILE_KEY kp) {
  std::wstringstream key;
  key << kp->Key << "," << kp->ShiftFlags << ",";
  if (kp->dpContext) key << kp->dpContext;
  return key.str();
}

KMX_DWORD VerifyUnreachableRules(PFILE_GROUP gp) {
  PFILE_KEY kp = gp->dpKeyArray;
  KMX_DWORD i;

  std::unordered_map<std::wstring, FILE_KEY> map;
  std::unordered_set<int> reportedLines;

  for (i = 0; i < gp->cxKeyArray; i++, kp++) {
    std::wstring key = MakeHashKeyFromFileKey(kp);
    if (map.count(key) > 0) {
      FILE_KEY const & k1 = map.at(key);
      if (kp->Line != k1.Line && reportedLines.count(kp->Line) == 0) {
        reportedLines.insert(kp->Line);
        currentLine = kp->Line;

        // _S2 wsprintf(ErrExtra, "Overridden by rule on line %d", k1.Line);
        PKMX_WCHAR p_ErrExtra;
        char16_t text[256] = u"Overridden by rule on line ";
        u16printf(&p_ErrExtra, 'd', 0x002e, createIntVector((int)(k1.Line)),   text );            

        AddWarning(CHINT_UnreachableRule);
      }
    }
    else {
      map.insert({ key, *kp });
    }
  }

  return CERR_None;
}
