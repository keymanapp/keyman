
#include "pch.h"

#include "compfile.h"
#include <comperr.h>
#include "../../../../common/windows/cpp/include/vkeys.h"
#include "kmcmpdll.h"

#include <unordered_map>
#include <unordered_set>
#include <string>
#include <sstream>

#include "UnreachableRules.h"

namespace kmcmp {

  WCHAR conv_KMX_WCHAR__TO__WCHAR(KMX_WCHAR in) {
    //  char16_t -> std::u16string -> std::string -> std::wstring -> wchar_t*
    std::u16string u16str(&in);
    std::string stri = string_from_u16string(u16str);
    std::wstring  wstr = wstring_from_string(stri);
    return *wstr.c_str();
  }

  std::wstring MakeHashKeyFromFileKey16(PFILE_KEY kp) {
    std::wstringstream key16;
    const WCHAR* ctxt = u16fmt((PKMX_WCHAR) kp->dpContext).c_str();
    key16 << conv_KMX_WCHAR__TO__WCHAR(kp->Key) << "," << (DWORD) kp->ShiftFlags << ",";
    if (ctxt) key16 << ctxt;
    return key16.str();
  };

  std::wstring MakeHashKeyFromFileKey(PFILE_KEY kp) {
    std::wstringstream key;
    key << kp->Key << "," << kp->ShiftFlags << ",";
    if (kp->dpContext) key << kp->dpContext;
    return key.str();
  }
}
KMX_DWORD VerifyUnreachableRules(PFILE_GROUP gp) {
  PFILE_KEY kp = gp->dpKeyArray;
  KMX_DWORD i;
  KMX_WCHAR ErrExtra[256];

  int oldCurrentLine = kmcmp::currentLine;

  std::unordered_map<std::wstring, FILE_KEY> map;
  std::unordered_set<int> reportedLines;

  for (i = 0; i < gp->cxKeyArray; i++, kp++) {
    std::wstring key = kmcmp::MakeHashKeyFromFileKey16(kp);
    if (map.count(key) > 0) {
      FILE_KEY const & k1 = map.at(key);
      if (kp->Line != k1.Line && reportedLines.count(kp->Line) == 0) {
        reportedLines.insert(kp->Line);
        kmcmp::currentLine = kp->Line;
        u16sprintf(ErrExtra, _countof(ErrExtra), L" Overridden by rule on line %d", k1.Line);
        kmcmp::ErrExtra_char = wstrtostr2(ErrExtra);
        AddWarning(CHINT_UnreachableRule);
      }
    }
    else {
      map.insert({ key, *kp });
    }
  }

  kmcmp::currentLine = oldCurrentLine;

  return CERR_None;
}
