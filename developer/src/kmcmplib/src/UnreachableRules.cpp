
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
    std::wstringstream Key_16;
    std::wstring Context_ws = u16fmt((const PKMX_WCHAR) kp->dpContext);

    Key_16 << conv_KMX_WCHAR__TO__WCHAR(kp->Key) << "," << (DWORD) kp->ShiftFlags << ",";
    if ((KMX_WCHART*) Context_ws.c_str())
      Key_16 << (KMX_WCHART*) Context_ws.c_str();
    return Key_16.str();
  };
}
KMX_DWORD VerifyUnreachableRules(PFILE_GROUP gp) {
  PFILE_KEY kp = gp->dpKeyArray;
  KMX_DWORD i;

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
        u16sprintf(ErrExtraW, 256, L" Overridden by rule on line %d", k1.Line);
        strcpy(ErrExtraLIB, wstrtostr2(ErrExtraW));
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
