
#include "pch.h"

#include "compfile.h"
#include <kmn_compiler_errors.h>
#include "../../../../common/windows/cpp/include/vkeys.h"
#include "kmcmplib.h"

#include <unordered_map>
#include <unordered_set>
#include <string>
#include <sstream>

#include "UnreachableRules.h"

namespace kmcmp {
  std::wstring MakeHashKeyFromFileKey(PFILE_KEY kp) {
    std::wstringstream key;
    key << kp->Key << "," << kp->ShiftFlags << ",";
    if (kp->dpContext) {
      std::wstring Context_ws = u16fmt((const PKMX_WCHAR) kp->dpContext);
      key << Context_ws;
    }
    return key.str();
  }

}
KMX_DWORD VerifyUnreachableRules(PFILE_GROUP gp) {
  PFILE_KEY kp = gp->dpKeyArray;
  KMX_DWORD i;

  int oldCurrentLine = kmcmp::currentLine;

  std::unordered_map<std::wstring, FILE_KEY> map;
  std::unordered_set<int> reportedLines;

  for (i = 0; i < gp->cxKeyArray; i++, kp++) {
    std::wstring key = kmcmp::MakeHashKeyFromFileKey(kp);
    if (map.count(key) > 0) {
      FILE_KEY const & k1 = map.at(key);
      if (kp->Line != k1.Line && reportedLines.count(kp->Line) == 0) {
        reportedLines.insert(kp->Line);
        kmcmp::currentLine = kp->Line;
        snprintf(ErrExtraLIB, ERR_EXTRA_LIB_LEN, " Overridden by rule on line %d", k1.Line);
        AddWarning(KmnCompilerMessages::HINT_UnreachableRule);
      }
    }
    else {
      map.insert({ key, *kp });
    }
  }

  kmcmp::currentLine = oldCurrentLine;

  return STATUS_Success;
}
