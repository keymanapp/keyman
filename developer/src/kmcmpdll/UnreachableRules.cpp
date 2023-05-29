
#include "pch.h"

#include <compfile.h>
#include <kmn_compiler_errors.h>
#include "../../../common/windows/cpp/include/vkeys.h"
#include <kmcmpdll.h>

#include <unordered_map>
#include <unordered_set>
#include <string>
#include <sstream>

#include "UnreachableRules.h"

std::wstring MakeHashKeyFromFileKey(PFILE_KEY kp) {
  std::wstringstream key;
  key << kp->Key << "," << kp->ShiftFlags << ",";
  if (kp->dpContext)
    key << kp->dpContext;
  return key.str();
}

DWORD VerifyUnreachableRules(PFILE_GROUP gp) {
  PFILE_KEY kp = gp->dpKeyArray;
  DWORD i;

  int oldCurrentLine = currentLine;

  std::unordered_map<std::wstring, FILE_KEY> map;
  std::unordered_set<int> reportedLines;

  for (i = 0; i < gp->cxKeyArray; i++, kp++) {
    std::wstring key = MakeHashKeyFromFileKey(kp);
    if (map.count(key) > 0) {
      FILE_KEY const & k1 = map.at(key);
      if (kp->Line != k1.Line && reportedLines.count(kp->Line) == 0) {
        reportedLines.insert(kp->Line);
        currentLine = kp->Line;
        wsprintf(ErrExtra, "Overridden by rule on line %d", k1.Line);
        AddWarning(CHINT_UnreachableRule);
      }
    }
    else {
      map.insert({ key, *kp });
    }
  }

  currentLine = oldCurrentLine;

  return CERR_None;
}
