#pragma once

#include "compfile.h"
#include "kmcmplib.h"

namespace kmcmp{
  void WarnDeprecatedHeader();
  void WarnDeprecatedValueFormat();
  void WarnDeprecatedCompileTarget(PFILE_KEYBOARD fk, const KMX_WCHAR *compileTarget);
  void WarnDeprecatedFix(PFILE_KEYBOARD fk);
  void CheckForDeprecatedFeatures(PFILE_KEYBOARD fk);
}
