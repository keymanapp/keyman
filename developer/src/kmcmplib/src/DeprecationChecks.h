#pragma once

#include "compfile.h"
#include "kmcmplib.h"

namespace kmcmp{
  void WarnDeprecatedHeader();
  void CheckForDeprecatedFeatures(PFILE_KEYBOARD fk);
}
