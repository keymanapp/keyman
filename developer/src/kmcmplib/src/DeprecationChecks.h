#pragma once

#include "compfile.h"
#include "kmcmplib.h"

namespace kmcmp{
KMX_BOOL WarnDeprecatedHeader();
KMX_BOOL CheckForDeprecatedFeatures(PFILE_KEYBOARD fk);
}
