#pragma once

#include "Compfile.h"
#include "kmcmpdll.h"

namespace kmcmp{
KMX_BOOL WarnDeprecatedHeader();
KMX_BOOL CheckForDeprecatedFeatures(PFILE_KEYBOARD fk);
}
