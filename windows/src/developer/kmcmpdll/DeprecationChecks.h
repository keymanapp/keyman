#pragma once

#include <windows.h>
#include <Compfile.h>

KMX_BOOL WarnDeprecatedHeader();
KMX_BOOL CheckForDeprecatedFeatures(PFILE_KEYBOARD fk);
//......................................................

KMX_BOOL KMX_CheckForDeprecatedFeatures(PKMX_FILE_KEYBOARD fk);