#pragma once

#include "compfile.h"
#include "kmcmplib.h"

KMX_DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk);
KMX_DWORD CheckFilenameConsistency(KMX_CHAR const * Filename, bool ReportMissingFile);
KMX_DWORD CheckFilenameConsistency(KMX_WCHAR const * Filename, bool ReportMissingFile);

bool IsRelativePath(KMX_CHAR const * p);
bool IsRelativePath(KMX_WCHAR const * p);
