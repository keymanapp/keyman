#pragma once

#include <windows.h>
#include <Compfile.h>

KMX_DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk);
KMX_DWORD CheckFilenameConsistency(KMX_CHAR const * Filename, bool ReportMissingFile);
KMX_DWORD CheckFilenameConsistency(KMX_WCHART const * Filename, bool ReportMissingFile);
bool IsRelativePath(KMX_CHAR const * p);
bool IsRelativePath(KMX_WCHART const * p);
