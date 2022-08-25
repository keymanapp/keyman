#pragma once

#include <windows.h>
#include <Compfile.h>

KMX_DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk);
KMX_DWORD CheckFilenameConsistency(KMX_CHAR const * Filename, BOOL ReportMissingFile);
KMX_DWORD CheckFilenameConsistency(KMX_WCHART const * Filename, BOOL ReportMissingFile);
//BOOL FileExists(char const * filename);
BOOL IsRelativePath(KMX_CHAR const * p);
BOOL IsRelativePath(KMX_WCHART const * p);
