#pragma once

#include <windows.h>
#include <Compfile.h>

DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk);
DWORD CheckFilenameConsistency(char const * Filename, BOOL ReportMissingFile);
DWORD CheckFilenameConsistency(wchar_t const * Filename, BOOL ReportMissingFile);
BOOL FileExists(char const * filename);
BOOL IsRelativePath(char const * p);
BOOL IsRelativePath(wchar_t const * p);
