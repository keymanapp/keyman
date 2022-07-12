#pragma once

#include <windows.h>
#include "../../../../developer/kmcompx/include/Compfile.h"         // _S2 #include <Compfile.h>


KMX_DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk);
KMX_DWORD CheckFilenameConsistency(   char const * Filename, BOOL ReportMissingFile);
KMX_DWORD CheckFilenameConsistency(wchar_t const * Filename, BOOL ReportMissingFile);
KMX_DWORD CheckFilenameConsistency(KMX_WCHAR const * Filename, bool ReportMissingFile) ;

bool FileExists(char const * filename);
bool IsRelativePath(char const * p);
bool IsRelativePath(wchar_t const * p);
bool IsRelativePath(char16_t const * p) ;

//******** old *****************
/*
#include <windows.h>
#include "../../../../developer/kmcompx/include/Compfile.h"         // _S2 #include <Compfile.h>


KMX_DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk);
KMX_DWORD CheckFilenameConsistency(   char const * Filename, BOOL ReportMissingFile);
KMX_DWORD CheckFilenameConsistency(wchar_t const * Filename, BOOL ReportMissingFile);
bool FileExists(char const * filename);
bool IsRelativePath(char const * p);
bool IsRelativePath(wchar_t const * p);
*/