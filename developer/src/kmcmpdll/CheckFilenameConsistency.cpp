
#include "pch.h"

#include <compfile.h>
#include <kmn_compiler_errors.h>
#include <kmcmpdll.h>
#include <io.h>
#include <string>
#include "CheckFilenameConsistency.h"

extern char CompileDir[MAX_PATH];

BOOL FileExists(char const * filename) {
  _finddata_t fi;
  intptr_t n;

  if ((n = _findfirst(filename, &fi)) != -1) {
    _findclose(n);
    return TRUE;
  }
  return FALSE;
}

BOOL IsRelativePath(char const * p) {
  // Relative path (returns TRUE):
  //  ..\...\BITMAP.BMP
  //  PATH\BITMAP.BMP
  //  BITMAP.BMP

  // Semi-absolute path (returns FALSE):
  //  \...\BITMAP.BMP

  // Absolute path (returns FALSE):
  //  C:\...\BITMAP.BMP
  //  \\SERVER\SHARE\...\BITMAP.BMP

  if (*p == '\\') return FALSE;
  if (*p && *(p + 1) == ':') return FALSE;

  return TRUE;
}

BOOL IsRelativePath(wchar_t const * p) {
  // Relative path (returns TRUE):
  //  ..\...\BITMAP.BMP
  //  PATH\BITMAP.BMP
  //  BITMAP.BMP

  // Semi-absolute path (returns FALSE):
  //  \...\BITMAP.BMP

  // Absolute path (returns FALSE):
  //  C:\...\BITMAP.BMP
  //  \\SERVER\SHARE\...\BITMAP.BMP

  if (*p == L'\\') return FALSE;
  if (*p && *(p + 1) == L':') return FALSE;

  return TRUE;
}

DWORD CheckFilenameConsistency(char const * Filename, BOOL ReportMissingFile) {
  PWCHAR WFilename = strtowstr((char *)Filename);
  DWORD const result = CheckFilenameConsistency(WFilename, ReportMissingFile);
  delete WFilename;
  return result;
}

DWORD CheckFilenameConsistency(wchar_t const * Filename, BOOL ReportMissingFile) {
  WCHAR Name[_MAX_PATH], FName[_MAX_FNAME], Ext[_MAX_EXT];
  _wfinddata_t fi;
  intptr_t n;

  if (IsRelativePath(Filename)) {
    PWCHAR WCompileDir = strtowstr(CompileDir);
    wcscpy_s(Name, _countof(Name), WCompileDir);  // I3481
    wcscat_s(Name, _countof(Name), Filename);  // I3481
  }
  else {
    wcscpy_s(Name, _countof(Name), Filename);  // I3481
  }

  if ((n = _wfindfirst(Name, &fi)) == -1) {
    if (ReportMissingFile) {
      wsprintf(ErrExtra, "referenced file '%ls'", Filename);
      AddWarning(CWARN_MissingFile);
    }
    return CERR_None;
  }

  _wsplitpath_s(Filename, nullptr, 0, nullptr, 0, FName, _MAX_FNAME, Ext, _MAX_EXT);
  _wmakepath_s(Name, _MAX_PATH, nullptr, nullptr, FName, Ext);
  if (wcscmp(Name, fi.name) != 0) {
    wsprintf(ErrExtra, "reference '%ls' does not match actual filename '%ls'", Name, fi.name);
    AddWarning(CHINT_FilenameHasDifferingCase);
  }
  return CERR_None;
}

DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk) {
  // call() statements depend on a fairly ugly hack for js,
  // where store(DllFunction) "my.dll:func" will look for a
  // file called function.call_js. This is ripe for rewrite!
  // But let's check what we have anyway
  PFILE_STORE sp;
  DWORD i, msg;
  for (i = 0, sp = fk->dpStoreArray; i < fk->cxStoreArray; i++, sp++) {
    if (!sp->fIsCall) continue;

    const std::wstring callsite(sp->dpString);
    const auto colon = callsite.find(':');
    if (colon == std::wstring::npos) continue;

    auto func = callsite.substr(colon + 1);
    func.append(L".call_js");
    if ((msg = CheckFilenameConsistency(func.c_str(), FALSE)) != CERR_None) {
      return msg;
    }
  }
  return CERR_None;
}
