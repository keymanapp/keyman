
#include "pch.h"

#include <compfile.h>
#include <comperr.h>
#include <kmcmpdll.h>
#include <io.h>
#include <string>
#include "CheckFilenameConsistency.h"

extern char CompileDir[MAX_PATH];

bool IsRelativePath(KMX_CHAR const * p) {
  // Relative path (returns TRUE):
  //  ..\...\BITMAP.BMP
  //  PATH\BITMAP.BMP
  //  BITMAP.BMP

  // Semi-absolute path (returns FALSE):
  //  \...\BITMAP.BMP

  // Absolute path (returns FALSE):
  //  C:\...\BITMAP.BMP
  //  \\SERVER\SHARE\...\BITMAP.BMP

#if defined(_WIN32) || defined(_WIN64)
  if (*p == '\\') return FALSE;
#else
  if (*p == '/') return FALSE;
#endif

  if (*p && *(p + 1) == ':') return FALSE;

  return TRUE;
}

bool IsRelativePath(KMX_WCHART const * p) {
  // Relative path (returns TRUE):
  //  ..\...\BITMAP.BMP
  //  PATH\BITMAP.BMP
  //  BITMAP.BMP

  // Semi-absolute path (returns FALSE):
  //  \...\BITMAP.BMP

  // Absolute path (returns FALSE):
  //  C:\...\BITMAP.BMP
  //  \\SERVER\SHARE\...\BITMAP.BMP

#if defined(_WIN32) || defined(_WIN64)
  if (*p == L'\\') return FALSE;
#else
  if (*p == L'/') return FALSE;
#endif

  if (*p && *(p + 1) == L':') return FALSE;

  return TRUE;
}

KMX_DWORD CheckFilenameConsistency(char const * Filename, BOOL ReportMissingFile) {
  PKMX_WCHAR WFilename = strtowstr((char *)Filename);
  KMX_DWORD const result = CheckFilenameConsistency(u16fmt(WFilename).c_str(), ReportMissingFile);
  delete WFilename;  
  return result;
}

KMX_DWORD CheckFilenameConsistency(KMX_WCHART const * Filename, bool ReportMissingFile) {
  KMX_WCHART Name[_MAX_PATH], FName[_MAX_FNAME], Ext[_MAX_EXT];
  KMX_WCHAR ErrExtra[256];
  intptr_t n;

  if (IsRelativePath(Filename)) {
    PKMX_WCHAR WCompileDir = strtowstr(CompileDir);
    wcscpy_s(Name, _countof(Name), u16fmt(WCompileDir).c_str());  // I3481
    wcscat_s(Name, _countof(Name), Filename);  // I3481
  }
  else {
    wcscpy_s(Name, _countof(Name), Filename);  // I3481
  }

#if defined(_WIN32) || defined(_WIN64)
  _wfinddata_t fi;
  n = _wfindfirst(Name, &fi);
#else
  n= access(Name,F_OK);
#endif

  if (n == -1){
    if (ReportMissingFile) {
      u16sprintf(ErrExtra,_countof(ErrExtra),L"referenced file %ls",Filename);
      AddWarning(CWARN_MissingFile);
    }
    return CERR_None;
  }

#if defined(_WIN32) || defined(_WIN64)
  const wchar_t* cptr1 = wcsrchr(Name, '\\');
#else
  const wchar_t* cptr1 = wcsrchr(Name, '/');
#endif
  cptr1++;

//TODO: sort out how to find common includes in non-Windows platforms:
#if defined(_WIN32) || defined(_WIN64)
  if (wcscmp(cptr1, fi.name) != 0) {
    u16sprintf(ErrExtra,_countof(ErrExtra),L"reference '%ls' does not match actual filename '%ls'", cptr1, &fi.name);
    AddWarning(CHINT_FilenameHasDifferingCase);
  }
#endif
  return CERR_None;
}

KMX_DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk) {
  // call() statements depend on a fairly ugly hack for js,
  // where store(DllFunction) "my.dll:func" will look for a
  // file called function.call_js. This is ripe for rewrite!
  // But let's check what we have anyway
  PFILE_STORE sp;
  DWORD i, msg;
  for (i = 0, sp = fk->dpStoreArray; i < fk->cxStoreArray; i++, sp++) {
    if (!sp->fIsCall) continue;

    const std::wstring callsite(u16fmt(sp->dpString).c_str());
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
