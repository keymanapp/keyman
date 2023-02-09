
#include "pch.h"

#include <compfile.h>
#include <comperr.h>
#include "kmcmpdll.h"
#include <io.h>
#include <string>
#include "CheckFilenameConsistency.h"

namespace kmcmp {
  extern  KMX_CHAR CompileDir[MAX_PATH];
}
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

bool IsRelativePath(KMX_WCHAR const * p) {
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
  if (*p == u'\\') return FALSE;
#else
  if (*p == u'/') return FALSE;
#endif

  if (*p && *(p + 1) == u':') return FALSE;

  return TRUE;
}

KMX_DWORD CheckFilenameConsistency( KMX_CHAR const * Filename, bool ReportMissingFile) {
  PKMX_WCHAR WFilename = strtowstr(( KMX_CHAR *)Filename);
  KMX_DWORD const result = CheckFilenameConsistency(WFilename, ReportMissingFile);
  delete WFilename;
  return result;
}

KMX_DWORD CheckFilenameConsistency(KMX_WCHAR const * Filename, bool ReportMissingFile) {
  // not ready yet: needs more attention-> e.g. _wfindfirst,... and common includes for non-Windows platforms
  KMX_WCHAR Name[_MAX_PATH], FName[_MAX_FNAME], Ext[_MAX_EXT];
  KMX_WCHAR ErrExtra[256];
  intptr_t n;
  FILE* nfile;

  if (IsRelativePath(Filename)) {
    PKMX_WCHAR WCompileDir = strtowstr(kmcmp::CompileDir);
    u16ncpy(Name, WCompileDir, _countof(Name));  // I3481
    u16ncat(Name, Filename, _countof(Name));  // I3481
  }
  else {
    u16ncpy(Name, Filename, _countof(Name));  // I3481   // _S2 wcscpy_s(Name, _countof(Name), Filename);  // I3481
  }

#if defined(_WIN32) || defined(_WIN64)
  //  convert char16_t*  -> std::u16string -> std::string -> std::wstring -> wchar_t*
  //  char16_t* -> std::u16string
  std::u16string u16str(Name);
  //  std::u16string -> std::string
  std::string stri = string_from_u16string(u16str);
  //  std::string -> std::wstring
  std::wstring  wstr = wstring_from_string(stri);
  //  std::wstring -> wchar_t*
  const KMX_WCHART* wchptr = wstr.c_str();

  nfile = _wfsopen(wchptr, L"rb", _SH_DENYWR);
  _wfinddata_t fi;
  n = _wfindfirst(wchptr, &fi);
  _findclose(n);
#endif

  if (nfile == NULL) {
    if (ReportMissingFile) {
      u16sprintf(ErrExtra,_countof(ErrExtra),L"referenced file %ls",Filename);
      strcpy(ErrExtraLIB, wstrtostr2(ErrExtra));
      AddWarning(CWARN_MissingFile);
    }
    return CERR_None;
  }

#if defined(_WIN32) || defined(_WIN64)
  const KMX_WCHAR* cptr1 = u16rchr(Name, '\\');   // _S2 const wchar_t* cptr1 = wcsrchr(Name, '\\');
#else
  const KMX_WCHAR* cptr1 = u16rchr(Name, '/');
#endif
  cptr1++;

//TODO: sort out how to find common includes in non-Windows platforms:
#if defined(_WIN32) || defined(_WIN64)
KMX_WCHAR fi_name_char16[260];
u16sprintf(fi_name_char16,_countof(fi.name),fi.name);

  if (u16cmp(cptr1, fi_name_char16) != 0) {
    u16sprintf(ErrExtra,_countof(ErrExtra),L"reference '%ls' does not match actual filename '%ls'", cptr1, &fi.name);
    strcpy(ErrExtraLIB, wstrtostr2(ErrExtra));
    AddWarning(CHINT_FilenameHasDifferingCase);
  }
#else
  #error Missing implementation for finding common includes
#endif
  return CERR_None;
}

KMX_DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk) {
  // call() statements depend on a fairly ugly hack for js,
  // where store(DllFunction) "my.dll:func" will look for a
  // file called function.call_js. ( or should this be func.call_js ? )
  // This is ripe for rewrite!
  // But let's check what we have anyway

  PFILE_STORE sp;
  DWORD i, msg;
  for (i = 0, sp = fk->dpStoreArray; i < fk->cxStoreArray; i++, sp++) {
    if (!sp->fIsCall) continue;

    const std::u16string callsite(sp->dpString);
    const auto colon = callsite.find(':');
    if (colon == std::u16string::npos) continue;

    auto func1 = callsite.substr(colon + 1);
    std::u16string str_js(u".call_js");
    std::u16string func = func1+ str_js;

    if ((msg = CheckFilenameConsistency(func.c_str(), FALSE)) != CERR_None) {
      return msg;
    }
  }
  return CERR_None;
}
