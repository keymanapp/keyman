
#include "pch.h"

#include "compfile.h"
#include <comperr.h>
#include "kmcmpdll.h"
#include <io.h>
#include <string>
#include "CheckFilenameConsistency.h"
#include "kmx_u16.h"

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
/*
#if defined(_WIN32) || defined(_WIN64)
  if (*p == '\\') return FALSE;
#else
  if (*p == '/') return FALSE;
#endif*/
if ((*p == '\\') || (*p == '/')) return FALSE;
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
/*
#if defined(_WIN32) || defined(_WIN64)
  if (*p == u'\\') return FALSE;
#else
  if (*p == u'/') return FALSE;
#endif
*/
if ((*p == u'\\') || (*p == u'/'))return FALSE;
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
  // not ready yet: needs more attention-> common includes for non-Windows platforms
  KMX_WCHAR Name[_MAX_PATH], FName[_MAX_FNAME], Ext[_MAX_EXT];
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
  std::wstring  Name_wstr = convert_pchar16T_To_wstr(Name);
  const KMX_WCHART* Name_wchptr = Name_wstr.c_str();

  nfile = Open_File(Name_wchptr, L"rb");

  if (nfile == NULL) {
    if (ReportMissingFile) {
      u16sprintf(ErrExtraW,256,L"referenced file %ls",Filename);
      strcpy(ErrExtraLIB, wstrtostr2(ErrExtraW));
      AddWarning(CWARN_MissingFile);
    }
    return CERR_None;
  }
  fclose(nfile);

  const KMX_WCHAR* cptr1 = u16rchr_LinWin((const PKMX_WCHAR) Name);

  cptr1++;

//TODO: sort out how to find common includes in non-Windows platforms:

KMX_WCHAR fi_name_char16[260];
#if defined(_WIN32) || defined(_WIN64)
  _wfinddata_t fi;
  n = _wfindfirst(Name_wchptr, &fi);
  _findclose(n);
  u16sprintf(fi_name_char16,_countof(fi.name),fi.name);
#else
  #error Missing implementation for finding common includes
#endif  
  if (u16cmp(cptr1, fi_name_char16) != 0) {
    u16sprintf(ErrExtraW,256,L"reference '%ls' does not match actual filename '%ls'", cptr1, &fi.name);
    strcpy(ErrExtraLIB, wstrtostr2(ErrExtraW));
    AddWarning(CHINT_FilenameHasDifferingCase);
  }

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
