
#include "pch.h"

#include "compfile.h"
#include <comperr.h>
#include "kmcmplib.h"
#include <string>
#include "CheckFilenameConsistency.h"
#include "kmx_u16.h"
#include "filesystem.h"

#ifdef _MSC_VER
#include <io.h>
#endif

namespace kmcmp {
  extern  KMX_CHAR CompileDir[260]; // TODO: this should not be a fixed buffer
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
  KMX_WCHAR Name[260];  // TODO: fixed buffer sizes bad

  if (IsRelativePath(Filename)) {
    PKMX_WCHAR WCompileDir = strtowstr(kmcmp::CompileDir);
    u16ncpy(Name, WCompileDir, _countof(Name));  // I3481
    u16ncat(Name, Filename, _countof(Name));  // I3481
    delete[] WCompileDir;
  }
  else {
    u16ncpy(Name, Filename, _countof(Name));  // I3481   // _S2 wcscpy_s(Name, _countof(Name), Filename);  // I3481
  }

#ifndef _MSC_VER
  // Filename consistency only needs to be checked on Windows, because other
  // platforms are going to fail if the filename is inconsistent anyway!
  if(!FileExists(Name)) {
    if (ReportMissingFile) {
      u16cpy(ErrExtraW, u"referenced file '");
      u16ncat(ErrExtraW, 256, FileName);
      u16ncat(ErrExtraW, 256, u"'");
      strcpy(ErrExtraLIB, string_from_u16string(ErrExtraW).c_str());
      AddWarning(CWARN_MissingFile);
    }
    return CERR_None;
  }
  return CERR_None;
#else
  _wfinddata_t fi;
  intptr_t n;
  if ((n = _wfindfirst((const wchar_t*) Name, &fi)) == -1) {
    if (ReportMissingFile) {
      sprintf(ErrExtraLIB, "referenced file '%ls'", (wchar_t*) Filename);
      AddWarning(CWARN_MissingFile);
    }
    return CERR_None;
  }

  _findclose(n);

  KMX_WCHAR FName[_MAX_FNAME], Ext[_MAX_EXT];
  wchar_t WChName[_MAX_PATH];
  _wsplitpath_s((const wchar_t*)Filename, nullptr, 0, nullptr, 0, (wchar_t*) FName, _MAX_FNAME, (wchar_t*) Ext, _MAX_EXT);
  _wmakepath_s(WChName, _MAX_PATH, nullptr, nullptr, (const wchar_t*) FName, (const wchar_t*) Ext);
  if (wcscmp(WChName, fi.name) != 0) {
    sprintf(ErrExtraLIB, "reference '%ls' does not match actual filename '%ls'", WChName, fi.name);

    AddWarning(CHINT_FilenameHasDifferingCase);
  }
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
  KMX_DWORD i, msg;
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
