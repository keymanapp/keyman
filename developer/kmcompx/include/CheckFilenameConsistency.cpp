#pragma once      // _S2
#include "../../../../developer/kmcompx/include/pch.h"              // _S2 #include "pch"
#include "../../../../developer/kmcompx/include/Compfile.h"         // _S2 #include <Compfile.h>
#include "../../../../developer/kmcompx/include/compiler.h"         // _S2 #include <compiler.h>
#include "../../../../developer/kmcompx/include/comperr.h"          // _S2 #include <comperr.h>
#include "../../../../developer/kmcompx/include/kmcmpdll.h"         // _S2 #include <kmcmpdll.h>
/*
#include "pch.h"
#include <compfile.h>
#include <compiler.h>
#include <comperr.h>
#include <kmcmpdll.h>
*/
#include <io.h>
#include <string>
#include "CheckFilenameConsistency.h"

extern char CompileDir[MAX_PATH];

bool FileExists(char const * filename) {
  _finddata_t fi;
  intptr_t n;

  if ((n = _findfirst(filename, &fi)) != -1) {
    _findclose(n);
    return TRUE;
  }
  return FALSE;
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

  if (*p == '\\') return FALSE;
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

  if (*p == L'\\') return FALSE;
  if (*p && *(p + 1) == L':') return FALSE;

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

  if (*p == u'\\') return FALSE;
  if (*p && *(p + 1) == u':') return FALSE;

  return TRUE;
}

KMX_DWORD CheckFilenameConsistency(KMX_CHAR const * Filename, BOOL ReportMissingFile) {
  
  PKMX_WCHAR WFilename = strtowstr((KMX_CHAR *)Filename);   // _S2 PWCHAR WFilename = strtowstr((char *)Filename);
  KMX_DWORD const result = CheckFilenameConsistency(WFilename, ReportMissingFile);
  delete WFilename;
  return result;
}
/*  _S2 we need to use char16_t so this is not needed
KMX_DWORD CheckFilenameConsistency(wchar_t const * Filename, BOOL ReportMissingFile) {
  WCHAR Name[_MAX_PATH], FName[_MAX_FNAME], Ext[_MAX_EXT];
  _wfinddata_t fi;
  intptr_t n;

  if (IsRelativePath(Filename)) {
    PKMX_WCHAR WCompileDir = strtowstr(CompileDir);
    u16ncpy(Name, WCompileDir, _countof(Name));  // I3481       // _S2 wcscpy_s(Name, _countof(Name), WCompileDir);  // I3481
    u16ncat(Name,  Filename, _countof(Name));  // I3481         // _S2 wcscat_s(Name, _countof(Name), Filename);  // I3481
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
*/
KMX_DWORD CheckFilenameConsistency(KMX_WCHAR const * Filename, bool ReportMissingFile) {
  KMX_WCHAR Name[_MAX_PATH], FName[_MAX_FNAME], Ext[_MAX_EXT];
  _wfinddata_t fi;
  intptr_t n;

  if (IsRelativePath(Filename)) {
    PKMX_WCHAR WCompileDir = strtowstr(CompileDir);
    u16ncpy(Name, WCompileDir, _countof(Name));  // I3481       // _S2 wcscpy_s(Name, _countof(Name), WCompileDir);  // I3481
    u16ncat(Name,  Filename, _countof(Name));  // I3481         // _S2 wcscat_s(Name, _countof(Name), Filename);  // I3481
  }
  else {
    u16ncpy(Name, Filename, _countof(Name));  // I3481         // _S2 wcscpy_s(Name, _countof(Name), Filename);  // I3481
  }
/*  _S2  needs to be included 
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
*/


  return CERR_None;
}




KMX_DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk) {
  // call() statements depend on a fairly ugly hack for js,
  // where store(DllFunction) "my.dll:func" will look for a
  // file called function.call_js. This is ripe for rewrite!
  // But let's check what we have anyway

  /*  _S2  needs to be included 
  PFILE_STORE sp;
  KMX_DWORD i, msg;
  
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

  */
  return CERR_None;
}
