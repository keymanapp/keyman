
#pragma once      // _S2

#include "pch.h"

#include "Compfile.h"         // _S2 #include <Compfile.h>
#include "comperr.h"          // _S2 #include <comperr.h>
#include "kmcmpdll.h"         // _S2 #include <kmcmpdll.h> 
#include <io.h>
#include <string>
#include "CheckFilenameConsistency.h"

// _S2 : 
// Why do we use wchar* for filenames in CheckFilenameConsistency ? (or convert them to wchar* first if called with char* )  
// In other functions (e.g. ImportBitmapFile) we do the opposite and work with char* (or convert them to to char* first if called with wchar*)
// Do we have to consider filenames compposed of Unicode-Characters > BMP?

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

// _S2 changed Version to call CheckFilenameConsistency(KMX_WCHAR const * Filename, bool ReportMissingFile)
KMX_DWORD CheckFilenameConsistency(KMX_CHAR const * Filename, BOOL ReportMissingFile) {
  
  PKMX_WCHAR WFilename = strtowstr((KMX_CHAR *)Filename);   // _S2 PWCHAR WFilename = strtowstr((char *)Filename);
  KMX_DWORD const result = CheckFilenameConsistency(WFilename, ReportMissingFile);
  delete WFilename;
  return result;
}

//  _S2 we want to use char16_t in kmcompx so this version using wchar_t should not be needed 
/*DWORD CheckFilenameConsistency(wchar_t const * Filename, BOOL ReportMissingFile) {
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
*/

// _S2 this version uses char16_t* ( but is not able to use _wfinddata_t,_wfindfirst,_wsplitpath_s, _wmakepath_s )
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

  // _S2  _wfindfirst,_wsplitpath_s, _wmakepath_s can only be used on windows but we want to use CheckFilenameConsistency on non-win platformns as well?
  /*
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

// _S2 commented out functionality because I have no idea how to change to use with char16_t on non-windows platforms
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

// _S2 added to use in CheckFilenameConsistencyCHAR
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

 // _S2 added version to use with char* but not able to use _wsplitpath_s, _wmakepath_s for adding warning of CHINT_FilenameHasDifferingCase
KMX_DWORD CheckFilenameConsistencyCHAR(KMX_CHAR const * Filename, bool ReportMissingFile)             
{
  KMX_CHAR Name[_MAX_PATH],Print_Name[_MAX_PATH], FName[_MAX_FNAME], Ext[_MAX_EXT];
  
    KMX_CHAR ErrExtra[256];                                                             // _S2   added
    FILE* fi;                                                                           // _S2 _wfinddata_t fi;
    intptr_t n;

    if (IsRelativePath(Filename)) {
      PKMX_CHAR WCompileDir = (CompileDir);                                             // _S2 PKMX_WCHAR WCompileDir = strtowstr(CompileDir);
      strcpy_s(Name, _countof(Name), WCompileDir);  // I3481                            // _S2 wcscpy_s(Name, _countof(Name), WCompileDir);  // I3481
      strcat_s(Name, _countof(Name),  Filename);  // I3481                              // _S2 wcscat_s(Name, _countof(Name), Filename);  // I3481
    }
    else {
      strcpy_s(Name, _countof(Name), Filename);  // I3481                               // _S2 wcscpy_s(Name, _countof(Name), Filename);  // I3481
    }

	fi = fopen(Filename, "rb");
	if (fi == NULL){
      if (ReportMissingFile) 
      {	
        strcpy_s(Print_Name, _countof(Print_Name), "referenced file ");
        strcat_s(Print_Name, _countof(Print_Name), Filename);  // I3481    
        sprintf(ErrExtra, Print_Name);                                                    
        AddWarning(CWARN_MissingFile);
      }
      return CERR_None;
    }
                                                                                        /*   _ S2 
                                                                                        if ((n = _wfindfirst(Name, &fi)) == -1) {
                                                                                          if (ReportMissingFile) {
                                                                                            wsprintf(ErrExtra, "referenced file '%ls'", Filename);
                                                                                            AddWarning(CWARN_MissingFile);
                                                                                          }
                                                                                          return CERR_None;
                                                                                        }

    // _S2 how would one do that using KMX_CHAR?
    // do you have upper/lowercase in filenames?                                        _wsplitpath_s(Filename, nullptr, 0, nullptr, 0, FName, _MAX_FNAME, Ext, _MAX_EXT);
                                                                                        _wmakepath_s(Name, _MAX_PATH, nullptr, nullptr, FName, Ext);
                                                                                        if (wcscmp(Name, fi.name) != 0) {
                                                                                          wsprintf(ErrExtra, "reference '%ls' does not match actual filename '%ls'", Name, fi.name);
                                                                                          AddWarning(CHINT_FilenameHasDifferingCase);
                                                                                        }
                                                                                      */


    return CERR_None;
}
