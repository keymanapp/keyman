
#define _SILENCE_EXPERIMENTAL_FILESYSTEM_DEPRECATION_WARNING 1
#include "pch.h"
#include "compfile.h"
#include <comperr.h>
#include "kmcmplib.h"
#include <io.h>
#include <string>
#include "CheckFilenameConsistency.h"
#include "kmx_u16.h"
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;
using std::experimental::filesystem::directory_iterator;

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
  // Comment for non-windows platforms: If files are different in casing only CWARN_MissingFile 
  // will be added. CHINT_FilenameHasDifferingCase will not be added on those platforms.

  KMX_WCHAR Name[_MAX_PATH], FName[_MAX_FNAME], Ext[_MAX_EXT];
  intptr_t n;
  FILE* nfile;

  if (IsRelativePath(Filename)) {
    PKMX_WCHAR WCompileDir = strtowstr(kmcmp::CompileDir);
    u16ncpy(Name, WCompileDir, _countof(Name));  // I3481
    u16ncat(Name, Filename, _countof(Name));  // I3481
  }
  else
    u16ncpy(Name, Filename, _countof(Name));  // I3481

  const KMX_WCHAR* pName = Name;
  nfile = Open_File(pName, u"rb");

  if (nfile == NULL) {
    if (ReportMissingFile) {
      u16sprintf(ErrExtraW,256,L"referenced file %ls",Filename);
      strcpy(ErrExtraLIB, wstrtostr2(ErrExtraW));
      AddWarning(CWARN_MissingFile);
    }
    return CERR_None;
  }
  fclose(nfile);

  const KMX_WCHAR* cptr1 = u16rchr_slash((const PKMX_WCHAR) Name);
  cptr1++;

  const KMX_WCHAR* dir_file_16;

  for (const auto & file : directory_iterator(kmcmp::CompileDir)) {
    std::string dir_file_path{ file.path().u8string() };
    std::u16string dir_file_path_str = u16string_from_string(dir_file_path);
    const KMX_WCHAR* dir_file_path_16 = dir_file_path_str.c_str();
    dir_file_16 = u16rchr_slash(dir_file_path_16);
    dir_file_16++;

    if (u16icmp(cptr1, dir_file_16) == 0) {
      if (u16cmp(cptr1, dir_file_16) != 0) {
        u16sprintf(ErrExtraW, 256, L"reference '%ls' does not match actual filename '%ls'", cptr1, dir_file_16);
        strcpy(ErrExtraLIB, wstrtostr2(ErrExtraW));
        AddWarning(CHINT_FilenameHasDifferingCase);
      }
    }
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
