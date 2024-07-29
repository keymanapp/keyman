#pragma once

#include <kmcmplibapi.h>
#include "compfile.h"
#include "NamedCodeConstants.h"
#include <CompilerErrors.h>

namespace kmcmp {
  extern int currentLine;
  extern KMX_BOOL FShouldAddCompilerVersion;
  extern KMX_BOOL  FSaveDebug, FCompilerWarningsAsErrors;   // I4865   // I4866
  extern int ErrChr;
  extern int nErrors;
  extern KMX_BOOL FMnemonicLayout;
  extern KMX_BOOL FOldCharPosMatching;
  extern int CompileTarget;
  extern int BeginLine[4];
  extern int currentLine;
  extern NamedCodeConstants *CodeConstants;

  void RecordDeadkeyNames(PFILE_KEYBOARD fk);
  KMX_BOOL AddCompilerVersionStore(PFILE_KEYBOARD fk);
}

extern kmcmp_LoadFileProc loadfileproc;

extern KMX_BOOL AWarnDeprecatedCode_GLOBAL_LIB;

PKMX_WCHAR strtowstr(PKMX_STR in);
PFILE_STORE FindSystemStore(PFILE_KEYBOARD fk, KMX_DWORD dwSystemID);
bool UTF16TempFromUTF8(KMX_BYTE* infile, int sz, KMX_BYTE** tempfile, int *sz16);
KMX_DWORD WriteCompiledKeyboard(PFILE_KEYBOARD fk, KMX_BYTE**data, size_t& dataSize);
KMX_BOOL AddStore(PFILE_KEYBOARD fk, KMX_DWORD SystemID, const KMX_WCHAR * str, KMX_DWORD *dwStoreID= NULL);
KMX_DWORD ReadLine(KMX_BYTE* infile, int sz, int& offset, PKMX_WCHAR wstr, KMX_BOOL PreProcess);
KMX_BOOL ParseLine(PFILE_KEYBOARD fk, PKMX_WCHAR str);
KMX_BOOL ProcessGroupLine(PFILE_KEYBOARD fk, PKMX_WCHAR p);
KMX_BOOL ProcessGroupFinish(PFILE_KEYBOARD fk);
KMX_BOOL ProcessStoreLine(PFILE_KEYBOARD fk, PKMX_WCHAR p);
int LineTokenType(PKMX_WCHAR *str);
KMX_BOOL BuildVKDictionary(PFILE_KEYBOARD fk); // I3438
