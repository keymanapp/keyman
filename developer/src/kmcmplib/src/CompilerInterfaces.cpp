#include "pch.h"
#include <kmcmplibapi.h>
#include <kmn_compiler_errors.h>
#include "kmcmplib.h"
#include "../../../../common/windows/cpp/include/ConvertUTF.h"
#include "CompileKeyboardBuffer.h"

EXTERN bool kmcmp_CompileKeyboard(
  const char* pszInfile,
  const KMCMP_COMPILER_OPTIONS& options,
  kmcmp_CompilerMessageProc messageProc,
  kmcmp_LoadFileProc loadFileProc,
  const void* procContext,
  KMCMP_COMPILER_RESULT& result
) {

  FILE_KEYBOARD fk;
  fk.extra = new KMCMP_COMPILER_RESULT_EXTRA;
  fk.extra->kmnFilename = pszInfile;

  kmcmp::FSaveDebug = options.saveDebug;   // I3681
  kmcmp::FCompilerWarningsAsErrors = options.compilerWarningsAsErrors;   // I4865
  AWarnDeprecatedCode_GLOBAL_LIB = options.warnDeprecatedCode;
  kmcmp::FShouldAddCompilerVersion = options.shouldAddCompilerVersion;
  kmcmp::CompileTarget = options.target;

  if (!messageProc || !loadFileProc || !pszInfile) {
    AddCompileError(CERR_BadCallParams);
    return FALSE;
  }

  msgproc = messageProc;
  loadfileproc = loadFileProc;
  msgprocContext = (void*)procContext;
  kmcmp::currentLine = 0;
  kmcmp::nErrors = 0;

  int sz;
  if(!loadFileProc(pszInfile, "", nullptr, &sz, msgprocContext)) {
    AddCompileError(CERR_InfileNotExist);
    return FALSE;
  }

  if(sz < 3) {
    // Technically, a 3 byte file can never be a valid .kmn, so we can shortcut
    // here and avoid testing outside memory bounds for looking at BOM
    AddCompileError(CERR_CannotReadInfile);
    return FALSE;
  }

  KMX_BYTE* infile = new KMX_BYTE[sz+1];
  if(!infile) {
    AddCompileError(CERR_CannotAllocateMemory);
    return FALSE;
  }
  if(!loadFileProc(pszInfile, "", infile, &sz, msgprocContext)) {
    delete[] infile;
    AddCompileError(CERR_CannotReadInfile);
    return FALSE;
  }
  infile[sz] = 0; // zero-terminate for safety, not technically needed but helps avoid memory bugs

  int offset = 0;
  if(infile[0] == (KMX_BYTE) UTF16Sig[0] && infile[1] == (KMX_BYTE) UTF16Sig[1]) {
    // UTF-16 source file
    offset = 2;
  } else {
    // UTF-8 source file
    KMX_BYTE* infile16;
    int sz16;
    if(!UTF16TempFromUTF8(infile, sz, &infile16, &sz16)) {
      delete[] infile;
      AddCompileError(CERR_CannotCreateTempfile);
      return FALSE;
    }
    delete[] infile;
    infile = infile16;
    sz = sz16;
  }

  kmcmp::CodeConstants = new kmcmp::NamedCodeConstants;
  bool success = CompileKeyboardBuffer(infile+offset, sz-offset, &fk);
  delete kmcmp::CodeConstants;

  delete[] infile;

  if (kmcmp::nErrors > 0 || !success) {
    return FALSE;
  }

  // fill in result data
  KMX_DWORD msg;
  KMX_BYTE* data = nullptr;
  size_t dataSize = 0;
  msg = WriteCompiledKeyboard(&fk, &data, dataSize);

  //TODO: FreeKeyboardPointers(fk);

  if(msg != CERR_None) {
    AddCompileError(msg);
    return FALSE;
  }

  result.kmx = data;
  result.kmxSize = dataSize;
  result.extra = *fk.extra;

  return TRUE;
}

