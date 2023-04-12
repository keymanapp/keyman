#include "pch.h"

#include <kmcmplibapi.h>
#include <comperr.h>
#include "kmcmplib.h"
#include "filesystem.h"
#include "CheckFilenameConsistency.h"
#include "CheckNCapsConsistency.h"
#include "DeprecationChecks.h"
#include "versioning.h"
#include "../../../../common/windows/cpp/include/ConvertUTF.h"
#include "../../../../common/windows/cpp/include/keymanversion.h"

#define SetError(err)       { if(AddCompileError(err) || (err & CERR_FATAL)) return FALSE; }

bool CompileKeyboardHandle(FILE* fp_in, PFILE_KEYBOARD fk);

EXTERN bool kmcmp_SetCompilerOptions(KMCMP_COMPILER_OPTIONS* options) {
  //printf("°°-> changed to SetCompilerOptions() of kmcmplib \n");
  if(!options || options->dwSize < sizeof(KMCMP_COMPILER_OPTIONS)) {
    return FALSE;
  }
  kmcmp::FShouldAddCompilerVersion = options->ShouldAddCompilerVersion;
  return TRUE;
}

#ifdef __EMSCRIPTEN__

/*
  WASM interface for compiler message callback
*/
EM_JS(int, wasm_msgproc, (int line, int msgcode, char* text, char* context), {
  const proc = globalThis[context];
  if(!proc || typeof proc != 'function') {
    console.log(`[${line}: ${msgcode}: ${UTF8ToString(text)}]`);
    return 0;
  } else {
    return proc(line, msgcode, UTF8ToString(text));
  }
});

int wasm_CompilerMessageProc(int line, uint32_t dwMsgCode, char* szText, void* context) {
  char* msgProc = static_cast<char*>(context);
  return wasm_msgproc(line, dwMsgCode, szText, msgProc);
}

EXTERN bool kmcmp_Wasm_SetCompilerOptions(int ShouldAddCompilerVersion) {
  KMCMP_COMPILER_OPTIONS options;
  options.dwSize = sizeof(KMCMP_COMPILER_OPTIONS);
  options.ShouldAddCompilerVersion = ShouldAddCompilerVersion;
  return kmcmp_SetCompilerOptions(&options);
}

EXTERN bool kmcmp_Wasm_CompileKeyboardFile(char* pszInfile,
  char* pszOutfile, int ASaveDebug, int ACompilerWarningsAsErrors,
	int AWarnDeprecatedCode, char* msgProc
) {
  return kmcmp_CompileKeyboardFile(
    pszInfile,
    pszOutfile,
    ASaveDebug,
    ACompilerWarningsAsErrors,
    AWarnDeprecatedCode,
    wasm_CompilerMessageProc,
    msgProc
  );
}
#endif

EXTERN bool kmcmp_CompileKeyboardFile(char* pszInfile,
  char* pszOutfile, bool ASaveDebug, bool ACompilerWarningsAsErrors,
	bool AWarnDeprecatedCode, kmcmp_CompilerMessageProc pMsgproc, void* AmsgprocContext
) {
  FILE* fp_in = NULL;
  FILE* fp_out = NULL;
  KMX_CHAR str[260];

  //printf("°°-> changed to CompileKeyboardFile() of kmcmplib \n");

  kmcmp::FSaveDebug = ASaveDebug;
  kmcmp::FCompilerWarningsAsErrors = ACompilerWarningsAsErrors;   // I4865
  AWarnDeprecatedCode_GLOBAL_LIB = AWarnDeprecatedCode;

  kmcmp::CompileTarget = CKF_KEYMAN;

  if (!pMsgproc || !pszInfile || !pszOutfile) SetError(CERR_BadCallParams);

  PKMX_STR p;

  if ((p = strrchr_slash(pszInfile)) != nullptr)
  {
    strncpy(kmcmp::CompileDir, pszInfile, (int)(p - pszInfile + 1));  // I3481
    kmcmp::CompileDir[(int)(p - pszInfile + 1)] = 0;
  }
  else
    kmcmp::CompileDir[0] = 0;

  msgproc = pMsgproc;
  msgprocContext = AmsgprocContext;
  kmcmp::currentLine = 0;
  kmcmp::nErrors = 0;

  fp_in = Open_File(pszInfile, "rb");

  if (fp_in == NULL) {
    SetError(CERR_InfileNotExist);
  }

  // Transfer the file to a memory stream for processing UTF-8 or ANSI to UTF-16?
  // What about really large files?  Transfer to a temp file...
  if (!fread(str, 1, 3, fp_in)) {
    fclose(fp_in);
    SetError(CERR_CannotReadInfile);
  }

  fseek(fp_in, 0, SEEK_SET);
  if (str[0] == UTF8Sig[0] && str[1] == UTF8Sig[1] && str[2] == UTF8Sig[2])
    fp_in = UTF16TempFromUTF8(fp_in, TRUE);
  else if (str[0] == UTF16Sig[0] && str[1] == UTF16Sig[1])
    fseek(fp_in, 2, SEEK_SET);
  else
    fp_in = UTF16TempFromUTF8(fp_in, FALSE);
  if (fp_in == NULL) {
    SetError(CERR_CannotCreateTempfile);
  }

  fp_out = Open_File(pszOutfile, "wb");

  if (fp_out == NULL) {
    SetError(CERR_CannotCreateOutfile);
  }

  FILE_KEYBOARD fk;
  kmcmp::CodeConstants = new kmcmp::NamedCodeConstants;
  bool result = CompileKeyboardHandle(fp_in, &fk);
  if(result) {
    KMX_DWORD msg;
    if ((msg = WriteCompiledKeyboard(&fk, fp_out)) != CERR_None) {
      result = FALSE;
      AddCompileError(msg);
    }
  } else {
    AddCompileError(CERR_InvalidValue);
  }

  fclose(fp_in);
  fclose(fp_out);

  delete kmcmp::CodeConstants;

  if (kmcmp::nErrors > 0)
  {
    remove(pszOutfile);
    return FALSE;
  }

  return result;
}



EXTERN bool kmcmp_CompileKeyboardFileToBuffer(char* pszInfile, void* pfkBuffer, bool ACompilerWarningsAsErrors, bool AWarnDeprecatedCode,
  kmcmp_CompilerMessageProc pMsgproc, void* AmsgprocContext, int Target)   // I4865   // I4866
{
  //printf("°°-> changed to CompileKeyboardFileToBuffer() of kmcmplib \n");
  FILE* fp_in = NULL;
  KMX_CHAR str[260];

  kmcmp::FSaveDebug = TRUE;   // I3681
  kmcmp::FCompilerWarningsAsErrors = ACompilerWarningsAsErrors;   // I4865
  AWarnDeprecatedCode_GLOBAL_LIB = AWarnDeprecatedCode;
  kmcmp::CompileTarget = Target;

  if (!pMsgproc || !pszInfile || !pfkBuffer) {
    SetError(CERR_BadCallParams);
  }

  PKMX_STR p;

  if ((p = strrchr_slash(pszInfile)) != nullptr) {
    strncpy(kmcmp::CompileDir, pszInfile, (int)(p - pszInfile + 1));  // I3481
    kmcmp::CompileDir[(int)(p - pszInfile + 1)] = 0;
  }
  else {
    kmcmp::CompileDir[0] = 0;
  }

  msgproc = pMsgproc;
  msgprocContext = AmsgprocContext;
  kmcmp::currentLine = 0;
  kmcmp::nErrors = 0;

  fp_in = Open_File(pszInfile,"rb");

  if (fp_in == NULL) {
    SetError(CERR_InfileNotExist);
  }

  // Transfer the file to a memory stream for processing UTF-8 or ANSI to UTF-16?
  // What about really large files?  Transfer to a temp file...

  if( !fread(str,1,3,fp_in))
  {
    fclose(fp_in);
    SetError(CERR_CannotReadInfile);
  }

  fseek( fp_in,0,SEEK_SET);
  if (str[0] == UTF8Sig[0] && str[1] == UTF8Sig[1] && str[2] == UTF8Sig[2])
    fp_in = UTF16TempFromUTF8(fp_in, TRUE);
  else if (str[0] == UTF16Sig[0] && str[1] == UTF16Sig[1])
    fseek( fp_in,2,SEEK_SET);
  else
    fp_in = UTF16TempFromUTF8(fp_in, FALSE);

  kmcmp::CodeConstants = new kmcmp::NamedCodeConstants;

  bool result = CompileKeyboardHandle(fp_in, static_cast<PFILE_KEYBOARD>(pfkBuffer));
  delete kmcmp::CodeConstants;
  fclose(fp_in);

  if (kmcmp::nErrors > 0) {
    return FALSE;
  }
  return result;
}

bool CompileKeyboardHandle(FILE* fp_in, PFILE_KEYBOARD fk)
{
  PKMX_WCHAR str, p;

  KMX_DWORD msg;

  kmcmp::FMnemonicLayout = FALSE;

  if (!fk) {
    SetError(CERR_SomewhereIGotItWrong);
  }

  str = new KMX_WCHAR[LINESIZE];
  if (!str) {
    SetError(CERR_CannotAllocateMemory);
  }

  fk->KeyboardID = 0;
  fk->version = 0;
  fk->dpStoreArray = NULL;
  fk->dpGroupArray = NULL;
  fk->cxStoreArray = 0;
  fk->cxGroupArray = 0;
  fk->StartGroup[0] = fk->StartGroup[1] = -1;
  fk->szName[0] = 0;
  fk->szCopyright[0] = 0;
  fk->dwFlags = KF_AUTOMATICVERSION;
  fk->currentGroup = 0xFFFFFFFF;
  fk->currentStore = 0;
  fk->cxDeadKeyArray = 0;
  fk->dpDeadKeyArray = NULL;
  fk->cxVKDictionary = 0;  // I3438
  fk->dpVKDictionary = NULL;  // I3438

/*	fk->szMessage[0] = 0;
  fk->szLanguageName[0] = 0;*/
  fk->dwBitmapSize = 0;
  fk->dwHotKey = 0;

  kmcmp::BeginLine[BEGIN_ANSI] = -1;
  kmcmp::BeginLine[BEGIN_UNICODE] = -1;
  kmcmp::BeginLine[BEGIN_NEWCONTEXT] = -1;
  kmcmp::BeginLine[BEGIN_POSTKEYSTROKE] = -1;


  /* Add a store for the Keyman 6.0 copyright information string */

  if(kmcmp::FShouldAddCompilerVersion) {
    u16sprintf(str,LINESIZE, L"Created with Keyman Developer version %d.%d.%d.%d", KEYMAN_VersionMajor, KEYMAN_VersionMinor, KEYMAN_VersionPatch, 0);
    AddStore(fk, TSS_KEYMANCOPYRIGHT, str);
  }

  /* Add a system store for the Keyman edition number */
  u16sprintf(str, LINESIZE, L"%d", 0);  // I3481
  AddStore(fk, TSS_CUSTOMKEYMANEDITION, str);

  PKMX_WCHAR tbuf = strtowstr((KMX_CHAR*) "Keyman");
  AddStore(fk, TSS_CUSTOMKEYMANEDITIONNAME, tbuf);
  delete tbuf;

  // must preprocess for group and store names -> this isn't really necessary, but never mind!
  while ((msg = ReadLine(fp_in, str, TRUE)) == CERR_None)
  {
    p = str;
    switch (LineTokenType(&p))
    {
      case T_VERSION:
        *(p + 4) = 0;
        if ((msg = AddStore(fk, TSS_VERSION, p)) != CERR_None) SetError(msg);
        break;

      case T_GROUP:
        if ((msg = ProcessGroupLine(fk, p)) != CERR_None) SetError(msg);
        break;

      case T_STORE:
        if ((msg = ProcessStoreLine(fk, p)) != CERR_None) SetError(msg);
        break;

      default:
        break;
    }
  }

  if (msg != CERR_EndOfFile) {
    SetError(msg);
  }

  fseek( fp_in,2,SEEK_SET);
  kmcmp::currentLine = 0;

  /* Reindex the list of codeconstants after stores added */

  kmcmp::CodeConstants->reindex();

  /* ReadLine will automatically skip over $Keyman lines, and parse wrapped lines */
  while ((msg = ReadLine(fp_in, str, FALSE)) == CERR_None)
  {
    msg = ParseLine(fk, str);
    if (msg != CERR_None) {
      SetError(msg);
    }
  }

  if (msg != CERR_EndOfFile) {
    SetError(msg);
  }

  ProcessGroupFinish(fk);

  if (kmcmp::FSaveDebug) kmcmp::RecordDeadkeyNames(fk);

  /* Add the compiler version as a system store */
  if ((msg = kmcmp::AddCompilerVersionStore(fk)) != CERR_None) {
    SetError(msg);
  }

  if ((msg = BuildVKDictionary(fk)) != CERR_None) {
    SetError(msg);  // I3438
  }

  if ((msg = CheckFilenameConsistencyForCalls(fk)) != CERR_None) {
    SetError(msg);
  }

  delete str;

  if (!kmcmp::CheckKeyboardFinalVersion(fk)) {
    return FALSE;
  }

  /* Warn on inconsistent use of NCAPS */
  if (!kmcmp::FMnemonicLayout) {
    CheckNCapsConsistency(fk);
  }

  /* Flag presence of deprecated features */
  kmcmp::CheckForDeprecatedFeatures(fk);

  return TRUE;
}