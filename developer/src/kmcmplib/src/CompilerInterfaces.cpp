#include "pch.h"

#include <kmcmplibapi.h>
#include <kmn_compiler_errors.h>
#include "kmcmplib.h"
#include "filesystem.h"
#include "CheckFilenameConsistency.h"
#include "CheckNCapsConsistency.h"
#include "DeprecationChecks.h"
#include "versioning.h"
#include "../../../../common/windows/cpp/include/ConvertUTF.h"
#include "../../../../common/windows/cpp/include/keymanversion.h"

bool CompileKeyboardHandle(FILE* fp_in, PFILE_KEYBOARD fk);
bool CompileKeyboard(const char* pszInfile,
  void* pfkBuffer, bool ASaveDebug, bool ACompilerWarningsAsErrors,
	bool AWarnDeprecatedCode, kmcmp_CompilerMessageProc pMsgproc, const void* AmsgprocContext,
  int Target);

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
  const proc = globalThis[UTF8ToString(context)];
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

//DEPRECATED
EXTERN bool kmcmp_Wasm_SetCompilerOptions(int ShouldAddCompilerVersion) {
  KMCMP_COMPILER_OPTIONS options;
  options.dwSize = sizeof(KMCMP_COMPILER_OPTIONS);
  options.ShouldAddCompilerVersion = ShouldAddCompilerVersion;
  return kmcmp_SetCompilerOptions(&options);
}

//DEPRECATED
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

EXTERN int kmcmp_Wasm_ParseUnicodeSet(char* pat,
  uint32_t* buf, int length
) {
  return kmcmp_ParseUnicodeSet(
    pat, buf, length
  );
}


struct COMPILER_INTERFACE {
  bool saveDebug;
  bool compilerWarningsAsErrors;
  bool warnDeprecatedCode;
  bool shouldAddCompilerVersion;
  int target;                     // CKF_KEYMAN, CKF_KEYMANWEB
  std::string messageCallback;    // int line, uint32_t dwMsgCode, char* szText
  std::string loadFileCallback;   // char* infile, char* filenameRelativeToInfile --> buffer
};

struct COMPILER_RESULT {
  bool result;
  // Following are pointer offsets in heap + buffer size
  int kmx;
  int kmxSize;
  // Following are compiler side-channel data, required for
  // follow-on transform
  std::string kvksFilename;
  // TODO: additional data to be passed back
};

COMPILER_RESULT kmcmp_compile(std::string pszInfile, const COMPILER_INTERFACE intf) {
  COMPILER_RESULT r = {false};

  FILE_KEYBOARD fk;

  // TODO: this should be included in CompileKeyboard?
  kmcmp::FShouldAddCompilerVersion = intf.shouldAddCompilerVersion;

  r.result = CompileKeyboard(
    pszInfile.c_str(),
    &fk,
    intf.saveDebug,
    intf.compilerWarningsAsErrors,
    intf.warnDeprecatedCode,
    wasm_CompilerMessageProc,
    intf.messageCallback.c_str(),
    intf.target);

  if(!r.result) {
    return r;
  }

  KMX_DWORD msg;
  KMX_BYTE* data = nullptr;
  size_t dataSize = 0;
  msg = WriteCompiledKeyboard(&fk, &data, dataSize);
  //TODO: FreeKeyboardPointers(fk);

  if(msg != CERR_None) {
    AddCompileError(msg);
    r.result = FALSE;
    return r;
  }

  r.kmx = (int) data;
  r.kmxSize = (int) dataSize;
  r.kvksFilename = string_from_u16string(fk.extra->kvksFilename); // convert to UTF8

  return r;
}

EMSCRIPTEN_BINDINGS(compiler_interface) {
  emscripten::class_<COMPILER_INTERFACE>("CompilerInterface")
    .constructor<>()
    .property("saveDebug", &COMPILER_INTERFACE::saveDebug)
    .property("compilerWarningsAsErrors", &COMPILER_INTERFACE::compilerWarningsAsErrors)
    .property("warnDeprecatedCode", &COMPILER_INTERFACE::warnDeprecatedCode)
    .property("shouldAddCompilerVersion", &COMPILER_INTERFACE::shouldAddCompilerVersion)
    .property("target", &COMPILER_INTERFACE::target)
    .property("messageCallback", &COMPILER_INTERFACE::messageCallback)
    .property("loadFileCallback", &COMPILER_INTERFACE::loadFileCallback)
    ;

  emscripten::class_<COMPILER_RESULT>("CompilerResult")
    .constructor<>()
    .property("result", &COMPILER_RESULT::result)
    .property("kmx", &COMPILER_RESULT::kmx)
    .property("kmxSize", &COMPILER_RESULT::kmxSize)
    .property("kvksFilename", &COMPILER_RESULT::kvksFilename)
    ;

  emscripten::function("kmcmp_compile", &kmcmp_compile);
}

#endif

bool CompileKeyboard(const char* pszInfile,
  void* pfkBuffer, bool ASaveDebug, bool ACompilerWarningsAsErrors,
	bool AWarnDeprecatedCode, kmcmp_CompilerMessageProc pMsgproc, const void* AmsgprocContext,
  int Target) {

  FILE* fp_in = NULL;
  KMX_CHAR str[260];

  kmcmp::FSaveDebug = ASaveDebug;   // I3681
  kmcmp::FCompilerWarningsAsErrors = ACompilerWarningsAsErrors;   // I4865
  AWarnDeprecatedCode_GLOBAL_LIB = AWarnDeprecatedCode;

  kmcmp::CompileTarget = Target;

  if (!pMsgproc || !pszInfile || !pfkBuffer) {
    AddCompileError(CERR_BadCallParams);
    return FALSE;
  }

  PKMX_STR p;

  if ((p = strrchr_slash((char*)pszInfile)) != nullptr)
  {
    strncpy(kmcmp::CompileDir, pszInfile, (int)(p - pszInfile + 1));  // I3481
    kmcmp::CompileDir[(int)(p - pszInfile + 1)] = 0;
  }
  else
    kmcmp::CompileDir[0] = 0;

  msgproc = pMsgproc;
  msgprocContext = (void*)AmsgprocContext;
  kmcmp::currentLine = 0;
  kmcmp::nErrors = 0;

  fp_in = Open_File(pszInfile, "rb");

  if (fp_in == NULL) {
    AddCompileError(CERR_InfileNotExist);
    return FALSE;
  }

  // Transfer the file to a memory stream for processing UTF-8 or ANSI to UTF-16?
  // What about really large files?  Transfer to a temp file...
  if (!fread(str, 1, 3, fp_in)) {
    fclose(fp_in);
    AddCompileError(CERR_CannotReadInfile);
    return FALSE;
  }

  fseek(fp_in, 0, SEEK_SET);
  if (str[0] == UTF8Sig[0] && str[1] == UTF8Sig[1] && str[2] == UTF8Sig[2])
    fp_in = UTF16TempFromUTF8(fp_in, TRUE);
  else if (str[0] == UTF16Sig[0] && str[1] == UTF16Sig[1])
    fseek(fp_in, 2, SEEK_SET);
  else
    fp_in = UTF16TempFromUTF8(fp_in, FALSE);
  if (fp_in == NULL) {
    AddCompileError(CERR_CannotCreateTempfile);
    return FALSE;
  }

  kmcmp::CodeConstants = new kmcmp::NamedCodeConstants;
  bool result = CompileKeyboardHandle(fp_in, static_cast<PFILE_KEYBOARD>(pfkBuffer));
  delete kmcmp::CodeConstants;

  fclose(fp_in);

  if (kmcmp::nErrors > 0) {
    return FALSE;
  }

  return result;
}

EXTERN bool kmcmp_CompileKeyboardFileToBuffer(char* pszInfile, void* pfkBuffer, bool ACompilerWarningsAsErrors, bool AWarnDeprecatedCode,
  kmcmp_CompilerMessageProc pMsgproc, void* AmsgprocContext, int Target)   // I4865   // I4866
{
  if (!pMsgproc || !pszInfile || !pfkBuffer) {
    AddCompileError(CERR_BadCallParams);
    return FALSE;
  }

  return CompileKeyboard(pszInfile, pfkBuffer, TRUE, ACompilerWarningsAsErrors, AWarnDeprecatedCode,
    pMsgproc, AmsgprocContext, Target);
}

EXTERN bool kmcmp_CompileKeyboardFile(char* pszInfile,
  char* pszOutfile, bool ASaveDebug, bool ACompilerWarningsAsErrors,
  bool AWarnDeprecatedCode, kmcmp_CompilerMessageProc pMsgproc, void* AmsgprocContext)   // I4865   // I4866
{
  if (!pMsgproc || !pszInfile || !pszOutfile) {
    AddCompileError(CERR_BadCallParams);
    return FALSE;
  }

  FILE_KEYBOARD fk;
  if(!CompileKeyboard(pszInfile, &fk, ASaveDebug, ACompilerWarningsAsErrors, AWarnDeprecatedCode,
      pMsgproc, AmsgprocContext, CKF_KEYMAN)) {
    // any errors will have been reported directly by CompileKeyboard
    return FALSE;
  }

  FILE* fp_out = Open_File(pszOutfile, "wb");
  if (fp_out == NULL) {
    AddCompileError(CERR_CannotCreateOutfile);
    return FALSE;
  }

  KMX_DWORD msg;
  KMX_BYTE* data = nullptr;
  size_t dataSize = 0;
  if ((msg = WriteCompiledKeyboard(&fk, &data, dataSize)) != CERR_None) {
    AddCompileError(msg);
  } else {
    if(fwrite(data, 1, dataSize, fp_out) != dataSize) {
      AddCompileError(CERR_UnableToWriteFully);
    }
    delete[] data;
  }

  fclose(fp_out);

  if (kmcmp::nErrors > 0) {
    remove(pszOutfile);
    return FALSE;
  }

  return TRUE;
}

bool CompileKeyboardHandle(FILE* fp_in, PFILE_KEYBOARD fk)
{
  PKMX_WCHAR str, p;

  KMX_DWORD msg;

  kmcmp::FMnemonicLayout = FALSE;

  if (!fk) {
    AddCompileError(CERR_SomewhereIGotItWrong);
    return FALSE;
  }

  str = new KMX_WCHAR[LINESIZE];
  if (!str) {
    AddCompileError(CERR_CannotAllocateMemory);
    return FALSE;
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
  fk->extra = new FILE_KEYBOARD_EXTRA;
  fk->extra->kvksFilename = u"";
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
  AddStore(fk, TSS_CUSTOMKEYMANEDITION, u"0");
  AddStore(fk, TSS_CUSTOMKEYMANEDITIONNAME, u"Keyman");

  // must preprocess for group and store names -> this isn't really necessary, but never mind!
  while ((msg = ReadLine(fp_in, str, TRUE)) == CERR_None)
  {
    p = str;
    switch (LineTokenType(&p))
    {
      case T_VERSION:
        *(p + 4) = 0;
        if ((msg = AddStore(fk, TSS_VERSION, p)) != CERR_None) {
          AddCompileError(msg);
          return FALSE;
        }
        break;

      case T_GROUP:
        if ((msg = ProcessGroupLine(fk, p)) != CERR_None) {
          AddCompileError(msg);
          return FALSE;
        }
        break;

      case T_STORE:
        if ((msg = ProcessStoreLine(fk, p)) != CERR_None) {
          AddCompileError(msg);
          return FALSE;
        }
        break;

      default:
        break;
    }
  }

  if (msg != CERR_EndOfFile) {
    AddCompileError(msg);
    return FALSE;
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
      AddCompileError(msg);
      return FALSE;
    }
  }

  if (msg != CERR_EndOfFile) {
    AddCompileError(msg);
    return FALSE;
  }

  ProcessGroupFinish(fk);

  if (kmcmp::FSaveDebug) kmcmp::RecordDeadkeyNames(fk);

  /* Add the compiler version as a system store */
  if ((msg = kmcmp::AddCompilerVersionStore(fk)) != CERR_None) {
    AddCompileError(msg);
    return FALSE;
  }

  if ((msg = BuildVKDictionary(fk)) != CERR_None) {
    AddCompileError(msg);
    return FALSE;
  }

  if ((msg = CheckFilenameConsistencyForCalls(fk)) != CERR_None) {
    AddCompileError(msg);
    return FALSE;
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
