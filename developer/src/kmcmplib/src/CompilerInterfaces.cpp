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

struct WASM_COMPILER_INTERFACE {
  std::string messageCallback;    // int line, uint32_t dwMsgCode, char* szText
  std::string loadFileCallback;   // TODO: char* filename, char* baseFilename --> buffer
};

struct WASM_COMPILER_RESULT {
  bool result;
  // Following are pointer offsets in heap + buffer size
  int kmx;
  int kmxSize;
  // Following are compiler side-channel data, required for
  // follow-on transform
  std::string kvksFilename;
  // TODO: additional data to be passed back
};

WASM_COMPILER_RESULT kmcmp_wasm_compile(std::string pszInfile, const KMCMP_COMPILER_OPTIONS options, const WASM_COMPILER_INTERFACE intf) {
  WASM_COMPILER_RESULT r = {false};
  KMCMP_COMPILER_RESULT kr;

  r.kmx = 0;
  r.kmxSize = 0;
  r.kvksFilename = "";

  r.result = kmcmp_CompileKeyboard(
    pszInfile.c_str(),
    options,
    wasm_CompilerMessageProc,
    nullptr, //wasm_LoadFileProc,
    intf.messageCallback.c_str(),
    kr
  );

  if(r.result) {
    // TODO: additional data as required by kmc_kmw
    r.kmx = (int) kr.kmx;
    r.kmxSize = (int) kr.kmxSize;
    r.kvksFilename = kr.kvksFilename;
  }

  return r;
}

EMSCRIPTEN_BINDINGS(compiler_interface) {

  emscripten::class_<KMCMP_COMPILER_OPTIONS>("CompilerOptions")
    .constructor<>()
    .property("saveDebug", &KMCMP_COMPILER_OPTIONS::saveDebug)
    .property("compilerWarningsAsErrors", &KMCMP_COMPILER_OPTIONS::compilerWarningsAsErrors)
    .property("warnDeprecatedCode", &KMCMP_COMPILER_OPTIONS::warnDeprecatedCode)
    .property("shouldAddCompilerVersion", &KMCMP_COMPILER_OPTIONS::shouldAddCompilerVersion)
    .property("target", &KMCMP_COMPILER_OPTIONS::target)
    ;

  emscripten::class_<WASM_COMPILER_INTERFACE>("CompilerInterface")
    .constructor<>()
    .property("messageCallback", &WASM_COMPILER_INTERFACE::messageCallback)
    .property("loadFileCallback", &WASM_COMPILER_INTERFACE::loadFileCallback)
    ;

  emscripten::class_<WASM_COMPILER_RESULT>("CompilerResult")
    .constructor<>()
    .property("result", &WASM_COMPILER_RESULT::result)
    .property("kmx", &WASM_COMPILER_RESULT::kmx)
    .property("kmxSize", &WASM_COMPILER_RESULT::kmxSize)
    .property("kvksFilename", &WASM_COMPILER_RESULT::kvksFilename)
    ;

  emscripten::function("kmcmp_compile", &kmcmp_wasm_compile);
  emscripten::function("kmcmp_parseUnicodeSet", &kmcmp_parseUnicodeSet);
}

#endif

EXTERN bool kmcmp_CompileKeyboard(
  const char* pszInfile,
  const KMCMP_COMPILER_OPTIONS& options,
  kmcmp_CompilerMessageProc messageProc,
  kmcmp_LoadFileProc loadFileProc,
  const void* procContext,
  KMCMP_COMPILER_RESULT& result
) {

  FILE* fp_in = NULL;
  KMX_CHAR str[260];
  FILE_KEYBOARD fk;

  kmcmp::FSaveDebug = options.saveDebug;   // I3681
  kmcmp::FCompilerWarningsAsErrors = options.compilerWarningsAsErrors;   // I4865
  AWarnDeprecatedCode_GLOBAL_LIB = options.warnDeprecatedCode;
  kmcmp::FShouldAddCompilerVersion = options.shouldAddCompilerVersion;
  kmcmp::CompileTarget = options.target;

  if (!messageProc || !pszInfile) { // TODO: add loadFileProc
    AddCompileError(CERR_BadCallParams);
    return FALSE;
  }

  PKMX_STR p;

  if ((p = strrchr_slash((char*)pszInfile)) != nullptr) {
    strncpy(kmcmp::CompileDir, pszInfile, (int)(p - pszInfile + 1));  // I3481
    kmcmp::CompileDir[(int)(p - pszInfile + 1)] = 0;
  }
  else {
    kmcmp::CompileDir[0] = 0;
  }

  msgproc = messageProc;
  //TODO: loadfileproc = loadFileProc;
  msgprocContext = (void*)procContext;
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
  bool success = CompileKeyboardHandle(fp_in, &fk);
  delete kmcmp::CodeConstants;

  fclose(fp_in);

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
  result.kvksFilename = string_from_u16string(fk.extra->kvksFilename); // convert to UTF8

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
