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

bool CompileKeyboardHandle(KMX_BYTE* infile, int sz, PFILE_KEYBOARD fk);

#ifdef __EMSCRIPTEN__

/*
  WASM interface for compiler message callback
*/
EM_JS(int, wasm_msgproc, (int line, int msgcode, const char* text, char* context), {
  const proc = globalThis[UTF8ToString(context)];
  if(!proc || typeof proc != 'function') {
    console.log(`[${line}: ${msgcode}: ${UTF8ToString(text)}]`);
    return 0;
  } else {
    return proc(line, msgcode, UTF8ToString(text));
  }
});

EM_JS(bool, wasm_loadfileproc, (const char* filename, const char* baseFilename, void* buffer, int* bufferSize, char* context), {
  const proc = globalThis[UTF8ToString(context)];
  if(!proc || typeof proc != 'function') {
    return 0;
  } else {
    return proc(UTF8ToString(filename), UTF8ToString(baseFilename), buffer, bufferSize);
  }
});

bool wasm_LoadFileProc(const char* filename, const char* baseFilename, void* buffer, int* bufferSize, void* context) {
  char* msgProc = static_cast<char*>(context);
  return wasm_loadfileproc(filename, baseFilename, buffer, bufferSize, msgProc);
}

int wasm_CompilerMessageProc(int line, uint32_t dwMsgCode, const char* szText, void* context) {
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
    wasm_LoadFileProc,
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

  FILE_KEYBOARD fk;

  kmcmp::FSaveDebug = options.saveDebug;   // I3681
  kmcmp::FCompilerWarningsAsErrors = options.compilerWarningsAsErrors;   // I4865
  AWarnDeprecatedCode_GLOBAL_LIB = options.warnDeprecatedCode;
  kmcmp::FShouldAddCompilerVersion = options.shouldAddCompilerVersion;
  kmcmp::CompileTarget = options.target;

  if (!messageProc || !loadFileProc || !pszInfile) {
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

  KMX_BYTE* infile = new KMX_BYTE[sz];
  if(!infile) {
    AddCompileError(CERR_CannotAllocateMemory);
    return FALSE;
  }
  if(!loadFileProc(pszInfile, "", infile, &sz, msgprocContext)) {
    delete[] infile;
    AddCompileError(CERR_CannotReadInfile);
    return FALSE;
  }

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
  bool success = CompileKeyboardHandle(infile+offset, sz-offset, &fk);
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
  result.kvksFilename = string_from_u16string(fk.extra->kvksFilename); // convert to UTF8

  return TRUE;
}

bool CompileKeyboardHandle(KMX_BYTE* infile, int sz, PFILE_KEYBOARD fk)
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

  int offset = 0;

  // must preprocess for group and store names -> this isn't really necessary, but never mind!
  while ((msg = ReadLine(infile, sz, offset, str, TRUE)) == CERR_None)
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

  offset = 0;
  kmcmp::currentLine = 0;

  /* Reindex the list of codeconstants after stores added */

  kmcmp::CodeConstants->reindex();

  /* ReadLine will automatically skip over $Keyman lines, and parse wrapped lines */
  while ((msg = ReadLine(infile, sz, offset, str, FALSE)) == CERR_None)
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
