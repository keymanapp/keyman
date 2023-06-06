#pragma once

#include <fstream>

#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
#include <emscripten/bind.h>

#else
#define EMSCRIPTEN_KEEPALIVE
#endif

#ifdef __cplusplus
#define EXTERN extern "C" EMSCRIPTEN_KEEPALIVE
#else
#define EXTERN EMSCRIPTEN_KEEPALIVE
#endif

/* Compile target */

#define CKF_KEYMAN    0
#define CKF_KEYMANWEB 1

struct KMCMP_COMPILER_OPTIONS {
  bool saveDebug;
  bool compilerWarningsAsErrors;
  bool warnDeprecatedCode;
  bool shouldAddCompilerVersion;
  int target;                     // CKF_KEYMAN, CKF_KEYMANWEB
};

struct KMCMP_COMPILER_RESULT {
  void* kmx;
  size_t kmxSize;
  std::string kvksFilename;
};

/**
 * @param szText UTF-8 string
*/
typedef int (*kmcmp_CompilerMessageProc)(int line, uint32_t dwMsgCode, const char* szText, void* context);

// parameters in UTF-8
// TODO typical usage:
// if(!kmcmp_LoadFileProc("filename.ico", "/tmp/filename.kmn", nullptr, &size)) {
//   return error;
// }
// buf = new unsigned char[size];
// if(!kmcmp_LoadFileProc("filename.ico", "/tmp/filename.kmn", buf, &size)) {
//   delete[] buf;
//   return error;
// }
typedef bool (*kmcmp_LoadFileProc)(const char* loadFilename, const char* baseFilename, void* buffer, int* bufferSize, void* context);

/**
 * @param pszInfile  UTF-8 path to file.kmn
 */
EXTERN bool kmcmp_CompileKeyboard(
  const char* pszInfile,
  const KMCMP_COMPILER_OPTIONS& options,
  kmcmp_CompilerMessageProc messageProc,
  kmcmp_LoadFileProc loadFileProc,
  const void* procContext,
  KMCMP_COMPILER_RESULT& result
);

/**
 * kmcmp_parseUnicodeSet is successful if it returns >= USET_OK
 */
static const int KMCMP_USET_OK = 0;

/**
 * Error: Unknown syntax err, failed to parse
 */
static const int KMCMP_ERROR_SYNTAX_ERR = -1;
/**
 * Error: Invalid, contains strings (`{abc}` form)
 */
static const int KMCMP_ERROR_HAS_STRINGS = -2;
/**
 * Error: Invalid, uses properties \p{Mn} or [:Mn:]
 */
static const int KMCMP_ERROR_UNSUPPORTED_PROPERTY = -3;
/**
 * Fatal: output buffer too small
 */
static const int KMCMP_FATAL_OUT_OF_RANGE = -4;

/**
 * Parse a UnicodeSet into 32-bit ranges.
 * For example, "[]" will return 0 (KMCMP_USET_OK) as a zero-length set.
 * "[" will return KMCMP_ERROR_SYNTAX_ERR,
 * and "[x A-C]" will return 2 and [0x41, 0x43, 0x78, 0x78]
 * @param text input txt, null terminated, in UTF-8 format
 * @param outputBuffer output buffer, owned by caller: Pairs of ranges in order
 * @param outputBufferSize length of output buffer. Needs to be twice the number of expected ranges
 * @return If >= KMCMP_USET_OK, number of ranges, otherwise one of the negative error values.
 */
EXTERN int kmcmp_parseUnicodeSet(
  const std::string text,
  uintptr_t outputBuffer_,
  uint32_t outputBufferSize
);

