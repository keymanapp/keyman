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

typedef struct _KMCMP_COMPILER_OPTIONS {
  uint32_t dwSize;
  bool ShouldAddCompilerVersion;
} KMCMP_COMPILER_OPTIONS;

EXTERN bool kmcmp_SetCompilerOptions(
  KMCMP_COMPILER_OPTIONS* options
);

typedef int (*kmcmp_CompilerMessageProc)(int line, uint32_t dwMsgCode, char* szText, void* context);

EXTERN bool kmcmp_CompileKeyboardFile(
  char* pszInfile,
  char* pszOutfile,
  bool ASaveDebug,
  bool ACompilerWarningsAsErrors,
	bool AWarnDeprecatedCode,
  kmcmp_CompilerMessageProc pMsgproc,
  void* AmsgprocContext
);

/* Compile target */

#define CKF_KEYMAN    0
#define CKF_KEYMANWEB 1

EXTERN bool kmcmp_CompileKeyboardFileToBuffer(
  char* pszInfile,
  void* pfkBuffer,
  bool ACompilerWarningsAsErrors,
  bool AWarnDeprecatedCode,
  kmcmp_CompilerMessageProc pMsgproc,
  void* AmsgprocContext,
  int Target
);

typedef bool (*kmcmp_ValidateJsonMessageProc)(int64_t offset, const char* szText, void* context);

EXTERN bool kmcmp_ValidateJsonFile(
  std::fstream& f,
  std::fstream& fd,
  kmcmp_ValidateJsonMessageProc MessageProc,
  void* context
);

/**
 * kmcmp_ParseUnicodeSet is successful if it returns >= USET_OK
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
 * Function pointer to kmcmp_ParseUnicodeSet
 */
typedef int (*kmcmp_ParseUnicodeSetProc)(const char* szText, uint32_t* output, uint32_t outputLength);

/**
 * Parse a UnicodeSet into 32-bit ranges.
 * For example, "[]" will return 0 (KMCMP_USET_OK) as a zero-length set.
 * "[" will return KMCMP_ERROR_SYNTAX_ERR,
 * and "[x A-C]" will return 2 and [0x41, 0x43, 0x78, 0x78]
 * @param szText input txt, null terminated, in UTF-8 format
 * @param outputBuffer output buffer, owned by caller: Pairs of ranges in order
 * @param outputBufferSize length of output buffer. Needs to be twice the number of expected ranges
 * @return If >= KMCMP_USET_OK, number of ranges, otherwise one of the negative error values.
 */
EXTERN int kmcmp_ParseUnicodeSet(
  const char* szText,
  uint32_t* outputBuffer,
  uint32_t outputBufferSize
);

