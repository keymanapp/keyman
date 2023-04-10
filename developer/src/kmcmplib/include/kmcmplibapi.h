#pragma once

#include <fstream>

#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
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
