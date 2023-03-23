#pragma once

#include <fstream>

typedef struct _KMCMP_COMPILER_OPTIONS {
  uint32_t dwSize;
  bool ShouldAddCompilerVersion;
} KMCMP_COMPILER_OPTIONS;

extern "C" bool kmcmp_SetCompilerOptions(
  KMCMP_COMPILER_OPTIONS* options
);

typedef int (*kmcmp_CompilerMessageProc)(int line, uint32_t dwMsgCode, char* szText, void* context);

extern "C" uint32_t kmcmp_CompileKeyboardFile(
  char* pszInfile,
  char* pszOutfile,
  bool ASaveDebug,
  bool ACompilerWarningsAsErrors,
	bool AWarnDeprecatedCode,
  kmcmp_CompilerMessageProc pMsgproc,
  void* AmsgprocContext
);

extern "C" uint32_t kmcmp_CompileKeyboardFileToBuffer(
  char* pszInfile,
  void* pfkBuffer,
  bool ACompilerWarningsAsErrors,
  bool AWarnDeprecatedCode,
  kmcmp_CompilerMessageProc pMsgproc,
  void* AmsgprocContext,
  int Target
);

typedef bool (*kmcmp_ValidateJsonMessageProc)(int64_t offset, const char* szText, void* context);

extern "C" bool kmcmp_ValidateJsonFile(
  std::fstream& f,
  std::fstream& fd,
  kmcmp_ValidateJsonMessageProc MessageProc,
  void* context
);
