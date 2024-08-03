#include "pch.h"

#include <kmcmplibapi.h>
#include <kmcmplib.h>
#include <kmn_compiler_errors.h>
#include <CompMsg.h>

char ErrExtraLIB[ERR_EXTRA_LIB_LEN]; // utf-8

namespace kmcmp {
  int ErrChr;
  int nErrors = 0;
  kmcmp_CompilerMessageProc msgproc = nullptr;
}

KMX_BOOL kmcmp::AddCompileWarning(PKMX_CHAR buf) // TODO: Used only for 'compiler version' message, so remove accordingly
{
  KMCMP_COMPILER_RESULT_MESSAGE message;
  message.errorCode = KmnCompilerMessages::INFO_Info;
  message.lineNumber = kmcmp::currentLine + 1;
  message.message = buf;
  (*msgproc)(message, msgprocContext);
  return FALSE;
}

void AddCompileError(KMX_DWORD msg)
{
  KMX_CHAR szText[COMPILE_ERROR_MAX_LEN];
  const KMX_CHAR* szTextp = NULL;
  const KMX_DWORD severity = msg & MESSAGE_SEVERITY_MASK;

  if (severity == CompilerErrorSeverity::Fatal)
  {
    szTextp = GetCompilerErrorString(msg);
    KMCMP_COMPILER_RESULT_MESSAGE message;
    message.errorCode = msg;
    message.lineNumber = kmcmp::currentLine + 1;
    message.message = szTextp;
    (*kmcmp::msgproc)(message, msgprocContext);
    kmcmp::nErrors++;
  }

  if (severity == CompilerErrorSeverity::Error) {
    kmcmp::nErrors++;
  }

  szTextp = GetCompilerErrorString(msg);

  if (szTextp) {
    strcpy(szText, szTextp);
  } else {
    snprintf(szText, COMPILE_ERROR_MAX_LEN, "Unknown error %x", msg);
  }

  if (kmcmp::ErrChr > 0) {
    char *szTextNull = strchr(szText, 0);
    snprintf(szTextNull, COMPILE_ERROR_MAX_LEN-(szTextNull-szText), " character offset: %d", kmcmp::ErrChr);
  }

  if (*ErrExtraLIB) {
    char *szTextNull = strchr(szText, 0);
    snprintf(szTextNull, COMPILE_ERROR_MAX_LEN-(szTextNull-szText), "%s", ErrExtraLIB);
  }

  kmcmp::ErrChr = 0;  *ErrExtraLIB =0;

  KMCMP_COMPILER_RESULT_MESSAGE message;
  message.errorCode = msg;
  message.lineNumber = kmcmp::currentLine + 1;
  message.message = szText;
  (*kmcmp::msgproc)(message, msgprocContext);
}
