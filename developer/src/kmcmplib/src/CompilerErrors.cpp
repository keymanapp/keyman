#include "pch.h"

#include <kmcmplibapi.h>
#include <kmcmplib.h>
#include <kmn_compiler_errors.h>
#include <CompMsg.h>

char ErrExtraLIB[ERR_EXTRA_LIB_LEN]; // utf-8

namespace kmcmp {
  int ErrChr;
  int nErrors = 0;
}

KMX_BOOL kmcmp::AddCompileWarning(PKMX_CHAR buf)
{
  (*msgproc)(kmcmp::currentLine + 1, CWARN_Info, buf, msgprocContext);
  return FALSE;
}

KMX_BOOL AddCompileError(KMX_DWORD msg)
{
  KMX_CHAR szText[COMPILE_ERROR_MAX_LEN];
  const KMX_CHAR* szTextp = NULL;

  if (msg & CERR_FATAL)
  {
    szTextp = GetCompilerErrorString(msg);
    (*msgproc)(kmcmp::currentLine + 1, msg, szTextp, msgprocContext);
    kmcmp::nErrors++;
    return TRUE;
  }

  if (msg & CERR_ERROR)
    kmcmp::nErrors++;
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
  if (!(*msgproc)(kmcmp::currentLine, msg, szText, msgprocContext)) return TRUE;
  return FALSE;
}
