
#include "pch.h"

#include <compfile.h>
#include <kmn_compiler_errors.h>
#include <kmcmpdll.h>

BOOL WarnDeprecatedHeader() {   // I4866
  if (FWarnDeprecatedCode) {
    AddWarning(CWARN_HeaderStatementIsDeprecated);
  }
  return TRUE;
}

/* Flag presence of deprecated features */
BOOL CheckForDeprecatedFeatures(PFILE_KEYBOARD fk) {
  /*
    For Keyman 10, we deprecated:
      // < Keyman 7
      #define TSS_LANGUAGE			4
      #define TSS_LAYOUT				5
      #define TSS_LANGUAGENAME		12
      #define TSS_ETHNOLOGUECODE		15

      // Keyman 7
      #define TSS_WINDOWSLANGUAGES 29
  */
  int oldCurrentLine = currentLine;
  DWORD i;
  PFILE_STORE sp;

  if (!FWarnDeprecatedCode) {
    return TRUE;
  }

  if (fk->version >= VERSION_100) {
    for (i = 0, sp = fk->dpStoreArray; i < fk->cxStoreArray; i++, sp++) {
      if (sp->dwSystemID == TSS_LANGUAGE ||
          sp->dwSystemID == TSS_LAYOUT ||
          sp->dwSystemID == TSS_LANGUAGENAME ||
          sp->dwSystemID == TSS_ETHNOLOGUECODE ||
          sp->dwSystemID == TSS_WINDOWSLANGUAGES) {
        currentLine = sp->line;
        AddWarning(CWARN_LanguageHeadersDeprecatedInKeyman10);
      }
    }
  }

  currentLine = oldCurrentLine;

  return TRUE;
}
