
#include "pch.h"

#include "compfile.h"
#include <kmn_compiler_errors.h>
#include "kmcmplib.h"
#include "DeprecationChecks.h"

void kmcmp::WarnDeprecatedHeader() {   // I4866
  if (AWarnDeprecatedCode_GLOBAL_LIB) {
    ReportCompilerMessage(KmnCompilerMessages::WARN_HeaderStatementIsDeprecated);
  }
}

/* Flag presence of deprecated features */
void kmcmp::CheckForDeprecatedFeatures(PFILE_KEYBOARD fk) {
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
  int oldCurrentLine = kmcmp::currentLine;
  KMX_DWORD i;
  PFILE_STORE sp;

  if (!AWarnDeprecatedCode_GLOBAL_LIB) {
    return;
  }

  if (fk->version >= VERSION_100) {
    for (i = 0, sp = fk->dpStoreArray; i < fk->cxStoreArray; i++, sp++) {
      if (sp->dwSystemID == TSS_LANGUAGE ||
          sp->dwSystemID == TSS_LAYOUT ||
          sp->dwSystemID == TSS_LANGUAGENAME ||
          sp->dwSystemID == TSS_ETHNOLOGUECODE ||
          sp->dwSystemID == TSS_WINDOWSLANGUAGES) {
        kmcmp::currentLine = sp->line;
        ReportCompilerMessage(KmnCompilerMessages::WARN_LanguageHeadersDeprecatedInKeyman10);
      }
    }
  }

  kmcmp::currentLine = oldCurrentLine;
}
