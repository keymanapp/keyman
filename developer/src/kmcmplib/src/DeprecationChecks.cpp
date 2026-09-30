
#include "pch.h"

#include "compfile.h"
#include <kmn_compiler_errors.h>
#include "kmcmplib.h"
#include "DeprecationChecks.h"

void kmcmp::WarnDeprecatedHeader() {   // I4866
  if (AWarnDeprecatedCode_GLOBAL_LIB) {
    // We warn on this for any keyboard version; keyboard authors should
    // be moving to system stores
    ReportCompilerMessage(KmnCompilerMessages::WARN_HeaderStatementIsDeprecated);
  }
}

void kmcmp::WarnDeprecatedValueFormat() {
  if (AWarnDeprecatedCode_GLOBAL_LIB) {
    // We warn on this for any keyboard version; keyboard authors should
    // be moving to U+xxxx format
    ReportCompilerMessage(KmnCompilerMessages::WARN_DeprecatedValueFormat);
  }
}

void kmcmp::WarnDeprecatedCompileTarget(PFILE_KEYBOARD fk, const KMX_WCHAR *compileTarget) {
  if (AWarnDeprecatedCode_GLOBAL_LIB && fk->version >= VERSION_190) {
    // We will warn on this for any keyboard version >= 19
    ReportCompilerMessage(KmnCompilerMessages::WARN_DeprecatedCompileTarget, {
      /* compileTarget */ string_from_u16string(compileTarget)
    });
  }
}

void kmcmp::WarnDeprecatedStatement(PFILE_KEYBOARD fk, std::string const &statement, KMX_DWORD version, std::string const &versionString) {
  if(AWarnDeprecatedCode_GLOBAL_LIB && fk->version >= version) {
    ReportCompilerMessage(KmnCompilerMessages::WARN_DeprecatedStatement, {statement, versionString});  // I3438
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
