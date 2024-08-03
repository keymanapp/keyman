#include "pch.h"

#include <kmcmplibapi.h>
#include <kmcmplib.h>
#include <kmn_compiler_errors.h>

namespace kmcmp {
  int ErrChr;
  int nErrors = 0;
  kmcmp_CompilerMessageProc msgproc = nullptr;
  void* msgprocContext = NULL;
  std::string messageFilename;
}

void ReportCompilerMessage(KMX_DWORD msg, const std::vector<std::string>& parameters) {
  const KMX_DWORD severity = msg & MESSAGE_SEVERITY_MASK;

  if (severity == CompilerErrorSeverity::Error || severity == CompilerErrorSeverity::Fatal) {
    kmcmp::nErrors++;
  }

  KMCMP_COMPILER_RESULT_MESSAGE message;
  message.errorCode = msg;
  message.lineNumber = kmcmp::currentLine + 1;
  message.columnNumber = kmcmp::ErrChr;
  message.parameters = parameters;
  message.filename = kmcmp::messageFilename;
  (*kmcmp::msgproc)(message, kmcmp::msgprocContext);

  kmcmp::ErrChr = 0;
}
