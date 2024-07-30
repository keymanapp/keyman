#pragma once

#include <string>
#include <vector>

namespace kmcmp {
  extern kmcmp_CompilerMessageProc msgproc;
}

extern void* msgprocContext;
void ReportCompilerMessage(KMX_DWORD msg, const std::vector<std::string>& parameters = {}); // TODO rename to AddCompileMessage

/// Use AddWarningBool for functions that return bool or KMX_BOOL; TODO merge with AddCompileMessage
#define AddWarningBool(warn)  ReportCompilerMessage(warn)
/// Use AddWarning for functions that return KMX_DWORD; TODO merge with AddCompileMessage
#define AddWarning(warn)      ReportCompilerMessage(warn)
