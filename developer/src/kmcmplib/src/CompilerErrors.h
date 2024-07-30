#pragma once

#include <string>
#include <vector>

namespace kmcmp {
  extern kmcmp_CompilerMessageProc msgproc;
}

extern void* msgprocContext;
void ReportCompilerMessage(KMX_DWORD msg, const std::vector<std::string>& parameters = {});
