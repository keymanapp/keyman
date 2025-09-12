#pragma once

#include <string>
#include <vector>
#include "kmcmplibapi.h"
#include "kmn_compiler_errors.h"

class CompilerMessage {
public:
  virtual void report(enum KmnCompilerMessages::KmnCompilerMessages msg, const std::vector<std::string>& parameters) = 0;
  virtual void report(enum KmnCompilerMessages::KmnCompilerMessages msg) = 0;
};

class DefaultCompilerMessage : public CompilerMessage {
  virtual void report(enum KmnCompilerMessages::KmnCompilerMessages msg, const std::vector<std::string>& parameters) ;
  virtual void report(enum KmnCompilerMessages::KmnCompilerMessages msg);
};

namespace kmcmp {
  extern kmcmp_CompilerMessageProc msgproc;
  extern void* msgprocContext;
  extern std::string messageFilename;
}

void ReportCompilerMessage(KMX_DWORD msg, const std::vector<std::string>& parameters = {});
