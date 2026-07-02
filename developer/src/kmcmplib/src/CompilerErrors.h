#pragma once

#include <string>
#include <vector>
#include "kmcmplibapi.h"
#include "kmn_compiler_errors.h"

class CompilerMessage {
public:
  virtual void report(enum KmnCompilerMessages::KmnCompilerMessages msg, const std::vector<std::string>& parameters) = 0;
  virtual void report(enum KmnCompilerMessages::KmnCompilerMessages msg) = 0;
  virtual ~CompilerMessage() = default;
};

class DefaultCompilerMessage : public CompilerMessage {
public:
  virtual void report(enum KmnCompilerMessages::KmnCompilerMessages msg, const std::vector<std::string>& parameters) ;
  virtual void report(enum KmnCompilerMessages::KmnCompilerMessages msg);
  virtual ~DefaultCompilerMessage() = default;
};

namespace kmcmp {
  extern kmcmp_CompilerMessageProc msgproc;
  extern void* msgprocContext;
  extern std::string messageFilename;
}

void ReportCompilerMessage(KMX_DWORD msg, const std::vector<std::string>& parameters = {});
