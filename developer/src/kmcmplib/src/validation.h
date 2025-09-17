/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-09-12
 *
 * Validation of various aspects of .kmn source files, such as identifiers
 */

#pragma once

#include "CompilerErrors.h"

class Validation {
private:
  CompilerMessage &compilerMessage;
public:
  Validation(CompilerMessage &cm) : compilerMessage(cm) {
  }

  virtual ~Validation() = default;
  virtual KMX_BOOL ValidateIdentifier(KMX_WCHAR const *p, size_t maxLength);
};

