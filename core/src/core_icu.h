/**
 * ICU modules used by Keyman Core
 */
#pragma once

#if !defined(HAVE_ICU4C)
#error icu4c is required for this code
#endif

#define U_FALLTHROUGH
#include "unicode/utypes.h"
#include "unicode/unistr.h"
#include "unicode/normalizer2.h"

#include "debuglog.h"
#include <assert.h>


/** @returns true on success */
inline bool uassert_success(const char *file, int line, const char *function, UErrorCode status) {
  if (U_FAILURE(status)) {
    DebugLog2(file, line, function, "U_FAILURE(%s)", u_errorName(status));
    return false;
  } else {
    return true;
  }
}

/**
 * Assert an ICU4C UErrorCode
 * the first assert is for debug builds, the second triggers the debuglog and has the return value.
 * */
#define UASSERT_SUCCESS(status) (assert(U_SUCCESS(status)), uassert_success(__FILE__, __LINE__, __FUNCTION__, status))
