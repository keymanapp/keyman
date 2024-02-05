/*
  Copyright:    © SIL International.
  Description:  Common LDML utilities
  Create Date:  6 Jan 2024
  Authors:      Steven R. Loomis
*/

#pragma once

namespace km {
namespace core {
namespace ldml {

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

}
}
}
