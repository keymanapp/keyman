/**
 * ICU modules used by Keyman Core
 */
#pragma once

#define KMN_NO_ICU 0 /* Temporary - keep ICU in for now while checking out build issues */

#ifdef __EMSCRIPTEN__
// define this in tests to keep ICU around
# if !defined(KMN_IN_LDML_TESTS)
#  if !defined(KMN_NO_ICU)
// under wasm, turn off ICU except in tests.
#   define KMN_NO_ICU 1
#  endif
# endif
#elif !defined(KMN_NO_ICU)
# define KMN_NO_ICU 0
#endif

#if KMN_NO_ICU

// NO ICU

// any shims needed here for disabling ICU

#else

// YES ICU

#if !defined(HAVE_ICU4C)
# error icu4c is required for this code
#endif

#define U_FALLTHROUGH
#include "unicode/utypes.h"
#include "unicode/unistr.h"
#include "unicode/normalizer2.h"

#include "keyman_core.h"
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

// ------------------ some ICU C++ utilities ----------------------------

namespace km {
namespace core {
namespace util {

/**
 * Convert a UnicodeString to a km_core_usv array
 * @return the 0-terminated array. Caller owns storage.
 */
km_core_usv *unicode_string_to_usv(icu::UnicodeString& src);

/**
 * Internal function to normalize with a specified mode.
 * Note: that this function _does_ assert failure, so it is not
 * required to assert its return code. The return is provided so
 * that callers can exit (such as making no change) if there was failure.
 *
 * Also note that "failure" here is something catastrophic: ICU not initialized,
 * or, more likely, some low memory situation. Does not fail on "bad" data.
 * @param n the ICU Normalizer to use
 * @param str input/output string
 * @param status error code, must be initialized on input
 * @return false if failure
 */
bool normalize(const icu::Normalizer2 *n, std::u16string &str, UErrorCode &status);


}
}
}

#endif /* KMN_NO_ICU */
