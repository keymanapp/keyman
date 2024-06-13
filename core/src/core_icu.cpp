/*
  Copyright:    © SIL International.
  Description:  Common LDML utilities
  Create Date:  24 May 2024
  Authors:      Steven R. Loomis
*/

#include "core_icu.h"

#if !KMN_NO_ICU

namespace km {
namespace core {
namespace util {

/**
 * Helper to convert icu::UnicodeString to a UTF-32 km_core_usv buffer,
 * nul-terminated
 */
km_core_usv *unicode_string_to_usv(icu::UnicodeString& src) {
  UErrorCode icu_status = U_ZERO_ERROR;

  km_core_usv *dst = new km_core_usv[src.length() + 1];

  src.toUTF32(reinterpret_cast<UChar32*>(dst), src.length(), icu_status);

  if(!UASSERT_SUCCESS(icu_status)) {
    DebugLog("toUTF32 failed with %x", icu_status);
    delete[] dst;
    return nullptr;
  }

  dst[src.length()] = 0;
  return dst;
}

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
bool normalize(const icu::Normalizer2 *n, std::u16string &str, UErrorCode &status) {
  if(!UASSERT_SUCCESS(status)) {
    return false;
  }
  assert(n != nullptr);
  icu::UnicodeString dest;
  icu::UnicodeString src = icu::UnicodeString(str.data(), (int32_t)str.length());
  n->normalize(src, dest, status);
  // the next line here will assert
  if (!UASSERT_SUCCESS(status)) {
    return false;
  } else {
    str.assign(dest.getBuffer(), dest.length());
    return true;
  }
}


}
}
} /* end km::core::util */

#endif
