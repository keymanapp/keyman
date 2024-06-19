#include <kmcmplibapi.h>
#include "kmcompx.h"

#include "unicode/uniset.h"
#include "unicode/unistr.h"

EXTERN int kmcmp_parseUnicodeSet(
  const std::string text,
  uintptr_t outputBuffer_,
  uint32_t outputBufferSize
) {
  const icu::UnicodeString str = icu::UnicodeString::fromUTF8(text.c_str());
  if (str.isBogus() || str.isEmpty()) {
    // empty string
    return KMCMP_ERROR_SYNTAX_ERR;
  }
  UErrorCode status = U_ZERO_ERROR;

  icu::UnicodeSet uset(str, status);
  // TODO-LDML: check for properties

  if (U_FAILURE(status)) {
    if (status == U_MISSING_RESOURCE_ERROR) {
      // Our special ICU returns this
      return KMCMP_ERROR_UNSUPPORTED_PROPERTY;
    } else {
      // Any other error.
      return KMCMP_ERROR_SYNTAX_ERR;
    }
  } else if (uset.hasStrings()) {
    // Error, strings are not allowed
    return KMCMP_ERROR_HAS_STRINGS;
  }
  const int32_t count = uset.getRangeCount();
  if (outputBufferSize == 0) {
    // pure preflight - return buffer size needed as negative
    return count;
  }
  uint32_t* outputBuffer = reinterpret_cast<uint32_t*>(outputBuffer_);
  if (outputBuffer == nullptr) {
    // fail if nullptr passed
    return KMCMP_FATAL_OUT_OF_RANGE;
  }
  if ((count * 2L) > outputBufferSize) {
    // output buffer too small
    return KMCMP_FATAL_OUT_OF_RANGE;
  }

  // set all ranges
  for (int32_t i=0; i<count; i++) {
    outputBuffer[(i*2)+0] = uset.getRangeStart(i);
    outputBuffer[(i*2)+1] = uset.getRangeEnd(i);
  }

  return count;
}
