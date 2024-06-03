/*
  Copyright:    © SIL International.
  Description:  Common LDML utilities
  Create Date:  6 Jan 2024
  Authors:      Steven R. Loomis
*/

#include "util_normalize.hpp"

#include "core_icu.h"
#include "kmx/kmx_xstring.h"

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#include "utfcodec.hpp"
// JS implementations

EM_JS(char*, NormalizeNFD, (const char* input), {
  if (!input) return input; // pass through null
  const instr = Module.UTF8ToString(input);
  const nfd = instr.normalize("NFD");
  return stringToNewUTF8(nfd);
});
#endif

namespace km {
namespace core {
namespace util {

#ifndef __EMSCRIPTEN__

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
static bool normalize(const icu::Normalizer2 *n, std::u16string &str, UErrorCode &status) {
  UASSERT_SUCCESS(status);
  assert(n != nullptr);
  icu::UnicodeString dest;
  icu::UnicodeString src = icu::UnicodeString(str.data(), (int32_t)str.length());
  n->normalize(src, dest, status);
  // the next line here will assert
  if (UASSERT_SUCCESS(status)) {
    str.assign(dest.getBuffer(), dest.length());
  }
  return U_SUCCESS(status);
}
#endif

bool normalize_nfd(std::u32string &str) {
  std::u16string rstr = km::core::kmx::u32string_to_u16string(str);
  if(!km::core::util::normalize_nfd(rstr)) {
    return false;
  } else {
    str = km::core::kmx::u16string_to_u32string(rstr);
    return true;
  }
}

bool normalize_nfd(std::u16string &str) {
#ifdef __EMSCRIPTEN__
  std::string instr = convert<char16_t,char>(str);
  const char *in = instr.c_str();
  char *out = NormalizeNFD(in);
  if (out == nullptr) {
    assert(out != nullptr);
    return false;
  }
  std::string outstr(out);
  str = convert<char,char16_t>(outstr);
  free(out);
  return true;
#else
  UErrorCode status = U_ZERO_ERROR;
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(status);
  UASSERT_SUCCESS(status);
  return normalize(nfd, str, status);
#endif
}

/**
 * Normalize the input string using ICU, out of place
 */
bool normalize_nfd(km_core_cu const * src, std::u16string &dst) {
  UErrorCode icu_status = U_ZERO_ERROR;
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(icu_status);
  assert(U_SUCCESS(icu_status));
  if(!U_SUCCESS(icu_status)) {
    // TODO: log the failure code
    return false;
  }
  icu::UnicodeString udst;
  icu::UnicodeString usrc = icu::UnicodeString(src);
  nfd->normalize(usrc, udst, icu_status);
  assert(U_SUCCESS(icu_status));
  if(!U_SUCCESS(icu_status)) {
    // TODO: log the failure code
    return false;
  }

  dst.assign(udst.getBuffer(), udst.length());
  return true;
}

}
}
}
