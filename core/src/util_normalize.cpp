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
#include <assert.h>
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
#ifdef __EMSCRIPTEN__
  dst = std::u16string(src);
  return normalize_nfd(dst); // vector to above fcn
#else
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
#endif
}

bool
normalize_nfd(km_core_usv cp, std::u32string &dst) {
  // set the output string to the original string
  dst.clear();
  dst.append(1, cp);
#ifdef __EMSCRIPTEN__
  auto str16 = convert<char32_t, char16_t>(dst);
  if (!normalize_nfd(str16)) {
    return false;  // failed, retain original str
  } else {
    dst = convert<char16_t, char32_t>(str16);
    return true;
  }
#else
  UErrorCode icu_status = U_ZERO_ERROR;
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(icu_status);
  assert(U_SUCCESS(icu_status));
  if (!U_SUCCESS(icu_status)) {
    // TODO: log the failure code
    return false;
  }
  icu::UnicodeString decomposition;
  if (!nfd->getDecomposition(cp, decomposition)) {
    return false;  // no error, just no decomposition
  } else {
    dst.clear();
    auto len = decomposition.countChar32();
    for (int i = 0; i < len; i++) {
      dst.append(1, decomposition.char32At(i));
    }
    return true;
  }
#endif
}

}
}
}
