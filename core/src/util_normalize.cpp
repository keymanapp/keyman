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

EM_JS(char*, NormalizeNFC, (const char* input), {
  if (!input) return input; // pass through null
  const instr = Module.UTF8ToString(input);
  const nfd = instr.normalize("NFC");
  return stringToNewUTF8(nfd);
});

// pull in the generated table
#include "util_normalize_table.h"

#endif

namespace km {
namespace core {
namespace util {

#ifndef __EMSCRIPTEN__
inline const icu::Normalizer2 *getNFD(UErrorCode &status) {
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(status);
  UASSERT_SUCCESS(status);
  return nfd;
}
inline const icu::Normalizer2 *getNFC(UErrorCode &status) {
  const icu::Normalizer2 *nfc = icu::Normalizer2::getNFCInstance(status);
  UASSERT_SUCCESS(status);
  return nfc;
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

bool normalize_nfc(std::u32string &str) {
  std::u16string rstr = km::core::kmx::u32string_to_u16string(str);
  if(!km::core::util::normalize_nfc(rstr)) {
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
  return normalize(getNFD(status), str, status);
#endif
}

bool normalize_nfc(std::u16string &str) {
#ifdef __EMSCRIPTEN__
  std::string instr = convert<char16_t,char>(str);
  const char *in = instr.c_str();
  char *out = NormalizeNFC(in);
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
  return normalize(getNFC(status), str, status);
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
  UErrorCode status = U_ZERO_ERROR;
  auto nfd = getNFD(status);
  if (nfd == nullptr) {
    return false;
  }
  icu::UnicodeString udst;
  icu::UnicodeString usrc = icu::UnicodeString(src);
  nfd->normalize(usrc, udst, status);
  if(!UASSERT_SUCCESS(status)) {
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

bool is_nfd(const std::u16string& str) {
#ifdef __EMSCRIPTEN__
  std::u16string o = str;
  normalize_nfd(o);
  return (o == str); // false if changed
#else
  UErrorCode status = U_ZERO_ERROR;
  auto nfd = getNFD(status);
  if (nfd == nullptr) return false;
  auto ustr = icu::UnicodeString(false, str.c_str(), (int)str.length());
  auto result = nfd->isNormalized(ustr, status);
  if (!UASSERT_SUCCESS(status)) {
    return false;
  } else {
    return result;
  }
#endif
}

bool is_nfd(const std::u32string& str) {
#ifdef __EMSCRIPTEN__
  std::u32string o = str;
  normalize_nfd(o);
  return (o == str); // false if changed
#else
  UErrorCode status = U_ZERO_ERROR;
  auto nfd = getNFD(status);
  if (nfd == nullptr) return false;
  auto ustr = icu::UnicodeString::fromUTF32(reinterpret_cast<const UChar32*>(str.c_str()), (int)str.length());
  auto result = nfd->isNormalized(ustr, status);
  if (!UASSERT_SUCCESS(status)) {
    return false;
  } else {
    return result;
  }
#endif
}

bool has_nfd_boundary_before(km_core_usv cp) {
#ifdef __EMSCRIPTEN__
// it's a negative table. entries in the table mean returning false. non-entries return true.
  for (auto i=0;i<(km_noBoundaryBefore_entries*2);i+=2) {
    auto start = km_noBoundaryBefore[i+0];
    if (start > cp) return true;
    auto count = km_noBoundaryBefore[i+1];
    auto limit = start+count;
    if (cp >= start && cp < limit) return false;
  }
  return true; // fallthrough
#else
  UErrorCode status = U_ZERO_ERROR;
  auto nfd = getNFD(status);
  if (nfd == nullptr) return false;
  return nfd->hasBoundaryBefore(cp);
#endif
}

/**
 * Helper to convert icu::UnicodeString to a UTF-32 km_core_usv buffer,
 * nul-terminated
 */
km_core_usv *string_to_usv(const std::u32string& src) {
  return km::core::kmx::u32dup(src.c_str());
}


}
}
}
