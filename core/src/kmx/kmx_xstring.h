#pragma once

#include "kmx_base.h"

namespace km {
namespace kbp {
namespace kmx {

/**
 * @brief True if a lead surrogate
 * \def Uni_IsSurrogate1
 */
#define Uni_IsSurrogate1(ch) ((ch) >= 0xD800 && (ch) <= 0xDBFF)
/**
 * @brief True if a trail surrogate
 * \def Uni_IsSurrogate2
 */
#define Uni_IsSurrogate2(ch) ((ch) >= 0xDC00 && (ch) <= 0xDFFF)

/**
 * @brief True if any surrogate
 * \def UniIsSurrogate
*/
#define Uni_IsSurrogate(ch) (Uni_IsSurrogate1(ch) || Uni_IsSurrogate2(ch))

/**
 * @brief Returns true if BMP (Plane 0)
 * \def Uni_IsBMP
 */
#define Uni_IsBMP(ch) ((ch) < 0x10000)

/**
 * @brief Convert two UTF-16 surrogates into one UTF-32 codepoint
 * @param ch lead surrogate - Uni_IsSurrogate1(ch) must == true
 * @param cl trail surrogate - Uni_IsSurrogate2(cl) must == true
 * \def Uni_SurrogateToUTF
 */
#define Uni_SurrogateToUTF32(ch, cl) (((ch) - 0xD800) * 0x400 + ((cl) - 0xDC00) + 0x10000)

/**
 * @brief Convert UTF-32 BMP to UTF-16 BMP
 * @param ch codepoint - Uni_IsBMP(ch) must == true
 * \def Uni_UTF32BMPToUTF16
 */
#define Uni_UTF32BMPToUTF16(ch) (ch & 0xFFFF)

#define Uni_UTF32ToSurrogate1(ch) (char16_t)(((ch) - 0x10000) / 0x400 + 0xD800)
#define Uni_UTF32ToSurrogate2(ch) (char16_t)(((ch) - 0x10000) % 0x400 + 0xDC00)

/**
 * @returns true if the character is a noncharacter
*/
bool Uni_IsNonCharacter(km_kbp_usv ch);

/**
 * @returns true if the character is a valid Unicode code point
*/
bool Uni_IsValid(km_kbp_usv ch);

/**
 * @returns true if the character is a valid Unicode code point range, that is, [start-end] are all
 * valid.
*/
bool Uni_IsValid(km_kbp_usv start, km_kbp_usv range);

/**
 * char16_t array big enough to hold a single Unicode codepoint,
 * including trailing null.
 */
typedef struct {
  char16_t ch[3];
} char16_single;

/**
 * Convert a UTF-32 codepoint to UTF-16 code unit(s).
 * @param ch32 input codepoint
 * @param ch16 output buffer
 * @return int length returned (not including null). Will return 1 (BMP) or 2
 */
int Utf32CharToUtf16(const KMX_DWORD ch32, char16_single& ch16);

PKMX_WCHAR incxstr(PKMX_WCHAR p);
PKMX_WCHAR decxstr(PKMX_WCHAR p, PKMX_WCHAR pStart);
int xstrlen(PKMX_WCHAR p);
int xstrlen_ignoreifopt(PKMX_WCHAR p);
int xstrpos(PKMX_WCHAR p1, PKMX_WCHAR p);
PKMX_WCHAR xstrchr(PKMX_WCHAR buf, PKMX_WCHAR chr);
int xchrcmp(PKMX_WCHAR ch1, PKMX_WCHAR ch2);

PKMX_CHAR wstrtostr(PKMX_WCHAR in);
PKMX_WCHAR strtowstr(PKMX_CHAR in);

const km_kbp_cp *u16chr(const km_kbp_cp *p, km_kbp_cp ch);
const km_kbp_cp *u16cpy(km_kbp_cp *dst, const km_kbp_cp *src);  // TODO: deprecate all usages
const km_kbp_cp *u16ncpy(km_kbp_cp *dst, const km_kbp_cp *src, size_t max);
size_t u16len(const km_kbp_cp *p);
int u16cmp(const km_kbp_cp *p, const km_kbp_cp *q);
int u16icmp(const km_kbp_cp *p, const km_kbp_cp *q);
int u16ncmp(const km_kbp_cp *p, const km_kbp_cp *q, size_t count);
km_kbp_cp *u16tok(km_kbp_cp *p, km_kbp_cp ch, km_kbp_cp **ctx);
km_kbp_cp *u16dup(km_kbp_cp *src);

//KMX_BOOL MapUSCharToVK(KMX_WORD ch, PKMX_WORD puKey, PKMX_DWORD puShiftFlags);

//  --- implementation ---

inline int
Utf32CharToUtf16(const KMX_DWORD ch32, char16_single &ch16) {
  int len;
  if (Uni_IsBMP(ch32)) {
    len        = 1;
    ch16.ch[0] = Uni_UTF32BMPToUTF16(ch32);
    ch16.ch[1] = 0;
  } else {
    len        = 2;
    ch16.ch[0] = Uni_UTF32ToSurrogate1(ch32);
    ch16.ch[1] = Uni_UTF32ToSurrogate2(ch32);
    ch16.ch[2] = 0;
  }
  return len;
}

/**
 * Convert a u16 string to a u32 string.
 * Mismatched surrogates or sliced surrogates are replaced with U+FFFD (replacement character).
 * @param source UTF-16 string
 * @return a UTF-32 string
 */
inline std::u32string
u16string_to_u32string(const std::u16string &source) {
  std::u32string out;

  for (auto ptr = source.begin(); ptr < source.end(); ptr++) {
    const char16_t lead = *ptr;
    if (Uni_IsSurrogate1(lead)) {
      ptr++;
      if (ptr == source.end()) {
        // DebugLog("End of string during surrogate pair");
        out.push_back(0xFFFD);  // error
        return out;
      }
      const char16_t trail = *ptr;
      if (!Uni_IsSurrogate2(trail)) {
        out.push_back(0xFFFD);  // error, mismatched lead surrogate
        ptr--;                  // reprocess remaining char
      } else {
        out.push_back(Uni_SurrogateToUTF32(lead, trail));
      }
    } else if (Uni_IsSurrogate2(lead)) {
      out.push_back(0xFFFD);  // error - mismatched trail surrogate
    } else {
      out.push_back(lead);
    }
  }
  return out;
}


inline bool Uni_IsNoncharacter(km_kbp_usv ch) {
  return (((ch) >= 0xFDD0 && (ch) <= 0xFDEF) || (((ch) & 0xFFFE) == 0xFFFE));
}

inline bool Uni_InCodespace(km_kbp_usv ch) {
  return ((ch) <= 0x10FFFF);
};

inline bool Uni_IsValid(km_kbp_usv ch) {
  return (Uni_InCodespace(ch) && !Uni_IsSurrogate(ch) && !Uni_IsNoncharacter(ch));
}

inline bool Uni_IsValid(km_kbp_usv start, km_kbp_usv end) {
  // quicker check
  if (!Uni_IsValid(start) || !Uni_IsValid(end)) {
    return false;
  }

  // brute force it
  for (km_kbp_usv i = start; i <= end; i++) {
    if (!Uni_IsValid(i)) return false;
  }

  return true;
}


} // namespace kmx
} // namespace kbp
} // namespace km
