#pragma once

#include "kmx_base.h"

namespace km {
namespace kbp {
namespace kmx {

#define Uni_IsSurrogate1(ch) ((ch) >= 0xD800 && (ch) <= 0xDBFF)
#define Uni_IsSurrogate2(ch) ((ch) >= 0xDC00 && (ch) <= 0xDFFF)
#define Uni_IsSMP(ch) ((ch) >= 0x10000)

#define Uni_SurrogateToUTF32(ch, cl) (((ch) - 0xD800) * 0x400 + ((cl) - 0xDC00) + 0x10000)

#define Uni_UTF32ToSurrogate1(ch) (((ch) - 0x10000) / 0x400 + 0xD800)
#define Uni_UTF32ToSurrogate2(ch) (((ch) - 0x10000) % 0x400 + 0xDC00)

PKMX_WCHAR incxstr(PKMX_WCHAR p);
PKMX_WCHAR decxstr(PKMX_WCHAR p);
int xstrlen(PKMX_WCHAR p);
int xstrlen_ignoreifopt(PKMX_WCHAR p);
int xstrpos(PKMX_WCHAR p1, PKMX_WCHAR p);
PKMX_WCHAR xstrchr(PKMX_WCHAR buf, PKMX_WCHAR chr);
int xchrcmp(PKMX_WCHAR ch1, PKMX_WCHAR ch2);

PKMX_CHAR wstrtostr(PKMX_WCHAR in);
PKMX_WCHAR strtowstr(PKMX_CHAR in);

const km_kbp_cp *u16chr(const km_kbp_cp *p, km_kbp_cp ch);
const km_kbp_cp *u16cpy(km_kbp_cp *dst, const km_kbp_cp *src);  // TODO: add buffer size, also u16ncpy version
size_t u16len(const km_kbp_cp *p);
int u16cmp(const km_kbp_cp *p, const km_kbp_cp *q);
int u16icmp(const km_kbp_cp *p, const km_kbp_cp *q);
int u16ncmp(const km_kbp_cp *p, const km_kbp_cp *q, size_t count);
km_kbp_cp *u16tok(km_kbp_cp *p, km_kbp_cp ch, km_kbp_cp **ctx);

} // namespace kmx
} // namespace kbp
} // namespace km
