#pragma once

#include "kmx_base.h"

#define Uni_IsSurrogate1(ch) ((ch) >= 0xD800 && (ch) <= 0xDBFF)
#define Uni_IsSurrogate2(ch) ((ch) >= 0xDC00 && (ch) <= 0xDFFF)
#define Uni_IsSMP(ch) ((ch) >= 0x10000)

#define Uni_SurrogateToUTF32(ch, cl) (((ch) - 0xD800) * 0x400 + ((cl) - 0xDC00) + 0x10000)

#define Uni_UTF32ToSurrogate1(ch)	(((ch) - 0x10000) / 0x400 + 0xD800)
#define Uni_UTF32ToSurrogate2(ch)	(((ch) - 0x10000) % 0x400 + 0xDC00)

PWSTR incxstr(PWSTR p);
PWSTR decxstr(PWSTR p);
int xstrlen(PWSTR p);
int xstrlen_ignoreifopt(PWSTR p);
int xstrpos(PWSTR p1, PWSTR p);
PWSTR xstrchr(PWSTR buf, PWSTR chr);
int xchrcmp(PWSTR ch1, PWSTR ch2);

PSTR wstrtostr(PWSTR in);
PWSTR strtowstr(PSTR in);
