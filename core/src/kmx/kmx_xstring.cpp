/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include <vector>
#include <iterator>
#include <locale>
#include "kmx_processevent.h"
#include "utfcodec.hpp"

using namespace km::core;
using namespace kmx;

const km_core_cu *km::core::kmx::u16chr(const km_core_cu *p, km_core_cu ch) {
  while (*p) {
    if (*p == ch) return p;
    p++;
  }
  return ch == 0 ? p : NULL;
}

const km_core_cu *km::core::kmx::u16cpy(km_core_cu *dst, const km_core_cu *src) {
  km_core_cu *o = dst;
  while (*src) {
    *dst++ = *src++;
  }
  *dst = 0;
  return o;
}

const km_core_cu *km::core::kmx::u16ncpy(km_core_cu *dst, const km_core_cu *src, size_t max) {
  km_core_cu *o = dst;
  while (*src && max > 0) {
    *dst++ = *src++;
    max--;
  }
  while(max > 0) {
    *dst++ = 0;
    max--;
  }
  return o;
}

size_t km::core::kmx::u16len(const km_core_cu *p) {
  int i = 0;
  while (*p) {
    p++;
    i++;
  }
  return i;
}

int km::core::kmx::u16cmp(const km_core_cu *p, const km_core_cu *q) {
  while (*p && *q) {
    if (*p != *q) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

int km::core::kmx::u16icmp(const km_core_cu *p, const km_core_cu *q) {
  while (*p && *q) {
    if (toupper(*p) != toupper(*q)) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

int km::core::kmx::u16ncmp(const km_core_cu *p, const km_core_cu *q, size_t count) {
  while (*p && *q && count) {
    if (*p != *q) return *p - *q;
    p++;
    q++;
    count--;
  }
  if (count)
    return *p - *q;
  return 0;
}

km_core_cu *km::core::kmx::u16tok(km_core_cu *p, km_core_cu ch, km_core_cu **ctx) {
  if (!p) {
    p = *ctx;
    if (!p) return NULL;
  }

  km_core_cu *q = p;
  while (*q && *q != ch) {
    q++;
  }
  if (*q) {
    *q = 0;
    q++;
    while (*q == ch) q++;
    *ctx = q;
  }
  else {
    *ctx = NULL;
  }
  return p;
}

km_core_cu *km::core::kmx::u16dup(km_core_cu *src) {
  km_core_cu *dup = new km_core_cu[u16len(src) + 1];
  memcpy(dup, src, (u16len(src) + 1) * sizeof(km_core_cu));
  return dup;
}

/*
* int xstrlen( PKMX_BYTE p );
*
* Parameters: p Pointer to string to get length of
*
* Returns:    length of string
*
*   Called by:  various functions
*
* xstrlen calculates the length of a string, ignoring some special chars.
*/
PKMX_WCHAR km::core::kmx::incxstr(PKMX_WCHAR p) {

  if (*p == 0)
    return p;
  if (*p != UC_SENTINEL) {
    if (*p >= 0xD800 && *p <= 0xDBFF && *(p + 1) >= 0xDC00 && *(p + 1) <= 0xDFFF)
      return p + 2;
    return p + 1;
  }
  // UC_SENTINEL(FFFF) with UC_SENTINEL_EXTENDEDEND(0x10) == variable length
  if (*(p + 1) == CODE_EXTENDED) {
    p += 2;
    while (*p && *p != UC_SENTINEL_EXTENDEDEND)
      p++;

    if (*p == 0)        return p;
    return p + 1;
  }

  if (*(p + 1) > CODE_LASTCODE || CODE__SIZE[*(p + 1)] == -1) {
    return p + 1;
  }

  int deltaptr = 2 + CODE__SIZE[*(p + 1)];

  // check for \0 between UC_SENTINEL(FFFF) and next printable character
  for (int i = 0; i < deltaptr; i++) {
    if (*p == 0)
      return p;
    p++;
  }
  return p;
}

PKMX_WCHAR km::core::kmx::decxstr(PKMX_WCHAR p, PKMX_WCHAR pStart)
{
  PKMX_WCHAR q;

  if(p <= pStart) {
    return NULL;
  }

  p--;
  if(*p == UC_SENTINEL_EXTENDEDEND) {
    int n = 0;
    while (p >= pStart && *p != UC_SENTINEL && n < 10) {
      p--; n++; }

    if(p < pStart) {
      // May be a malformed virtual key
      return pStart;
    }
    return p;
  }

  if (p == pStart) {
    // Don't allow test before pStart
    return p;
  }

  if(*p >= 0xDC00 && *p <= 0xDFFF && *(p-1) >= 0xD800 && *(p-1) <= 0xDBFF) {
    return p-1;
  }

  // Look for a UC_SENTINEL to jump to
  // note: If we are pointing to the middle of a UC_SENTINEL CODE_x, then we won't treat it as valid,
  //       and will just go back a single wchar
  q = p;
  for (int i = 0; i < CODE__SIZE_MAX && q >= pStart; i++, q--) {
    //  *q == UC_SENTINEL &&  *(q + 1) is within CODE__SIZE && next CODE_ right of UC_SENTINEL ( looked up in CODE__SIZE+1) has value i
    if (*q == UC_SENTINEL &&  *(q + 1) <= CODE_LASTCODE     && CODE__SIZE[*(q + 1)] + 1 == i)
      return q;
  }

  return p;
}

int km::core::kmx::xstrlen_ignoreifopt(PKMX_WCHAR p)
{
  int i;
  for(i = 0; *p; i++, p=incxstr(p))
  {
    if(*p == UC_SENTINEL && (*(p+1) == CODE_IFOPT || *(p+1) == CODE_IFSYSTEMSTORE)) i--;  // I3432
  }
  return i;
}

int km::core::kmx::xstrlen(PKMX_WCHAR p)
{
  int i;
  for(i = 0; *p; i++, p=incxstr(p));
  return i;
}

int km::core::kmx::xstrpos(PKMX_WCHAR p1, PKMX_WCHAR p)
{
  int i;
  for(i = 0; p < p1; p = incxstr(p), i++);
  return i;
}

PKMX_WCHAR km::core::kmx::xstrchr(PKMX_WCHAR buf, PKMX_WCHAR chr)
{
  for(PKMX_WCHAR q = incxstr(buf); *buf; buf = q, q = incxstr(buf))
    if(!u16ncmp(buf, chr, (intptr_t)(q-buf)))
      return buf;
  return NULL;
}

int km::core::kmx::xchrcmp(PKMX_WCHAR ch1, PKMX_WCHAR ch2)
{
  PKMX_WCHAR nch1 = incxstr(ch1);
  if(nch1 == ch1) return *ch2 - *ch1; /* comparing *ch2 to nul */
  return u16ncmp(ch1, ch2, (intptr_t)(nch1-ch1));
}

PKMX_WCHAR km::core::kmx::strtowstr(PKMX_CHAR in)
{
  PKMX_WCHAR result;

  auto s = convert<char,char16_t>(in);
  result = new char16_t[s.length() + 1];
  s.copy(result, s.length());
  result[s.length()] = 0;
  return result;
}

PKMX_CHAR km::core::kmx::wstrtostr(PKMX_WCHAR in)
{
  PKMX_CHAR result;

  auto s = convert<char16_t,char>(in);
  result = new char[s.length() + 1];
  s.copy(result, s.length());
  result[s.length()] = 0;
  return result;
}
