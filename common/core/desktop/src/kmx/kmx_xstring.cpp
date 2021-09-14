/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include <vector>
#include <iterator>
#include <codecvt>
#include <locale>
#include "kmx_processevent.h"
#include "utfcodec.hpp"



using namespace km::kbp;
using namespace kmx;

const km_kbp_cp *km::kbp::kmx::u16chr(const km_kbp_cp *p, km_kbp_cp ch) {
  while (*p) {
    if (*p == ch) return p;
    p++;
  }
  return ch == 0 ? p : NULL;
}

const km_kbp_cp *km::kbp::kmx::u16cpy(km_kbp_cp *dst, const km_kbp_cp *src) {
  km_kbp_cp *o = dst;
  while (*src) {
    *dst++ = *src++;
  }
  *dst = 0;
  return o;
}

const km_kbp_cp *km::kbp::kmx::u16ncpy(km_kbp_cp *dst, const km_kbp_cp *src, size_t max) {
  km_kbp_cp *o = dst;
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

size_t km::kbp::kmx::u16len(const km_kbp_cp *p) {
  int i = 0;
  while (*p) {
    p++;
    i++;
  }
  return i;
}

int km::kbp::kmx::u16cmp(const km_kbp_cp *p, const km_kbp_cp *q) {
  while (*p && *q) {
    if (*p != *q) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

int km::kbp::kmx::u16icmp(const km_kbp_cp *p, const km_kbp_cp *q) {
  while (*p && *q) {
    if (toupper(*p) != toupper(*q)) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

int km::kbp::kmx::u16ncmp(const km_kbp_cp *p, const km_kbp_cp *q, size_t count) {
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

km_kbp_cp *km::kbp::kmx::u16tok(km_kbp_cp *p, km_kbp_cp ch, km_kbp_cp **ctx) {
  if (!p) {
    p = *ctx;
    if (!p) return NULL;
  }

  km_kbp_cp *q = p;
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

PKMX_WCHAR km::kbp::kmx::incxstr(PKMX_WCHAR p)
{
  int deltaptr;			// how many bytes to jump over 

  if (*p == 0) return p;
  if (*p != UC_SENTINEL)
  {
    if (*p >= 0xD800 && *p <= 0xDBFF && *(p + 1) >= 0xDC00 && *(p + 1) <= 0xDFFF) return p + 2;
    return p + 1;
  }
  else
  {
    // UC_SENTINEL(FFFF) with UC_SENTINEL_EXTENDEDEND(0x10) == variable length
    if (*(p + 1) == CODE_EXTENDED) {
      p += 2;
      while (*(p - 1) && *p && *p != UC_SENTINEL_EXTENDEDEND)
        p++;

      if (*p == 0) return p;
      if (*p == UC_SENTINEL_EXTENDEDEND)		return p + 1;
    }

    //  UC_SENTINEL(FFFF) followed by other control
    switch (*(p + 1))
    {
    case CODE_ANY:						deltaptr = 3; break;
    case CODE_NOTANY:					deltaptr = 3; break;
    case CODE_INDEX:					deltaptr = 4; break;
    case CODE_USE:						deltaptr = 3; break;
    case CODE_DEADKEY:				deltaptr = 3; break;
    case CODE_CLEARCONTEXT:		deltaptr = 3; break;
    case CODE_CALL:						deltaptr = 3; break;
    case CODE_CONTEXTEX:			deltaptr = 3; break;
    case CODE_IFOPT:          deltaptr = 5; break;
    case CODE_IFSYSTEMSTORE:	deltaptr = 5; break;
    case CODE_SETOPT:					deltaptr = 4; break;
    case CODE_SETSYSTEMSTORE:	deltaptr = 4; break;
    case CODE_RESETOPT:				deltaptr = 3; break;
    case CODE_SAVEOPT:				deltaptr = 3; break;
    default:									deltaptr = 2;
    }

    // check for \0 between FFFF and next printable character
    for (int i = 0; i < (deltaptr); i++) {
      if (*p==0)
        return p;
      p += 1;    }
    
    return p;
  }
  return p;
}

PKMX_WCHAR km::kbp::kmx::decxstr(PKMX_WCHAR p, PKMX_WCHAR pStart)
{
  if(p <= pStart) {
    return NULL;
  }

  p--;
  if(*p == UC_SENTINEL_EXTENDEDEND)
  {
    int n = 0;
    while(*p != UC_SENTINEL && n < 10) { p--; n++; }

    if(p < pStart) {
      // May be a malformed virtual key
      return pStart;
    }
    return p;
  }

  if(p == pStart) return p; // Don't allow test before pStart

  if(*p >= 0xDC00 && *p <= 0xDFFF && *(p-1) >= 0xD800 && *(p-1) <= 0xDBFF)
  {
    return p-1;
  }
  else if(*(p-1) == UC_SENTINEL) return p-1;
  else if(p > pStart+1 && *(p-2) == UC_SENTINEL)
  {
    switch(*(p-1))
    {
      case CODE_ANY:
      case CODE_NOTANY:
      case CODE_USE:
      case CODE_DEADKEY:
      case CODE_CLEARCONTEXT:
      case CODE_CALL:
      case CODE_CONTEXTEX:
      case CODE_RESETOPT:
      case CODE_SAVEOPT:
        return p-2;
    }
  }
  else if(p > pStart+2 && *(p-3) == UC_SENTINEL)
  {
    switch(*(p-2))
    {
      case CODE_INDEX:
      case CODE_SETOPT:
      case CODE_SETSYSTEMSTORE:
        return p-3;
    }
  }
  else if(p > pStart+3 && *(p-4) == UC_SENTINEL)
  {
    switch(*(p-3))
    {
      case CODE_IFOPT:
      case CODE_IFSYSTEMSTORE:  // I3432
        return p-4;
    }
  }
  return p;
}

int km::kbp::kmx::xstrlen_ignoreifopt(PKMX_WCHAR p)
{
  int i;
  for(i = 0; *p; i++, p=incxstr(p))
  {
    if(*p == UC_SENTINEL && (*(p+1) == CODE_IFOPT || *(p+1) == CODE_IFSYSTEMSTORE)) i--;  // I3432
  }
  return i;
}

int km::kbp::kmx::xstrlen(PKMX_WCHAR p)
{
  int i;
  for(i = 0; *p; i++, p=incxstr(p));
  return i;
}

int km::kbp::kmx::xstrpos(PKMX_WCHAR p1, PKMX_WCHAR p)
{
  int i;
  for(i = 0; p < p1; p = incxstr(p), i++);
  return i;
}

PKMX_WCHAR km::kbp::kmx::xstrchr(PKMX_WCHAR buf, PKMX_WCHAR chr)
{
  for(PKMX_WCHAR q = incxstr(buf); *buf; buf = q, q = incxstr(buf))
    if(!u16ncmp(buf, chr, (intptr_t)(q-buf)))
      return buf;
  return NULL;
}

int km::kbp::kmx::xchrcmp(PKMX_WCHAR ch1, PKMX_WCHAR ch2)
{
  PKMX_WCHAR nch1 = incxstr(ch1);
  if(nch1 == ch1) return *ch2 - *ch1; /* comparing *ch2 to nul */
  return u16ncmp(ch1, ch2, (intptr_t)(nch1-ch1));
}

PKMX_WCHAR km::kbp::kmx::strtowstr(PKMX_CHAR in)
{
  PKMX_WCHAR result;

  auto s = convert<char,char16_t>(in);
  result = new char16_t[s.length() + 1];
  s.copy(result, s.length());
  result[s.length()] = 0;
  return result;
}


PKMX_CHAR km::kbp::kmx::wstrtostr(PKMX_WCHAR in)
{
  PKMX_CHAR result;

  auto s = convert<char16_t,char>(in);
  result = new char[s.length() + 1];
  s.copy(result, s.length());
  result[s.length()] = 0;
  return result;
}
