/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "pch.h"
#include <vector>
#include <iterator>
#include <codecvt>
#include <locale>

const km_kbp_cp *u16chr(const km_kbp_cp *p, km_kbp_cp ch) {
  while (*p) {
    if (*p == ch) return p;
    p++;
  }
  return ch == 0 ? p : NULL;
}

const km_kbp_cp *u16cpy(km_kbp_cp *dst, const km_kbp_cp *src) {
  km_kbp_cp *o = dst;
  while (*src) {
    *dst++ = *src++;
  }
  *dst = 0;
  return o;
}

size_t u16len(const km_kbp_cp *p) {
  int i = 0;
  while (*p) {
    p++;
    i++;
  }
  return i;
}

int u16cmp(const km_kbp_cp *p, const km_kbp_cp *q) {
  while (*p && *q) {
    if (*p != *q) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

int u16icmp(const km_kbp_cp *p, const km_kbp_cp *q) {
  while (*p && *q) {
    if (toupper(*p) != toupper(*q)) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

int u16ncmp(const km_kbp_cp *p, const km_kbp_cp *q, size_t count) {
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

km_kbp_cp *u16tok(km_kbp_cp *p, km_kbp_cp ch, km_kbp_cp **ctx) {
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
*	int xstrlen( LPBYTE p );
*
*	Parameters:	p	Pointer to string to get length of
*
*	Returns:    length of string
*
*   Called by:  various functions
*
*	xstrlen calculates the length of a string, ignoring some special chars.
*/

PWSTR incxstr(PWSTR p)
{
	if(*p == 0) return p;
	if(*p != UC_SENTINEL)
	{
		if(*p >= 0xD800 && *p <= 0xDBFF && *(p+1) >= 0xDC00 && *(p+1) <= 0xDFFF) return p+2;
		return p+1;
	}

	p+=2;
	switch(*(p-1))
	{
		case CODE_ANY:			return p+1;
		case CODE_NOTANY:   return p+1;
		case CODE_INDEX:		return p+2;
		case CODE_USE:			return p+1;
		case CODE_DEADKEY:		return p+1;
		case CODE_EXTENDED:		p += 2; while(*p != UC_SENTINEL_EXTENDEDEND) p++; return p+1;
		case CODE_CLEARCONTEXT: return p+1;
		case CODE_CALL:			return p+1;
		case CODE_CONTEXTEX:	return p+1;
    case CODE_IFOPT:    return p+3;
    case CODE_IFSYSTEMSTORE: return p+3;
    case CODE_SETOPT:   return p+2;
    case CODE_SETSYSTEMSTORE: return p+2;
    case CODE_RESETOPT: return p+1;
    case CODE_SAVEOPT:  return p+1;
		default:				return p;
	}
}

PWSTR decxstr(PWSTR p)
{
	p--;
	if(*p == UC_SENTINEL_EXTENDEDEND)
	{
		int n = 0;
		while(*p != UC_SENTINEL && n < 10) { p--; n++; }
		return p;
	}

	if(*p >= 0xDC00 && *p <= 0xDFFF && *(p-1) >= 0xD800 && *(p-1) <= 0xDBFF)
	{
		return p-1;
	}
	else if(*(p-1) == UC_SENTINEL) return p-1;
	else if(*(p-2) == UC_SENTINEL) 
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
	else if(*(p-3) == UC_SENTINEL) 
	{
		switch(*(p-2))
		{
			case CODE_INDEX:
      case CODE_SETOPT:   
      case CODE_SETSYSTEMSTORE:
				return p-3;
		}
	}
  else if(*(p-4) == UC_SENTINEL)
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

int xstrlen_ignoreifopt(PWSTR p)
{
  int i;
  for(i = 0; *p; i++, p=incxstr(p))
  {
    if(*p == UC_SENTINEL && (*(p+1) == CODE_IFOPT || *(p+1) == CODE_IFSYSTEMSTORE)) i--;  // I3432
  }
  return i;
}

int xstrlen(PWSTR p)
{
	int i;
	for(i = 0; *p; i++, p=incxstr(p));
	return i;
}

int xstrpos(PWSTR p1, PWSTR p)
{
  int i;
	for(i = 0; p < p1; p = incxstr(p), i++);
	return i;
}

PWSTR xstrchr(PWSTR buf, PWSTR chr)
{
  for(PWSTR q = incxstr(buf); *buf; buf = q, q = incxstr(buf))
    if(!u16ncmp(buf, chr, (int)(q-buf)))
      return buf;
  return NULL;
}

int xchrcmp(PWSTR ch1, PWSTR ch2)
{
  PWSTR nch1 = incxstr(ch1);
  if(nch1 == ch1) return *ch2 - *ch1; /* comparing *ch2 to nul */
  return u16ncmp(ch1, ch2, (int)(nch1-ch1));
}


/*
  This function exists because of a bug in Visual Studio 2015 and 2017:
  https://social.msdn.microsoft.com/Forums/en-US/8f40dcd8-c67f-4eba-9134-a19b9178e481/vs-2015-rc-linker-stdcodecvt-error?forum=vcgeneral
  https://stackoverflow.com/a/35103224/1836776
*/
#if _MSC_VER >= 1900 /* VS 2015 */ && _MSC_VER <= 1915 /* VS 2017 */
std::string utf16_to_utf8(std::u16string utf16_string)
{
  std::wstring_convert<std::codecvt_utf8_utf16<int16_t>, int16_t> convert;
  auto p = reinterpret_cast<const int16_t *>(utf16_string.data());
  return convert.to_bytes(p, p + utf16_string.size());
}
#else
std::string utf16_to_utf8(std::u16string utf16_string)
{
  std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;
  return convert.to_bytes(utf16_string);
}
#endif

PWSTR strtowstr(PSTR in)
{
  km_kbp_cp *result;

  std::wstring_convert<std::codecvt_utf8_utf16<int16_t>, int16_t> convert;
  auto s = convert.from_bytes(in, strchr(in, 0));
  result = new WCHAR[s.length() + 1];
  s.copy(reinterpret_cast<int16_t *>(result), s.length());
  result[s.length()] = 0;
  return result;
}


PSTR wstrtostr(PWSTR in)
{
  PSTR result;

  auto s = utf16_to_utf8(in);
  result = new char[s.length() + 1];
  s.copy(result, s.length());
  result[s.length()] = 0;
  return result;
}

#if 0
namespace kmx {
  struct kmx_char {
    uint8_t type;
    union {
      /* codepoint */           struct { uint32_t ch; } ch;
      /* CODE_ANY */            struct { uint16_t store; } any;
      /* CODE_INDEX */          struct { uint16_t store, context_offset; } index;
      // CODE_CONTEXT, CODE_NUL have no params
      /* CODE_USE */            struct { uint16_t group; } use;                     
      // CODE_RETURN, CODE_BEEP have no params
      /* CODE_DEADKEY */        struct { uint16_t deadkey; } deadkey;
      /* CODE_EXTENDED */       struct { uint16_t modifier, vkey; } extended;
      // CODE_EXTENDEDEND not used
      // CODE_SWITCH, CODE_KEY deprecated
      // CODE_CLEARCONTEXT deprecated
      /* CODE_CALL */           struct { uint16_t store; } call;  // Not supported cross-platform
      /* CODE_CONTEXTEX */      struct { uint16_t context_offset; } contextex;
      /* CODE_NOTANY */         struct { uint16_t store; } notany;
      /* CODE_SETOPT */         struct { uint16_t store1, store2; } setopt;
      /* CODE_IFOPT */          struct { uint16_t store1, store2, op; } ifopt;
      /* CODE_SAVEOPT */        struct { uint16_t store; } saveopt;
      /* CODE_RESETOPT */       struct { uint16_t store; } resetopt;
      /* CODE_IFSYSTEMSTORE */  struct { uint16_t store1, store2, op; } ifsystemstore;
      /* CODE_SETSYSTEMSTORE */ struct { uint16_t store1, store2; } setsystemstore;
    };
  };

  class kmx_string : public std::vector<kmx_char> {
    kmx_string(km_kbp_cp *source);
  };


  kmx_string::kmx_string(km_kbp_cp *source) {
    auto ss = std::u16string(source);
    for (auto i = ss.begin(); i != ss.end(); i++) {
      if (*i == UC_SENTINEL) {

      }
      else {

      }
      switch (*i) {

      }
    }
  }
};
#endif