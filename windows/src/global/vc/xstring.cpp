/*
  Name:             xstring
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      27 Aug 2012

  Modified Date:    27 Aug 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          27 Aug 2012 - mcdurdin - I3439 - V9.0 - Refactor xstring support in C++ code
*/
#include "pch.h"


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

PWSTR decxstr(PWSTR p, PWSTR pStart)
{
  if (p <= pStart) {
    assert("Attempted to move prior to start of string");
    return NULL;
  }

  p--;
	if(*p == UC_SENTINEL_EXTENDEDEND)
	{
		int n = 0;
		while(*p != UC_SENTINEL && n < 10) { p--; n++; }

    if (p < pStart) {
      // May be a malformed virtual key
      return pStart;
    }
    return p;
	}

  if (p == pStart) return p; // Don't allow test before pStart

  if(*p >= 0xDC00 && *p <= 0xDFFF && *(p-1) >= 0xD800 && *(p-1) <= 0xDBFF)
	{
		return p-1;
	}
	else if(*(p-1) == UC_SENTINEL) return p-1;
	else if(p > pStart + 1 && *(p-2) == UC_SENTINEL)
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
	else if(p > pStart + 2 && *(p-3) == UC_SENTINEL)
	{
		switch(*(p-2))
		{
			case CODE_INDEX:
      case CODE_SETOPT:
      case CODE_SETSYSTEMSTORE:
				return p-3;
		}
	}
  else if(p > pStart + 3 && *(p-4) == UC_SENTINEL)
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

/*PWSTR xstrchr(PWSTR buf, int x)
{
	int clow, chigh;
	if(x < 0x10000) { chigh = x; clow = -1; }
	else { chigh = Uni_UTF32ToSurrogate1(x); clow = Uni_UTF32ToSurrogate2(x); }

	for(; *buf; buf = incxstr(buf))
		if(*buf == chigh && (*(buf+1) == clow || clow == -1)) return buf;
	return NULL;
}*/

PWSTR xstrchr(PWSTR buf, PWSTR chr)
{
  for(PWSTR q = incxstr(buf); *buf; buf = q, q = incxstr(buf))
    if(!wcsncmp(buf, chr, (intptr_t)(q-buf)))
      return buf;
  return NULL;
}

int xchrcmp(PWSTR ch1, PWSTR ch2)
{
  PWSTR nch1 = incxstr(ch1);
  if(nch1 == ch1) return *ch2 - *ch1; /* comparing *ch2 to nul */
  return wcsncmp(ch1, ch2, (intptr_t)(nch1-ch1));
}
