
#include "pch.h"
#include "xstring.h"
#include "../../../common/cpp/kmx_file_codes.h"
// used to be  windows/src/global/vc/xstring.cpp
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
PKMX_WCHAR incxstr(PKMX_WCHAR p) {

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

PKMX_WCHAR decxstr(PKMX_WCHAR p, PKMX_WCHAR pStart)
{
  PKMX_WCHAR q;

  if(p <= pStart) {
    return NULL;
  }

  p--;
  if(*p == UC_SENTINEL_EXTENDEDEND) {
    int n = 0;
    while (p > pStart && *p != UC_SENTINEL && n < 10) {
      p--;
      n++;
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

int xstrlen_ignoreifopt(PKMX_WCHAR p)
{
  int i;
  for(i = 0; *p; i++, p=incxstr(p))
  {
    if(*p == UC_SENTINEL && (*(p+1) == CODE_IFOPT || *(p+1) == CODE_IFSYSTEMSTORE)) i--;  // I3432
  }
  return i;
}

int xstrlen(PKMX_WCHAR p)
{
  int i;
  for(i = 0; *p; i++, p=incxstr(p));
  return i;
}

int xstrpos(PKMX_WCHAR p1, PKMX_WCHAR p)
{
  int i;
  for(i = 0; p < p1; p = incxstr(p), i++);
  return i;
}

// Ensure that all CODE_### sizes are defined
static_assert(sizeof(CODE__SIZE) / sizeof(CODE__SIZE[0]) == (CODE_LASTCODE + 1), "Size of array CODE__SIZE not correct");
