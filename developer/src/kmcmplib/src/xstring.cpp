
#include "pch.h"
#include "xstring.h"
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

const int CODE__SIZE[] = {
    -1,  // undefined                0x00
    1,   // CODE_ANY                 0x01
    2,   // CODE_INDEX               0x02
    0,   // CODE_CONTEXT             0x03
    0,   // CODE_NUL                 0x04
    1,   // CODE_USE                 0x05
    0,   // CODE_RETURN              0x06
    0,   // CODE_BEEP                0x07
    1,   // CODE_DEADKEY             0x08
    -1,  // unused                   0x09
    2,   // CODE_EXTENDED            0x0A
    -1,  // CODE_EXTENDEDEND         0x0B (unused)
    1,   // CODE_SWITCH              0x0C
    -1,  // CODE_KEY                 0x0D (never used)
    0,   // CODE_CLEARCONTEXT        0x0E
    1,   // CODE_CALL                0x0F
    -1,  // UC_SENTINEL_EXTENDEDEND  0x10 (not valid with UC_SENTINEL)
    1,   // CODE_CONTEXTEX           0x11
    1,   // CODE_NOTANY              0x12
    2,   // CODE_SETOPT              0x13
    3,   // CODE_IFOPT               0x14
    1,   // CODE_SAVEOPT             0x15
    1,   // CODE_RESETOPT            0x16
    3,   // CODE_IFSYSTEMSTORE       0x17
    2    // CODE_SETSYSTEMSTORE      0x18
};

// Ensure that all CODE_### sizes are defined
static_assert(sizeof(CODE__SIZE) / sizeof(CODE__SIZE[0]) == (CODE_LASTCODE + 1), "Size of array CODE__SIZE not correct");
