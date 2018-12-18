/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include <kmx/kmx_processor.h>

using namespace km::kbp;
using namespace kmx;

/* KMX_Context */

KMX_Context::KMX_Context()
{
  Reset();
}

void KMX_Context::Add(KMX_WCHAR ch)
{
  if(pos == MAXCONTEXT - 1)
  {
    memmove(CurContext, &CurContext[1], MAXCONTEXT*2 - 2); pos--;
  }

  CurContext[pos++] = ch;
  CurContext[pos] = 0;

  DebugLog("KMX_Context: Add(%x) [%d]: %s", ch, pos, Debug_UnicodeString(CurContext));
}


KMX_WCHAR *KMX_Context::Buf(int n)
{
  KMX_WCHAR *p;

  for(p = (KMX_WCHAR *) u16chr(CurContext, 0); n > 0 && p > CurContext; p = decxstr(p), n--);

  if(n > 0) return NULL;
  return p;
}

void KMX_Context::Delete()
{
  if (CharIsDeadkey()) {
    pos -= 2;
  } else if (CharIsSurrogatePair()) {
    pos--;
  }

  if(pos > 0) pos--;
  CurContext[pos] = 0;
}

void KMX_Context::Reset()
{
  pos = 0;
  CurContext[0] = 0;
}

void KMX_Context::Get(KMX_WCHAR *buf, int bufsize)
{
  for(KMX_WCHAR *p = CurContext; *p && bufsize > 0; p++, bufsize--)
  {
    *buf = *p; buf++;
    if(*p >= 0xD800 && *p <= 0xDBFF) { *buf = *(++p); bufsize--; buf++; }
  }
  *buf = 0;
}

void KMX_Context::CopyFrom(KMX_Context *source)   // I3575
{
  DebugLog("KMX_Context::CopyFrom source=%s; before copy, dest=%s", Debug_UnicodeString(source->CurContext, 0), Debug_UnicodeString(CurContext, 1));
  u16cpy(CurContext, /*_countof(CurContext),*/ source->CurContext);
  pos = source->pos;
}


void KMX_Context::Set(const KMX_WCHAR *buf)
{
  const KMX_WCHAR *p;
  KMX_WCHAR *q;
  for(p = buf, q = CurContext; *p && (int)(q-CurContext) < MAXCONTEXT - 1; p++, q++)
  {
    *q = *p;
    if(*p >= 0xD800 && *p <= 0xDBFF) { *(++q) = *(++p); }
  }
  *q = 0;
  pos = (int)(q-CurContext);
  CurContext[MAXCONTEXT-1] = 0; 
}

KMX_BOOL KMX_Context::CharIsDeadkey()
{
  if(pos < 3)
    return FALSE;
  return CurContext[pos-3] == UC_SENTINEL && 
       CurContext[pos-2] == CODE_DEADKEY;
}

KMX_BOOL KMX_Context::CharIsSurrogatePair()
{
  if (pos < 2)
    return FALSE;

  return Uni_IsSurrogate1(CurContext[pos - 2]) &&
    Uni_IsSurrogate2(CurContext[pos - 1]);
}


