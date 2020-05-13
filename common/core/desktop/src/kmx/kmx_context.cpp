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
  DebugLog("KMX_Context::Add(%x): ENTER [%d]: %s", ch, pos, Debug_UnicodeString(CurContext));

  if(pos == MAXCONTEXT - 1)
  {
    memmove(CurContext, &CurContext[1], MAXCONTEXT*2 - 2); pos--;
  }

  CurContext[pos++] = ch;
  CurContext[pos] = 0;

  //DebugLog("KMX_Context::Add(%x):  EXIT [%d]: %s", ch, pos, Debug_UnicodeString(CurContext));
}


KMX_WCHAR *KMX_Context::Buf(int n)
{
  KMX_WCHAR *p;

  for(p = (KMX_WCHAR *) u16chr(CurContext, 0); n > 0 && p > CurContext; p = decxstr(p, CurContext), n--);

  if(n > 0) return NULL;
  return p;
}

void KMX_Context::Delete()
{
  DebugLog("KMX_Context::Delete: ENTER: %s", Debug_UnicodeString(CurContext));
  if (CharIsDeadkey()) {
    pos -= 2;
  } else if (CharIsSurrogatePair()) {
    pos--;
  }

  if(pos > 0) pos--;
  CurContext[pos] = 0;
  //DebugLog("KMX_Context::Delete:  EXIT: %s", Debug_UnicodeString(CurContext));
}

void KMX_Context::Reset()
{
  DebugLog("KMX_Context::Reset");
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
  DebugLog("KMX_Context::Set(%s): ENTER: %s", Debug_UnicodeString(buf), Debug_UnicodeString(CurContext, 1));
  const KMX_WCHAR *p;
  KMX_WCHAR *q;

  // We may be past a buffer longer than our internal
  // buffer. So we shift to make sure we capture the end
  // of the string, not the start
  p = u16chr(buf, 0);
  q = (KMX_WCHAR*)p;
  while(p > buf && (intptr_t)(q-p) < MAXCONTEXT - 1) {
    p = decxstr((KMX_WCHAR*)p, (KMX_WCHAR*)buf);
  }

  // If the first character in the buffer is a surrogate pair,
  // or a deadkey, our buffer may be too long, so move to the
  // next character in the buffer
  if ((intptr_t)(q-p) > MAXCONTEXT - 1) {
    p = incxstr((KMX_WCHAR*)p);
  }

  for(q = CurContext; *p; p++, q++)
  {
    *q = *p;
  }

  *q = 0;
  pos = (intptr_t)(q-CurContext);
  CurContext[MAXCONTEXT-1] = 0;

  //DebugLog("KMX_Context::Set(%s):  EXIT: %s", Debug_UnicodeString(buf), Debug_UnicodeString(CurContext, 1));
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


