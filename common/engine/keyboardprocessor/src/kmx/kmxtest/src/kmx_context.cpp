/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "pch.h"
#include <stdlib.h>
#include <corecrt_wstring.h>

const LPSTR ItemTypes[QIT_MAX+1] = {
	"QIT_VKEYDOWN", "QIT_VKEYUP", "QIT_VSHIFTDOWN", "QIT_VSHIFTUP",
  "QIT_CHAR", "QIT_DEADKEY", "QIT_BELL", "QIT_BACK", "QIT_CAPSLOCK",
  "QIT_INVALIDATECONTEXT" };

/* KMX_Context */

KMX_Context::KMX_Context()
{
	Reset();
}

void KMX_Context::Add(WCHAR ch)
{
	if(pos == MAXCONTEXT - 1)
	{
		memmove(CurContext, &CurContext[1], MAXCONTEXT*2 - 2); pos--;
	}

	CurContext[pos++] = ch;
	CurContext[pos] = 0;

  DebugLog("KMX_Context: Add(%x) [%d]: %s", ch, pos, Debug_UnicodeString(CurContext));
}

WCHAR *KMX_Context::Buf(int n)
{
	WCHAR *p;

	for(p = wcschr(CurContext, 0); n > 0 && p > CurContext; p = decxstr(p), n--);

	if(n > 0) return NULL;
	return p;
}

WCHAR *KMX_Context::BufMax(int n)  // Used only by IMX DLLs
{
	WCHAR *p = wcschr(CurContext, 0);  // I3091

	if(CurContext == p || n == 0) return p; /* empty context or 0 characters requested, return pointer to end of context */  // I3091

  WCHAR *q = p;  // I3091
	for(; p > CurContext && (int)(q-p) < n; p = decxstr(p));  // I3091

  if((int)(q-p) > n) p = incxstr(p); /* Copes with deadkey or supplementary pair at start of returned buffer making it too long */  // I3091

  return p;  // I3091
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

void KMX_Context::Get(WCHAR *buf, int bufsize)
{
	for(WCHAR *p = CurContext; *p && bufsize > 0; p++, bufsize--)
	{
		*buf = *p; buf++;
		if(*p >= 0xD800 && *p <= 0xDBFF) { *buf = *(++p); bufsize--; buf++; }
	}
	//wcsncpy(buf, CurContext, bufsize); 
	*buf = 0;
	//buf[bufsize-1] = 0; 
}

void KMX_Context::CopyFrom(KMX_Context *source)   // I3575
{
  DebugLog("KMX_Context::CopyFrom source=%s; before copy, dest=%s", Debug_UnicodeString(source->CurContext, 0), Debug_UnicodeString(CurContext, 1));
  wcscpy_s(CurContext, _countof(CurContext), source->CurContext);
	pos = source->pos;
}


void KMX_Context::Set(const WCHAR *buf)
{
	const WCHAR *p;
	WCHAR *q;
	for(p = buf, q = CurContext; *p && (int)(q-CurContext) < MAXCONTEXT - 1; p++, q++)
	{
		*q = *p;
		if(*p >= 0xD800 && *p <= 0xDBFF) { *(++q) = *(++p); }
	}
	*q = 0;
  pos = (int)(q-CurContext);  // I1129 - Irregular behaviour with context rules
	CurContext[MAXCONTEXT-1] = 0; 
}

BOOL KMX_Context::CharIsDeadkey()
{
	if(pos < 3) // code_sentinel, deadkey, #, 0
		return FALSE;
	return CurContext[pos-3] == UC_SENTINEL && 
		   CurContext[pos-2] == CODE_DEADKEY;
}

BOOL KMX_Context::CharIsSurrogatePair()
{
  if (pos < 2) // low_surrogate, high_surrogate
    return FALSE;

  return Uni_IsSurrogate1(CurContext[pos - 2]) &&
    Uni_IsSurrogate2(CurContext[pos - 1]);
}


