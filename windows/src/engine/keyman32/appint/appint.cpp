/*
  Name:             appint
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      5 Nov 2007

  Modified Date:    23 Jun 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          05 Nov 2007 - mcdurdin - I1129 - Fix irregular behaviour with context rules in debugger
                    27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    03 Oct 2011 - mcdurdin - I3091 - IMX DLLs cause an assertion failure due to buffer size mismatch
                    17 Nov 2012 - mcdurdin - I3575 - V9.0 - context must be cached for legacy mode in TSF as rules are processed twice
                    10 Jun 2014 - mcdurdin - I4262 - V9.0 - TSF deadkeys do not function correctly
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
*/
   // I4287
#include "pch.h"

PWSTR decxstr(PWSTR p, PWSTR pStart);

const LPSTR ItemTypes[8] = {
	"QIT_VKEYDOWN", "QIT_VKEYUP", "QIT_VSHIFTDOWN", "QIT_VSHIFTUP",
    "QIT_CHAR", "QIT_DEADKEY", "QIT_BELL", "QIT_BACK" };

/* AppContext */

AppContext::AppContext()
{
	Reset();
}

void AppContext::Add(WCHAR ch)
{
	if(pos == MAXCONTEXT - 1)
	{
//    SendDebugMessageFormat(0, sdmAIDefault, 0, "AppContext: MAXCONTEXT[%d]: %ws", pos, CurContext);
		memmove(CurContext, &CurContext[1], MAXCONTEXT*2 - 2); pos--;
	}

	CurContext[pos++] = ch;
	CurContext[pos] = 0;

  SendDebugMessageFormat(0, sdmAIDefault, 0, "AppContext: Add(%x) [%d]: %s", ch, pos, Debug_UnicodeString(CurContext));
}

WCHAR *AppContext::Buf(int n)
{
	WCHAR *p;

	//SendDebugMessageFormat(0, sdmAIDefault, 0, "AppContext::Buf(%d)", n);
	//if(n == 0) return wcschr(CurContext, 0);
	//if(*CurContext == 0) return NULL;

	for(p = wcschr(CurContext, 0); p != NULL && n > 0 && p > CurContext; p = decxstr(p, CurContext), n--);
	//for(p = wcschr(CurContext, 0); n > 0 && p > CurContext; p--, n--);

	if(n > 0) return NULL;
	return p;
}

WCHAR *AppContext::BufMax(int n)
{
	WCHAR *p = wcschr(CurContext, 0);  // I3091

	if(CurContext == p || n == 0) return p; /* empty context or 0 characters requested, return pointer to end of context */  // I3091

  WCHAR *q = p;  // I3091
	for(; p != NULL && p > CurContext && (INT_PTR)(q-p) < n; p = decxstr(p, CurContext));  // I3091

  if((INT_PTR)(q-p) > n) p = incxstr(p); /* Copes with deadkey or supplementary pair at start of returned buffer making it too long */  // I3091

  return p;  // I3091
}

void AppContext::Delete()
{
  if (CharIsDeadkey()) {
    pos -= 2;
  } else if (CharIsSurrogatePair()) {
    pos--;
  }
	//SendDebugMessageFormat(0, sdmAIDefault, 0, "AppContext::Delete");

	if(pos > 0) pos--;
	CurContext[pos] = 0;
	//if(--pos < 0) pos = 0;
	//SendDebugMessageFormat(0, sdmAIDefault, 0, "AppContext: Delete");
}

void AppContext::Reset()
{
	pos = 0;
	CurContext[0] = 0;

//	SendDebugMessageFormat(0, sdmAIDefault, 0, "AppContext: Reset");
}

void AppContext::Get(WCHAR *buf, int bufsize)
{
  // surrogate pairs need to be treated as a single unit, therefore use
  // BufMax to find a start index.
  // BufMax handles the case where a surrogate pair at the
  // start of the buffer is split by bufsize
  for (WCHAR *p = this->BufMax(bufsize); *p && bufsize > 0; p++, bufsize--)
  {
    *buf = *p;
    if(Uni_IsSurrogate1(*p) && bufsize - 2 > 0) { 
      buf++; p++;
      *buf = *p;
      bufsize--;
    }
    buf++;
  }

  *buf = 0;
}

void AppContext::CopyFrom(AppContext *source)   // I3575
{
  SendDebugMessageFormat(0, sdmAIDefault, 0, "AppContext::CopyFrom source=%s; before copy, dest=%s", Debug_UnicodeString(source->CurContext, 0), Debug_UnicodeString(CurContext, 1));
  wcscpy_s(CurContext, _countof(CurContext), source->CurContext);
	pos = source->pos;
}


void AppContext::Set(const WCHAR *buf)
{
	const WCHAR *p;
	WCHAR *q;
	for(p = buf, q = CurContext; *p && (INT_PTR)(q-CurContext) < MAXCONTEXT - 1; p++, q++)
	{
		*q = *p;
		if(*p >= 0xD800 && *p <= 0xDBFF) { *(++q) = *(++p); }
	}
	*q = 0;
  pos = (int)(INT_PTR)(q-CurContext);  // I1129 - Irregular behaviour with context rules
	CurContext[MAXCONTEXT-1] = 0;
}

BOOL AppContext::CharIsDeadkey()
{
	if(pos < 3) // code_sentinel, deadkey, #, 0
		return FALSE;
	return CurContext[pos-3] == UC_SENTINEL &&
		   CurContext[pos-2] == CODE_DEADKEY;
}

BOOL AppContext::CharIsSurrogatePair()
{
  if (pos < 2) // low_surrogate, high_surrogate
    return FALSE;

  return Uni_IsSurrogate1(CurContext[pos - 2]) &&
    Uni_IsSurrogate2(CurContext[pos - 1]);
}

/* AppActionQueue */

AppActionQueue::AppActionQueue()
{
  memset(Queue, 0, sizeof(APPACTIONQUEUEITEM) * MAXACTIONQUEUE);
	ResetQueue();
}

void AppActionQueue::ResetQueue()
{
	//SendDebugMessageFormat(0, sdmAIDefault, 0, "App::ResetQueue");
	QueueSize = 0;   // I4262
}

BOOL AppActionQueue::QueueAction(int ItemType, DWORD dwData)
{
	if(QueueSize > MAXACTIONQUEUE - 1)
	{
		MessageBeep(0xFFFFFFFF);
		return FALSE;
	}

	Queue[QueueSize].ItemType = ItemType;
	Queue[QueueSize].dwData   = dwData;

	QueueSize++;

	SendDebugMessageFormat(0, sdmAIDefault, 0, "App::QueueAction: %s %x", ItemTypes[ItemType], dwData);

	return TRUE;
}

/* AppIntegration */

AppIntegration::AppIntegration()
{
	hwnd = NULL;
  FShiftFlags = 0;
}

BOOL ContextItemsFromAppContext(WCHAR const* buf, km_kbp_context_item** outPtr)
{
  assert(buf);
  assert(outPtr);
  km_kbp_context_item* context_items  = new km_kbp_context_item[wcslen(buf) + 1];
  WCHAR const *p = buf;
  uint8_t contextIndex = 0;
  while (*p) {
    if (*p == UC_SENTINEL) {
      assert(*(p + 1) == CODE_DEADKEY);
      // we know the only uc_sentinel code in the context is code_deadkey, which has only 1 parameter: uc_sentinel code_deadkey <deadkey_id>
      // setup dead key context item
      p += 2;
      context_items[contextIndex++] = km_kbp_context_item{ KM_KBP_CT_MARKER, {0,}, {*p} };
    } else if (Uni_IsSurrogate1(*p) && Uni_IsSurrogate2(*(p + 1))) {
      // handle surrogate
      context_items[contextIndex++] = km_kbp_context_item{ KM_KBP_CT_CHAR, {0,}, {(char32_t)Uni_SurrogateToUTF32(*p, *(p + 1))} };
      p++;
    } else {
      context_items[contextIndex++] = km_kbp_context_item{ KM_KBP_CT_CHAR, {0,}, {*p} };
    }
    p++;
  }
  // terminate the context_items array.
  context_items[contextIndex] = km_kbp_context_item KM_KBP_CONTEXT_ITEM_END;

  *outPtr = context_items;
  return true;
}
