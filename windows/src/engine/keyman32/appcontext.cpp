#include "pch.h"
// AppContext Class Methods
AppContext::AppContext() {
  Reset();
}

WCHAR *
AppContext::BufMax(int n) {
  WCHAR *p = wcschr(CurContext, 0);  // I3091

  if (CurContext == p || n == 0)
    return p; /* empty context or 0 characters requested, return pointer to end of context */  // I3091

  WCHAR *q = p;  // I3091
  for (; p != NULL && p > CurContext && (INT_PTR)(q - p) < n; p = decxstr(p, CurContext))
    ;  // I3091

  if ((INT_PTR)(q - p) > n)
    p = incxstr(p); /* Copes with deadkey or supplementary pair at start of returned buffer making it too long */  // I3091

  return p;  // I3091
}

void
AppContext::Delete() {
  if (CharIsDeadkey()) {
    pos -= 2;
  } else if (CharIsSurrogatePair()) {
    pos--;
  }
  // SendDebugMessageFormat(0, sdmAIDefault, 0, "AppContext::Delete");

  if (pos > 0)
    pos--;
  CurContext[pos] = 0;
  // if(--pos < 0) pos = 0;
  // SendDebugMessageFormat(0, sdmAIDefault, 0, "AppContext: Delete");
}

void
AppContext::Reset() {
  pos           = 0;
  CurContext[0] = 0;

  //	SendDebugMessageFormat(0, sdmAIDefault, 0, "AppContext: Reset");
}

void
AppContext::Get(WCHAR *buf, int bufsize) {
  // surrogate pairs need to be treated as a single unit, therefore use
  // BufMax to find a start index.
  // BufMax handles the case where a surrogate pair at the
  // start of the buffer is split by bufsize
  for (WCHAR *p = this->BufMax(bufsize); *p && bufsize > 0; p++, bufsize--) {
    *buf = *p;
    if (Uni_IsSurrogate1(*p) && bufsize - 2 > 0) {
      buf++;
      p++;
      *buf = *p;
      bufsize--;
    }
    buf++;
  }

  *buf = 0;
}

void
AppContext::Set(const WCHAR *buf) {
  const WCHAR *p;
  WCHAR *q;

  // We may be past a buffer longer than our internal
  // buffer. So we shift to make sure we capture the end
  // of the string, not the start
  p = wcschr(buf, 0);
  q = (WCHAR *)p;
  while (p != NULL && p > buf && (intptr_t)(q - p) < MAXCONTEXT - 1) {
    p = decxstr((WCHAR *)p, (WCHAR *)buf);
  }

  // If the first character in the buffer is a surrogate pair,
  // or a deadkey, our buffer may be too long, so move to the
  // next character in the buffer
  if ((intptr_t)(q - p) > MAXCONTEXT - 1) {
    p = incxstr((WCHAR *)p);
  }

  for (q = CurContext; *p; p++, q++) {
    *q = *p;
  }

  *q                         = 0;
  pos                        = (int)(intptr_t)(q - CurContext);
  CurContext[MAXCONTEXT - 1] = 0;
}

BOOL
AppContext::CharIsDeadkey() {
  if (pos < 3)  // code_sentinel, deadkey, #, 0
    return FALSE;
  return CurContext[pos - 3] == UC_SENTINEL && CurContext[pos - 2] == CODE_DEADKEY;
}

BOOL
AppContext::CharIsSurrogatePair() {
  if (pos < 2)  // low_surrogate, high_surrogate
    return FALSE;

  return Uni_IsSurrogate1(CurContext[pos - 2]) && Uni_IsSurrogate2(CurContext[pos - 1]);
}

BOOL
AppContext::IsEmpty() {
  return (BOOL)(pos == 0);
}

BOOL
ContextItemToAppContext(km_core_context_item *contextItems, PWSTR outBuf, DWORD len) {
  assert(contextItems);
  assert(outBuf);

  km_core_context_item *km_core_context_it = contextItems;
  uint8_t contextLen                       = 0;
  for (; km_core_context_it->type != KM_CORE_CT_END; ++km_core_context_it) {
    ++contextLen;
  }

  WCHAR *buf         = new WCHAR[(contextLen * 3) + 1];  // *3 if every context item was a deadkey
  uint8_t idx        = 0;
  km_core_context_it = contextItems;
  for (; km_core_context_it->type != KM_CORE_CT_END; ++km_core_context_it) {
    switch (km_core_context_it->type) {
    case KM_CORE_CT_CHAR:
      if (Uni_IsSMP(km_core_context_it->character)) {
        buf[idx++] = static_cast<WCHAR> Uni_UTF32ToSurrogate1(km_core_context_it->character);
        buf[idx++] = static_cast<WCHAR> Uni_UTF32ToSurrogate2(km_core_context_it->character);
      } else {
        buf[idx++] = (km_core_cu)km_core_context_it->character;
      }
      break;
    case KM_CORE_CT_MARKER:
      assert(km_core_context_it->marker > 0);
      buf[idx++] = UC_SENTINEL;
      buf[idx++] = CODE_DEADKEY;
      buf[idx++] = static_cast<WCHAR>(km_core_context_it->marker);
      break;
    }
  }

  buf[idx] = 0;  // Null terminate character array

  if (wcslen(buf) > len) {
    // Truncate to length 'len' using AppContext so that the context closest to the caret is preserved
    // and the truncation will not split deadkeys or surrogate pairs
    // Note by using the app context class we will truncate the context to the MAXCONTEXT length if 'len'
    // is greater than MAXCONTEXT
    AppContext context;
    context.Set(buf);
    context.Get(outBuf, len);
  } else {
    wcscpy_s(outBuf, wcslen(buf) + 1, buf);
  }
  delete[] buf;
  return TRUE;
}
