
#include "stdafx.h"
#include <stdio.h>

void DoLog(int level, const char *file, int line, const wchar_t *message, ...)
{
  wchar_t fmtbuf[256], outbuf[300];

  if (level < 0 || level > 2) level = 0;
  const wchar_t *state[] = { L"Info", L"Warning", L"Error" };

  va_list vars;
  va_start(vars, message);
  _vsnwprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, message, vars);  // I2248   // I3547
  fmtbuf[255] = 0;

  wsprintfW(outbuf, L"[MMLog%s(%S:%d)] %s\n", state[level], file, line, fmtbuf);

  if (LogToDebugConsole()) {
    OutputDebugString(outbuf);
  }

#ifndef _USRDLL
  outbuf[wcslen(outbuf) - 1] = 0; // remove final \n
  _putws(outbuf);
#endif
}

inline BOOL LogToDebugConsole() {
  //#ifdef _DEBUG
  return TRUE;
  //#else
    return FALSE;
  //#endif
}

BOOL DoVerboseLogging() {
  // 
  return TRUE;
}

constexpr char hexmap[] = {
  '0', '1', '2', '3', '4', '5', '6', '7',
  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' // lowercase because Delphi's System.Classes.HexToBin requires it
};

void hexStr2(PBYTE data, int len, wchar_t *out) {
  for (int i = 0; i < len; ++i) {
    out[2 * i] = hexmap[(data[i] & 0xF0) >> 4];
    out[2 * i + 1] = hexmap[data[i] & 0x0F];
  }
  out[2 * len] = 0;
}
