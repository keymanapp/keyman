/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "pch.h"
#include <stdio.h>
#include <stdarg.h>

inline BOOL ShouldDebug() {
  return g_debug_KeymanLog;
}

#define TAB "\t"
#define NL  "\n"

int DebugLog_1(char *file, int line, char *function, char *fmt, ...)
{
	char fmtbuf[256];

	va_list vars;
	va_start(vars, fmt);
	vsnprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, fmt, vars);  // I2248   // I3547
	fmtbuf[255] = 0;

	if(g_debug_KeymanLog) {
    if(g_debug_ToConsole) {   // I3951
      char windowinfo[1024];
      wsprintf(windowinfo,
        "%d" TAB     //"TickCount" TAB
        "%s:%d" TAB  //"SourceFile" TAB
        "%s" TAB     //"Function"
        "%s" NL,     //"Message"

        GetTickCount(),   //"TickCount" TAB
        file, line,       //"SourceFile" TAB
        function,         //"Function" TAB
        fmtbuf);          //"Message"
      OutputDebugString(windowinfo);
    }
	}

  return 0;
}

extern const char *VKeyNames[256];

char *Debug_VirtualKey(WORD vk) {
  __declspec(thread) static char buf[256];
  if (!ShouldDebug()) {
    return "";
  }

  if (vk < 256) {
    wsprintf(buf, "['%s' 0x%x]", VKeyNames[vk], vk);
  }
  else {
    wsprintf(buf, "[0x%x]", vk);
  }
  return buf;
}

char *Debug_UnicodeString(PWSTR s, int x) {
  if (!ShouldDebug()) {
    return "";
  }
  __declspec(thread) static char bufout[2][128 * 7];
  WCHAR *p;
  char *q;
  for (p = s, q = bufout[x]; *p && (p - s < 128); p++)
  {
    wsprintf(q, "U+%4.4X ", *p); q = strchr(q, 0);
  }
  //WideCharToMultiByte(CP_ACP, 0, buf, -1, bufout, 128, NULL, NULL);
  return bufout[x];
}
