/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "kmx_processor.h"
#include <stdarg.h>
#include <iostream>

using namespace km::kbp;
using namespace kmx;

#define TAB "\t"
#define NL  "\n"

#include <chrono>

#ifdef _MSC_VER
#define _USE_WINDOWS
#endif

#ifdef _USE_WINDOWS
#define DECLSPEC_IMPORT __declspec(dllimport)
#define WINBASEAPI DECLSPEC_IMPORT
#define VOID void
#define WINAPI      __stdcall

extern "C"
__declspec(dllimport)
void
__stdcall
OutputDebugStringA(
  char *lpOutputString
);

#else
  #include <syslog.h>
#endif

// simulation of Windows GetTickCount()
unsigned long
GetTickCount()
{
  using namespace std::chrono;
  return (unsigned long) duration_cast<milliseconds>(steady_clock::now().time_since_epoch()).count();
}

int km::kbp::kmx::DebugLog_1(const char *file, int line, const char *function, const char *fmt, ...)
{
  char fmtbuf[256];

  va_list vars;
  va_start(vars, fmt);
  vsnprintf(fmtbuf, sizeof(fmtbuf) / sizeof(fmtbuf[0]), fmt, vars);  // I2248   // I3547
  fmtbuf[255] = 0;
  va_end(vars);

  if(g_debug_KeymanLog) {
    if(g_debug_ToConsole) {   // I3951
      char windowinfo[1024];
      sprintf(windowinfo,
        "%ld" TAB     //"TickCount" TAB
        "%s:%d" TAB  //"SourceFile" TAB
        "%s" TAB     //"Function"
        "%s" NL,     //"Message"

        GetTickCount(),   //"TickCount" TAB
        file, line,       //"SourceFile" TAB
        function,         //"Function" TAB
        fmtbuf);          //"Message"
#ifdef _USE_WINDOWS
      std::cout << windowinfo << std::endl; // OutputDebugStringA(windowinfo);
#else
      syslog(LOG_DEBUG, "%s", windowinfo);
#endif
    }
  }

  return 0;
}

const char *km::kbp::kmx::Debug_ModifierName(KMX_UINT modifiers) {
#ifdef _MSC_VER
  __declspec(thread)
#endif 
  static char buf[256];
  buf[0] = 0;
  for(int i = 0; s_modifier_names[i].name; i++)
    if (modifiers & s_modifier_names[i].modifier) {
      strcat(buf, " ");
      strcat(buf, s_modifier_names[i].name);
    }

  if (*buf) return buf + 1;
  return "Unmodified";
}

const char *km::kbp::kmx::Debug_VirtualKey(KMX_WORD vk) {
#ifdef _MSC_VER
  __declspec(thread)
#endif 
  static char buf[256];
  if (!ShouldDebug()) {
    return "";
  }

  if (vk < 256) {
    sprintf(buf, "['%s' 0x%x]", s_key_names[vk], vk);
  }
  else {
    sprintf(buf, "[0x%x]", vk);
  }
  return buf;
}

const char *km::kbp::kmx::Debug_UnicodeString(PKMX_WCHAR s, int x) {
  if (!ShouldDebug()) {
    return "";
  }
#ifdef _MSC_VER
  __declspec(thread)
#endif 
  static char bufout[2][128 * 7];
  KMX_WCHAR *p;
  char *q;
  bufout[x][0] = 0;
  for (p = s, q = bufout[x]; *p && (p - s < 128); p++)
  {
    sprintf(q, "U+%4.4X ", *p); q = strchr(q, 0);
  }
  //WideCharToMultiByte(CP_ACP, 0, buf, -1, bufout, 128, NULL, NULL);
  return bufout[x];
}

const char *km::kbp::kmx::Debug_UnicodeString(std::u16string s, int x) {
  if (!ShouldDebug()) {
    return "";
  }
#ifdef _MSC_VER
  __declspec(thread)
#endif 
  static char bufout[2][128 * 7];
  auto p = s.begin();
  char *q;
  bufout[x][0] = 0;
  for (q = bufout[x]; (int)(q-bufout[x]) < (128*7) && p != s.end(); p++)
  {
    sprintf(q, "U+%4.4X ", *p); q = strchr(q, 0);
  }
  return bufout[x];
}

void km::kbp::kmx::write_console(KMX_BOOL error, const wchar_t *fmt, ...) {
  if (!g_silent || error) {
    va_list vars;
    va_start(vars, fmt);
    vwprintf(fmt, vars);
    va_end(vars);
  }
}
