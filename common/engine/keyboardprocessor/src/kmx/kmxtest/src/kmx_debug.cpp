/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "pch.h"
#include <stdarg.h>

inline KMX_BOOL ShouldDebug() {
  return g_debug_KeymanLog;
}

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

int DebugLog_1(const char *file, int line, const char *function, const char *fmt, ...)
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
        "%d" TAB     //"TickCount" TAB
        "%s:%d" TAB  //"SourceFile" TAB
        "%s" TAB     //"Function"
        "%s" NL,     //"Message"

        GetTickCount(),   //"TickCount" TAB
        file, line,       //"SourceFile" TAB
        function,         //"Function" TAB
        fmtbuf);          //"Message"
#ifdef _USE_WINDOWS
      OutputDebugStringA(windowinfo);
#else
      syslog(LOG_DEBUG, "%s", windowinfo);
#endif
    }
	}

  return 0;
}

struct Debug_ModifierNames {
  char *name;
  KMX_UINT flag;
};

extern const char *VKeyNames[256];

const struct Debug_ModifierNames s_modifierNames[14] = {
  {" LCTRL", 0x0001},		// Left Control flag
  {" RCTRL", 0x0002},		// Right Control flag
  {" LALT", 0x0004},		// Left Alt flag
  {" RALT", 0x0008},		// Right Alt flag
  {" SHIFT", 0x0010},		// Either shift flag
  {" CTRL", 0x0020},		// Either ctrl flag -- don't use this for inputs
  {" ALT", 0x0040},		// Either alt flag -- don't use this for inputs
  {" CAPS", 0x0100},		// Caps lock on
  {" NCAPS", 0x0200},		// Caps lock NOT on
  {" NUMLOCK", 0x0400},		// Num lock on
  {" NNUMLOCK", 0x0800},		// Num lock NOT on
  {" SCROLL", 0x1000},		// Scroll lock on
  {" NSCROLL", 0x2000},		// Scroll lock NOT on
  {NULL, 0}
};

char *Debug_ModifierName(KMX_UINT modifiers) {
#ifdef _MSC_VER
  __declspec(thread)
#endif 
  static char buf[256];
  buf[0] = 0;
  for(int i = 0; s_modifierNames[i].name; i++)
    if (modifiers & s_modifierNames[i].flag) {
      strcat(buf, s_modifierNames[i].name);
    }

  if (*buf) return buf + 1;
  return "Unmodified";
}

char *Debug_VirtualKey(KMX_WORD vk) {
#ifdef _MSC_VER
  __declspec(thread)
#endif 
  static char buf[256];
  if (!ShouldDebug()) {
    return "";
  }

  if (vk < 256) {
    sprintf(buf, "['%s' 0x%x]", VKeyNames[vk], vk);
  }
  else {
    sprintf(buf, "[0x%x]", vk);
  }
  return buf;
}

char *Debug_UnicodeString(PKMX_WCHAR s, int x) {
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

char *Debug_UnicodeString(std::u16string s, int x) {
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
  for (p, q = bufout[x]; p != s.end(); p++)
  {
    sprintf(q, "U+%4.4X ", *p); q = strchr(q, 0);
  }
  //WideCharToMultiByte(CP_ACP, 0, buf, -1, bufout, 128, NULL, NULL);
  return bufout[x];
}

void write_console(KMX_BOOL error, const wchar_t *fmt, ...) {
  if (!g_silent || error) {
    va_list vars;
    va_start(vars, fmt);
    vwprintf(fmt, vars);
    va_end(vars);
  }
}
