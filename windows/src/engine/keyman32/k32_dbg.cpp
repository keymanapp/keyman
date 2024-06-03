/*
  Name:             K32_DBG
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      14 Sep 2006

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          14 Sep 2006 - mcdurdin - Rework internal debugging to use mailslot passing to Keyman
                    05 Dec 2006 - mcdurdin - Disable temporary file-based logging
                    15 Jan 2007 - mcdurdin - Use _FILELOG define
                    16 Jan 2009 - mcdurdin - Clearer logging of key messages
                    30 Jan 2009 - mcdurdin - I1835 - Improve performance of debugging
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    07 Sep 2009 - mcdurdin - I2098 - Some debug messages lost in multithreaded apps
                    30 Nov 2009 - mcdurdin - I2157 - Deletion of buffer crash if LIBRARY_NAME has no version info
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    11 Dec 2009 - mcdurdin - I934 - identify x64 debugging
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    22 Mar 2010 - mcdurdin - I2248 - Crash in Keyman Engine outputting log information
                    29 Mar 2010 - mcdurdin - I2263 - Show thread and process info in debug log
                    19 Apr 2010 - mcdurdin - I2310 - system.log needs to be reformatted for simple import into database
                    04 May 2010 - mcdurdin - I2348 - Debug log needs some reworking
                    04 May 2010 - mcdurdin - I2349 - Pause key to send debug log event
                    04 May 2010 - mcdurdin - I2352 - Debug logging not reliable in some apps
                    04 May 2010 - mcdurdin - I2353 - Need to report Win32 failures more reliably
                    28 Jun 2010 - mcdurdin - I2445 - Try and reconnect debug pipe on error
                    31 Jan 2011 - mcdurdin - I2690 - Add ForegroundWindow info to debug log
                    12 May 2011 - mcdurdin - I2908 - Fix double strike issue - clear up logging
                    05 Nov 2012 - mcdurdin - I3547 - V9.0 Use _countof to tidyup code
                    17 Nov 2012 - mcdurdin - I3569 - V9.0 - Simplify debug logs by removing unused data
                    17 Nov 2012 - mcdurdin - I3570 - V9.0 - Output debug log to debug console
                    13 Dec 2012 - mcdurdin - I3656 - V9.0 - Debug logs show too many columns for host messages
                    07 Nov 2013 - mcdurdin - I3951 - V9.0 - Add debug-to-console hidden option for Keyman32
                    24 Apr 2014 - mcdurdin - I4196 - V9.0 - wm_kmmoreposting must be refactored for TIP work as it is not sequential
                    09 Aug 2015 - mcdurdin - I4843 - Log reported modifier state as well as Keyman current modifier state
*/
#include "pch.h"
#include "serialkeyeventcommon.h"
#include <stdio.h>
#include <stdarg.h>
#include <evntprov.h>

//#define _FILELOG

#ifdef _FILELOG
//FILE *fp = NULL;
#endif

// {DA621615-E08B-4283-918E-D2502D3757AE}
static const GUID guid_EtwProviderId =
{ 0xda621615, 0xe08b, 0x4283, { 0x91, 0x8e, 0xd2, 0x50, 0x2d, 0x37, 0x57, 0xae } };


enum {NO_DEBUG_WINDOWS=100, UNABLE_TO_CREATE_FILE_MAPPING, UNABLE_TO_MAP_VIEW_OF_FILE};

extern void GetWindowsVersion(char *buf);

void InitDebuggingEx(PKEYMAN64THREADDATA _td) {
	if(Globals::get_debug_KeymanLog()) {
    DWORD dwErr = EventRegister(&guid_EtwProviderId, NULL, NULL, &_td->etwRegHandle);
    if (dwErr != ERROR_SUCCESS) {
      OutputDebugString("Keyman k32_dbg: Failed to EventRegister");//TODO: Build a helper function with GLE?
    }
	}
}

void InitDebugging() {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

	InitDebuggingEx(_td);
	_td->debug_DebugInit = TRUE;

	if(Globals::get_debug_KeymanLog())
	{
		VS_FIXEDFILEINFO *ffi;
		DWORD sz;
		UINT ffilen;
		char *buf = new char[1024], fname[260];

		GetWindowsVersion(buf);
		SendDebugMessage(0, sdmKeyboard, 0, buf);
		delete buf;

		GetModuleFileName(GetModuleHandle(LIBRARY_NAME), fname, 260);
		sz = GetFileVersionInfoSize(fname, 0);
		if(sz > 0)
		{
			buf = new char[sz];
			GetFileVersionInfo(fname, 0, sz, buf);
			VerQueryValue(buf, "\\", (void **) &ffi, &ffilen);

			SendDebugMessageFormat(0, sdmKeyboard, 0, "Keyman version: %d.%d.%d.%d",
				HIWORD(ffi->dwProductVersionMS), LOWORD(ffi->dwProductVersionMS),
				HIWORD(ffi->dwProductVersionLS), LOWORD(ffi->dwProductVersionLS));
  		delete buf; // I2157
		}
		else
			SendDebugMessage(0, sdmKeyboard, 0, "Keyman version: damaged");
	}
}

void UninitDebuggingEx()
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

  if (_td->etwRegHandle != NULL) {
    EventUnregister(_td->etwRegHandle);
    _td->etwRegHandle = NULL;
  }

  _td->debug_DebugInit = FALSE;
}

void UninitDebugging() {
  SendDebugMessage(0, sdmGlobal, 0, "--- UninitDebugging() - shutting down ---");
}

const TSDMState DebugState[] = {sdmInternat, sdmAIDefault, sdmMessage, sdmKeyboard, sdmGlobal, sdmMenu, sdmDebug, sdmLoad, sdmOther};

BOOL ShouldDebug_1() {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

  if (!_td->debug_DebugInit) InitDebuggingEx(_td);
  _td->debug_DebugInit = TRUE;
  return Globals::get_debug_KeymanLog();
}

int SendDebugMessageFormat_1(HWND hwnd, TSDMState state, int kmn_lineno, wchar_t*file, int line, char *fmt, ...)
{
	char fmtbuf[256];

	va_list vars;
	va_start(vars, fmt);
	vsnprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, fmt, vars);  // I2248   // I3547
	fmtbuf[255] = 0;
	SendDebugMessage_1(hwnd, state, kmn_lineno, file, line, fmtbuf);

  return 0;
}

int SendDebugMessageFormatW_1(HWND hwnd, TSDMState state, int kmn_lineno, wchar_t* file, int line, wchar_t* fmt, ...)
{
  wchar_t fmtbuf[256];

  va_list vars;
  va_start(vars, fmt);
  _vsnwprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, fmt, vars);  // I2248   // I3547
  fmtbuf[255] = 0;
  SendDebugMessageW_1(hwnd, state, kmn_lineno, file, line, fmtbuf);

  return 0;
}

/*void FillWindowInfo(HWND hwnd, char *sClassName, char *sWindowText)
{
	if(hwnd == 0 || !GetClassName(hwnd, sClassName, 32))
		sClassName[0] = 0;
  else
	  sClassName[31] = 0;

  WCHAR wsWindowText[32];

  if(hwnd == 0 || !InternalGetWindowText(hwnd, wsWindowText, 32))
    sWindowText[0] = 0;
  else
  {
    WideCharToMultiByte(CP_ACP, 0, wsWindowText, -1, sWindowText, 32, NULL, NULL);
    sWindowText[31] = 0;
  }
}*/


int SendDebugMessageW_1(HWND hwnd, TSDMState state, int kmn_lineno, wchar_t*file, int line, wchar_t* msg)
{
  UNREFERENCED_PARAMETER(hwnd);
  UNREFERENCED_PARAMETER(state);   // I3569
  UNREFERENCED_PARAMETER(kmn_lineno);

  Keyman_WriteDebugEventW(file, line, msg);

  return 0;
}

int SendDebugMessage_1(HWND hwnd, TSDMState state, int kmn_lineno, wchar_t*file, int line, char *msg)
{
  UNREFERENCED_PARAMETER(hwnd);
  UNREFERENCED_PARAMETER(state);   // I3569
  UNREFERENCED_PARAMETER(kmn_lineno);

  // TODO: convert debugging to Unicode
  PWSTR msgW = strtowstr(msg);
  Keyman_WriteDebugEventW(file, line, msgW);
  delete [] msgW;

  return 0;
}

const
char *msgnames[] = {
"WM_KEYDOWN",
"WM_KEYUP",
"WM_CHAR",
"WM_DEADCHAR",
"WM_SYSKEYDOWN",
"WM_SYSKEYUP",
"WM_SYSCHAR",
"WM_SYSDEADCHAR",
"WM_x108",
"WM_UNICHAR"
};

void DebugMessage(LPMSG msg, WPARAM wParam)  // I2908
{
	char ds[256];

	if(msg->message == wm_keymankeydown)
    wsprintf(ds, "DebugMessage(%x, wm_keymankeydown: %s lParam: %X) [message flags: %x time: %d]", PtrToInt(msg->hwnd),
      Debug_VirtualKey((WORD) msg->wParam), (unsigned int) msg->lParam, wParam, (int) msg->time);
	else if(msg->message == wm_keymankeyup)
    wsprintf(ds, "DebugMessage(%x, wm_keymankeyup: %s lParam: %X) [message flags: %x time: %d]", PtrToInt(msg->hwnd),
      Debug_VirtualKey((WORD) msg->wParam), (unsigned int) msg->lParam, wParam, (int) msg->time);
  else if (msg->message == WM_KEYMAN_KEY_EVENT)
    wsprintf(ds, "DebugMessage(%x, WM_KEYMAN_KEY_EVENT: %s lParam: %X) [message flags: %x time: %d]", PtrToInt(msg->hwnd),
      Debug_VirtualKey((WORD)msg->wParam), (unsigned int) msg->lParam, wParam, (int) msg->time);
  else if(msg->message == WM_KEYDOWN || msg->message == WM_KEYUP || msg->message == WM_SYSKEYDOWN || msg->message == WM_SYSKEYUP)
    wsprintf(ds, "DebugMessage(%x, %s, wParam: %s, lParam: %X) [message flags: %x time: %d extra: %x]",
      PtrToInt(msg->hwnd),
      msgnames[msg->message-WM_KEYDOWN],
      Debug_VirtualKey((WORD) msg->wParam),
      (unsigned int) msg->lParam,
      wParam,
      (int) msg->time,
      (unsigned int) GetMessageExtraInfo());
	else if(msg->message >= WM_KEYDOWN && msg->message <= WM_UNICHAR)
    wsprintf(ds, "DebugMessage(%x, %s, wParam: '%c' (U+%04X), lParam: %X) [message flags: %x time: %d extra: %x]",
      PtrToInt(msg->hwnd),
      msgnames[msg->message-WM_KEYDOWN],
      msg->wParam,
      msg->wParam,
      (unsigned int) msg->lParam,
      wParam,
      (int) msg->time,
      (unsigned int) GetMessageExtraInfo());
	else
    wsprintf(ds, "%x: %d: wParam: %d, lParam: %X [message flags: %x time: %d]", PtrToInt(msg->hwnd), msg->message, msg->wParam, (unsigned int) msg->lParam, wParam, (int) msg->time);

	SendDebugMessage(msg->hwnd, sdmMessage, 0, ds);
}

void DebugShift(char *function, char *point)
{
	BYTE kbd[256];
	GetKeyboardState(kbd);

	SendDebugMessageFormat(0, sdmKeyboard, 0, "%s - %s Alt: [%x] %x %x Ctrl: [%x] %x %x Shift: %x",
		function, point,
		kbd[VK_MENU],
		kbd[VK_LMENU],
		kbd[VK_RMENU],
		kbd[VK_CONTROL],
		kbd[VK_LCONTROL],
		kbd[VK_RCONTROL],
		kbd[VK_SHIFT]);
}

BOOL DebugSignalPause(BOOL fIsUp)
{
  if(ShouldDebug(sdmDebug))
  {
    if(fIsUp)
    {
      SendDebugMessage_1(NULL, sdmDebug, -1, NULL, 0, NULL); // Signals an event to the controller
    }
    return TRUE;
  }
  return FALSE;
}

void DebugLastError_1(DWORD err, char *context, wchar_t *file, int line, char *func)
{
  if(ShouldDebug(sdmDebug))
  {
    char msg[256];
    if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_ARGUMENT_ARRAY, NULL, err, 0, msg, sizeof(msg), NULL) == 0) msg[0] = 0;
    for (char *p = msg; *p; p++) {
      if (*p == '\r' || *p == '\n') *p = ' ';
    }
    SendDebugMessageFormat_1(NULL, sdmDebug, 0, file, line, "ERROR %d in %s calling %s: %s", err, func, context, msg);
  }
}

char *Debug_VirtualKey(WORD vk) {
  __declspec(thread) static char buf[256];
  if (!ShouldDebug(sdmDebug)) {
    return "";
  }

  if (vk < 256) {
    wsprintf(buf, "['%s' 0x%x]", KeyNames[vk], vk);
  }
  else {
    wsprintf(buf, "[0x%x]", vk);
  }
  return buf;
}

char *Debug_UnicodeString(PWSTR s, int x) {
  if (!ShouldDebug(sdmDebug)) {
    return "";
  }
  __declspec(thread) static char bufout[2][128 * 7];
  WCHAR *p;
  char *q;
  memset(bufout, 0, sizeof bufout);
  for (p = s, q = bufout[x]; *p && (p - s < 128); p++)
  {
    wsprintf(q, "U+%4.4X ", *p); q = strchr(q, 0);
  }
  //WideCharToMultiByte(CP_ACP, 0, buf, -1, bufout, 128, NULL, NULL);
  return bufout[x];
}

BOOL DebugAssert_1(BOOL condition, char *message, wchar_t *file, int line)
{
  if (!(condition)) {
    SendDebugMessage_1(0, sdmGlobal, 0, file, line, message);
    if (IsDebugAssertEnabled()) {
      assert(condition);
    }
    return FALSE;
  }
  return TRUE;
}

#ifdef _DEBUG
void _OutputThreadDebugString(char *s) {
  char buf[256];
  sprintf_s(buf, "[%d]: %s\n", GetCurrentThreadId(), s);
  OutputDebugString(buf);
}
#endif

BOOL IsDebugAssertEnabled()
{
#ifdef _DEBUG
  return IsDebuggerPresent();
#else
  return FALSE;
#endif
}

void WINAPI Keyman_Diagnostic(int mode) {
  if (mode == 0) {
    RaiseException(0xDEADBEEF, EXCEPTION_NONCONTINUABLE, 0, NULL);
  }
}
