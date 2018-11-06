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
#include <stdio.h>
#include <stdarg.h>
#include <evntprov.h>

//#define _FILELOG

#ifdef _WIN64
#define DEBUG_PLATFORM_STRING "x64"
#define DEBUG_PLATFORM_STRINGW L"x64"
#else
#define DEBUG_PLATFORM_STRING "x86"
#define DEBUG_PLATFORM_STRINGW L"x86"
#endif

#ifdef _FILELOG
//FILE *fp = NULL;
#endif

enum {NO_DEBUG_WINDOWS=100, UNABLE_TO_CREATE_FILE_MAPPING, UNABLE_TO_MAP_VIEW_OF_FILE};

extern void GetWindowsVersion(char *buf);

void InitDebuggingEx(PKEYMAN64THREADDATA _td) {
}

void InitDebugging() {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

	InitDebuggingEx(_td);
	_td->debug_DebugInit = TRUE;

	if(g_debug_KeymanLog)
	{
		char *buf = new char[1024];

		GetWindowsVersion(buf);
		SendDebugMessage(0, sdmKeyboard, 0, buf);
		delete buf;
	}
}

void UninitDebuggingEx()
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

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
  return g_debug_KeymanLog;
}

int SendDebugMessageFormat_1(HWND hwnd, TSDMState state, int kmn_lineno, char *file, int line, char *fmt, ...)
{
	char fmtbuf[256];

	va_list vars;
	va_start(vars, fmt);
	vsnprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, fmt, vars);  // I2248   // I3547
	fmtbuf[255] = 0;
	SendDebugMessage_1(hwnd, state, kmn_lineno, file, line, fmtbuf);

  return 0;
}

#define TAB "\t"

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


int GetActualShiftState() {   // I4843
  int state = 0;
	if(GetKeyState(VK_NUMLOCK) & 1) state |= NUMLOCKFLAG;
	if(GetKeyState(VK_CAPITAL) & 1) state |= CAPITALFLAG;
	if(GetKeyState(VK_SHIFT) < 0) state |= K_SHIFTFLAG;
	if(GetKeyState(VK_RCONTROL) < 0) state |= RCTRLFLAG;
	if(GetKeyState(VK_LCONTROL) < 0) state |= LCTRLFLAG;
	if(GetKeyState(VK_LMENU) < 0) state |= LALTFLAG;
	if(GetKeyState(VK_RMENU) < 0) state |= RALTFLAG;

  if (GetKeyState(VK_LSHIFT) < 0) state |= 0x10000;
  if (GetKeyState(VK_RSHIFT) < 0) state |= 0x20000;

  return state;
}

int SendDebugMessage_1(HWND hwnd, TSDMState state, int kmn_lineno, char *file, int line, char *msg)
{
  UNREFERENCED_PARAMETER(hwnd);
  UNREFERENCED_PARAMETER(state);   // I3569
  UNREFERENCED_PARAMETER(kmn_lineno);
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return 0;

  if (msg == NULL) {
    return 0;
  }

	if(g_debug_KeymanLog) {
    GUITHREADINFO gti;
    char
      //sClassName[32], sWindowText[32], 
      //sFocusClassName[32], sFocusWindowText[32], 
      //sActiveClassName[32], sActiveWindowText[32], 
      sProcessPath[256], sProcessName[32];

    gti.cbSize = sizeof(gti);
    GetGUIThreadInfo(GetCurrentThreadId(), &gti);

    //FillWindowInfo(hwnd, sClassName, sWindowText);   // I3656
    //FillWindowInfo(gti.hwndActive, sActiveClassName, sActiveWindowText);   // I3656
    //FillWindowInfo(gti.hwndFocus, sFocusClassName, sFocusWindowText);   // I3656

		GetModuleFileName(NULL, sProcessPath, 256);
		_splitpath_s(sProcessPath, NULL, 0, NULL, 0, sProcessName, 32, NULL, 0);
		sProcessName[31] = 0;

		if(strlen(msg) > 256) msg[255] = 0;

    if(g_debug_ToConsole) {   // I3951
      char windowinfo[1024];
      wsprintf(windowinfo,
        DEBUG_PLATFORM_STRING TAB //"Platform" TAB
        "%s" TAB  //"Process" TAB
        "%x" TAB  //"PID" TAB
        "%x" TAB  //"TID" TAB
      //  "%x" TAB  //"QueueStatus" TAB   // I3569
        "%x" TAB  //"ShiftState" TAB
        "%x" TAB  //"ActualShiftState" TAB   // I4843
        "%d" TAB  //"TickCount" TAB
      //  "%x" TAB  //"LogHWND" TAB   // I3569
      //  "%s" TAB  //"LogWindowClassName" TAB   // I3569
      //  "%s" TAB  //"LogWindowText" TAB   // I3569
        "%x" TAB  //"FocusHWND" TAB
      //  "%s" TAB  //"FocusWindowClassName" TAB   // I3569
      //  "%s" TAB  //"FocusWindowText" TAB   // I3569
      //  "%x" TAB  //"ActiveHWND" TAB   // I3569
      //  "%s" TAB  //"ActiveWindowClassName" TAB   // I3569
      //  "%s" TAB  //"ActiveWindowText" TAB   // I3569
      //  "%x" TAB  //"ForegroundHWND" TAB  // I2690   // I3569
      //  "%d" TAB  //"MessageType" TAB   // I3569
      //  "%d" TAB  //"SourceLine" TAB   // I3569
        "%8x" TAB //"ActiveHKL" TAB
        "%s:%d" TAB  //"SourceFile" TAB
        "%s",     //"Message"

        sProcessName,                    //"Process" TAB
        GetCurrentProcessId(),           //"PID" TAB
        GetCurrentThreadId(),            //"TID" TAB
    //    GetQueueStatus(QS_KEY),          //"QueueStatus" TAB   // I3569
        g_shiftState,       //"ShiftState" TAB
        GetActualShiftState(),  // ActualShiftState TAB   // I4843
        GetTickCount(),                  //"TickCount" TAB

    //    hwnd,                            //"LogHWND" TAB   // I3569
    //    sClassName,                      //"LogWindowClassName" TAB   // I3569
    //    sWindowText,                     //"LogWindowText" TAB   // I3569

        gti.hwndFocus,                   //"FocusHWND" TAB
    //    sFocusClassName,                 //"FocusWindowClassName" TAB   // I3569
    //    sFocusWindowText,                //"FocusWindowText" TAB   // I3569

    //    gti.hwndActive,                  //"ActiveHWND" TAB   // I3569
    //    sActiveClassName,                //"ActiveWindowClassName" TAB   // I3569
    //    sActiveWindowText,               //"ActiveWindowText" TAB   // I3569

    //    GetForegroundWindow(),           //"ForegroundHWND" TAB // I2690   // I3569

    //    state,                           //"MessageType" TAB   // I3569
    //    kmn_lineno,                      //"SourceLine" TAB   // I3569
        GetKeyboardLayout(0),            //"ActiveHKL" TAB
        file, line,                      //"SourceFile" TAB
        msg);                            //"Message"
      OutputDebugString(windowinfo);   // I3570   // I3951
      OutputDebugString("\n");   // I3570   // I3951
    }
	}

  return 0;
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

void DebugLastError_1(DWORD err, char *context, char *file, int line, char *func)
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

extern const char *VKeyNames[256];

char *Debug_VirtualKey(WORD vk) {
  __declspec(thread) static char buf[256];
  if (!ShouldDebug(sdmDebug)) {
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
  if (!ShouldDebug(sdmDebug)) {
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