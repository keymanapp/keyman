/*
  Name:             K32_DBG
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    23 Feb 2016
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

#include "globals.h"
#include "kmtip.h"
#include <stdio.h>
#include <stdarg.h>

#ifdef _WIN64
#define DEBUG_PLATFORM_STRING "x64"
#else
#define DEBUG_PLATFORM_STRING "x86"
#endif

enum {NO_DEBUG_WINDOWS=100, UNABLE_TO_CREATE_FILE_MAPPING, UNABLE_TO_MAP_VIEW_OF_FILE};

BOOL g_debug_KeymanLog = FALSE, g_debug_ToConsole = FALSE;
HANDLE g_debug_hLogMailSlot = 0, g_debug_hLogEvent = 0;
BOOL g_debug_DebugInit = FALSE;

void InitDebuggingEx() {
	if(g_debug_hLogMailSlot != 0 && g_debug_hLogMailSlot != INVALID_HANDLE_VALUE) {
    CloseHandle(g_debug_hLogMailSlot);
  }

  if(g_debug_hLogEvent) {
    CloseHandle(g_debug_hLogEvent);
  }

  g_debug_hLogMailSlot = 0;
  g_debug_hLogEvent = 0;
  g_debug_KeymanLog = FALSE;

	g_debug_hLogMailSlot = CreateFile("\\\\.\\mailslot\\Tavultesoft_KeymanEngine_Debug", 
			GENERIC_WRITE, 
			FILE_SHARE_READ | FILE_SHARE_WRITE,  // required to write to a mailslot 
			(LPSECURITY_ATTRIBUTES) NULL, 
			OPEN_EXISTING, 
			FILE_FLAG_OVERLAPPED | FILE_ATTRIBUTE_NORMAL, 
			(HANDLE) NULL);

  if(!g_debug_hLogMailSlot || g_debug_hLogMailSlot == INVALID_HANDLE_VALUE) {
    return;
  }

	g_debug_hLogEvent = CreateEvent(NULL, FALSE, FALSE, "Tavultesoft_KeymanEngine_DebugWrite");
  if(g_debug_hLogEvent == 0) {
    CloseHandle(g_debug_hLogMailSlot);
    g_debug_hLogMailSlot = 0;
    return;
  }

  g_debug_KeymanLog = TRUE;
}

inline void TestInitDebug() {
	if(!g_debug_DebugInit) InitDebuggingEx();
	g_debug_DebugInit = TRUE;
}

void InitDebugging() {
	InitDebuggingEx();
	g_debug_DebugInit = TRUE;

	if(g_debug_KeymanLog)	{
		VS_FIXEDFILEINFO *ffi;
		DWORD sz;
		UINT ffilen;
		char *buf = new char[1024], fname[260];

		GetModuleFileName(GetModuleHandle("kmtip.dll"), fname, 260);
		sz = GetFileVersionInfoSize(fname, 0);
		if(sz > 0) {
			buf = new char[sz];
			GetFileVersionInfo(fname, 0, sz, buf);
			VerQueryValue(buf, "\\", (void **) &ffi, &ffilen);

			SendDebugMessageFormat("kmtip version: %d.%d.%d.%d", 
				HIWORD(ffi->dwProductVersionMS), LOWORD(ffi->dwProductVersionMS), 
				HIWORD(ffi->dwProductVersionLS), LOWORD(ffi->dwProductVersionLS));
  		delete buf; // I2157
		} else {
			SendDebugMessage("kmtip version: damaged");
    }
	}
}

void UninitDebuggingEx() {
  if(g_debug_hLogMailSlot) {
    CloseHandle(g_debug_hLogMailSlot);
  }
	g_debug_hLogMailSlot = 0;
	if(g_debug_hLogEvent) {
    CloseHandle(g_debug_hLogEvent);
  }
	g_debug_hLogEvent = 0;
  g_debug_DebugInit = FALSE;
}

BOOL ShouldDebug_1() {
  TestInitDebug();
	return g_debug_KeymanLog;
}

int SendDebugMessageFormat_1(char *fmt, ...) {
	char fmtbuf[256];

	va_list vars;
	va_start(vars, fmt);
	if(vsnprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, fmt, vars) <= 0) {
    strcpy_s(fmtbuf, "fail");  // I2248   // I3547
  }
	fmtbuf[255] = 0;
	SendDebugMessage(fmtbuf);

  return 0;
}

#define TAB "\t"

int SendDebugMessage_1(char *msg) {
	if(!g_debug_KeymanLog) {
    return 0;
  }

	OVERLAPPED ov;
	DWORD cbWritten; 
	char windowinfo[1024];

  GUITHREADINFO gti;
	char sProcessPath[256], sProcessName[32];

  gti.cbSize = sizeof(gti);
  GetGUIThreadInfo(GetCurrentThreadId(), &gti);

	GetModuleFileName(NULL, sProcessPath, 256);
	_splitpath_s(sProcessPath, NULL, 0, NULL, 0, sProcessName, 32, NULL, 0);
	sProcessName[31] = 0;

	if(strlen(msg) > 256) msg[255] = 0;

	wsprintf(windowinfo, 
    DEBUG_PLATFORM_STRING TAB //"Platform" TAB
    "%s" TAB  //"Process" TAB
    "%x" TAB  //"PID" TAB
    "%x" TAB  //"TID" TAB
    " " TAB  //"ShiftState" TAB
    " " TAB  //"ActualShiftState" TAB   // I4843
    "%d" TAB  //"TickCount" TAB
    "%x" TAB  //"FocusHWND" TAB
    "%8x" TAB //"ActiveHKL" TAB
    "%s",     //"Message"
  			
    sProcessName,                    //"Process" TAB
    GetCurrentProcessId(),           //"PID" TAB
    GetCurrentThreadId(),            //"TID" TAB
    //0,                               //"ShiftState" TAB
    GetTickCount(),                  //"TickCount" TAB
    gti.hwndFocus,                   //"FocusHWND" TAB
    GetKeyboardLayout(0),            //"ActiveHKL" TAB
    msg);                            //"Message"

  if(g_debug_ToConsole) {   // I3951
    OutputDebugString(windowinfo);   // I3570   // I3951
    OutputDebugString("\n");   // I3570   // I3951
  }

	if(g_debug_hLogMailSlot != 0) {
		ov.Offset = 0;
		ov.OffsetHigh = 0;
		ov.hEvent = g_debug_hLogEvent;
		if(!WriteFile(g_debug_hLogMailSlot,
			windowinfo, 
			(DWORD) strlen(windowinfo) + 1,  // include terminating null 
			&cbWritten, 
			&ov)) {
      switch(GetLastError()) { // I2445 - Try and reconnect debug pipe on error
      case ERROR_HANDLE_EOF:
      case ERROR_BROKEN_PIPE:
        UninitDebuggingEx();
        InitDebuggingEx();
			  if(WriteFile(g_debug_hLogMailSlot,
				  windowinfo, 
				  (DWORD) strlen(windowinfo) + 1,  // include terminating null 
				  &cbWritten, 
				  &ov)) break;
      default:
        UninitDebuggingEx();
        g_debug_KeymanLog = FALSE;  /// Failed again - we could log to system log I guess
      }
    }
  }

  return 0;
}

void DebugLastError_1(char *file, int line, char *func, char *date) {
  DWORD err = GetLastError();
  if(ShouldDebug()) {
    char msg[256];
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_ARGUMENT_ARRAY, NULL, err, 0, msg, sizeof(msg), NULL);
    SendDebugMessageFormat_1("ERROR %d in %s [%s:%d]: %s", err, func, file, line, date, msg);
  }
}
