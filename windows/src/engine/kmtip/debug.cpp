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

#include "pch.h"
#include "kmtip.h"
#include <stdio.h>
#include <stdarg.h>

#ifdef _WIN64
#define DEBUG_PLATFORM_STRING "x64"
#else
#define DEBUG_PLATFORM_STRING "x86"
#endif


BOOL ShouldDebug() {
  return TRUE;
}

void InitDebugging() {
  if (ShouldDebug()) {
    VS_FIXEDFILEINFO *ffi;
    DWORD sz;
    UINT ffilen;
    char fname[260];

    GetModuleFileName(GetModuleHandle("kmtip.dll"), fname, 260);
    sz = GetFileVersionInfoSize(fname, 0);
    if (sz > 0) {
      char *buf = new char[sz];
      GetFileVersionInfo(fname, 0, sz, buf);
      VerQueryValue(buf, "\\", (void **)&ffi, &ffilen);

      SendDebugMessageFormat(L"kmtip version: %d.%d.%d.%d",
        HIWORD(ffi->dwProductVersionMS), LOWORD(ffi->dwProductVersionMS),
        HIWORD(ffi->dwProductVersionLS), LOWORD(ffi->dwProductVersionLS));
      delete[] buf; // I2157
    }
    else {
      SendDebugMessage(L"kmtip version: damaged");
    }
  }
}

int SendDebugMessageFormat_1(wchar_t *file, int line, wchar_t* function, PWCHAR fmt, ...) {
  if (ShouldDebug()) {
    WCHAR fmtbuf[256];

    va_list vars;
    va_start(vars, fmt);
    if (_vsnwprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, fmt, vars) <= 0) {
      wcscpy_s(fmtbuf, L"fail");  // I2248   // I3547
    }
    fmtbuf[255] = 0;
    SendDebugMessage_1(file, line, function, fmtbuf);
  }
  return 0;
}

int SendDebugMessage_1(wchar_t *file, int line, wchar_t* function, PWCHAR msg) {
  if (ShouldDebug()) {
    Keyman32Interface::WriteDebugEvent(file, line, function, msg);
  }
  return 0;
}

int SendDebugEntry_1(wchar_t* file, int line, wchar_t* function) {
  Keyman32Interface::WriteDebugEntry(file, line, function);
  return 0;
}

int SendDebugExit_1(wchar_t* file, int line, wchar_t* function) {
  Keyman32Interface::WriteDebugExit(file, line, function);
  return 0;
}

void DebugLastError_1(wchar_t *file, int line, wchar_t* function, PWCHAR msg, DWORD err) {
  if(ShouldDebug()) {
    WCHAR buf[256];
    FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_ARGUMENT_ARRAY, NULL, err, 0, buf, _countof(buf), NULL);
    SendDebugMessageFormat_1(file, line, function, L"ERROR %d calling %s: %s", err, msg, buf);
  }
}

void WINAPI Keyman_Diagnostic(int mode) {
  if (mode == 0) {
    RaiseException(0xDEADBEEF, EXCEPTION_NONCONTINUABLE, 0, NULL);
  }
}

BOOL _LogSUCCEEDED(wchar_t *file, int line, wchar_t* function, PSTR callee, HRESULT hr) {
  BOOL result = SUCCEEDED(hr);
  if (!result) {
    SendDebugMessageFormat_1(file, line, function, L"call to %hs failed with %x", callee, hr);
  }
  return result;
}
