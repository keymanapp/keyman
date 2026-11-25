/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2026-11-24
 *
 * Track and report on thread safety violations in keyman32. Intended for use in
 * threads within the keyman.exe process space.
 */
#pragma once

#define THREADSAFETY_MAXTHREADS           3

#define TV_ANY                            0x0     /// Documents that the function is considered thread-safe (e.g. pure)

#define TV_MAIN                           0x1     /// Main UI thread
#define TV_LLKH                           0x2     /// Low Level Keyboard Hook thread
#define TV_SKES                           0x3     /// Serial Key Event Server thread

/**
 * Track and report on thread safety violations in keyman32. Intended for use in
 * threads within the keyman.exe process space.
 */
class KeymanThreadSafety {
private:
  static DWORD m_ThreadIds[THREADSAFETY_MAXTHREADS];
public:
  static void setThreadId(WORD threadContext, DWORD threadId);
  static void assertThread(wchar_t* file, int line, wchar_t* function, WORD threadContext);
};

/**
 * Verify that a function is called from the expected thread context. Reports
 * violations to the debug log, but does not impact program flow.
 */
#define KeymanThreadSafety_assertThread(context) KeymanThreadSafety::assertThread(__WFILE__, __LINE__, __WFUNCTION__, (context))
