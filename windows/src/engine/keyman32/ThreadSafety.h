#pragma once

#define THREADSAFETY_MAXTHREADS           3

#define TV_ANY                            0x0     /// Documents that the function is considered thread-safe (e.g. pure)

#define TV_MAIN                           0x1     /// Main thread
#define TV_LLKH                           0x2     /// Low Level Keyboard Hook thread
#define TV_SKES                           0x3     /// Serial Key Event Server thread

class KeymanThreadSafety {
private:
  static DWORD m_ThreadIds[THREADSAFETY_MAXTHREADS];
public:
  static void setThreadId(WORD threadContext, DWORD threadId);
  static void assertThread(wchar_t* file, int line, wchar_t* function, WORD threadContext);
};

#define KeymanThreadSafety_assertThread(context) KeymanThreadSafety::assertThread(__WFILE__, __LINE__, __WFUNCTION__, (context))
