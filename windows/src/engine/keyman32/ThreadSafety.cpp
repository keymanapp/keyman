
#include "pch.h"
#include "ThreadSafety.h"

DWORD KeymanThreadSafety::m_ThreadIds[THREADSAFETY_MAXTHREADS] = {0};

void KeymanThreadSafety::setThreadId(WORD threadContext, DWORD threadId) {
  if(threadContext < 1 || threadContext > THREADSAFETY_MAXTHREADS) {
    // Unknown context
    SendDebugMessageFormat("Invalid context %d (tid=%d)", threadContext, threadId);
    return;
  }
  KeymanThreadSafety::m_ThreadIds[threadContext-1] = threadId;
  SendDebugMessageFormat("Thread context %d set to tid %d", threadContext, threadId);
}

void KeymanThreadSafety::assertThread(wchar_t* file, int line, wchar_t* function, WORD threadContext) {
  if(threadContext == TV_ANY) {
    return;
  }

  DWORD const actualTid = GetCurrentThreadId();
  if(threadContext < 1 || threadContext > THREADSAFETY_MAXTHREADS) {
    // Unknown context
    if(ShouldDebug()) {
      SendDebugMessageFormat_1(file, line, function, "KeymanThreadSafety::assertThread: invalid context %d (tid=%d)", threadContext, actualTid);
    }
    return;
  }

  DWORD const expectedTid = KeymanThreadSafety::m_ThreadIds[threadContext - 1];

  if (actualTid == expectedTid) {
    // good outcome
    return;
  }

  // report mismatch
  for(int i = 0; i < THREADSAFETY_MAXTHREADS; i++) {
    if(actualTid == KeymanThreadSafety::m_ThreadIds[i]) {
      if(ShouldDebug()) {
        SendDebugMessageFormat_1(file, line, function, "KeymanThreadSafety::assertThread: actual thread context: %d (tid=%d); expected thread context %d (tid=%d)", i+1, actualTid, threadContext, expectedTid);
      }
      return;
    }
  }

  // not a recognized thread!
  if(ShouldDebug()) {
    SendDebugMessageFormat_1(file, line, function, "KeymanThreadSafety::assertThread: actual thread context: unknown (tid=%d); expected thread context %d (tid=%d)", actualTid, threadContext, expectedTid);
  }
}
