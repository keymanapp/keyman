/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2026-11-24
 *
 * Track and report on thread safety violations in keyman32. Intended for use in
 * threads within the keyman.exe process space.
 */

#include "pch.h"
#include "ThreadSafety.h"

DWORD KeymanThreadSafety::m_ThreadIds[THREADSAFETY_MAXTHREADS] = {0};

/**
 * Set the Thread ID for a given thread context; call during thread startup
 * or main thread initialization
 *
 * @param threadContext  One of the available TV_ thread context values from ThreadSafety.h
 * @param threadId       The Windows thread id
 *
 * Note: TV_ANY is not a valid thread context value
 */
void KeymanThreadSafety::setThreadId(WORD threadContext, DWORD threadId) {
  if(threadContext < 1 || threadContext > THREADSAFETY_MAXTHREADS) {
    // Unknown context
    SendDebugMessageFormat("Invalid context %d (tid=%d)", threadContext, threadId);
    return;
  }
  KeymanThreadSafety::m_ThreadIds[threadContext-1] = threadId;
  SendDebugMessageFormat("Thread context %d set to tid %d", threadContext, threadId);
}

/**
 * Verify that a function is called in the expected thread context. If it fails,
 * reports a message to the debug log. In future, may report violations to
 * Sentry.
 *
 * Do not call this function directly; instead use the
 * `KeymanThreadSafety_assertThread` macro, which wraps the `file`, `line`, and
 * `function` parameters.
 *
 * @param file            Filename of caller
 * @param line            Line number of caller
 * @param function        Name of calling function
 * @param threadContext   One of the expected thread context TV_ values from
 *                        ThreadSafety.h
 *
 * Note: use `TV_ANY` to say that the function can be called from any thread,
 * as documentation.
 */
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
