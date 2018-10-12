#include "pch.h"
#include "security.h"

#ifdef USE_KEYEVENTSENDERTHREAD

#define KEYEVENT_WINDOW_CLASS "Keyman_KeyEventConsumerWnd"

#ifdef _WIN64
#define GLOBAL_KEY_EVENT_NAME "KeymanEngine_KeyEvent_x64"
#define GLOBAL_KEY_MUTEX_NAME "KeymanEngine_KeyMutex_x64"
#else
#define GLOBAL_KEY_EVENT_NAME "KeymanEngine_KeyEvent_x86"
#define GLOBAL_KEY_MUTEX_NAME "KeymanEngine_KeyMutex_x86"
#endif

/*
  All input is posted to the key event sender window, which then uses SendInput to post
  final input to the target thread. Because SendInput calls in UWP apps will fail silently
  due to restricted permissions, all SendInput must be done from this thread, which runs
  in the Keyman main process.

  NOTE: Postponing writing architecture technical note because of change to architecture 
  below...

  TODO: For simplicity of proof-of-concept data sharing, we ran two copies of the key event 
  sender thread: one in the 32 bit space, and one in the 64 bit space. This means that we 
  can still have a race condition because we lose serialization guarantees. Input is first
  processed in the Low Level Keyboard Hook which runs in the keyman.exe 32 bit space. This
  then gets forwarded to the target application with the necessary flags on the message to
  tell Keyman to not reprocess it. However, after keystroke processing, the target 
  application fills the shared data structure and signals the key event sender thread in its 
  own bitness space (32 or 64 bit). The key event sender thread then takes the final shared 
  data and sends it to the target app. And that breaks the serialization guarantee because 
  the 64 bit apps are not serialized with the original 32 bit captured input.

  The fix is to redesign the shared data to use a memory mapped file, which can be shared 
  across the 32-64 boundary. Must tweak the permissions on this file, of course.

  TODO: Console apps still not working

*/

//
// Server application functionality
// This runs only in the host applications keyman.exe and keymanx64.exe
//

// TODO: refactor this into the KeyEventConsumer class and provide getters for them
HWND f_hwndKeyEventSender = 0;

class KeyEventConsumer {
private:

  DWORD idThread;
  HANDLE hThread, hThreadExitEvent;
  HANDLE m_hKeyEvent, m_hKeyMutex;
  HWND m_hwnd;

public:
  KeyEventConsumer() {
    if (!InitSharedData()) {
      return;
    }

    hThreadExitEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (!hThreadExitEvent) {
      DebugLastError("CreateEvent");
      return;
    }

    hThread = CreateThread(NULL, 0, KeyEventConsumerThread, (LPVOID)this, 0, &idThread);
    if (!hThread) {
      DebugLastError("CreateThread");
    }
  }

  ~KeyEventConsumer() {
    if (hThreadExitEvent != NULL) {
      if (!SetEvent(hThreadExitEvent)) {
        DebugLastError("SetEvent");
      }
      if (!CloseHandle(hThreadExitEvent)) {
        DebugLastError("CloseHandle(hThreadExitEvent)");
      }
    }
    if (hThread != NULL && !CloseHandle(hThread)) {
      DebugLastError("CloseHandle(hThread)");
    }

    CloseSharedData();
  }

  BOOL InitSharedData() {
    // TODO: Create mmf

    m_hKeyEvent = CreateEvent(NULL, FALSE, FALSE, GLOBAL_KEY_EVENT_NAME);
    if (!m_hKeyEvent) {
      DebugLastError("CreateEvent");
      return FALSE;
    }

    if (!SetObjectToLowIntegrity(m_hKeyEvent)) {
      return FALSE;
    }

    if (!GrantPermissionToAllApplicationPackages(m_hKeyEvent, EVENT_MODIFY_STATE)) {
      return FALSE;
    }

    m_hKeyMutex = CreateMutex(NULL, FALSE, GLOBAL_KEY_MUTEX_NAME);
    if (!m_hKeyMutex) {
      DebugLastError("CreateMutex");
      return FALSE;
    }
    
    if (!SetObjectToLowIntegrity(m_hKeyMutex)) {
      return FALSE;
    }
      
    if(!GrantPermissionToAllApplicationPackages(m_hKeyMutex, MUTEX_ALL_ACCESS)) {
      return FALSE;
    }

    return TRUE;
  }

  BOOL CloseSharedData() {
    // TODO: Close mmf
    BOOL bRet = TRUE;
    if (!CloseHandle(m_hKeyEvent)) {
      DebugLastError("CloseHandle(m_hKeyEvent)");
      bRet = FALSE;
    }
    if (!CloseHandle(m_hKeyMutex)) {
      DebugLastError("CloseHandle(m_hKeyMutex)");
      bRet = FALSE;
    }
    return bRet;
  }

private:
  /**
    Stub callback thread procedure
  */
  static DWORD WINAPI KeyEventConsumerThread(
    _In_ LPVOID lpParameter
  ) {
    return ((KeyEventConsumer *)lpParameter)->ThreadMain();
  }

  /**
    Thread main procedure
  */
  DWORD ThreadMain() {
    if (!InitThread()) {
      return 0;
    }

    MessageLoop();

    CleanupThread();

    return 0;
  }
  
  /**
    Create listener window which serializes input events and
    sends out input to the target focus window
  */
  BOOL InitThread() {
    WNDCLASS wndClass = { 0 };
    wndClass.lpfnWndProc = KeyEventConsumerWndProc;
    wndClass.cbClsExtra = sizeof(this);
    wndClass.lpszClassName = KEYEVENT_WINDOW_CLASS;
    wndClass.hInstance = g_hInstance;
    if (!RegisterClass(&wndClass)) {
      DebugLastError("RegisterClass");
      return FALSE;
    }

    m_hwnd = CreateWindow(KEYEVENT_WINDOW_CLASS, "", WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, 0, 0, g_hInstance, NULL);
    if (m_hwnd == NULL) {
      DebugLastError("CreateWindow");
      return FALSE;
    }

    SetClassLongPtr(m_hwnd, 0, (LONG_PTR)this);

    f_hwndKeyEventSender = m_hwnd;

    return TRUE;
  }

  /**
    Cleanup when thread main finishes
  */
  void CleanupThread() {
    if (!DestroyWindow(m_hwnd)) {
      DebugLastError("DestroyWindow");
    }
    if (!UnregisterClass(KEYEVENT_WINDOW_CLASS, g_hInstance)) {
      DebugLastError("UnregisterClass");
    }
  }

  void MessageLoop() {
    HANDLE events[2] = { hThreadExitEvent, m_hKeyEvent };
    while (TRUE) {
      switch (MsgWaitForMultipleObjectsEx(2, events, INFINITE, QS_ALLINPUT, 0)) {
      case WAIT_OBJECT_0:
        // Thread has been signalled, return
        return;
      case WAIT_OBJECT_0 + 1:
        PostMessage(m_hwnd, WM_USER, 0, 0);
        break;
      case WAIT_OBJECT_0 + 2:
        MSG msg;
        while (PeekMessage(&msg, NULL, NULL, NULL, PM_REMOVE)) {
          DispatchMessage(&msg);
        }
        break;
      default:
        DebugLastError("MsgWaitForMultipleObjectsEx");
        return;
      }
    }
  }

  /**
    Reads input events from the shared buffer and sends them to the
    focused window with the SendInput API.
  */
  BOOL ProcessQueuedKeyEvents() {
    SendDebugMessage(0, sdmDebug, 0, "Processing queued key events");

    HANDLE handles[2] = { hThreadExitEvent, m_hKeyMutex };

    //
    // Wait for access to the shared data (must also watch out for 
    // shutdown event so we don't stall forever here
    //
    switch (WaitForMultipleObjects(2, handles, FALSE, INFINITE)) {
    case WAIT_OBJECT_0:
      // thread exit has been signalled, we are shutting down
      return FALSE;
    case WAIT_OBJECT_0 + 1:
      break;
    default:
      DebugLastError("WaitForMultipleObjects");
      return FALSE;
    }

    //
    // Read the shared data from the buffer and send the input
    //
    DWORD *pn = Globals::nInputBuf();
    INPUT *pi = Globals::InputBuf();
    if (*pn > 0) {
      if (SendInput(*pn, pi, sizeof(INPUT)) == 0) {
        DebugLastError("SendInput");
      }

      *pn = 0;
    }

    //
    // Allow the focused application to generate more events
    //
    if (!ReleaseMutex(m_hKeyMutex)) {
      DebugLastError("ReleaseMutex");
    }

    return TRUE;
  }

  /**
    Stub window proc that calls the consumer wndproc
  */
  static LRESULT CALLBACK KeyEventConsumerWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    KeyEventConsumer *consumer = (KeyEventConsumer *)GetClassLongPtr(hwnd, 0);
    if (consumer == NULL) {
      return DefWindowProc(hwnd, msg, wParam, lParam);
    }
      
    return consumer->WndProc(hwnd, msg, wParam, lParam);
  }

  /**
    Process window messages for the key event sender window
  */
  LRESULT WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    if (msg == WM_USER) {
      ProcessQueuedKeyEvents();
    }

    /*
      Serializes all input events back onto the focused thread by repeating any externally generated events. This
      is required to ensure that we can get the correct modifier state when we need to push a modifier release/set around
      keystroke output (mostly for the purposes of the backspace event).

      We need to release Alt and Ctrl modifiers (and we do Shift as well for completeness) when emitting Backspace to
      modify context in order to avoid triggering Alt+Backspace -> Undo or Ctrl+Backspace -> 0x7F / Word delete left
      instead of a Char delete left event.

      However it is possible that a modifier release event generated by the user is already in the queue at the time we
      send the input. Since we have no way to detect or prevent this happening, what we do instead is re-inject each
      keystroke into the queue in the focused input thread, which means we can guarantee order of events and sort out
      the modifier state as required.  This looks really messy in the message event queue but turns out to be robust
      in practice.

      You can disable this flag with flag_ShouldSerializeInput.
    */

    if (msg == wm_keyman_keyevent && flag_ShouldSerializeInput  /*&& _td->lpActiveKeyboard*/) {
      INPUT input;
      input.type = INPUT_KEYBOARD;
      input.ki.wVk = (WORD)wParam;
      input.ki.wScan = (lParam & 0xFF0000) >> 16;
      input.ki.time = GetMessageTime();
      input.ki.dwExtraInfo = EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT;
      input.ki.dwFlags = lParam & 0xFFFF;

      SendInput(1, &input, sizeof(INPUT));
    }

    return DefWindowProc(hwnd, msg, wParam, lParam);
  }
};

// TODO: find a good name for this class, thread et al, rename file to "<keyeventsender>server.cpp" where <keyeventsender> is the new class name
KeyEventConsumer *consumer = NULL;

void StartupConsumer() {
  consumer = new KeyEventConsumer();
}

void ShutdownConsumer() {
  delete consumer;
}

//
// Client application functionality
//

// TODO: Turn this into a class as well, and move to a "<keyeventsender>client.cpp" (see above)
HANDLE f_hKeyEvent = 0;
HANDLE f_hKeyMutex = 0;

/**
  Provide a copy of the input data to the Key Event Sender thread and signal it
  to send the input back to the client application.
*/
BOOL SignalKeyEventSenderThread(PINPUT pInputs, DWORD nInputs) {
  //
  // Initialisation
  //
  if (f_hKeyEvent == 0) {
    f_hKeyEvent = OpenEvent(EVENT_MODIFY_STATE, FALSE, GLOBAL_KEY_EVENT_NAME);
    if (f_hKeyEvent == 0) {
      DebugLastError("OpenEvent");
      return FALSE;
    }
  }

  if (f_hKeyMutex == 0) {
    f_hKeyMutex = OpenMutex(MUTEX_ALL_ACCESS, FALSE, GLOBAL_KEY_MUTEX_NAME);
    if (f_hKeyMutex == 0) {
      DebugLastError("OpenMutex");
      return FALSE;
    }
  }

  //
  // Check inputs
  //
  if (nInputs > MAX_KEYEVENT_INPUTS) {
    SendDebugMessageFormat(0, sdmGlobal, 0, "Too many INPUT events for queue (%d)", nInputs);
    nInputs = MAX_KEYEVENT_INPUTS;
  }

  //
  // Capture the mutex and copy input buffer into global shared buffer
  //
  switch (WaitForSingleObject(f_hKeyMutex, 500)) {
  case WAIT_OBJECT_0:
    break;
  case WAIT_TIMEOUT:
    SendDebugMessage(0, sdmGlobal, 0, "Timed out waiting to send input to host app");
    return FALSE;
  case WAIT_ABANDONED_0:
    SendDebugMessage(0, sdmGlobal, 0, "Host app closed mutex");
    return FALSE;
  case WAIT_FAILED:
  default:
    DebugLastError("WaitForSingleObject");
    return FALSE;
  }

  memcpy(Globals::InputBuf(), pInputs, nInputs * sizeof(INPUT));
  *Globals::nInputBuf() = nInputs;

  //
  // Force CPU to complete all memory operations before we signal
  // the host
  //
  MemoryBarrier();

  //
  // Release the mutex and signal the host application to process
  // the input
  //
  if (!ReleaseMutex(f_hKeyMutex)) {
    DebugLastError("ReleaseMutex");
    return FALSE;
  }

  if (!SetEvent(f_hKeyEvent)) {
    DebugLastError("SetEvent");
    return FALSE;
  }

  return TRUE;
}

#endif
