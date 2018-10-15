#include "pch.h"
#include "serialkeyeventserver.h"
#include "security.h"

#ifdef USE_SERIALKEYEVENTSERVER

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

// TODO: refactor this into the SerialKeyEventServer class and provide getters for them
HWND f_hwndKeyEventSender = 0;


class SerialKeyEventServer {

private:
  // Process shared data
  DWORD m_idThread;
  HANDLE m_hThread, m_hThreadExitEvent;

  // Thread Local data
  HANDLE m_hKeyEvent, m_hKeyMutex, m_hMMF;
  HWND m_hwnd;
  PINPUT m_pInputs;
  SerialKeyEventSharedData *m_pSharedData;

  //////////////////////////////////////////////////////
  // Main thread
  //////////////////////////////////////////////////////

public:

  SerialKeyEventServer() {
    // We create the file mapping and global data on the main thread but release it on the 
    // local thread. This ensures that these objects are available for other processes to
    // open even if we haven't completed startup of the local thread.
    if (!InitSharedData()) {
      return;
    }

    m_hThreadExitEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (!m_hThreadExitEvent) {
      DebugLastError("CreateEvent");
      return;
    }

    m_hThread = CreateThread(NULL, 0, ServerThreadProc, (LPVOID)this, 0, &m_idThread);
    if (!m_hThread) {
      DebugLastError("CreateThread");
    }
  }

  ~SerialKeyEventServer() {
    if (m_hThreadExitEvent != NULL) {
      if (!SetEvent(m_hThreadExitEvent)) {
        DebugLastError("SetEvent");
      }

      if (m_hThread != NULL) {
        // Wait for the thread to terminate so we know that we'll not receive
        // additional events after this object is destroyed
        if (WaitForSingleObject(m_hThread, 5000) != WAIT_OBJECT_0) {
          DebugLastError("WaitForSingleObject(m_hThread)");
        }

        if (!CloseHandle(m_hThread)) {
          DebugLastError("CloseHandle(m_hThread)");
        }
      }

      if (!CloseHandle(m_hThreadExitEvent)) {
        DebugLastError("CloseHandle(m_hThreadExitEvent)");
      }
    }
  }

private:

  //////////////////////////////////////////////////////
  // Global shared data management
  //////////////////////////////////////////////////////

  /**
    This function is called by the main thread. We create the file mapping and global data on the main thread but
    release it on the local thread. This ensures that these objects are available for other processes to open
    even if we haven't completed startup of the local thread.
  */
  BOOL InitSharedData() {
    m_hMMF = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE | SEC_COMMIT, 0, sizeof(SerialKeyEventSharedData), GLOBAL_FILE_MAPPING_NAME);
    if (!m_hMMF) {
      DebugLastError("CreateFileMapping");
      return FALSE;
    }

    if (!SetObjectToLowIntegrity(m_hMMF) ||
      !GrantPermissionToAllApplicationPackages(m_hMMF, FILE_MAP_ALL_ACCESS)) {
      return FALSE;
    }

    m_pSharedData = (SerialKeyEventSharedData *)MapViewOfFile(m_hMMF, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(SerialKeyEventSharedData));
    if (!m_pSharedData) {
      DebugLastError("MapViewOfFile");
      return FALSE;
    }

    m_hKeyEvent = CreateEvent(NULL, FALSE, FALSE, GLOBAL_KEY_EVENT_NAME);
    if (!m_hKeyEvent) {
      DebugLastError("CreateEvent");
      return FALSE;
    }

    if (!SetObjectToLowIntegrity(m_hKeyEvent) ||
      !GrantPermissionToAllApplicationPackages(m_hKeyEvent, EVENT_MODIFY_STATE)) {
      return FALSE;
    }

    m_hKeyMutex = CreateMutex(NULL, FALSE, GLOBAL_KEY_MUTEX_NAME);
    if (!m_hKeyMutex) {
      DebugLastError("CreateMutex");
      return FALSE;
    }

    if (!SetObjectToLowIntegrity(m_hKeyMutex) ||
      !GrantPermissionToAllApplicationPackages(m_hKeyMutex, MUTEX_ALL_ACCESS)) {
      return FALSE;
    }

    return TRUE;
  }

  BOOL CloseSharedData() {
    BOOL bRet = TRUE;

    if (m_hKeyMutex != NULL && !CloseHandle(m_hKeyMutex)) {
      DebugLastError("CloseHandle(m_hKeyMutex)");
      bRet = FALSE;
    }

    if (m_hKeyEvent != NULL && !CloseHandle(m_hKeyEvent)) {
      DebugLastError("CloseHandle(m_hKeyEvent)");
      bRet = FALSE;
    }

    if (m_pSharedData != NULL && !UnmapViewOfFile((LPCVOID)m_pSharedData)) {
      DebugLastError("CloseHandle(m_pSharedData)");
      bRet = FALSE;
    }

    if (m_hMMF != NULL && !CloseHandle(m_hMMF)) {
      DebugLastError("CloseHandle(m_hMMF)");
      bRet = FALSE;
    }

    return bRet;
  }

  //////////////////////////////////////////////////////
  // Local thread
  //////////////////////////////////////////////////////

  /**
    Stub callback thread procedure
  */
  static DWORD WINAPI ServerThreadProc(
    _In_ LPVOID lpParameter
  ) {
    return ((SerialKeyEventServer *)lpParameter)->ThreadMain();
  }

  /**
    Thread main procedure
  */
  DWORD ThreadMain() {
    if (!InitThread()) {
      return 1;
    }

    MessageLoop();

    CleanupThread();

    CloseSharedData();

    return 0;
  }

  /**
    Create listener window which serializes input events and
    sends out input to the target focus window, and setup local
    buffers
  */
  BOOL InitThread() {
    m_pInputs = new INPUT[MAX_KEYEVENT_INPUTS];

    WNDCLASS wndClass = { 0 };
    wndClass.lpfnWndProc = ServerWndProc;
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

    if (m_pInputs != NULL) {
      delete m_pInputs;
    }
  }

  /**
    Main message loop for thread. Terminates on error or when
    m_hThreadExitEvent is signaled. Sleeps until either a 
    window message is received or a key event is signaled from
    a client app.
  */
  void MessageLoop() {
    HANDLE events[2] = { m_hThreadExitEvent, m_hKeyEvent };
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

    HANDLE handles[2] = { m_hThreadExitEvent, m_hKeyMutex };

    //
    // Wait for access to the shared data (must also watch out for 
    // shutdown event so we don't stall forever here)
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
    // Copy the shared data from the buffer
    //
    DWORD nInputs = m_pSharedData->nInputs;
    for (DWORD i = 0; i < nInputs; i++) {
      m_pInputs[i].type = INPUT_KEYBOARD;
      m_pInputs[i].ki.wVk = m_pSharedData->inputs[i].wVk;
      m_pInputs[i].ki.wScan = m_pSharedData->inputs[i].wScan;
      m_pInputs[i].ki.dwFlags = m_pSharedData->inputs[i].dwFlags;
      m_pInputs[i].ki.time = m_pSharedData->inputs[i].time;
      m_pInputs[i].ki.dwExtraInfo = (ULONG_PTR) m_pSharedData->inputs[i].extraInfo;
    }

    //
    // Reset the shared buffer and ensure the data is written out of cache for
    // multiprocessor systems
    //
    m_pSharedData->nInputs = 0;
    MemoryBarrier();

    //
    // Release mutex early to allow the focused application to generate more events
    //
    if (!ReleaseMutex(m_hKeyMutex)) {
      DebugLastError("ReleaseMutex");
    }

    //
    // Send the input to the system input queue
    //
    if (SendInput(nInputs, m_pInputs, sizeof(INPUT)) == 0) {
      DebugLastError("SendInput");
    }

    return TRUE;
  }

  /**
    Stub window proc that calls the g_SerialKeyEventServer wndproc
  */
  static LRESULT CALLBACK ServerWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    SerialKeyEventServer *server = (SerialKeyEventServer *)GetClassLongPtr(hwnd, 0);
    if (server == NULL) {
      return DefWindowProc(hwnd, msg, wParam, lParam);
    }
      
    return server->WndProc(hwnd, msg, wParam, lParam);
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

#ifndef _WIN64
SerialKeyEventServer *g_SerialKeyEventServer = NULL;

void StartupSerialKeyEventServer() {
  g_SerialKeyEventServer = new SerialKeyEventServer();
}

void ShutdownSerialKeyEventServer() {
  delete g_SerialKeyEventServer;
}

#else

// We only need one server, on Win32
void StartupSerialKeyEventServer() {
}

void ShutdownSerialKeyEventServer() {
}


#endif // !_WIN64

#endif
