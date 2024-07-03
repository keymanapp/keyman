#include "pch.h"
#include "serialkeyeventserver.h"
#include "security.h"
#include "kbd.h"

#ifndef _WIN64

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

class SerialKeyEventServer: public ISerialKeyEventServer {

private:
  // Process shared data
  DWORD m_idThread;
  HANDLE m_hThread, m_hThreadExitEvent;

  // Thread Local data
  BYTE m_ModifierKeyboardState[256];
  HANDLE m_hKeyEvent, m_hKeyMutex, m_hMMF;
  HWND m_hwnd;
  int m_nInputs;
  PINPUT m_pInputs;
  SerialKeyEventSharedData *m_pSharedData;

  //////////////////////////////////////////////////////
  // Main thread
  //////////////////////////////////////////////////////

public:

  SerialKeyEventServer() {
    m_idThread = 0;
    m_hThread = NULL;
    m_hThreadExitEvent = NULL;
    memset(m_ModifierKeyboardState, 0, 256);
    m_hKeyEvent = NULL;
    m_hKeyMutex = NULL;
    m_hMMF = NULL;
    m_hwnd = NULL;
    m_nInputs = 0;
    m_pInputs = NULL;
    m_pSharedData = NULL;

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

  virtual ~SerialKeyEventServer() {
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

    // Normally, this is cleaned up by thread termination, but this
    // handles error conditions better
    CloseSharedData();
  }

  virtual HWND GetWindow() const {
    // At destruction time, m_hwnd may be NULL
    return m_hwnd;
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

    m_hKeyMutex = CreateMutex(NULL, FALSE, GLOBAL_KEY_MUTEX_NAME);
    if (!m_hKeyMutex) {
      DebugLastError("CreateMutex");
      return FALSE;
    }

    if (!SetObjectToLowIntegrity(m_hKeyMutex) ||
      !GrantPermissionToAllApplicationPackages(m_hKeyMutex, MUTEX_ALL_ACCESS)) {
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

    return TRUE;
  }

  BOOL CloseSharedData() {
    BOOL bRet = TRUE;

    if (m_hKeyMutex != NULL && !CloseHandle(m_hKeyMutex)) {
      DebugLastError("CloseHandle(m_hKeyMutex)");
      bRet = FALSE;
    }
    m_hKeyMutex = NULL;

    if (m_hKeyEvent != NULL && !CloseHandle(m_hKeyEvent)) {
      DebugLastError("CloseHandle(m_hKeyEvent)");
      bRet = FALSE;
    }
    m_hKeyEvent = NULL;

    if (m_pSharedData != NULL && !UnmapViewOfFile((LPCVOID)m_pSharedData)) {
      DebugLastError("CloseHandle(m_pSharedData)");
      bRet = FALSE;
    }
    m_pSharedData = NULL;

    if (m_hMMF != NULL && !CloseHandle(m_hMMF)) {
      DebugLastError("CloseHandle(m_hMMF)");
      bRet = FALSE;
    }
    m_hMMF = NULL;

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

    GetKeyboardState(m_ModifierKeyboardState);

    WNDCLASS wndClass = { 0 };
    wndClass.lpfnWndProc = ServerWndProc;
    wndClass.cbClsExtra = sizeof(this);
    wndClass.lpszClassName = KEYEVENT_WINDOW_CLASS;
    wndClass.hInstance = g_hInstance;
    if (!RegisterClass(&wndClass)) {
      DebugLastError("RegisterClass");
      return FALSE;
    }

    m_hwnd = CreateWindow(KEYEVENT_WINDOW_CLASS, "", 0, 0, 0, 0, 0, HWND_MESSAGE, 0, g_hInstance, NULL);
    if (m_hwnd == NULL) {
      DebugLastError("CreateWindow");
      return FALSE;
    }

    SetClassLongPtr(m_hwnd, 0, (LONG_PTR)this);

    return TRUE;
  }

  /**
    Cleanup when thread main finishes
  */
  void CleanupThread() {
    // Slightly naive way of locking out m_hwnd use
    HWND hwnd = m_hwnd;
    m_hwnd = NULL;
    MemoryBarrier();

    if (!DestroyWindow(hwnd)) {
      DebugLastError("DestroyWindow");
    }

    if (!UnregisterClass(KEYEVENT_WINDOW_CLASS, g_hInstance)) {
      DebugLastError("UnregisterClass");
    }

    if (m_pInputs != NULL) {
      delete[] m_pInputs;
      m_pInputs = NULL;
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
      case WAIT_OBJECT_0: // m_hThreadExitEvent signaled
        // Thread has been signalled, return
        return;
      case WAIT_OBJECT_0 + 1: // m_hKeyEvent signaled
        PostMessage(m_hwnd, WM_USER, 0, 0);
        break;
      case WAIT_OBJECT_0 + 2: // Windows message received
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
    SendDebugMessage("Processing queued key events");

    HANDLE handles[2] = { m_hThreadExitEvent, m_hKeyMutex };

    //
    // Wait for access to the shared data (must also watch out for
    // shutdown event so we don't stall forever here)
    //
    switch (WaitForMultipleObjects(2, handles, FALSE, INFINITE)) {
    case WAIT_OBJECT_0: // m_hThreadExitEvent signaled
      // thread exit has been signalled, we are shutting down
      return FALSE;
    case WAIT_OBJECT_0 + 1: // m_hKeyMutex ownership granted
      break;
    default:
      DebugLastError("WaitForMultipleObjects");
      return FALSE;
    }

    //
    // Copy the shared data from the buffer
    //
    PrepareInjectedInput();

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
    if (SendInput(m_nInputs, m_pInputs, sizeof(INPUT)) == 0) {
      DebugLastError("SendInput");
    }
    m_nInputs = 0;

    return TRUE;
  }

  /**
    Add modifier state adjustment events and then copy the new input
    events from the shared buffer
  */
  void PrepareInjectedInput() {
    DWORD nInputs = min(m_pSharedData->nInputs, MAX_KEYEVENT_INPUTS);

    m_nInputs = 0;
    keybd_shift(m_pInputs, &m_nInputs, FALSE, m_ModifierKeyboardState);

    for (DWORD i = 0; i < nInputs && m_nInputs < MAX_KEYEVENT_INPUTS - MAX_KEYEVENT_INPUTS_MODIFIERS; i++, m_nInputs++) {
      m_pInputs[m_nInputs].type = INPUT_KEYBOARD;
      m_pInputs[m_nInputs].ki.wVk = m_pSharedData->inputs[i].wVk;
      m_pInputs[m_nInputs].ki.wScan = m_pSharedData->inputs[i].wScan;
      m_pInputs[m_nInputs].ki.dwFlags = m_pSharedData->inputs[i].dwFlags;
      m_pInputs[m_nInputs].ki.time = m_pSharedData->inputs[i].time;
      m_pInputs[m_nInputs].ki.dwExtraInfo = (ULONG_PTR)m_pSharedData->inputs[i].extraInfo;
    }

    keybd_shift(m_pInputs, &m_nInputs, TRUE, m_ModifierKeyboardState);
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

    if ((msg == WM_KEYMAN_KEY_EVENT || msg == WM_KEYMAN_MODIFIER_EVENT) && flag_ShouldSerializeInput  /*&& _td->lpActiveKeyboard*/) {

      SendDebugMessageFormat("hwnd=%x msg=%s wParam=%x lParam=%x m_ModifierKeyboardState=[LS:%x LC:%x LA:%x RS:%x RC:%x RA:%x]",
        hwnd, msg == WM_KEYMAN_KEY_EVENT ? "WM_KEYMAN_KEY_EVENT" : "WM_KEYMAN_MODIFIER_EVENT", wParam, lParam,
        m_ModifierKeyboardState[VK_LSHIFT], m_ModifierKeyboardState[VK_LCONTROL], m_ModifierKeyboardState[VK_LMENU],
        m_ModifierKeyboardState[VK_RSHIFT], m_ModifierKeyboardState[VK_RCONTROL], m_ModifierKeyboardState[VK_RMENU]);

      if (wParam == VK_RMENU && (lParam & (KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP)) == (KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP) && GetKeyState(VK_LCONTROL) < 0) {
        /*
          When Windows has a European layout that uses AltGr installed, it can emit an additional LCtrl down via software
          when RAlt is pressed. However, the corresponding LCtrl up is never received, seemingly because when Keyman
          re-emits the LCtrl+RAlt, there are subtle differences in the event flags which we cannot duplicate -- specifically
          the flag that emits WM_SYSKEYDOWN for the VK_LCONTROL, even though it is received before the VK_RALT event. It
          appears that Windows figures this out by giving this VK_LCONTROL the scan code 0x21D instead of 0x1D. But we
          are unable to emit that scan code: Windows truncates the scan code sent through SendInput so that we can only
          send 0x1D.

          So we simulate the release of the Left Control key ourselves when the release of the Right Alt is received,
          using VK_CONTROL and scan 0x1D, and hope for the best.

          The full Windows sequence is:

            WM_SYSKEYDOWN VK_CONTROL 0x21D
            WM_SYSKEYDOWN VK_MENU 0x38 EXTENDED_BIT
            ...
            WM_KEYUP VK_CONTROL 0x21D
            WM_KEYUP VK_MENU 0x38 EXTENDED_BIT

          The best Keyman can do is:

            WM_KEYDOWN VK_CONTROL 0x1D
            WM_KEYDOWN VK_MENU 0x38 EXTENDED_BIT
            ...
            WM_KEYUP VK_CONTROL 0x1D
            WM_KEYUP VK_MENU 0x38 EXTENDED_BIT

          There is a possibility that some apps may try and sniff that 0x21D scan code and get confused because Keyman
          doesn't emit it. Hopefully this is rare.
        */
        INPUT input[2];
        input[0].type = INPUT_KEYBOARD;
        input[0].ki.wVk = VK_CONTROL;
        input[0].ki.wScan = 0x21D; // Yeah, Windows chops this to 0x1D. Such is life.
        input[0].ki.time = GetMessageTime();
        input[0].ki.dwExtraInfo = EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT;
        input[0].ki.dwFlags = KEYEVENTF_KEYUP;

        input[1].type = INPUT_KEYBOARD;
        input[1].ki.wVk = (WORD)wParam;
        input[1].ki.wScan = (lParam & 0xFFF0000) >> 16;
        input[1].ki.time = GetMessageTime();
        input[1].ki.dwExtraInfo = EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT;
        input[1].ki.dwFlags = lParam & 0xFFFF;

        if (msg == WM_KEYMAN_KEY_EVENT) {
          // We track changes to modifiers with WM_KEYMAN_MODIFIER_EVENT, but only ever
          // pass them on to the app when we receive them with the WM_KEYMAN_KEY_EVENT
          // message.
          if (!SendInput(2, input, sizeof(INPUT))) {
            DebugLastError("SendInput");
          }
        }


        UpdateLocalModifierState(
          (BYTE)input[0].ki.wVk,
          input[0].ki.dwFlags & KEYEVENTF_EXTENDEDKEY ? TRUE : FALSE,
          (BYTE)input[0].ki.wScan,
          input[0].ki.dwFlags & KEYEVENTF_KEYUP ? TRUE : FALSE);

        UpdateLocalModifierState(
          (BYTE)input[1].ki.wVk,
          input[1].ki.dwFlags & KEYEVENTF_EXTENDEDKEY ? TRUE : FALSE,
          (BYTE)input[1].ki.wScan,
          input[1].ki.dwFlags & KEYEVENTF_KEYUP ? TRUE : FALSE);
      }
      else {
        INPUT input;
        input.type = INPUT_KEYBOARD;
        input.ki.wVk = (WORD)wParam;
        input.ki.wScan = (lParam & 0xFFF0000) >> 16;
        input.ki.time = GetMessageTime();
        input.ki.dwExtraInfo = EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT;
        input.ki.dwFlags = lParam & 0xFFFF;

        if (msg == WM_KEYMAN_KEY_EVENT){
          if (!SendInput(1, &input, sizeof(INPUT))) {
            DebugLastError("SendInput");
          }
        }

        UpdateLocalModifierState(
          (BYTE)input.ki.wVk,
          input.ki.dwFlags & KEYEVENTF_EXTENDEDKEY ? TRUE : FALSE,
          (BYTE)input.ki.wScan,
          input.ki.dwFlags & KEYEVENTF_KEYUP ? TRUE : FALSE);
      }

    }

    return DefWindowProc(hwnd, msg, wParam, lParam);
  }

  /**
   When a physical key event is received by the serializer, we know that this will
   reflect the key state that the app sees at the time that the input is sent.
   We maintain a local modifier state here rather than using GetKeyState because that
   ensures that we are keeping the keyboard state consistent with our version of
   reality.
  */
  void UpdateLocalModifierState(BYTE bVk, BOOL fIsExtendedKey, BYTE bScan, BOOL fIsUp) {
    switch (bVk) {
    case VK_CONTROL:
      // Left and right control are distinguished by a 0xE0 prefix byte
      bVk = fIsExtendedKey ? VK_RCONTROL : VK_LCONTROL;
      break;
    case VK_MENU:
      // Left and right alt are distinguished by a 0xE0 prefix byte
      bVk = fIsExtendedKey ? VK_RMENU : VK_LMENU;
      break;
    case VK_SHIFT:
      // Left and right shift are distinguished by scan code alone
      bVk = bScan == SCANCODE_RSHIFT ? VK_RSHIFT : VK_LSHIFT;
      break;
    case VK_LCONTROL:
    case VK_RCONTROL:
    case VK_LSHIFT:
    case VK_RSHIFT:
    case VK_LMENU:
    case VK_RMENU:
      // These are technically not needed but perhaps some app will send them through SendInput
      // and we'll have to deal with them?
      break;
    default:
      return;
    }

    m_ModifierKeyboardState[bVk] = fIsUp ? 0 : 0x80;
  }
};

ISerialKeyEventServer *ISerialKeyEventServer::sm_server = NULL;

void ISerialKeyEventServer::Startup() {
  ISerialKeyEventServer::sm_server = new SerialKeyEventServer();
}

void ISerialKeyEventServer::Shutdown() {
  delete ISerialKeyEventServer::sm_server;
}

#endif // !_WIN64
