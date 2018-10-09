#include "keyman64.h"

#ifdef USE_KEYEVENTSENDERTHREAD

DWORD WINAPI KeyEventConsumerThread(
  _In_ LPVOID lpParameter
);

extern HINSTANCE g_hInstance;
extern HANDLE f_hKeyEvent;

LRESULT CALLBACK KeyEventConsumerWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
  return DefWindowProc(hwnd, msg, wParam, lParam);
}

class KeyEventConsumer {
  DWORD idThread;
  HANDLE hThread, hThreadExitEvent;

public:
  KeyEventConsumer() {
    hThreadExitEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (!hThreadExitEvent) {
      DebugLastError("CreateEvent");
    }
    hThread = CreateThread(NULL, 0, KeyEventConsumerThread, (LPVOID)this, 0, &idThread);
    if (!hThread) {
      DebugLastError("CreateThread");
    }
  }

  ~KeyEventConsumer() {
    SetEvent(hThreadExitEvent);
    CloseHandle(hThreadExitEvent);
    CloseHandle(hThread);
  }

public:
  DWORD Execute() {
    WNDCLASS wndClass = { 0 };
    wndClass.lpfnWndProc = KeyEventConsumerWndProc;
    wndClass.lpszClassName = "Keyman_KeyEventConsumerWnd";
    wndClass.hInstance = g_hInstance;
    if (!RegisterClass(&wndClass)) {
      DebugLastError("RegisterClass");
      return 0;
    }

    HWND hwnd = CreateWindow("Keyman_KeyEventConsumerWnd", "", WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, 0, 0, g_hInstance, NULL);
    if (hwnd == NULL) {
      DebugLastError("CreateWindow");
      return 0;
    }

    MessageLoop();

    DestroyWindow(hwnd);
    UnregisterClassW(L"Keyman_KeyEventConsumerWnd", g_hInstance);
    return 0;
  }

private:
  void MessageLoop() {
    HANDLE events[2] = { f_hKeyEvent, hThreadExitEvent };
    while (TRUE) {
      switch (MsgWaitForMultipleObjectsEx(2, events, INFINITE, QS_ALLINPUT, 0)) {
      case WAIT_OBJECT_0:
        ProcessQueuedKeyEvents();
        break;
      case WAIT_OBJECT_0 + 1:
        return;
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

  void ProcessQueuedKeyEvents() {
    // Read from the circular buffer and then return
    SendDebugMessage(0, sdmDebug, 0, "Processing queued key events");
    // POC: don't use circular buffer. Just last bunch of events from the buffer
    DWORD *pn = Globals::nInputBuf();
    DWORD n = *pn;
    *pn = 0;
    INPUT *pi = Globals::InputBuf();
    if (SendInput(n, pi, sizeof(INPUT)) == 0) {
      DebugLastError("SendInput");
    }
  }
};

DWORD WINAPI KeyEventConsumerThread(
  _In_ LPVOID lpParameter
) {
  return ((KeyEventConsumer *)lpParameter)->Execute();
}

KeyEventConsumer *consumer = NULL;

void StartupConsumer() {
  consumer = new KeyEventConsumer();
}

void ShutdownConsumer() {
  delete consumer;
}

#endif
