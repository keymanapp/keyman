#include "pch.h"
#include "serialkeyeventserver.h"
#include "security.h"
#include "kbd.h"

// This file is used only in keyman32.dll; implements the serial key event server
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
// This runs only in the host applications keyman.exe, keymanhp.x64.exe and keymanhp.arm64.exe
//

// TODO: refactor this into the SerialKeyEventServer class and provide getters for them

/**
  #8064 FR-002 / FR-006. The production end of the PMODIFIERDIAGNOSTIC seam: turn a
  ModifierDiagnosticCode into a debug line. The batch reports a code and a VK, never a string, so
  the wording below can be rewritten by anyone without touching what the suite asserts -- which is
  the whole reason the seam is typed. The suite binds a recorder instead of this, because
  SendDebugMessageFormat resolves to ETW (K32_DBG.CPP) and nothing in-process can read it back.

  A file-local free function rather than a member: PMODIFIERDIAGNOSTIC is a plain function pointer,
  for the same reason PGETASYNCKEYSTATE is (gmock is not linked into the test project).
*/
/*
  #8064 FR-104. WTSRegisterSessionNotification lives in wtsapi32.dll, and it is bound at RUNTIME
  rather than linked.

  keyman32.dll is loaded into every process that takes keyboard input, so a new static import is a
  new DLL every one of those processes must load, whether or not it ever reaches this code path. That
  is a real cost for one notification. Binding it here keeps the import table as it was, and it makes
  the absent case explicit instead of fatal: if either export cannot be found the signal is simply
  poisoned and stays that way for keys it does not re-observe, which is exactly the degradation
  FR-104 already specifies for "the signal cannot report".

  NOTIFY_FOR_THIS_SESSION is 0 -- defined here rather than pulled in with wtsapi32.h, which would
  reintroduce the header dependency this avoids.
*/
typedef BOOL (WINAPI *PWTSREGISTERSESSIONNOTIFICATION)(HWND hWnd, DWORD dwFlags);
typedef BOOL (WINAPI *PWTSUNREGISTERSESSIONNOTIFICATION)(HWND hWnd);

#define KM_NOTIFY_FOR_THIS_SESSION 0

static HMODULE g_hWtsApi                                        = NULL;
static PWTSREGISTERSESSIONNOTIFICATION g_pWtsRegisterSession     = NULL;
static PWTSUNREGISTERSESSIONNOTIFICATION g_pWtsUnregisterSession = NULL;

// Returns TRUE when both entry points are available. Idempotent; the module is released in
// CleanupThread.
static BOOL
BindSessionNotificationApi() {
  if (g_pWtsRegisterSession != NULL && g_pWtsUnregisterSession != NULL) {
    return TRUE;
  }

  if (g_hWtsApi == NULL) {
    g_hWtsApi = LoadLibraryW(L"wtsapi32.dll");
    if (g_hWtsApi == NULL) {
      return FALSE;
    }
  }

  g_pWtsRegisterSession =
    (PWTSREGISTERSESSIONNOTIFICATION)GetProcAddress(g_hWtsApi, "WTSRegisterSessionNotification");
  g_pWtsUnregisterSession =
    (PWTSUNREGISTERSESSIONNOTIFICATION)GetProcAddress(g_hWtsApi, "WTSUnRegisterSessionNotification");

  return g_pWtsRegisterSession != NULL && g_pWtsUnregisterSession != NULL;
}

static void
ReportModifierDiagnostic(ModifierDiagnosticCode code, BYTE vk) {
  switch (code) {
  case ReleasedWithoutCacheClaim:
    // The hold this batch could not keep. FR-001 accepts the drop; FR-002 is why it is not silent.
    SendDebugMessageFormat(
      "#8064 dropped hold: released vkey=%s that the OS reports held and the cache does not claim, "
      "so the restore half will not press it back. Expect the user to report this modifier dead "
      "until they press it again",
      Debug_VirtualKey(vk));
    break;
  case PossibleDesktopSwitch:
    // vk is 0 here by contract: the condition is a property of the batch. The keys involved are
    // named by ReconcileModifierCache's own clearing lines, which follow immediately.
    UNREFERENCED_PARAMETER(vk);
    SendDebugMessageFormat(
      "#8064 possible desktop switch: every managed modifier reads up live while the cache claimed "
      "two or more held. The reconcile is clearing them and the restore half will press nothing, so "
      "those holds are lost. The 'clearing vkey=' lines that follow name them");
    break;
  default:
    SendDebugMessageFormat("#8064 unknown modifier diagnostic code=%d vkey=%s", (int)code, Debug_VirtualKey(vk));
    break;
  }
}

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

  // #8064 FR-015b. One entry per bit of the restore mask, giving the m_pInputs index of that
  // modifier's restore KEYDOWN, or -1. Filled by PrepareInjectedInput, read after SendInput returns
  // so a short send can be reconciled exactly rather than conservatively.
  int m_restoreEventIndex[KEYMAN_MODIFIER_VK_COUNT];

  // #8064 W5. The user-held signal: what a NON-KEYMAN source last said about each managed modifier,
  // fed from WM_INPUT. See UserHeldModifierSignal in serialkeyeventcommon.h. It starts fully
  // poisoned, so before the feed is established the restore half falls back to the cache alone
  // (FR-104) -- an unfed signal must never look like "nothing is held", because "nothing is held"
  // is an assertion and this has none to make yet.
  UserHeldModifierSignal m_userHeld;

  // #8064 FR-104b. RegisterRawInputDevices is per-process LAST-WRITER-WINS for a usage page, so a
  // later registration anywhere in keyman.exe silently redirects this feed with no error surfaced.
  // TRUE once we have registered; checked against GetRegisteredRawInputDevices on the loop's
  // existing wake, and every key is poisoned on a mismatch.
  BOOL m_rawInputRegistered;
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
    for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
      m_restoreEventIndex[i] = -1;
    }

    // #8064 W5 / FR-104: born unknown. held is zero and every managed modifier is poisoned, so the
    // signal contributes nothing until it has actually observed something.
    memset(&m_userHeld, 0, sizeof(m_userHeld));
    PoisonAllUserHeldKeys(&m_userHeld);
    m_rawInputRegistered = FALSE;

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

    // This thread has no input queue yet, so GetKeyboardState looks like it should return nothing;
    // it returns live state. See FreshThreadKeyboardStateReflectsLiveModifiers. A modifier
    // held at launch is captured here and goes stale if released before the hook feed starts.
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

    // #8064 W5 / FR-100. Register for raw keyboard input against THE EXISTING message-only window,
    // on THIS thread. Not a new window and not a new thread: the W0 probe measured that WM_INPUT
    // reaches a message-only window on a worker thread even while the main thread is stalled, which
    // is the whole reason this route carried -- the stall is the failure window the signal has to
    // survive.
    //
    // RIDEV_INPUTSINK, so input arrives whether or not this window has focus. It never has focus.
    RAWINPUTDEVICE rid;
    rid.usUsagePage = 0x01; // generic desktop controls
    rid.usUsage     = 0x06; // keyboard
    rid.dwFlags     = RIDEV_INPUTSINK;
    rid.hwndTarget  = m_hwnd;

    if (!RegisterRawInputDevices(&rid, 1, sizeof(rid))) {
      // Not fatal, and deliberately so: the serializer's job does not depend on the signal. Every
      // key stays poisoned, so the restore half falls back to the cache alone and behaves exactly
      // as it did before US0. Degrading is the safe direction; failing to start is not.
      DebugLastError("RegisterRawInputDevices");
      SendDebugMessageFormat(
        "#8064 raw keyboard registration failed; the user-held signal stays unavailable and the "
        "restore half will use the cache alone");
    } else {
      m_rawInputRegistered = TRUE;
    }

    // #8064 FR-104. Console session changes -- fast user switching, RDP connect and disconnect, lock
    // and unlock. On any of them the signal has no standing to speak about any key until it observes
    // one again.
    if (!BindSessionNotificationApi() || !g_pWtsRegisterSession(m_hwnd, KM_NOTIFY_FOR_THIS_SESSION)) {
      // Also not fatal. Without it a session change goes unnoticed, so say so rather than pretend.
      DebugLastError("WTSRegisterSessionNotification");
      SendDebugMessageFormat(
        "#8064 session notifications unavailable; a session change will not poison the user-held "
        "signal, so it is poisoned now and stays that way for keys it does not re-observe");
      PoisonAllUserHeldKeys(&m_userHeld);
    }

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

    // #8064 W5. Raw input registration is torn down with the window; the session notification is
    // not, so it is unregistered explicitly.
    if (m_hwnd != NULL && g_pWtsUnregisterSession != NULL) {
      g_pWtsUnregisterSession(m_hwnd);
    }

    if (g_hWtsApi != NULL) {
      FreeLibrary(g_hWtsApi);
      g_hWtsApi               = NULL;
      g_pWtsRegisterSession   = NULL;
      g_pWtsUnregisterSession = NULL;
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
        // #8064 FR-104b. On the loop's EXISTING wake, not on a new timer: check that our raw-input
        // registration is still ours. A timer would be a second reason to wake this thread, and the
        // check has nothing to do with elapsed time -- it only matters when something happened, and
        // something happening is what woke us.
        CheckRawInputRegistrationStillOurs();

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
    #8064 FR-104. Is the active input desktop the one this thread is attached to?

    If it is not -- the UAC secure desktop, the lock screen, a switched-to desktop -- then this
    process is not receiving the user's input at all, so whatever the signal last observed is stale
    and it must say so. There is no notification for entering the secure desktop, which is why this
    is checked per batch rather than waited on.

    A failure to open the desktop is itself the answer: no access means it is not ours.
  */
  BOOL ActiveDesktopIsTheUsers() {
    HDESK hInput = OpenInputDesktop(0, FALSE, DESKTOP_READOBJECTS);
    if (hInput == NULL) {
      return FALSE;
    }

    HDESK hThread   = GetThreadDesktop(GetCurrentThreadId());
    BOOL sameDesktop = FALSE;

    if (hThread != NULL) {
      WCHAR inputName[256]  = { 0 };
      WCHAR threadName[256] = { 0 };
      DWORD needed          = 0;

      if (GetUserObjectInformationW(hInput, UOI_NAME, inputName, sizeof(inputName), &needed) &&
          GetUserObjectInformationW(hThread, UOI_NAME, threadName, sizeof(threadName), &needed)) {
        sameDesktop = (wcscmp(inputName, threadName) == 0);
      }
    }

    CloseDesktop(hInput);
    return sameDesktop;
  }

  /**
    #8064 FR-104b. Is the raw keyboard feed still pointed at our window?

    RegisterRawInputDevices is per-process last-writer-wins for a usage page. Another component
    inside keyman.exe registering page 1 / usage 6 silently redirects this feed, and NOTHING is
    surfaced: no error, no message, no callback. The failure is that the signal quietly stops being
    updated while continuing to report its last observations -- a stale shadow, which is the sharpest
    risk in this whole design because it manufactures an unmatched press in the one state
    ReconcileModifierCache is structurally blind to.

    So it is detected rather than assumed away, by reading the registration back. On a mismatch every
    key is poisoned: the signal has no standing to speak about any of them, and the restore half
    falls back to the cache alone until each key is observed again.

    Cheap enough for the loop's wake: two calls and a small stack array, only while registered.
  */
  void CheckRawInputRegistrationStillOurs() {
    if (!m_rawInputRegistered || m_hwnd == NULL) {
      return;
    }

    UINT count = 0;
    if (GetRegisteredRawInputDevices(NULL, &count, sizeof(RAWINPUTDEVICE)) == (UINT)-1 && count == 0) {
      // Cannot tell. Unknown is the honest answer, and unknown is what the signal is for.
      PoisonAllUserHeldKeys(&m_userHeld);
      return;
    }

    // A handful of devices at most in practice; a cap keeps this off the heap on the input path.
    const UINT kMaxDevices = 32;
    RAWINPUTDEVICE devices[kMaxDevices];
    if (count > kMaxDevices) {
      count = kMaxDevices;
    }

    const UINT got = GetRegisteredRawInputDevices(devices, &count, sizeof(RAWINPUTDEVICE));
    if (got == (UINT)-1) {
      PoisonAllUserHeldKeys(&m_userHeld);
      return;
    }

    BOOL stillOurs = FALSE;
    for (UINT i = 0; i < got; i++) {
      if (devices[i].usUsagePage == 0x01 && devices[i].usUsage == 0x06 && devices[i].hwndTarget == m_hwnd) {
        stillOurs = TRUE;
        break;
      }
    }

    if (!stillOurs) {
      SendDebugMessageFormat(
        "#8064 raw keyboard registration has been displaced -- page 1 usage 6 no longer targets this "
        "window. Poisoning every key: the user-held signal cannot be trusted until each is observed "
        "again");
      PoisonAllUserHeldKeys(&m_userHeld);
      m_rawInputRegistered = FALSE; // do not keep re-reporting the same displacement every wake
    }
  }

  /**
    #8064 W5 / FR-100a. Feeds one WM_INPUT keyboard event into the user-held signal.

    The policy lives in UpdateUserHeldFromRawKeyboard (keybd_shift.cpp, so the suite can reach it);
    this only unpacks the message. In particular the "is it ours" decision is NOT made here and is
    NOT made on hDevice: the discriminator is "not Keyman's own", and RDP and the OSK deliver genuine
    user input as OS-injected events.
  */
  void ProcessRawInput(HRAWINPUT hRawInput) {
    RAWINPUT raw;
    UINT size = sizeof(raw);

    if (GetRawInputData(hRawInput, RID_INPUT, &raw, &size, sizeof(RAWINPUTHEADER)) == (UINT)-1) {
      DebugLastError("GetRawInputData");
      return;
    }

    if (raw.header.dwType != RIM_TYPEKEYBOARD) {
      return;
    }

    UpdateUserHeldFromRawKeyboard(
      &m_userHeld, raw.data.keyboard.VKey, raw.data.keyboard.MakeCode, raw.data.keyboard.Flags,
      raw.data.keyboard.ExtraInformation);
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
    DWORD restorePressedMask = PrepareInjectedInput();

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
    // #8064 FR-015b. `!= m_nInputs`, not `== 0`. The old check's own excuse -- "not a latch source:
    // the restore KEYDOWNs are last, so truncation drops presses, never releases" -- is true about
    // latching and false about the mask. The restore presses being last is exactly why a short send
    // drops THEM, and restorePressedMask would then name presses the OS never received. The
    // verification pass corrects on cache-up-and-live-down; for a press that was never sent, live IS
    // down, so it would release a modifier on the strength of an event that does not exist.
    //
    // Reported with the counts, because "SendInput failed" and "SendInput sent 251 of 256" need
    // different responses and GetLastError does not distinguish them.
    const UINT sent = SendInput(m_nInputs, m_pInputs, sizeof(INPUT));
    if (sent != (UINT)m_nInputs) {
      DebugLastError("SendInput");
      SendDebugMessageFormat("#8064 short send: SendInput accepted %u of %d events", sent, m_nInputs);

      // EXACTLY the bits whose press did not go out, and not one more. Clearing the whole mask on
      // any short send would suppress the correction for the presses that DID land -- trading a
      // second dropped hold for the first, which is not a fix. m_restoreEventIndex says where each
      // press was, so the boundary is decidable rather than guessed.
      for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
        if ((restorePressedMask & (1u << i)) && m_restoreEventIndex[i] >= (int)sent) {
          SendDebugMessageFormat(
            "#8064 short send: restore press at index %d was not delivered, dropping it from the "
            "verification mask",
            m_restoreEventIndex[i]);
          restorePressedMask &= ~(1u << i);
        }
      }
    }
    m_nInputs = 0;

    // #8064 Schedule the post-batch verification pass; WM_KEYMAN_VERIFY_MODIFIER_EVENT says why it
    // must be a self-post. Skipped with the feed off, where m_ModifierKeyboardState is stale by
    // construction and "the cache says up" means nothing.
    if (flag_ShouldSerializeInput && restorePressedMask != 0) {
      PostMessage(m_hwnd, WM_KEYMAN_VERIFY_MODIFIER_EVENT, (WPARAM)restorePressedMask, 0);
    }

    return TRUE;
  }

  /**
    Add modifier state adjustment events and then copy the new input events from the shared
    buffer. Returns the bitmask of managed modifiers this batch's restore half pressed (see
    PrepareInjectedInputBatch's pRestorePressedMask), so the caller can decide whether the
    post-batch verification pass (#8064) is needed.
  */
  DWORD PrepareInjectedInput() {
    // In keybd_shift.cpp so the gtest project can reach it; this file is #ifndef _WIN64 and this is
    // a private member, so nothing here is testable. See #8064.
    DWORD restorePressedMask = 0;
    // #8064 FR-104. The signal is handed over only when the active desktop is the user's -- on any
    // other desktop this process cannot observe input, so its last observations are stale by
    // construction. Poisoned rather than withheld, so the reason survives into the next batch too.
    if (!ActiveDesktopIsTheUsers()) {
      PoisonAllUserHeldKeys(&m_userHeld);
    }

    m_nInputs = PrepareInjectedInputBatch(
      m_pInputs, m_ModifierKeyboardState, m_pSharedData, GetAsyncKeyState, flag_ShouldSerializeInput,
      &restorePressedMask, ReportModifierDiagnostic, m_restoreEventIndex, &m_userHeld);
    return restorePressedMask;
  }

  /**
    #8064 FR-103a. Applies every raw keyboard event already sitting in this thread's queue to the
    user-held signal, and returns only when there is nothing left to apply.

    THIS IS NOT AN OPTIMISATION AND IT IS NOT DEFENSIVE PADDING. Without it, the signal that
    ProcessModifierVerification reads has NOT yet seen observations the OS made BEFORE the verify
    message was even posted -- and a signal reporting a hold the user has already let go of is
    precisely the input that makes the correction decline. It is the stale shadow
    CheckRawInputRegistrationStillOurs was written to prevent, arriving by a second route.

    The reason is message RETRIEVAL ORDER, which is by class and not by arrival time. GetMessage and
    PeekMessage return sent messages, then POSTED messages, then INPUT (hardware) messages, then
    WM_PAINT, then WM_TIMER. WM_INPUT is signalled by QS_RAWINPUT, QS_RAWINPUT is part of QS_INPUT,
    so WM_INPUT is retrieved in the input class -- BEHIND every posted message, however much earlier
    it arrived. WM_KEYMAN_VERIFY_MODIFIER_EVENT is posted. So a user's modifier KEYUP the OS observed
    before the batch's SendInput even returned is still undispatched when the verify runs, and
    m_userHeld still reports that key held.

    The self-post's OTHER ordering guarantee is untouched by this, and deliberately: the drain
    filters on WM_INPUT alone, so it removes nothing from the posted queue. Every
    WM_KEYMAN_MODIFIER_EVENT posted before the verify was already dispatched before it by
    posted-message FIFO -- that claim is between two posted messages and it was always sound. This
    repairs only the half of the ordering that spans two different message classes.

    Dispatched rather than handled inline, so ProcessRawInput reads each HRAWINPUT inside its own
    WM_INPUT dispatch and WndProc still falls through to DefWindowProc for the system's cleanup. A
    raw input handle is valid only for the delivery of the message that carries it; nothing is
    stashed and nothing is read after its message is done.

    Pulling forward a raw event that arrived AFTER the verify post is possible and harmless. A KEYUP
    pulled forward makes the correction fire, which is the outcome wanted. A KEYDOWN pulled forward
    makes it decline, which is the safe-direction error PrepareModifierVerificationCorrection's own
    doc comment already accepts -- an unmatched KEYUP is re-pressable, an unmatched KEYDOWN on
    hardware with no physical Right Ctrl is not.
  */
  void DrainPendingRawInput() {
    if (m_hwnd == NULL) {
      return;
    }

    // A bound, because typematic repeat refills the queue while we empty it and this runs on the
    // input path. Two orders of magnitude above a realistic repeat rate for the microseconds this
    // takes, so reaching it means something pathological -- and it is reported rather than passed
    // over, because a silent cap here reads as "the signal is current" when it is not.
    const int kMaxDrain = 256;
    int drained = 0;

    MSG msg;
    while (drained < kMaxDrain && PeekMessage(&msg, m_hwnd, WM_INPUT, WM_INPUT, PM_REMOVE)) {
      DispatchMessage(&msg);
      drained++;
    }

    if (drained >= kMaxDrain) {
      SendDebugMessageFormat(
        "#8064 verification: stopped draining raw input at %d events with more still queued; the "
        "user-held signal is more current than it was but is not guaranteed current",
        kMaxDrain);
    }
  }

  /**
    #8064 Handles WM_KEYMAN_VERIFY_MODIFIER_EVENT: rechecks the VKs the batch restored against the
    cache and live state as they stand now, and releases any the OS still holds that the cache says
    nobody holds.
  */
  void ProcessModifierVerification(DWORD restorePressedMask) {
    // #8064 FR-103a. BEFORE the signal is read, never after: posted messages are retrieved ahead of
    // input messages, so raw observations older than this verify message are still queued behind it.
    // See DrainPendingRawInput.
    DrainPendingRawInput();

    INPUT correction[MAX_KEYEVENT_INPUTS_MODIFIERS];
    // In keybd_shift.cpp so the gtest project can reach it, same reasoning as PrepareInjectedInput.
    // #8064 FR-103a: the same signal the restore half consulted, or the pass would release what the
    // batch just pressed. FR-101/FR-103 and FR-103a land together or not at all.
    int n = PrepareModifierVerificationCorrection(
      correction, m_ModifierKeyboardState, restorePressedMask, GetAsyncKeyState, &m_userHeld);
    if (n > 0) {
      if (!SendInput(n, correction, sizeof(INPUT))) {
        DebugLastError("SendInput");
      }
    }
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

    // #8064 Not inline in ProcessQueuedKeyEvents: the point of posting is to land behind every
    // WM_KEYMAN_MODIFIER_EVENT already queued when the batch's SendInput returned.
    if (msg == WM_KEYMAN_VERIFY_MODIFIER_EVENT) {
      ProcessModifierVerification((DWORD)wParam);
    }

    // #8064 W5 / FR-100a. The user-held signal's feed.
    //
    // NO LOOP CHANGE IS NEEDED, and that is worth stating because the obvious instinct is to add a
    // wait: MsgWaitForMultipleObjectsEx already waits on QS_ALLINPUT, and QS_ALLINPUT includes
    // QS_INPUT which includes QS_RAWINPUT. The loop already wakes for raw input.
    //
    // WHAT THE LOOP DOES NOT GIVE IS ORDER. An earlier draft of this comment claimed FIFO dispatch
    // put a raw event that arrived before WM_KEYMAN_VERIFY_MODIFIER_EVENT ahead of it. That is
    // wrong, and it was the load-bearing assumption under FR-103a. Retrieval is ordered by message
    // CLASS -- sent, then posted, then input, then WM_PAINT, then WM_TIMER -- so a posted message is
    // returned ahead of a WM_INPUT that has been queued since long before it. The posted-FIFO claim
    // holds only between two posted messages, which is the WM_KEYMAN_MODIFIER_EVENT case, and is
    // stated that way in JUSTIFICATION.md.
    //
    // So the verify pass drains this queue itself before reading the signal: see
    // DrainPendingRawInput. Ordinary arrivals are still applied here, in dispatch order.
    if (msg == WM_INPUT) {
      ProcessRawInput((HRAWINPUT)lParam);
    }

    // #8064 FR-104 / FR-104a. A session change means the signal has no standing to speak about any
    // key: fast user switching, RDP connect and disconnect, lock and unlock all move input somewhere
    // this feed cannot see, and the UAC secure desktop is the same problem without a notification.
    // Poison is per key and clears only on a fresh observation of that key -- never on a timer -- so
    // a modifier the user releases on the secure desktop is not reported held on their return.
    if (msg == WM_WTSSESSION_CHANGE) {
      SendDebugMessageFormat(
        "#8064 session change (%x): poisoning the user-held signal; each key becomes usable again "
        "only when it is next observed",
        (unsigned)wParam);
      PoisonAllUserHeldKeys(&m_userHeld);
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

          See also:
          * PostVisualKeyboardModifierEvent in k32_visualkeyboardinterface.cpp
          * TfrmOSKOnScreenKeyboard.OskModifierEvent in UfrmOSKOnScreenKeyboard.cpp
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
    // In keybd_shift.cpp so the gtest project can reach it; this file is #ifndef _WIN64 and this is
    // a private member, so nothing here is testable. See #8064.
    UpdateModifierCacheFromKeyEvent(m_ModifierKeyboardState, bVk, fIsExtendedKey, bScan, fIsUp);
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
