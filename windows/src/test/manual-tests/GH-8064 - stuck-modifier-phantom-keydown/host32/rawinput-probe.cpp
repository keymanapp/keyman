/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * FR-100's raw input probe for GH-8064 -- spec 003-8064-audit-closeout, workstream W0.
 *
 * THIS IS A MEASUREMENT, NOT A FIX. It changes no production file, it repairs nothing, and it
 * gates exactly one thing: W5, the probe-gated half of the serializer work (FR-101 ... FR-105).
 * Every other workstream in that spec ships whatever this program reports. If both routes are
 * refuted, FR-106 fires and the FR-100...FR-106 block is struck; nothing else waits on the answer.
 *
 * The question. dwExtraInfo has survived SendInput -> the low level hook since 2018
 * (keyman64.h:137-144). The SendInput -> *raw input* leg has never been measured, and Route 2 --
 * a second modifier shadow in the serializer, fed by WM_INPUT on the serializer's own thread --
 * rests entirely on it. Alongside it, three delivery questions: does WM_INPUT reach a
 * message-only window at all, does it reach one owned by a WORKER thread while the process's
 * MAIN thread is stalled, and does it reach one while the probe is unfocused. Raw input queues on
 * the REGISTERING thread; the low level hook marshals every event to the thread that installed it
 * (keyman.exe's main thread, keyman32.cpp:275-280), which is the thread whose stall causes the
 * eviction. That difference is the whole of Route 2's case.
 *
 * -------------------------------------------------------------------------------------------
 * FR-100a -- READ THIS BEFORE READING ANY hDevice COLUMN THIS PROGRAM PRINTS
 * -------------------------------------------------------------------------------------------
 *
 *   RAWINPUTHEADER.hDevice is recorded FOR COMPLETENESS ONLY.
 *
 *   Keying the signal on it is REFUTED -- not discouraged, refuted -- because genuine USER input
 *   from Remote Desktop and from the Keyman On Screen Keyboard is OS-injected. Both are
 *   populations this branch deliberately protects: RemoteDesktopInputIsNotKeymans and
 *   TheOnScreenKeyboardIsNotKeymans (keybd_shift.tests.cpp:1305-1308,1327-1338). An
 *   injected-versus-physical filter, however it is spelled -- hDevice, LLKHF_INJECTED, a device
 *   handle allowlist -- blinds Keyman for both of them.
 *
 *   THE ADMISSIBLE POLICY IS TAG EQUALITY, identical to
 *   IsKeymanInjectedKeyEvent(MakeCode, ExtraInformation), that is:
 *
 *     MakeCode == SCAN_FLAG_KEYMAN_KEY_EVENT || ExtraInformation == EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP
 *
 *   The hDevice column below is NOT a licence for a device-provenance filter. No later reader may
 *   read it as one, and no implementation may key on it.
 * -------------------------------------------------------------------------------------------
 *
 * Build (with the Keyman build environment sourced) -- the harness's documented one-liner,
 * ../README.md:202-203, the same line host32.cpp is built with:
 *   cl /nologo /W4 /EHsc /MT /DUNICODE /D_UNICODE rawinput-probe.cpp \
 *      /link /SUBSYSTEM:WINDOWS user32.lib gdi32.lib /OUT:rawinput-probe.exe
 *
 * Usage:
 *   rawinput-probe.exe --step1-register  [options]
 *   rawinput-probe.exe --step2-delivery  [options] [--self-stall MS] [--fakefreeze PATH]
 *   rawinput-probe.exe --step3-tags      [options]
 *   rawinput-probe.exe --step4-rdp-osk   [options]
 *   options: [--wait SECONDS] [--out PATH]
 *
 * Every line goes to stdout AND to OutputDebugString -- so a DebugView capture taken alongside an
 * engine trace interleaves correctly -- and to --out if given. The output is markdown: paste a run
 * straight into evidence/rawinput-probe-<date>.md, which per T011 must carry the FR-100a header
 * above.
 *
 * Exit codes: 0 the step ran and its verdicts are recorded, 2 inconclusive (a precondition the
 * step needed was absent, so nothing was measured), 3 setup error.
 */
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

/*
  Constants copied in, not included. This probe is standalone by design -- it pulls in no engine
  header, so it builds with the harness's cl one-liner and no build system, exactly as host32.cpp
  does. Provenance, so a drift is findable:

    SCAN_FLAG_KEYMAN_KEY_EVENT           windows/src/engine/keyman32/keyman64.h:134
    EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP  windows/src/engine/keyman32/keyman64.h:145
    SCANCODE_RSHIFT                      kbd.h in the Windows SDK -- the header
                                         keybd_shift.cpp:89 cites ("from kbd.h") and
                                         tests/keybd_shift.tests.cpp:2 includes. Corroborated
                                         in-tree at ../README.md:298 and host32.cpp:46.

  If any of the three changes in the tree and not here, this probe measures the wrong thing.
*/
#define SCAN_FLAG_KEYMAN_KEY_EVENT          0xFF
#define EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP 0x4B4D0001
#define SCANCODE_RSHIFT                     0x36

static const wchar_t *CLASS_NAME  = L"GH8064RawInputProbe";
static const wchar_t *WINDOW_NAME = L"GH-8064 rawinput-probe";

#define MAX_RECORDS 512

// One RAWKEYBOARD as it arrived, plus the header field FR-100a forbids keying on.
struct RawRecord {
  USHORT    vkey;
  USHORT    makeCode;
  USHORT    flags;             // RI_KEY_MAKE / RI_KEY_BREAK / RI_KEY_E0 / RI_KEY_E1
  UINT      message;           // WM_KEYDOWN / WM_KEYUP / WM_SYSKEYDOWN / WM_SYSKEYUP
  ULONG_PTR extraInformation;  // RAWKEYBOARD.ExtraInformation -- a ULONG, see the note in step 3
  HANDLE    hDevice;           // FR-100a: FOR COMPLETENESS ONLY. Not a discriminator.
  DWORD     tick;
};

static RawRecord        g_records[MAX_RECORDS];
static LONG             g_recordCount = 0;
static CRITICAL_SECTION g_lock;
static BOOL             g_lockReady = FALSE;

static HWND   g_hwndProbe     = NULL;
static HANDLE g_hThread       = NULL;
static HANDLE g_hReady        = NULL;
static DWORD  g_threadId      = 0;
static BOOL   g_registered    = FALSE;
static DWORD  g_registerError = 0;
static DWORD  g_windowError   = 0;

static FILE *g_report = NULL;

static int     g_wait      = 12;
static int     g_selfStall = 0;
static wchar_t g_outPath[MAX_PATH]    = L"";
static wchar_t g_fakefreeze[MAX_PATH] = L"";

enum ProbeStep { STEP_NONE = 0, STEP_REGISTER, STEP_DELIVERY, STEP_TAGS, STEP_RDP_OSK };
static ProbeStep g_step = STEP_NONE;

/*
  The gate, restated byte for byte from keybd_shift.cpp:483-485 -- copied rather than
  approximated, because what step 3 measures is precisely whether THIS predicate can be evaluated
  on a RAWKEYBOARD. Two arms; neither covers the managed set alone. The scan arm cannot carry
  Right Shift, whose 0xFF do_keybd_event overwrites with SCANCODE_RSHIFT (keybd_shift.cpp:88-89),
  which is why shape (c) exists at all.
*/
static BOOL
IsKeymanInjectedKeyEvent(DWORD scanCode, ULONG_PTR extraInfo) {
  return scanCode == SCAN_FLAG_KEYMAN_KEY_EVENT || extraInfo == EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP;
}

// ------------------------------------------------------------------------------------------------
// Reporting

/*
  stdout, OutputDebugString and optionally a file, like host32.cpp's Report. It uses the CRT's
  _vsnwprintf_s rather than host32's wvsprintf for one reason: wvsprintf cannot format a 64-bit
  value, and hDevice is a HANDLE. A probe that could not print its own hDevice column would be a
  poor way of recording it "for completeness".
*/
static void
Report(const wchar_t *fmt, ...) {
  wchar_t buf[2048];
  va_list args;
  va_start(args, fmt);
  _vsnwprintf_s(buf, _countof(buf), _TRUNCATE, fmt, args);
  va_end(args);

  wprintf(L"%s\n", buf);
  fflush(stdout);
  if (g_report != NULL) {
    fwprintf(g_report, L"%s\n", buf);
    fflush(g_report);
  }
  OutputDebugString(buf);
  OutputDebugString(L"\n");
}

// ------------------------------------------------------------------------------------------------
// Capture store. Written by the worker thread, read by the main thread.

static void
StoreRecord(const RawRecord *rec) {
  EnterCriticalSection(&g_lock);
  if (g_recordCount < MAX_RECORDS) {
    g_records[g_recordCount] = *rec;
    g_recordCount++;
  }
  LeaveCriticalSection(&g_lock);
}

static LONG
CaptureCount(void) {
  LONG n;
  EnterCriticalSection(&g_lock);
  n = g_recordCount;
  LeaveCriticalSection(&g_lock);
  return n;
}

static void
ResetCapture(void) {
  EnterCriticalSection(&g_lock);
  g_recordCount = 0;
  LeaveCriticalSection(&g_lock);
}

// The record at index, if there is one. FALSE means this shape captured nothing, which is
// reported as "not measured" and never as a negative result.
static BOOL
GetRecord(LONG index, RawRecord *out) {
  BOOL ok = FALSE;
  EnterCriticalSection(&g_lock);
  if (index >= 0 && index < g_recordCount) {
    *out = g_records[index];
    ok = TRUE;
  }
  LeaveCriticalSection(&g_lock);
  return ok;
}

// ------------------------------------------------------------------------------------------------
// The worker thread and its message-only window

static LRESULT CALLBACK
RawWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
  if (msg == WM_INPUT) {
    // RAWINPUT by value rather than a BYTE array: it is correctly aligned by construction, and a
    // keyboard record always fits.
    RAWINPUT ri;
    UINT size = (UINT)sizeof(ri);
    if (GetRawInputData((HRAWINPUT)lParam, RID_INPUT, &ri, &size, sizeof(RAWINPUTHEADER)) != (UINT)-1) {
      if (ri.header.dwType == RIM_TYPEKEYBOARD) {
        RawRecord rec;
        rec.vkey             = ri.data.keyboard.VKey;
        rec.makeCode         = ri.data.keyboard.MakeCode;
        rec.flags            = ri.data.keyboard.Flags;
        rec.message          = ri.data.keyboard.Message;
        rec.extraInformation = (ULONG_PTR)ri.data.keyboard.ExtraInformation;
        rec.hDevice          = ri.header.hDevice;
        rec.tick             = GetTickCount();
        StoreRecord(&rec);
      }
    }
    // DefWindowProc must still see WM_INPUT so the system can clean up the raw input buffer.
    return DefWindowProc(hwnd, msg, wParam, lParam);
  }
  return DefWindowProc(hwnd, msg, wParam, lParam);
}

static DWORD WINAPI
RawInputThread(LPVOID param) {
  HINSTANCE hInstance = (HINSTANCE)param;
  WNDCLASS wc;
  RAWINPUTDEVICE rid;
  MSG msg;

  g_threadId = GetCurrentThreadId();

  ZeroMemory(&wc, sizeof(wc));
  wc.lpfnWndProc   = RawWndProc;
  wc.hInstance     = hInstance;
  wc.lpszClassName = CLASS_NAME;
  if (!RegisterClass(&wc)) {
    g_windowError = GetLastError();
    SetEvent(g_hReady);
    return 3;
  }

  /*
    HWND_MESSAGE: no pixels, never focusable, not enumerated -- the same shape as the serializer's
    existing window (serialkeyeventserver.cpp:266), which is the window Route 2 would register
    against. Whether WM_INPUT is delivered to such a window at all is the one thing the spec calls
    "a standard pattern, but not explicitly documented", and it is what step 1 exists to settle.
  */
  g_hwndProbe = CreateWindowEx(0, CLASS_NAME, WINDOW_NAME, 0, 0, 0, 0, 0,
                               HWND_MESSAGE, NULL, hInstance, NULL);
  if (g_hwndProbe == NULL) {
    g_windowError = GetLastError();
    SetEvent(g_hReady);
    return 3;
  }

  ZeroMemory(&rid, sizeof(rid));
  rid.usUsagePage = 0x01;             // HID_USAGE_PAGE_GENERIC
  rid.usUsage     = 0x06;             // HID_USAGE_GENERIC_KEYBOARD
  rid.dwFlags     = RIDEV_INPUTSINK;  // deliver even when this process is not in the foreground
  rid.hwndTarget  = g_hwndProbe;
  g_registered    = RegisterRawInputDevices(&rid, 1, sizeof(rid));
  g_registerError = g_registered ? 0 : GetLastError();

  SetEvent(g_hReady);

  while (GetMessage(&msg, NULL, 0, 0) > 0) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
  return 0;
}

static BOOL
StartRawInputThread(HINSTANCE hInstance) {
  g_hReady = CreateEvent(NULL, TRUE, FALSE, NULL);
  if (g_hReady == NULL) {
    return FALSE;
  }
  g_hThread = CreateThread(NULL, 0, RawInputThread, (LPVOID)hInstance, 0, NULL);
  if (g_hThread == NULL) {
    return FALSE;
  }
  return WaitForSingleObject(g_hReady, 10000) == WAIT_OBJECT_0;
}

static void
StopRawInputThread(void) {
  if (g_threadId != 0) {
    PostThreadMessage(g_threadId, WM_QUIT, 0, 0);
  }
  if (g_hThread != NULL) {
    WaitForSingleObject(g_hThread, 3000);
    CloseHandle(g_hThread);
    g_hThread = NULL;
  }
  if (g_hReady != NULL) {
    CloseHandle(g_hReady);
    g_hReady = NULL;
  }
}

// ------------------------------------------------------------------------------------------------
// Small helpers

static const wchar_t *
YesNo(BOOL b) {
  return b ? L"yes" : L"no";
}

static const wchar_t *
OkFail(BOOL b) {
  return b ? L"[OK]" : L"[FAIL]";
}

// Is some other process's window in the foreground? Step 2's "unfocused" case is only meaningful
// if the answer is yes -- though a message-only window can never be focused in any case.
static BOOL
ForegroundIsElsewhere(void) {
  DWORD pid = 0;
  HWND fg = GetForegroundWindow();
  if (fg == NULL) {
    return TRUE;
  }
  GetWindowThreadProcessId(fg, &pid);
  return pid != GetCurrentProcessId();
}

// Counts what arrives over `seconds`, printing a countdown so the operator can act on the prompt.
static LONG
WatchFor(int seconds, const wchar_t *what) {
  LONG before = CaptureCount();
  int i;
  Report(L"[INFO] %s", what);
  Report(L"[INFO] watching for %d s ...", seconds);
  for (i = seconds; i > 0; i--) {
    Sleep(1000);
    if ((i % 5) == 0 || i <= 3) {
      Report(L"       %d s remaining (raw keyboard records so far: %d)",
             i, (int)(CaptureCount() - before));
    }
  }
  return CaptureCount() - before;
}

/*
  Blocks the MAIN thread without yielding. A stalled Delphi UI pump does not sleep politely
  either, and the property under test is that the WORKER thread keeps receiving WM_INPUT while
  this thread is going nowhere. The authoritative staller for the real engine is fakefreeze, which
  stalls keyman.exe's main thread; this is the probe's own, so the worker-thread delivery property
  is observable even where fakefreeze is not to hand.
*/
static void
BusyStallMainThread(int ms) {
  DWORD start = GetTickCount();
  volatile ULONGLONG spin = 0;
  while ((DWORD)(GetTickCount() - start) < (DWORD)ms) {
    spin++;
  }
}

static BOOL
RunFakeFreeze(PROCESS_INFORMATION *pi) {
  STARTUPINFO si;
  wchar_t cmd[MAX_PATH + 4];

  ZeroMemory(&si, sizeof(si));
  si.cb          = sizeof(si);
  si.dwFlags     = STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_HIDE;
  ZeroMemory(pi, sizeof(*pi));

  swprintf_s(cmd, _countof(cmd), L"\"%s\"", g_fakefreeze);
  return CreateProcess(NULL, cmd, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &si, pi);
}

// One INPUT, shaped exactly as do_keybd_event shapes it (keybd_shift.cpp:96-100): wVk and wScan
// both set, no KEYEVENTF_SCANCODE, dwExtraInfo carried per event.
static UINT
InjectKey(BYTE vk, WORD scan, DWORD flags, ULONG_PTR extraInfo) {
  INPUT input;
  ZeroMemory(&input, sizeof(input));
  input.type           = INPUT_KEYBOARD;
  input.ki.wVk         = vk;
  input.ki.wScan       = scan;
  input.ki.dwFlags     = flags;
  input.ki.time        = 0;
  input.ki.dwExtraInfo = extraInfo;
  return SendInput(1, &input, sizeof(INPUT));
}

// ------------------------------------------------------------------------------------------------
// Capture rows -- the paste-ready table shared by steps 3 and 4

static void
PrintCaptureTableHeader(void) {
  Report(L"");
  Report(L"| shape | RAWKEYBOARD.ExtraInformation | RAWKEYBOARD.MakeCode | RAWINPUTHEADER.hDevice | IsKeymanInjectedKeyEvent |");
  Report(L"|---|---|---|---|---|");
}

static void
PrintCaptureRow(const wchar_t *shape, const RawRecord *rec, BOOL captured) {
  if (!captured) {
    Report(L"| %s | *(nothing captured)* | *(nothing captured)* | *(nothing captured)* | *(not measured)* |",
           shape);
    return;
  }
  Report(L"| %s | 0x%08X | 0x%04X | 0x%016I64X | %s |",
         shape,
         (unsigned)rec->extraInformation,
         (unsigned)rec->makeCode,
         (unsigned __int64)(ULONG_PTR)rec->hDevice,
         IsKeymanInjectedKeyEvent(rec->makeCode, rec->extraInformation) ? L"TRUE" : L"FALSE");
}

static void
PrintCaptureDetail(const wchar_t *shape, const RawRecord *rec, BOOL captured) {
  if (!captured) {
    Report(L"- %s: nothing arrived within the wait. Nothing is claimed about this shape.", shape);
    return;
  }
  Report(L"- %s: VKey 0x%02X, Flags 0x%04X (E0 %s, E1 %s, %s), Message 0x%04X",
         shape, (unsigned)rec->vkey, (unsigned)rec->flags,
         YesNo((rec->flags & RI_KEY_E0) != 0),
         YesNo((rec->flags & RI_KEY_E1) != 0),
         (rec->flags & RI_KEY_BREAK) ? L"BREAK" : L"MAKE",
         (unsigned)rec->message);
}

/*
  The three shapes FR-100 names, captured in order. Returns TRUE only if all three produced a
  record -- a shape that produced nothing is reported as not measured, never as a negative result.

    (a) a physical keystroke                          -- operator prompted
    (b) injected, dwExtraInfo = the wrap tag, scan 0xFF
    (c) injected, the same tag, scan SCANCODE_RSHIFT   -- the Right Shift wrap case the scan arm
                                                         cannot carry (keybd_shift.cpp:88-89)
*/
static BOOL
CaptureThreeShapes(const wchar_t *context, const wchar_t *physicalPrompt) {
  RawRecord physical, injectedFF, injectedRShift;
  BOOL gotPhysical, gotFF, gotRShift;

  ZeroMemory(&physical, sizeof(physical));
  ZeroMemory(&injectedFF, sizeof(injectedFF));
  ZeroMemory(&injectedRShift, sizeof(injectedRShift));

  Report(L"");
  Report(L"### Capture -- %s", context);

  // (a) physical
  ResetCapture();
  WatchFor(g_wait, physicalPrompt);
  gotPhysical = GetRecord(0, &physical);

  // (b) injected with the wrap tag and the 0xFF scan overload
  ResetCapture();
  InjectKey(VK_SHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, 0, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP);
  Sleep(250);
  InjectKey(VK_SHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP);
  Sleep(400);
  gotFF = GetRecord(0, &injectedFF);

  // (c) the same tag, with the Right Shift scan code
  ResetCapture();
  InjectKey(VK_SHIFT, SCANCODE_RSHIFT, 0, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP);
  Sleep(250);
  InjectKey(VK_SHIFT, SCANCODE_RSHIFT, KEYEVENTF_KEYUP, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP);
  Sleep(400);
  gotRShift = GetRecord(0, &injectedRShift);

  PrintCaptureTableHeader();
  PrintCaptureRow(L"(a) physical keystroke", &physical, gotPhysical);
  PrintCaptureRow(L"(b) injected, tag + scan 0xFF", &injectedFF, gotFF);
  PrintCaptureRow(L"(c) injected, tag + SCANCODE_RSHIFT", &injectedRShift, gotRShift);
  Report(L"");
  Report(L"FR-100a: the hDevice column above is recorded FOR COMPLETENESS ONLY. Keying the signal");
  Report(L"on it is refuted -- genuine user input from RDP and from the Keyman OSK is OS-injected.");
  Report(L"The admissible policy is tag equality, identical to IsKeymanInjectedKeyEvent.");
  Report(L"");
  PrintCaptureDetail(L"(a) physical keystroke", &physical, gotPhysical);
  PrintCaptureDetail(L"(b) injected, tag + scan 0xFF", &injectedFF, gotFF);
  PrintCaptureDetail(L"(c) injected, tag + SCANCODE_RSHIFT", &injectedRShift, gotRShift);
  Report(L"");

  if (gotFF) {
    BOOL survived = (injectedFF.extraInformation == EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP);
    Report(L"%s (b) the tag %s the SendInput -> raw input trip (got 0x%08X, wanted 0x%08X).",
           OkFail(survived), survived ? L"SURVIVED" : L"did NOT survive",
           (unsigned)injectedFF.extraInformation, (unsigned)EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP);
  } else {
    Report(L"[INFO] (b) not measured -- no raw record arrived for the injected 0xFF-scan shape.");
  }

  if (gotRShift) {
    BOOL survived = (injectedRShift.extraInformation == EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP);
    Report(L"%s (c) the tag %s for the Right Shift shape (got 0x%08X, wanted 0x%08X). This is the",
           OkFail(survived), survived ? L"SURVIVED" : L"did NOT survive",
           (unsigned)injectedRShift.extraInformation, (unsigned)EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP);
    Report(L"     shape the scan arm cannot carry, so the tag arm is the ONLY cover for it.");
    Report(L"[INFO] (c) MakeCode arrived as 0x%04X; SCANCODE_RSHIFT is 0x%04X.",
           (unsigned)injectedRShift.makeCode, (unsigned)SCANCODE_RSHIFT);
  } else {
    Report(L"[INFO] (c) not measured -- no raw record arrived for the injected RSHIFT-scan shape.");
  }

  if (gotPhysical) {
    BOOL clean = !IsKeymanInjectedKeyEvent(physical.makeCode, physical.extraInformation);
    Report(L"%s (a) the physical keystroke is %s by tag equality, which is the required direction:",
           OkFail(clean), clean ? L"NOT claimed as Keyman's" : L"WRONGLY claimed as Keyman's");
    Report(L"     a discriminator that swallows the user's own keys is worse than none.");
  } else {
    Report(L"[INFO] (a) not measured -- no physical keystroke arrived within the wait.");
  }

  return gotPhysical && gotFF && gotRShift;
}

// ------------------------------------------------------------------------------------------------
// Step 1 -- registration, and delivery to a message-only window

static int
Step1Register(void) {
  UINT count = 0;
  UINT got;
  LONG delivered;

  Report(L"## Step 1 -- registration, and delivery to a message-only window (plan.md W0 step 1)");
  Report(L"");
  Report(L"| item | value |");
  Report(L"|---|---|");
  Report(L"| worker thread id | %u |", g_threadId);
  Report(L"| main thread id | %u |", GetCurrentThreadId());
  Report(L"| message-only window (HWND_MESSAGE) | 0x%016I64X |",
         (unsigned __int64)(ULONG_PTR)g_hwndProbe);
  Report(L"| CreateWindowEx GetLastError | %u |", g_windowError);
  Report(L"| RegisterRawInputDevices(page 0x01, usage 0x06, RIDEV_INPUTSINK) | %s |",
         g_registered ? L"succeeded" : L"FAILED");
  Report(L"| RegisterRawInputDevices GetLastError | %u |", g_registerError);

  /*
    Read the registration back. FR-104b's displacement detection would use exactly this call:
    RegisterRawInputDevices is per-process last-writer-wins for a usage page, so a later
    registration elsewhere in the process silently redirects the feed with no error surfaced
    anywhere. Recorded here so W5 knows the readback is available before it depends on it.
  */
  got = GetRegisteredRawInputDevices(NULL, &count, sizeof(RAWINPUTDEVICE));
  if (got == (UINT)-1 && count > 0) {
    RAWINPUTDEVICE *devices = (RAWINPUTDEVICE *)LocalAlloc(LPTR, count * sizeof(RAWINPUTDEVICE));
    if (devices != NULL) {
      UINT n = count;
      if (GetRegisteredRawInputDevices(devices, &n, sizeof(RAWINPUTDEVICE)) != (UINT)-1) {
        UINT i;
        for (i = 0; i < n; i++) {
          Report(L"| readback %u: page 0x%02X usage 0x%02X flags 0x%08X, target | 0x%016I64X |",
                 i, (unsigned)devices[i].usUsagePage, (unsigned)devices[i].usUsage,
                 (unsigned)devices[i].dwFlags, (unsigned __int64)(ULONG_PTR)devices[i].hwndTarget);
        }
      }
      LocalFree(devices);
    }
  } else {
    Report(L"| GetRegisteredRawInputDevices readback | unavailable (count %u) |", count);
  }
  Report(L"");

  if (!g_registered) {
    Report(L"[FAIL] registration failed, so nothing downstream was measured.");
    Report(L"RESULT: INCONCLUSIVE - Route 2's first precondition did not hold on this machine.");
    return 2;
  }
  Report(L"[OK] RegisterRawInputDevices succeeded against a message-only window.");

  delivered = WatchFor(g_wait, L"Press and release a few keys now, anywhere on this machine.");
  Report(L"");
  Report(L"| question | answer | records |");
  Report(L"|---|---|---|");
  Report(L"| is WM_INPUT delivered to a message-only window at all | %s | %d |",
         YesNo(delivered > 0), (int)delivered);
  Report(L"%s WM_INPUT delivery to HWND_MESSAGE: %s.",
         OkFail(delivered > 0), delivered > 0 ? L"observed" : L"NOT observed");
  if (delivered == 0) {
    Report(L"RESULT: INCONCLUSIVE - no keys were seen. Either none were pressed, or WM_INPUT does");
    Report(L"        not reach a message-only window on this OS build. Re-run and press keys.");
    return 2;
  }
  Report(L"RESULT: step 1 recorded. Registration succeeds and WM_INPUT reaches HWND_MESSAGE.");
  return 0;
}

// ------------------------------------------------------------------------------------------------
// Step 2 -- delivery while unfocused, and while the main thread is stalled

static int
Step2Delivery(void) {
  LONG unfocused, stalled;
  BOOL fgElsewhere;
  PROCESS_INFORMATION pi;
  BOOL froze = FALSE;
  int stallMs;

  Report(L"## Step 2 -- delivery while unfocused, and with the MAIN thread stalled (W0 step 2)");
  Report(L"");
  Report(L"This is the property Route 2 rests on. The low level hook marshals every event to the");
  Report(L"thread that installed it -- keyman.exe's MAIN thread (keyman32.cpp:275-280) -- which is");
  Report(L"the thread whose stall causes the eviction. Raw input queues on the REGISTERING thread.");
  Report(L"If WM_INPUT still arrives here while this process's main thread is going nowhere, the");
  Report(L"feed survives exactly the window in which the hook does not.");
  Report(L"");

  if (!g_registered) {
    Report(L"[FAIL] not registered for raw input; step 1 must pass first.");
    Report(L"RESULT: INCONCLUSIVE - nothing was measured.");
    return 2;
  }

  // --- case 1: unfocused --------------------------------------------------------------------
  Report(L"### Case 1 -- unfocused");
  Report(L"[INFO] this probe owns no visible window at all, so it can never take focus: a");
  Report(L"       message-only window is not focusable by construction. The check below confirms");
  Report(L"       another process owned the foreground while the keys were pressed.");
  ResetCapture();
  unfocused   = WatchFor(g_wait, L"Click into another application and type there now.");
  fgElsewhere = ForegroundIsElsewhere();
  Report(L"");
  Report(L"| case | foreground is another process | raw keyboard records | verdict |");
  Report(L"|---|---|---|---|");
  Report(L"| unfocused | %s | %d | %s |",
         YesNo(fgElsewhere), (int)unfocused,
         (unfocused > 0 && fgElsewhere) ? L"DELIVERED" : L"not established");
  Report(L"%s case 1: WM_INPUT %s while unfocused.",
         OkFail(unfocused > 0 && fgElsewhere),
         (unfocused > 0 && fgElsewhere) ? L"is delivered" : L"was NOT established as delivered");
  Report(L"");

  // --- case 2: main thread stalled ----------------------------------------------------------
  Report(L"### Case 2 -- the MAIN thread stalled");
  ZeroMemory(&pi, sizeof(pi));
  if (g_fakefreeze[0] != 0) {
    if (RunFakeFreeze(&pi)) {
      froze = TRUE;
      Report(L"[INFO] fakefreeze started: %s", g_fakefreeze);
      Report(L"       It stalls keyman.exe's main thread -- the thread the low level hook is");
      Report(L"       marshalled to, and so the thread whose stall causes the eviction.");
    } else {
      Report(L"[WARN] could not start fakefreeze: %s", g_fakefreeze);
    }
  }
  if (g_selfStall <= 0 && !froze) {
    Report(L"[INFO] neither --self-stall nor a working --fakefreeze was supplied, so the stalled");
    Report(L"       case was NOT measured. It is reported as unmeasured rather than as a pass: a");
    Report(L"       precondition that merely might have held produces a false verdict, and a false");
    Report(L"       verdict here would carry W5 on nothing.");
    Report(L"RESULT: INCONCLUSIVE - case 2 not measured. Re-run with --self-stall 5000.");
    return 2;
  }

  stallMs = g_selfStall > 0 ? g_selfStall : 5000;
  ResetCapture();
  Report(L"[INFO] Press and release keys CONTINUOUSLY for the next few seconds. This thread is");
  Report(L"       about to busy-block for %d ms; the worker thread keeps its own queue.", stallMs);
  Sleep(2000);
  {
    LONG before = CaptureCount();
    BusyStallMainThread(stallMs);
    stalled = CaptureCount() - before;
  }
  Report(L"");
  Report(L"| case | staller | raw keyboard records DURING the stall | verdict |");
  Report(L"|---|---|---|---|");
  Report(L"| main thread stalled | %s%s | %d | %s |",
         g_selfStall > 0 ? L"self-stall" : L"none",
         froze ? L" + fakefreeze" : L"",
         (int)stalled, stalled > 0 ? L"DELIVERED" : L"not established");
  Report(L"%s case 2: WM_INPUT %s while the main thread was blocked.",
         OkFail(stalled > 0), stalled > 0 ? L"is delivered" : L"was NOT established as delivered");

  if (froze) {
    WaitForSingleObject(pi.hProcess, 20000);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
  }
  Report(L"");
  if (unfocused > 0 && stalled > 0) {
    Report(L"RESULT: step 2 recorded. Delivery holds unfocused AND with the main thread stalled.");
    return 0;
  }
  Report(L"RESULT: step 2 recorded, and at least one case did not deliver. Per plan.md W0's");
  Report(L"        decision rule, delivery failing in the stalled or unfocused case refutes");
  Report(L"        Route 2 even if the tag survives: it cannot close the failure window.");
  return 0;
}

// ------------------------------------------------------------------------------------------------
// Step 3 -- the decisive capture

static int
Step3Tags(void) {
  Report(L"## Step 3 -- THE DECISIVE CAPTURE (W0 step 3)");
  Report(L"");
  Report(L"dwExtraInfo has survived SendInput -> the low level hook since 2018");
  Report(L"(keyman64.h:137-144). The raw input leg has never been measured, and it is what Route 2");
  Report(L"needs: RAWKEYBOARD carries ExtraInformation per event, which is the second reason W0");
  Report(L"probes Route 2 before Route 1 -- WM_KEY* exposes no per-event dwExtraInfo at all.");
  Report(L"");
  Report(L"Note on widths: RAWKEYBOARD.ExtraInformation is a 32-bit ULONG, while the hook's");
  Report(L"dwExtraInfo is ULONG_PTR. EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP is 0x%08X and fits, so the",
         (unsigned)EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP);
  Report(L"tag itself is unaffected; the column is printed to 8 hex digits for that reason.");

  if (!g_registered) {
    Report(L"[FAIL] not registered for raw input; step 1 must pass first.");
    Report(L"RESULT: INCONCLUSIVE - nothing was measured.");
    return 2;
  }

  if (!CaptureThreeShapes(L"local desktop, no RDP, no OSK",
                          L"Press and release Left Shift (or any key) now.")) {
    Report(L"RESULT: INCONCLUSIVE - at least one shape produced no record, so the decisive");
    Report(L"        question is unanswered. Re-run; do not read a missing row as a negative.");
    return 2;
  }
  Report(L"RESULT: step 3 recorded. Apply plan.md W0's decision rule to the rows above. That rule");
  Report(L"        was written BEFORE this run so the result cannot be rationalised afterwards.");
  return 0;
}

// ------------------------------------------------------------------------------------------------
// Step 4 -- RDP, the Keyman OSK, higher integrity, and the secure desktop

static int
Step4RdpOsk(void) {
  LONG elevated, secure;
  BOOL remote = GetSystemMetrics(SM_REMOTESESSION) != 0;
  RawRecord osk;
  BOOL gotOsk;

  ZeroMemory(&osk, sizeof(osk));

  Report(L"## Step 4 -- RDP, the Keyman OSK, higher integrity, the secure desktop (W0 step 4)");
  Report(L"");
  Report(L"RDP and the OSK are the two populations keybd_shift.tests.cpp:1305-1308,1327-1338");
  Report(L"protect (RemoteDesktopInputIsNotKeymans, TheOnScreenKeyboardIsNotKeymans). Both deliver");
  Report(L"genuine USER input that is OS-injected, which is why FR-100a refutes an");
  Report(L"injected-versus-physical filter and why hDevice is not a discriminator.");
  Report(L"");
  Report(L"| item | value |");
  Report(L"|---|---|");
  Report(L"| GetSystemMetrics(SM_REMOTESESSION) | %s |", YesNo(remote));
  Report(L"");
  if (!remote) {
    Report(L"[INFO] this run is NOT inside a remote session. Re-run it over RDP to record the RDP");
    Report(L"       rows; the capture below is still taken, and is labelled as local.");
  }

  if (!g_registered) {
    Report(L"[FAIL] not registered for raw input; step 1 must pass first.");
    Report(L"RESULT: INCONCLUSIVE - nothing was measured.");
    return 2;
  }

  // --- RDP ----------------------------------------------------------------------------------
  CaptureThreeShapes(remote ? L"under RDP (SM_REMOTESESSION set)"
                            : L"NOT under RDP -- local run, recorded as such",
                     L"Press and release a key on the REMOTE keyboard now. mstsc stamps its own "
                     L"dwExtraInfo (0x4321DCBA) on genuine remote user input.");

  // --- the Keyman OSK -----------------------------------------------------------------------
  Report(L"");
  Report(L"### Case -- the Keyman On Screen Keyboard");
  Report(L"[INFO] The OSK's latch KEYDOWN is deliberately fed to the server's modifier cache as");
  Report(L"       USER input. A signal that filtered it out as 'injected' would stop the OSK's");
  Report(L"       sticky modifier being real machine-wide, which is #8064 from the other end.");
  ResetCapture();
  WatchFor(g_wait, L"Open the Keyman OSK and click a modifier (Shift/Ctrl/Alt) now.");
  gotOsk = GetRecord(0, &osk);
  PrintCaptureTableHeader();
  PrintCaptureRow(L"(d) Keyman OSK click", &osk, gotOsk);
  Report(L"");
  if (gotOsk) {
    BOOL claimed = IsKeymanInjectedKeyEvent(osk.makeCode, osk.extraInformation);
    PrintCaptureDetail(L"(d) Keyman OSK click", &osk, TRUE);
    Report(L"%s the OSK click is %s by tag equality. It MUST NOT be claimed: the OSK's latch is",
           OkFail(!claimed), claimed ? L"CLAIMED as Keyman's" : L"not claimed as Keyman's");
    Report(L"     the user's own intent, and TheOnScreenKeyboardIsNotKeymans pins that.");
  } else {
    Report(L"[INFO] (d) not measured -- nothing arrived. No claim is made about the OSK.");
  }

  // --- a higher-integrity window has focus ---------------------------------------------------
  Report(L"");
  Report(L"### Case -- a higher-integrity window has focus");
  Report(L"[INFO] Production keyman.exe is uiAccess=true; the debug manifest is not");
  Report(L"       (windows/src/engine/keyman/build.sh:13-14,38). Whether RIDEV_INPUTSINK still");
  Report(L"       delivers while an elevated window owns the foreground decides how much of the");
  Report(L"       user's day the shadow can see.");
  ResetCapture();
  elevated = WatchFor(g_wait, L"Focus an ELEVATED window (an admin console, say) and type there.");
  Report(L"");
  Report(L"| case | raw keyboard records | verdict |");
  Report(L"|---|---|---|");
  Report(L"| higher-integrity window focused | %d | %s |",
         (int)elevated, elevated > 0 ? L"DELIVERED" : L"not delivered / not measured");
  Report(L"%s delivery with a higher-integrity window focused: %s.",
         OkFail(elevated > 0), elevated > 0 ? L"observed" : L"NOT observed");

  // --- the secure desktop --------------------------------------------------------------------
  Report(L"");
  Report(L"### Case -- the secure desktop");
  Report(L"[INFO] Nothing is expected here, and that expectation is the point. FR-104's shadow");
  Report(L"       MUST report *unknown* for keys whose transitions it may have missed, never a");
  Report(L"       stale 'held'. A user who holds Ctrl into a UAC prompt, releases it there and");
  Report(L"       returns is exactly the case FR-104a's per-key poisoning exists for.");
  ResetCapture();
  secure = WatchFor(g_wait, L"Raise the secure desktop (Ctrl+Alt+Del, or a UAC prompt), press keys "
                            L"there, then dismiss it.");
  Report(L"");
  Report(L"| case | raw keyboard records | expected | verdict |");
  Report(L"|---|---|---|---|");
  Report(L"| secure desktop | %d | 0 | %s |", (int)secure,
         secure == 0 ? L"as expected: nothing delivered" : L"UNEXPECTED: records arrived");
  Report(L"%s secure desktop: %d record(s). Zero is the expected and safe answer; anything else",
         OkFail(secure == 0), (int)secure);
  Report(L"     must be recorded and explained before W5 relies on the poisoning rule.");
  Report(L"");
  Report(L"RESULT: step 4 recorded. Rows that say 'not measured' are exactly that -- a missing row");
  Report(L"        is not a negative result.");
  return 0;
}

// ------------------------------------------------------------------------------------------------
// Startup

static void
Usage(void) {
  Report(L"GH-8064 raw input probe (FR-100). A MEASUREMENT: it changes no production code, and it");
  Report(L"gates only W5, the probe-gated half of the serializer work.");
  Report(L"");
  Report(L"usage: rawinput-probe.exe <step> [options]");
  Report(L"");
  Report(L"steps:");
  Report(L"  --step1-register   Message-only window on a WORKER thread; RegisterRawInputDevices");
  Report(L"                     for usage page 1 / usage 6 with RIDEV_INPUTSINK targeting it.");
  Report(L"                     Records whether registration succeeded and whether WM_INPUT is");
  Report(L"                     delivered to a message-only window at all.");
  Report(L"  --step2-delivery   WM_INPUT for physical keys while the probe is UNFOCUSED, and again");
  Report(L"                     while its MAIN thread is deliberately stalled. --self-stall is the");
  Report(L"                     probe's own staller; --fakefreeze additionally stalls keyman.exe's");
  Report(L"                     main thread. One verdict line per case.");
  Report(L"  --step3-tags       THE DECISIVE STEP. SendInput three shapes and record");
  Report(L"                     RAWKEYBOARD.ExtraInformation, RAWKEYBOARD.MakeCode and");
  Report(L"                     RAWINPUTHEADER.hDevice for each: (a) a physical keystroke;");
  Report(L"                     (b) injected with EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP and scan");
  Report(L"                     0xFF; (c) the same tag with SCANCODE_RSHIFT.");
  Report(L"  --step4-rdp-osk    The step-3 capture repeated under RDP and with the Keyman OSK,");
  Report(L"                     plus delivery with a higher-integrity window focused and on the");
  Report(L"                     secure desktop, where nothing is expected to be delivered.");
  Report(L"");
  Report(L"options:");
  Report(L"  --wait SECONDS     How long each operator prompt waits (default 12).");
  Report(L"  --self-stall MS    Step 2 only: busy-block the MAIN thread for MS milliseconds.");
  Report(L"  --fakefreeze PATH  Step 2 only: also run fakefreeze, which stalls keyman.exe.");
  Report(L"  --out PATH         Also write every line to PATH, for evidence/.");
  Report(L"");
  Report(L"FR-100a: hDevice is recorded FOR COMPLETENESS ONLY. Keying the signal on it is refuted,");
  Report(L"because genuine user input from RDP and from the OSK is OS-injected. The admissible");
  Report(L"policy is tag equality, identical to IsKeymanInjectedKeyEvent(MakeCode, ExtraInformation).");
}

static BOOL
ParseArgs(void) {
  BOOL ok = TRUE;
  int i;

  for (i = 1; i < __argc && ok; i++) {
    const wchar_t *a = __wargv[i];
    if (lstrcmpi(a, L"--step1-register") == 0) {
      g_step = STEP_REGISTER;
    } else if (lstrcmpi(a, L"--step2-delivery") == 0) {
      g_step = STEP_DELIVERY;
    } else if (lstrcmpi(a, L"--step3-tags") == 0) {
      g_step = STEP_TAGS;
    } else if (lstrcmpi(a, L"--step4-rdp-osk") == 0) {
      g_step = STEP_RDP_OSK;
    } else if (lstrcmpi(a, L"--help") == 0 || lstrcmpi(a, L"-h") == 0 || lstrcmpi(a, L"/?") == 0) {
      g_step = STEP_NONE;
      return TRUE;
    } else if (lstrcmpi(a, L"--wait") == 0 && i + 1 < __argc) {
      g_wait = _wtoi(__wargv[++i]);
    } else if (lstrcmpi(a, L"--self-stall") == 0 && i + 1 < __argc) {
      g_selfStall = _wtoi(__wargv[++i]);
    } else if (lstrcmpi(a, L"--fakefreeze") == 0 && i + 1 < __argc) {
      lstrcpyn(g_fakefreeze, __wargv[++i], MAX_PATH);
    } else if (lstrcmpi(a, L"--out") == 0 && i + 1 < __argc) {
      lstrcpyn(g_outPath, __wargv[++i], MAX_PATH);
    } else {
      ok = FALSE;
    }
  }
  if (g_wait < 1) {
    g_wait = 1;
  }
  return ok;
}

/*
  A GUI subsystem app has no console of its own. Attach to the shell that launched us where there
  is one, as host32.cpp does, and otherwise allocate one -- every step here prompts an operator
  standing at the machine, so a run started from Explorer still has to be readable.
*/
static void
OpenConsole(void) {
  FILE *reopened = NULL;
  if (!AttachConsole(ATTACH_PARENT_PROCESS)) {
    AllocConsole();
  }
  freopen_s(&reopened, "CONOUT$", "w", stdout);
}

/*
  The version fields, honestly. GetVersionEx lies to an unmanifested caller; RtlGetVersion does
  not, and it costs no import library the harness's one-liner does not already link. The struct is
  declared here because RTL_OSVERSIONINFOW lives in the WDK, which this probe does not use.
*/
typedef struct _PROBE_OSVERSIONINFOW {
  ULONG dwOSVersionInfoSize;
  ULONG dwMajorVersion;
  ULONG dwMinorVersion;
  ULONG dwBuildNumber;
  ULONG dwPlatformId;
  WCHAR szCSDVersion[128];
} PROBE_OSVERSIONINFOW;

typedef LONG(WINAPI *PRTLGETVERSION)(PROBE_OSVERSIONINFOW *);

static void
PrintRunHeader(void) {
  wchar_t machine[MAX_COMPUTERNAME_LENGTH + 1];
  DWORD cch = _countof(machine);
  PROBE_OSVERSIONINFOW osv;
  SYSTEMTIME now;
  HMODULE ntdll;

  ZeroMemory(&osv, sizeof(osv));
  osv.dwOSVersionInfoSize = sizeof(osv);
  ntdll = GetModuleHandle(L"ntdll.dll");
  if (ntdll != NULL) {
    PRTLGETVERSION rtlGetVersion = (PRTLGETVERSION)GetProcAddress(ntdll, "RtlGetVersion");
    if (rtlGetVersion != NULL) {
      rtlGetVersion(&osv);
    }
  }
  if (!GetComputerName(machine, &cch)) {
    lstrcpyn(machine, L"(unknown)", _countof(machine));
  }
  GetLocalTime(&now);

  Report(L"# GH-8064 raw input probe (FR-100) -- W0, spec 003-8064-audit-closeout");
  Report(L"");
  Report(L"**This is a measurement.** No production code changed. It gates only W5.");
  Report(L"");
  Report(L"**FR-100a.** `hDevice` below is recorded **for completeness only**. Keying the signal on");
  Report(L"it is **refuted**, because genuine user input from RDP and from the Keyman OSK is");
  Report(L"OS-injected. The admissible policy is **tag equality, identical to");
  Report(L"`IsKeymanInjectedKeyEvent(MakeCode, ExtraInformation)`**. The `hDevice` column is not a");
  Report(L"licence for an injected-versus-physical filter.");
  Report(L"");
  Report(L"| | |");
  Report(L"|---|---|");
  Report(L"| machine | %s |", machine);
  Report(L"| OS | %u.%u build %u |",
         (unsigned)osv.dwMajorVersion, (unsigned)osv.dwMinorVersion, (unsigned)osv.dwBuildNumber);
  Report(L"| probe bitness | %u-bit |", (unsigned)(sizeof(void *) * 8));
  Report(L"| session is remote (SM_REMOTESESSION) | %s |",
         YesNo(GetSystemMetrics(SM_REMOTESESSION) != 0));
  Report(L"| run at | %04u-%02u-%02u %02u:%02u:%02u local |",
         now.wYear, now.wMonth, now.wDay, now.wHour, now.wMinute, now.wSecond);
  Report(L"| operator wait per prompt | %d s |", g_wait);
  Report(L"");
}

int WINAPI
wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PWSTR pCmdLine, int nCmdShow) {
  int rc = 3;

  UNREFERENCED_PARAMETER(hPrevInstance);
  UNREFERENCED_PARAMETER(pCmdLine);
  UNREFERENCED_PARAMETER(nCmdShow);

  OpenConsole();
  InitializeCriticalSection(&g_lock);
  g_lockReady = TRUE;

  if (!ParseArgs()) {
    Usage();
    rc = 3;
    goto done;
  }
  if (g_step == STEP_NONE) {
    Usage();
    rc = 0;
    goto done;
  }
  if (g_outPath[0] != 0) {
    _wfopen_s(&g_report, g_outPath, L"w, ccs=UTF-8");
  }

  PrintRunHeader();

  if (!StartRawInputThread(hInstance)) {
    Report(L"[FAIL] could not start the raw input worker thread (%u).", GetLastError());
    Report(L"RESULT: setup error - nothing was measured.");
    rc = 3;
    goto done;
  }
  if (g_hwndProbe == NULL) {
    Report(L"[FAIL] the message-only window was not created (%u).", g_windowError);
    Report(L"RESULT: setup error - nothing was measured.");
    rc = 3;
    goto done;
  }

  switch (g_step) {
  case STEP_REGISTER:
    rc = Step1Register();
    break;
  case STEP_DELIVERY:
    rc = Step2Delivery();
    break;
  case STEP_TAGS:
    rc = Step3Tags();
    break;
  case STEP_RDP_OSK:
    rc = Step4RdpOsk();
    break;
  case STEP_NONE:
  default:
    Usage();
    rc = 3;
    break;
  }

  Report(L"");
  Report(L"---");
  Report(L"Paste the whole of the above into evidence/rawinput-probe-<date>.md, keeping the");
  Report(L"FR-100a paragraph with it. T012 then applies plan.md W0's decision rule -- written");
  Report(L"before the run -- and records the verdict in the same file.");

done:
  StopRawInputThread();
  if (g_report != NULL) {
    fclose(g_report);
    g_report = NULL;
  }
  if (g_lockReady) {
    DeleteCriticalSection(&g_lock);
  }
  return rc;
}
