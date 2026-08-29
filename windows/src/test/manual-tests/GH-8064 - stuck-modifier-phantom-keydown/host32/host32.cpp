/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Self-driving 32-bit host for the GH-8064 reproduction. 32-bit because the modifier cache lives in
 * serialkeyeventserver.cpp, which is #ifndef _WIN64; self-driving because a background process
 * cannot give another process's window keyboard focus (SetFocus needs the calling thread to own the
 * active window). ../README.md has both, the sequence run per iteration, and why the oracle reads
 * all nine modifier VKs rather than the probe text.
 *
 * Build (32-bit, with the Keyman build environment sourced):
 *   cl /nologo /W4 /EHsc /MT /DUNICODE /D_UNICODE host32.cpp \
 *      /link /SUBSYSTEM:WINDOWS user32.lib gdi32.lib shell32.lib /OUT:host32.exe
 *
 * Usage:
 *   host32.exe --fakefreeze PATH [--iterations N] [--release-delay MS] [--control]
 *              [--modifier LSHIFT|RSHIFT|LCTRL|RCTRL|LALT|RALT] [--out PATH]
 *              [--allow-no-transform]
 *
 * Exit codes: 0 PASS, 1 FAIL (defect reproduced), 2 INCONCLUSIVE, 3 setup error.
 */
#include <windows.h>
#include <shellapi.h>
#include <stdio.h>
#include <stdlib.h>

static const wchar_t *CLASS_NAME  = L"GH8064Host";
static const wchar_t *WINDOW_NAME = L"GH-8064 host32";

// Ask the UI thread to focus the edit. SetFocus only works from the thread owning the window.
#define WM_APP_FOCUS_EDIT (WM_APP + 1)

// Overridable with --probe: which keys a Keyman keyboard actually transforms is keyboard-specific,
// and a probe the active keyboard passes through unchanged makes the run inconclusive.
static wchar_t g_probe[256] = L"aeiouknsd";

struct ModifierDef {
  const wchar_t *name;
  BYTE vk;
  BYTE scan;
  BOOL extended;
};

// Real hardware scan codes, so the engine sees hardware-shaped input.
static const ModifierDef MODIFIERS[] = {
  { L"LSHIFT", VK_LSHIFT,   0x2A, FALSE },
  { L"RSHIFT", VK_RSHIFT,   0x36, FALSE },
  { L"LCTRL",  VK_LCONTROL, 0x1D, FALSE },
  { L"RCTRL",  VK_RCONTROL, 0x1D, TRUE  },
  { L"LALT",   VK_LMENU,    0x38, FALSE },
  { L"RALT",   VK_RMENU,    0x38, TRUE  },
};

// All nine, per the oracle note above.
static const struct { const wchar_t *name; int vk; } ORACLE_VKS[] = {
  { L"SHIFT",  VK_SHIFT    }, { L"CTRL",   VK_CONTROL  }, { L"ALT",    VK_MENU     },
  { L"LSHIFT", VK_LSHIFT   }, { L"RSHIFT", VK_RSHIFT   },
  { L"LCTRL",  VK_LCONTROL }, { L"RCTRL",  VK_RCONTROL },
  { L"LALT",   VK_LMENU    }, { L"RALT",   VK_RMENU    },
};

static HWND  g_main   = NULL;
static HWND  g_edit   = NULL;
static FILE *g_report = NULL;

static int   g_iterations   = 5;
static int   g_releaseDelay = 1500;
static BOOL  g_control      = FALSE;
static BOOL  g_allowNoXform = FALSE;
static int   g_cycleLayouts = 0;
static int   g_waitForRule  = 0;
static const ModifierDef *g_mod = &MODIFIERS[0];
static wchar_t g_fakefreeze[MAX_PATH] = L"";
static wchar_t g_outPath[MAX_PATH]    = L"";

// ------------------------------------------------------------------------------------------------

static void
Report(const wchar_t *fmt, ...) {
  wchar_t buf[1024];
  va_list args;
  va_start(args, fmt);
  wvsprintf(buf, fmt, args);
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

static void
PublishLayout(HWND hwnd) {
  // A reader outside this process cannot get this: GetKeyboardLayout(idThread) returns 0 for a
  // thread in another process. The host knows its own layout, and a title is readable anywhere.
  wchar_t title[128];
  wsprintf(title, L"%s [HKL=%08X]", WINDOW_NAME, (unsigned)(ULONG_PTR)GetKeyboardLayout(0));
  SetWindowText(hwnd, title);
}

static void
SendOne(BYTE vk, BYTE scan, DWORD flags) {
  INPUT input;
  ZeroMemory(&input, sizeof(input));
  input.type           = INPUT_KEYBOARD;
  input.ki.wVk         = vk;
  input.ki.wScan       = scan;
  input.ki.dwFlags     = flags;
  input.ki.dwExtraInfo = 0; // hardware-shaped: deliberately carries no Keyman tag
  SendInput(1, &input, sizeof(INPUT));
}

static void
HoldModifier(const ModifierDef *m, BOOL up) {
  DWORD flags = up ? KEYEVENTF_KEYUP : 0;
  if (m->extended) {
    flags |= KEYEVENTF_EXTENDEDKEY;
  }
  SendOne(m->vk, m->scan, flags);
}

static void
TapKey(BYTE vk) {
  SendOne(vk, 0, 0);
  Sleep(30);
  SendOne(vk, 0, KEYEVENTF_KEYUP);
  Sleep(60);
}

// A KEYUP for a key that is not down is harmless, so this is safe to run unconditionally.
static void
ClearAllModifiers(void) {
  int i;
  for (i = 0; i < _countof(MODIFIERS); i++) {
    HoldModifier(&MODIFIERS[i], TRUE);
  }
  SendOne(VK_SHIFT, 0, KEYEVENTF_KEYUP);
  SendOne(VK_CONTROL, 0, KEYEVENTF_KEYUP);
  SendOne(VK_MENU, 0, KEYEVENTF_KEYUP);
  Sleep(200);
}

// Names of every modifier the OS reports held, into buf. Returns how many.
static int
HeldModifiers(wchar_t *buf, int cch) {
  int count = 0;
  int i;
  buf[0] = 0;
  for (i = 0; i < _countof(ORACLE_VKS); i++) {
    if (GetAsyncKeyState(ORACLE_VKS[i].vk) < 0) {
      if (count > 0) {
        lstrcpyn(buf + lstrlen(buf), L", ", cch - lstrlen(buf));
      }
      lstrcpyn(buf + lstrlen(buf), ORACLE_VKS[i].name, cch - lstrlen(buf));
      count++;
    }
  }
  return count;
}

/*
  Whether the Keyman engine has attached to THIS process -- the decisive precondition, and the one
  easy to mistake for a pass. keyman32.dll is injected only once a Keyman keyboard is active here;
  without it no batch is assembled, and every modifier reads clear. Checked from inside, because
  from outside this is indistinguishable from a legitimately pass-through keyboard.
*/
static BOOL
KeymanEngineAttached(void) {
  return GetModuleHandle(L"keyman32.dll") != NULL;
}

static BOOL
KeymanResponsive(void) {
  HWND km = FindWindow(L"TfrmKeyman7Main", NULL);
  DWORD_PTR result = 0;
  if (km == NULL) {
    return FALSE;
  }
  return SendMessageTimeout(km, WM_NULL, 0, 0, SMTO_ABORTIFHUNG | SMTO_BLOCK, 400, &result) != 0;
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

  wsprintf(cmd, L"\"%s\"", g_fakefreeze);
  return CreateProcess(NULL, cmd, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &si, pi);
}

static void
GetEditText(wchar_t *buf, int cch) {
  buf[0] = 0;
  SendMessage(g_edit, WM_GETTEXT, (WPARAM)cch, (LPARAM)buf);
}

/*
  Taps the legacy Alt+Shift input-language hotkey and reports the layout after each tap. Keyman
  keyboards are TSF profiles, which WM_INPUTLANGCHANGEREQUEST cannot select; the hotkey is the one
  route a program can drive, being just keystrokes into its own foreground window.
*/
static void
CycleLayouts(int taps) {
  int i;
  wchar_t title[128];

  for (i = 0; i < taps; i++) {
    SendOne(VK_MENU, 0x38, 0);
    SendOne(VK_LSHIFT, 0x2A, 0);
    Sleep(40);
    SendOne(VK_LSHIFT, 0x2A, KEYEVENTF_KEYUP);
    SendOne(VK_MENU, 0x38, KEYEVENTF_KEYUP);
    Sleep(900);

    SendMessage(g_main, WM_APP_FOCUS_EDIT, 0, 0);
    GetWindowText(g_main, title, _countof(title));
    Report(L"    layout after tap %d: %s   engine attached: %s",
           i + 1, title, KeymanEngineAttached() ? L"yes" : L"no");
  }
  ClearAllModifiers();
}

// Types the probe into the edit and reports whether the active keyboard transformed it.
static BOOL
ProbeFiresARule(void) {
  const wchar_t *p;
  wchar_t got[1024];

  SendMessage(g_main, WM_APP_FOCUS_EDIT, 0, 0);
  SetWindowText(g_edit, L"");
  Sleep(150);

  for (p = g_probe; *p; p++) {
    SHORT vks = VkKeyScan(*p);
    if (vks != -1) {
      TapKey((BYTE)(vks & 0xFF));
    }
  }
  Sleep(500);
  GetEditText(got, _countof(got));
  SetWindowText(g_edit, L"");

  return (lstrlen(got) > 0) && (lstrcmp(got, g_probe) != 0);
}

/*
  Waits for a Keyman keyboard whose rules actually fire to be selected in this window. Selecting one
  cannot be automated -- TSF profile, so no WM_INPUTLANGCHANGEREQUEST; Alt+Shift is not the Windows
  11 switcher; kmshell.exe -i opens a dialog -- so wait for a person instead. Empirical rather than
  an HKL check: a keyboard can be active and still pass the probe through unchanged.
*/
static BOOL
WaitForRuleCapableKeyboard(int seconds) {
  int waited = 0;

  Report(L"Waiting up to %d s for a Keyman keyboard whose rules fire on \"%s\".", seconds, g_probe);
  Report(L"  Switch the keyboard for THIS window now (Win+Space, or the taskbar language button).");
  Report(L"  Probing every 2 s; the test starts by itself as soon as the probe transforms.");

  while (waited < seconds) {
    if (ProbeFiresARule()) {
      wchar_t title[128];
      GetWindowText(g_main, title, _countof(title));
      Report(L"[OK] rules are firing. %s", title);
      return TRUE;
    }
    Sleep(2000);
    waited += 2;
  }
  return FALSE;
}

// ------------------------------------------------------------------------------------------------

static DWORD WINAPI
TestThread(LPVOID param) {
  int stuckRuns = 0, frozeRuns = 0, xformRuns = 0;
  int exitCode = 0;
  int iter;
  wchar_t held[256];
  wchar_t after[1024];
  wchar_t title[128];

  UNREFERENCED_PARAMETER(param);

  Sleep(1200); // let the window settle and the engine attach to this process

  Report(L"=== GH-8064 self-driving reproduction ===");
  Report(L"modifier %s (vk 0x%02X scan 0x%02X), %s, %d iteration(s), release at %d ms",
         g_mod->name, g_mod->vk, g_mod->scan,
         g_control ? L"NO freeze (control)" : L"freeze", g_iterations, g_releaseDelay);
  GetWindowText(g_main, title, _countof(title));
  Report(L"host: %s", title);

  if (HeldModifiers(held, _countof(held)) > 0) {
    Report(L"[WARN] modifiers held at baseline: %s; recovering", held);
    ClearAllModifiers();
    if (HeldModifiers(held, _countof(held)) > 0) {
      Report(L"[FAIL] still held after recovery: %s", held);
      Report(L"RESULT: INCONCLUSIVE - cannot tell a pre-existing wedge from one this run caused");
      PostMessage(g_main, WM_CLOSE, 0, 0);
      return 3;
    }
  }
  Report(L"[OK] baseline clean");

  if (g_cycleLayouts > 0) {
    Report(L"cycling input layouts with Alt+Shift, %d tap(s):", g_cycleLayouts);
    CycleLayouts(g_cycleLayouts);
  }

  if (g_waitForRule > 0 && !WaitForRuleCapableKeyboard(g_waitForRule)) {
    Report(L"[FAIL] no rule fired within %d s.", g_waitForRule);
    Report(L"       Install a keyboard with unshifted rules and select it. This repo ships one:");
    Report(L"       common/test/keyboards/baseline/k_0301___multiple_deadkeys.kmx, where typing");
    Report(L"       1x gives \"1=OK \" and no Shift is involved -- which matters, because Shift is");
    Report(L"       the modifier under test. Then re-run with --probe 1x2x3x.");
    ClearAllModifiers();
    Report(L"RESULT: INCONCLUSIVE - no batch could be assembled, so nothing was measured.");
    PostMessage(g_main, WM_CLOSE, 0, 0);
    return 2;
  }

  if (!KeymanEngineAttached()) {
    Report(L"[FAIL] keyman32.dll is not loaded in this process: the Keyman engine has not attached.");
    Report(L"       No rule can fire and no batch can be assembled, so the restore half never runs");
    Report(L"       and the defect cannot reproduce however the freeze is timed.");
    Report(L"       Select a Keyman keyboard in this window, then re-run. The title shows the");
    Report(L"       active layout; a Keyman keyboard installs as a TSF profile, which is why it");
    Report(L"       cannot be selected programmatically.");
    ClearAllModifiers();
    Report(L"RESULT: INCONCLUSIVE - the engine was never in the process under test.");
    PostMessage(g_main, WM_CLOSE, 0, 0);
    return 2;
  }
  Report(L"[OK] keyman32.dll is loaded: the engine has attached to this process");

  for (iter = 1; iter <= g_iterations; iter++) {
    PROCESS_INFORMATION pi;
    BOOL froze = FALSE;
    BOOL transformed;
    const wchar_t *p;

    Report(L"--- iteration %d of %d ---", iter, g_iterations);

    SendMessage(g_main, WM_APP_FOCUS_EDIT, 0, 0);
    SetWindowText(g_edit, L"");
    Sleep(200);

    HoldModifier(g_mod, FALSE);
    Sleep(200);

    ZeroMemory(&pi, sizeof(pi));
    if (!g_control) {
      if (!RunFakeFreeze(&pi)) {
        Report(L"[FAIL] could not start fakefreeze: %s", g_fakefreeze);
        HoldModifier(g_mod, TRUE);
        ClearAllModifiers();
        Report(L"RESULT: INCONCLUSIVE - the stimulus never ran");
        PostMessage(g_main, WM_CLOSE, 0, 0);
        return 3;
      }
      Sleep(400);
      froze = !KeymanResponsive();
      Report(L"    freeze active: %s", froze ? L"yes" : L"NO - controller still responding");
      if (froze) {
        frozeRuns++;
      }
    }

    // The whole test: release while the hook is gone.
    Sleep(g_releaseDelay);
    HoldModifier(g_mod, TRUE);
    Report(L"    modifier released%s", g_control ? L"" : L" (inside the stall)");

    if (!g_control) {
      WaitForSingleObject(pi.hProcess, 15000);
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
      Sleep(800);
    }

    // Make Keyman produce output, so a batch is assembled and the restore half runs.
    SendMessage(g_main, WM_APP_FOCUS_EDIT, 0, 0);
    Sleep(200);
    for (p = g_probe; *p; p++) {
      // VkKeyScan maps the character to a VK on the current layout; the low byte is the VK.
      SHORT vks = VkKeyScan(*p);
      if (vks != -1) {
        TapKey((BYTE)(vks & 0xFF));
      }
    }
    Sleep(700);
    GetEditText(after, _countof(after));

    transformed = (lstrlen(after) > 0) && (lstrcmp(after, g_probe) != 0);
    if (transformed) {
      xformRuns++;
    }
    Report(L"    typed \"%s\" -> got \"%s\"  transformed: %s",
           g_probe, after, transformed ? L"yes" : L"no");

    if (HeldModifiers(held, _countof(held)) > 0) {
      Report(L"    [FAIL] stuck: %s", held);
      stuckRuns++;
    } else {
      Report(L"    [ok] no modifier held");
    }

    ClearAllModifiers();
  }

  ClearAllModifiers();

  Report(L"=== summary ===");
  Report(L"iterations %d   freeze confirmed %d   text transformed %d   stuck %d",
         g_iterations, frozeRuns, xformRuns, stuckRuns);

  if (HeldModifiers(held, _countof(held)) > 0) {
    Report(L"[WARN] modifiers still held at exit: %s", held);
  }

  if (stuckRuns > 0) {
    Report(L"RESULT: FAIL - the defect reproduced. A modifier was held with nothing pressed.");
    exitCode = 1;
  } else if (!g_control && frozeRuns == 0) {
    Report(L"RESULT: INCONCLUSIVE - the freeze never took effect, so no KEYUP was dropped.");
    Report(L"        If Keyman runs elevated and this process does not, UIPI blocks the PostMessage.");
    exitCode = 2;
  } else if (xformRuns == 0 && !g_allowNoXform) {
    Report(L"RESULT: INCONCLUSIVE - Keyman never transformed the probe text, so no rule fired and");
    Report(L"        no batch was assembled. A clean modifier state proves nothing here. Select a");
    Report(L"        Keyman keyboard that remaps the probe characters, or --allow-no-transform.");
    exitCode = 2;
  } else {
    Report(L"RESULT: PASS - batches were assembled and no modifier stuck in any iteration.");
    exitCode = 0;
  }

  PostMessage(g_main, WM_CLOSE, 0, 0);
  return (DWORD)exitCode;
}

// ------------------------------------------------------------------------------------------------

static LRESULT CALLBACK
WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
  switch (msg) {
  case WM_CREATE:
    g_edit = CreateWindowEx(
        0, L"EDIT", L"",
        WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_LEFT | ES_MULTILINE | ES_AUTOVSCROLL | ES_WANTRETURN,
        0, 0, 0, 0, hwnd, NULL, NULL, NULL);
    if (g_edit == NULL) {
      return -1;
    }
    PublishLayout(hwnd);
    return 0;

  case WM_INPUTLANGCHANGE:
    PublishLayout(hwnd);
    return DefWindowProc(hwnd, msg, wParam, lParam);

  case WM_SIZE:
    if (g_edit != NULL) {
      MoveWindow(g_edit, 0, 0, LOWORD(lParam), HIWORD(lParam), TRUE);
    }
    return 0;

  case WM_ACTIVATE:
    if (LOWORD(wParam) != WA_INACTIVE && g_edit != NULL) {
      SetFocus(g_edit);
    }
    return 0;

  case WM_SETFOCUS:
    if (g_edit != NULL) {
      SetFocus(g_edit);
    }
    return 0;

  case WM_APP_FOCUS_EDIT:
    // Sent by the test thread before it types. This runs on the UI thread, which owns the window,
    // so SetFocus is allowed here and would fail if attempted from the test thread.
    if (g_edit != NULL) {
      SetForegroundWindow(hwnd);
      SetFocus(g_edit);
    }
    return 0;

  case WM_CLOSE:
    DestroyWindow(hwnd);
    return 0;

  case WM_DESTROY:
    PostQuitMessage(0);
    return 0;

  default:
    break;
  }
  return DefWindowProc(hwnd, msg, wParam, lParam);
}

static const ModifierDef *
FindModifier(const wchar_t *name) {
  int i;
  for (i = 0; i < _countof(MODIFIERS); i++) {
    if (lstrcmpi(MODIFIERS[i].name, name) == 0) {
      return &MODIFIERS[i];
    }
  }
  return NULL;
}

static BOOL
ParseArgs(void) {
  int argc = 0;
  LPWSTR *argv = CommandLineToArgvW(GetCommandLine(), &argc);
  BOOL ok = TRUE;
  int i;

  if (argv == NULL) {
    return FALSE;
  }

  for (i = 1; i < argc && ok; i++) {
    if (lstrcmpi(argv[i], L"--control") == 0) {
      g_control = TRUE;
    } else if (lstrcmpi(argv[i], L"--allow-no-transform") == 0) {
      g_allowNoXform = TRUE;
    } else if (lstrcmpi(argv[i], L"--iterations") == 0 && i + 1 < argc) {
      g_iterations = _wtoi(argv[++i]);
    } else if (lstrcmpi(argv[i], L"--release-delay") == 0 && i + 1 < argc) {
      g_releaseDelay = _wtoi(argv[++i]);
    } else if (lstrcmpi(argv[i], L"--modifier") == 0 && i + 1 < argc) {
      const ModifierDef *m = FindModifier(argv[++i]);
      if (m == NULL) {
        ok = FALSE;
      } else {
        g_mod = m;
      }
    } else if (lstrcmpi(argv[i], L"--fakefreeze") == 0 && i + 1 < argc) {
      lstrcpyn(g_fakefreeze, argv[++i], MAX_PATH);
    } else if (lstrcmpi(argv[i], L"--wait-for-rule") == 0 && i + 1 < argc) {
      g_waitForRule = _wtoi(argv[++i]);
    } else if (lstrcmpi(argv[i], L"--cycle-layouts") == 0 && i + 1 < argc) {
      g_cycleLayouts = _wtoi(argv[++i]);
    } else if (lstrcmpi(argv[i], L"--probe") == 0 && i + 1 < argc) {
      lstrcpyn(g_probe, argv[++i], _countof(g_probe));
    } else if (lstrcmpi(argv[i], L"--out") == 0 && i + 1 < argc) {
      lstrcpyn(g_outPath, argv[++i], MAX_PATH);
    } else {
      ok = FALSE;
    }
  }

  LocalFree(argv);
  if (g_iterations < 1) {
    g_iterations = 1;
  }
  return ok;
}

int WINAPI
wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PWSTR pCmdLine, int nCmdShow) {
  WNDCLASS wc;
  HANDLE hTest;
  DWORD threadId = 0;
  DWORD exitCode = 3;
  MSG msg;

  UNREFERENCED_PARAMETER(hPrevInstance);
  UNREFERENCED_PARAMETER(pCmdLine);

  // So Report() reaches the shell that launched us: a GUI subsystem app has no console of its own.
  if (AttachConsole(ATTACH_PARENT_PROCESS)) {
    FILE *reopened = NULL;
    freopen_s(&reopened, "CONOUT$", "w", stdout);
  }

  if (!ParseArgs()) {
    Report(L"usage: host32.exe --fakefreeze PATH [--iterations N] [--release-delay MS]");
    Report(L"                  [--control] [--modifier LSHIFT|RSHIFT|LCTRL|RCTRL|LALT|RALT]");
    Report(L"                  [--probe TEXT] [--out PATH] [--allow-no-transform]");
    return 3;
  }
  if (g_fakefreeze[0] == 0) {
    Report(L"[FAIL] --fakefreeze PATH is required. Build it with");
    Report(L"       ./windows/src/support/fakefreeze/build.sh --debug build:x86");
    return 3;
  }
  if (GetFileAttributes(g_fakefreeze) == INVALID_FILE_ATTRIBUTES) {
    Report(L"[FAIL] fakefreeze not found: %s", g_fakefreeze);
    return 3;
  }
  if (g_outPath[0] != 0) {
    _wfopen_s(&g_report, g_outPath, L"w, ccs=UTF-8");
  }

  ZeroMemory(&wc, sizeof(wc));
  wc.lpfnWndProc   = WndProc;
  wc.hInstance     = hInstance;
  wc.lpszClassName = CLASS_NAME;
  wc.hCursor       = LoadCursor(NULL, IDC_IBEAM);
  wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
  if (!RegisterClass(&wc)) {
    Report(L"[FAIL] RegisterClass failed (%d)", GetLastError());
    return 3;
  }

  g_main = CreateWindowEx(0, CLASS_NAME, WINDOW_NAME, WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT, CW_USEDEFAULT, 640, 320, NULL, NULL, hInstance, NULL);
  if (g_main == NULL) {
    Report(L"[FAIL] CreateWindowEx failed (%d)", GetLastError());
    return 3;
  }

  ShowWindow(g_main, nCmdShow);
  SetForegroundWindow(g_main);
  PublishLayout(g_main);
  SetFocus(g_edit);

  hTest = CreateThread(NULL, 0, TestThread, NULL, 0, &threadId);
  if (hTest == NULL) {
    Report(L"[FAIL] CreateThread failed (%d)", GetLastError());
    return 3;
  }

  while (GetMessage(&msg, NULL, 0, 0) > 0) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }

  WaitForSingleObject(hTest, 5000);
  GetExitCodeThread(hTest, &exitCode);
  CloseHandle(hTest);

  if (g_report != NULL) {
    fclose(g_report);
  }
  return (int)exitCode;
}
