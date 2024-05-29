
#include "pch.h"
#include "keyman-debug-etw.h"

#ifdef _WIN64
#define DEBUG_PLATFORM_STRING "x64"
#define DEBUG_PLATFORM_STRINGW L"x64"
#else
#define DEBUG_PLATFORM_STRING "x86"
#define DEBUG_PLATFORM_STRINGW L"x86"
#endif

#define TAB L"\t"


int GetActualShiftState() {   // I4843
  int state = 0;
  if (GetKeyState(VK_NUMLOCK) & 1) state |= NUMLOCKFLAG;
  if (GetKeyState(VK_CAPITAL) & 1) state |= CAPITALFLAG;
  if (GetKeyState(VK_SHIFT) < 0) state |= K_SHIFTFLAG;
  if (GetKeyState(VK_RCONTROL) < 0) state |= RCTRLFLAG;
  if (GetKeyState(VK_LCONTROL) < 0) state |= LCTRLFLAG;
  if (GetKeyState(VK_LMENU) < 0) state |= LALTFLAG;
  if (GetKeyState(VK_RMENU) < 0) state |= RALTFLAG;

  if (GetKeyState(VK_LSHIFT) < 0) state |= 0x10000;
  if (GetKeyState(VK_RSHIFT) < 0) state |= 0x20000;

  return state;
}

extern "C" void _declspec(dllexport) WINAPI Keyman_WriteDebugEvent(char *file, int line, PWCHAR msg) {
  // This function exists solely to provide a stable transition between versions
  // of Keyman, so that things don't crash if an older version of kmtip.dll
  // happens to call this function in a newer installation. It is not used in
  // current versions of Keyman. This change came in around 17.0.260-alpha
  PWCHAR fileW = strtowstr(file);
  Keyman_WriteDebugEventW(fileW, line, msg);
  delete[] fileW;
}

extern "C" void _declspec(dllexport) WINAPI Keyman_WriteDebugEventW(PWCHAR file, int line, PWCHAR msg) {
  if (file == NULL || msg == NULL) {
    return;
  }

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return;
  }

  if (!Globals::get_debug_KeymanLog()) {
    return;
  }

  GUITHREADINFO gti;
  WCHAR sProcessPath[256], sProcessName[32];

  gti.cbSize = sizeof(gti);
  GetGUIThreadInfo(GetCurrentThreadId(), &gti);

  GetModuleFileNameW(NULL, sProcessPath, 256);
  _wsplitpath_s(sProcessPath, NULL, 0, NULL, 0, sProcessName, 32, NULL, 0);
  sProcessName[31] = 0;

  if (wcslen(msg) > 256) {
    msg[255] = 0;
  }

  DWORD pid = GetCurrentProcessId();
  DWORD tid = GetCurrentThreadId();
  DWORD shiftState = Globals::get_ShiftState();
  DWORD actualShiftState = GetActualShiftState();
  DWORD tickCount = GetTickCount();
  HKL activeHKL = GetKeyboardLayout(0);

  if (Globals::get_debug_ToConsole()) {   // I3951
    WCHAR windowinfo[1024];
    wsprintfW(windowinfo,
      DEBUG_PLATFORM_STRINGW TAB //"Platform" TAB
      L"%ls" TAB  //"Process" TAB
      L"%x" TAB  //"PID" TAB
      L"%x" TAB  //"TID" TAB
      L"%x" TAB  //"ShiftState" TAB
      L"%x" TAB  //"ActualShiftState" TAB
      L"%d" TAB  //"TickCount" TAB
      L"%x" TAB  //"FocusHWND" TAB
      L"%8x" TAB //"ActiveHKL" TAB
      L"%hs:%d" TAB  //"SourceFile" TAB
      L"%ls\n",     //"Message"
      sProcessName,                    //"Process" TAB
      (unsigned int) pid,                             //"PID" TAB
      (unsigned int) tid,                             //"TID" TAB
      (unsigned int) shiftState,                      //"ShiftState" TAB
      (unsigned int) actualShiftState,                // ActualShiftState TAB
      (int) tickCount,                       //"TickCount" TAB
      PtrToInt(gti.hwndFocus),                   //"FocusHWND" TAB
      PtrToInt(activeHKL),                       //"ActiveHKL" TAB
      file, line,                      //"SourceFile" TAB
      msg);                            //"Message"

    OutputDebugStringW(windowinfo);
  }

  if (_td->etwRegHandle != NULL) {
#define MAX_DESCRIPTORS 12
    EVENT_DATA_DESCRIPTOR Descriptors[MAX_DESCRIPTORS];

#ifdef _WIN64
    DWORD platform = 2;
#else
    DWORD platform = 1;
#endif


    // These must match the manifest template in keyman-debug-etw.man
    EventDataDescCreate(&Descriptors[0], &platform, sizeof(DWORD)); // <data name = "Platform" inType = "Platform" / >
    EventDataDescCreate(&Descriptors[1], sProcessName, (ULONG)(wcslen(sProcessName) + 1) * sizeof(WCHAR)); //  <data name = "Process" inType = "win:UnicodeString" / >
    EventDataDescCreate(&Descriptors[2], &pid, sizeof(DWORD)); //  <data name = "PID" inType = "win:UInt32" / >
    EventDataDescCreate(&Descriptors[3], &tid, sizeof(DWORD)); //  <data name = "TID" inType = "win:UInt32" / >
    EventDataDescCreate(&Descriptors[4], &shiftState, sizeof(DWORD)); //  <data name = "ShiftState" inType = "ShiftState" / >
    EventDataDescCreate(&Descriptors[5], &actualShiftState, sizeof(DWORD)); //  <data name = "ActualShiftState" inType = "ShiftState" / >
    EventDataDescCreate(&Descriptors[6], &tickCount, sizeof(DWORD)); //  <data name = "TickCount" inType = "win:UInt32" / >
    EventDataDescCreate(&Descriptors[7], (PDWORD)&gti.hwndFocus, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
    EventDataDescCreate(&Descriptors[8], (PDWORD)&activeHKL, sizeof(DWORD)); //  <data name = "ActiveHKL" inType = "win:UInt32" / >
    EventDataDescCreate(&Descriptors[9], file, (ULONG)(wcslen(file) + 1) * sizeof(WCHAR)); //  <data name = "SourceFile" inType = "win:UnicodeString" / >
    EventDataDescCreate(&Descriptors[10], (PDWORD)&line, sizeof(DWORD)); //  <data name = "SourceLine" inType = "win:UInt32" / >
    EventDataDescCreate(&Descriptors[11], msg, (ULONG)(wcslen(msg) + 1) * sizeof(WCHAR)); //  <data name = "Message" inType = "win:UnicodeString" / >

    DWORD dwErr = EventWrite(_td->etwRegHandle, &DebugEvent, (ULONG)MAX_DESCRIPTORS, &Descriptors[0]);
    if (dwErr != ERROR_SUCCESS) {
      OutputDebugString("Keyman k32_dbg: Failed to call EventWrite");
    }

  }
}
