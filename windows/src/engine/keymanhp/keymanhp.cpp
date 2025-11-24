/*
  Name:             keymanhp
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      11 Dec 2009

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    03 Oct 2011 - mcdurdin - I3093 - Keyman Engine x64 shows a garbled error message if it cannot start
                    03 Oct 2011 - mcdurdin - I3092 - Keyman Engine does not restart nicely if shutdown uncleanly
                    05 Nov 2012 - mcdurdin - I3547 - V9.0 Use _countof to tidyup code
                    01 Jan 2013 - mcdurdin - I3677 - V9.0 - OSK and Keyman menu show an entry in taskbar
                    17 Jan 2013 - mcdurdin - I3758 - V9.0 - keymanx64 can find incorrect window handle for keyman.exe
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/
#include "keymanhp.h"   // I5136
#if defined(_M_X64)
  #include "../../../../common/windows/cpp/include/keymansentry.h"
#endif

// Forward declarations of functions included in this code module

BOOL             Run(HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow);
ATOM             MyRegisterClass(HINSTANCE hInstance);
BOOL             InitInstance(HINSTANCE, int);
LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);
BOOL             Fail(HWND, PWSTR);
BOOL             ParseCmdLine(LPTSTR lpCmdLine);
BOOL             CreateWatcherThread(HWND hWnd);

// External functions in Keyman64.dll

extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_Initialise(HWND Handle, BOOL FSingleApp);
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_ResetInitialisation(void);  // I3092
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_StartExit(void);  // I3092
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_Exit(void);
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_RegisterControllerThread(DWORD tid);
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_RegisterMasterController(HWND hwnd);
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_UnregisterControllerThread(DWORD tid);
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_UnregisterMasterController();

#define KM_EXIT 4
#define KM_EXITFLUSH 5

// Global variables

HINSTANCE hInst;                            // Current instance
HWND hwndController = NULL;                 // Keyman x86 Application window handle
DWORD dwControllerThreadId = NULL;
HANDLE hParentProcessHandle = NULL;         // Keyman x86 process handle
DWORD dwParentProcessID = 0;

HANDLE hWatcherThread = NULL;
HANDLE hWatcherThreadTerminateEvent = NULL;

BOOL KeymanStarted = FALSE;

// Global strings
#if defined(_M_X64)
  #define WINDOW_TITLE  L"Keymanhp-x64"
  #define EXECUTABLE_TITLE L"Keyman Engine x64"
  #define ARCH_LOG_FILE ".keymanhp-x64"
#elif defined(_M_ARM64)
  #define WINDOW_TITLE  L"Keymanhp-arm64"
  #define EXECUTABLE_TITLE L"Keyman Engine Arm64"
  #define ARCH_LOG_FILE ".keymanhp-arm64"
#else
  #error Unknown architecture
#endif

const PWSTR
  szWindowClass = WINDOW_TITLE, // Do not localize
  szWindowTitle = WINDOW_TITLE, // Do not localize

  szTitle = EXECUTABLE_TITLE,
  szError_Keymanx86NotFound = L"Keyman Engine x86 is not running.  Do not run keymanhp.*.exe -- it must be started by Keyman Engine x86",
  szError_Keymanx86NotFound_Comms = L"Keyman Engine x86 has closed unexpectedly.  Closing down Keyman Engine hp64",
  szError_FailedToRegisterController = L"Failed to register controller window",
  szError_FailedToInitialise = L"Failed to initialise Keyman Engine hp",
  szError_FailedToFindParentProcess = L"Unable to open parent process; it may have disappeared",

  szError_ChangeWindowMessageFilter = L"Failed to prepare message filters",

  szError_CannotRunMultipleInstances = L"Only one instance of Keyman Engine hp can run at a time",
  szError_FailedToRegister = L"Failed to register window class",
  szError_FailedToInitInstance = L"Failed to initialise application",

  szError_InvalidCommandline = L"Incorrect command line for Keyman Engine hp; should be passed handle of Keyman Engine x86",
  szError_WatcherThreadFailed = L"Failed to create watcher thread",

  szFail_UnknownError = L"Unknown error %d",
  szFail_ErrorFormat = L"%s: %s (%d)",
  szFail_ErrorFormat_OtherUnknown = L"%s: An unknown error occurred",

  szWindowClass_x86_Wnd = L"TfrmKeyman7Main"; // Do not localize

//const char *szGPA_ChangeWindowMessageFilter = "ChangeWindowMessageFilter"; // Do not localize
#define KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_KEYMANHP KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE ARCH_LOG_FILE

//
//   FUNCTION: _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int
//
//   PURPOSE: Program entry point
//
//   COMMENTS:
//
//        A mutex is used to ensure only one instance of Keyman Engine HP
//        for each architecture.
//
int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
#if defined(_M_X64)
  keyman_sentry_init(false, KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_KEYMANHP);
  keyman_sentry_setexceptionfilter();

  // We used the 'Started' event when testing Sentry integration in 14.0 alpha
  // but we don't want or need it for stable.
  // keyman_sentry_report_message(KEYMAN_SENTRY_LEVEL_INFO, "Started " KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_KEYMANX64);

  if (!wcscmp(lpCmdLine, L"-sentry-client-test-exception")) {
    keyman_sentry_test_crash();
  }
#endif

  BOOL result = Run(hInstance, lpCmdLine, nCmdShow);
#if defined(_M_X64)
  keyman_sentry_shutdown();
#endif

  return result ? 0 : 1;
}

BOOL Run(HINSTANCE hInstance,
         LPTSTR    lpCmdLine,
         int       nCmdShow) {
	MSG msg;

  if (!ParseCmdLine(lpCmdLine)) {
    return Fail(0, szError_InvalidCommandline);
  }

  // We pass the parent process ID rather than a handle because it is much
  // easier to instantiate a UIAccess=true process with ShellExecuteEx than
  // with CreateProcess. However, ShellExecuteEx does not allow us to
  // inherit handles, so we'll just open the parent process as soon as we
  // can after loading. There is a small chance of the parent process
  // disappearing before the handle can be opened, in which case it is in
  // theory possible that this could be waiting on the wrong process. We have
  // reduced this chance of this happening by comparing the hwndController
  // PID with the parent PID (see ParseCmdLine).
  hParentProcessHandle = OpenProcess(SYNCHRONIZE, FALSE, dwParentProcessID);
  if (hParentProcessHandle == NULL) {
    return Fail(0, szError_FailedToFindParentProcess);
  }

  if (!MyRegisterClass(hInstance)) {
    return Fail(0, szError_FailedToRegister);
  }

  if (!InitInstance(hInstance, nCmdShow)) {
    if (GetLastError() != ERROR_SUCCESS) {
      // If WM_CREATE returns -1, then CreateWindow fails but
      // GetLastError returns ERROR_SUCCESS. In this situation,
      // we know that the error has already been reported via
      // StartKeyman() so we don't want to re-report it. If
      // CreateWindow fails for another reason, then we should
      // be reporting it.
      Fail(0, szError_FailedToInitInstance);
    }
    return FALSE;
  }

	// Main message loop:
	while (GetMessage(&msg, NULL, 0, 0))
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

  // If msg.wParam is non-zero, there was a
  // failure returned from the main window
	return msg.wParam ? FALSE : TRUE;
}

//
//   FUNCTION: ParseCmdLine(LPSTSTR)
//
//   PURPOSE: Parses command line and finds input parameters
//
//   COMMENTS:
//
//        Reads parent process handle and master controller window
//        handle from the command line and validates them.
//
BOOL ParseCmdLine(LPTSTR lpCmdLine) {
  while (iswspace(*lpCmdLine)) lpCmdLine++;
  if (!*lpCmdLine) return FALSE;
  dwParentProcessID = (DWORD) wcstoul(lpCmdLine, &lpCmdLine, 10);
  if (dwParentProcessID == 0 || dwParentProcessID == ULONG_MAX) return FALSE;

  while (iswspace(*lpCmdLine)) lpCmdLine++;
  if (!*lpCmdLine) return FALSE;
  hwndController = (HWND) wcstoull(lpCmdLine, &lpCmdLine, 10);
  if (hwndController == 0 || hwndController == (HWND) ULLONG_MAX) return FALSE;

  DWORD dwControllerProcessID;
  dwControllerThreadId = GetWindowThreadProcessId(hwndController, &dwControllerProcessID);
  if (dwControllerThreadId == NULL || dwControllerProcessID != dwParentProcessID) return FALSE;

  return TRUE;
}

//
//   FUNCTION: MyRegisterClass(HINSTANCE)
//
//   PURPOSE: Register the window class for Keyman Engine x64
//
ATOM MyRegisterClass(HINSTANCE hInstance)
{
	WNDCLASSEX wcex;

	wcex.cbSize = sizeof(WNDCLASSEX);

	wcex.style			= CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc	= WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= hInstance;
  wcex.hIcon			= LoadIcon(hInstance, MAKEINTRESOURCE(1));
	wcex.hCursor		= LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
	wcex.lpszMenuName	= 0;
	wcex.lpszClassName	= szWindowClass;
  wcex.hIconSm		= LoadIcon(hInstance, MAKEINTRESOURCE(1));

	return RegisterClassEx(&wcex);
}

//
//   FUNCTION: Fail(HWND, PWSTR)
//
//   PURPOSE: Displays an error message and always returns FALSE
//
//   COMMENTS:
//
//        The last error code and message will be loaded and shown as well
//        create and display the main program window.
//
BOOL Fail(HWND hwnd, PWSTR msg)
{
#ifndef _DEBUG
  UNREFERENCED_PARAMETER(hwnd);
#endif

  WCHAR buf[512], outbuf[1024];
  DWORD err = GetLastError();
  if(err != ERROR_SUCCESS)
  {
    if(!FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, err, 0, buf, _countof(buf), NULL))  // I3093   // I3547
    {
      wsprintfW(buf, szFail_UnknownError, err);
    }
    wsprintfW(outbuf, szFail_ErrorFormat, msg, buf, err);
  }
  else
    wsprintfW(outbuf, szFail_ErrorFormat_OtherUnknown, msg);

  size_t sz = WideCharToMultiByte(CP_UTF8, 0, outbuf, -1, NULL, 0, NULL, NULL);
  if (sz == 0) return FALSE;

  char *buffer = (char *) calloc(sz, sizeof(char));
  if (buffer == NULL) return FALSE;

  if (WideCharToMultiByte(CP_UTF8, 0, outbuf, -1, buffer, (int) sz, NULL, NULL) != 0) {
    buffer[sz - 1] = 0;
    #if defined(_M_X64)
      keyman_sentry_report_message(KEYMAN_SENTRY_LEVEL_ERROR, buffer, true);
    #endif
  }

  free(buffer);

#ifdef _DEBUG
  MessageBox(hwnd, outbuf, szTitle, MB_OK | MB_ICONERROR);
#endif

  return FALSE;
}

//
//   FUNCTION: InitInstance(HINSTANCE, int)
//
//   PURPOSE: Saves instance handle and creates main window
//
//   COMMENTS:
//
//        In this function, we save the instance handle in a global variable and
//        create the main program window.
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
  UNREFERENCED_PARAMETER(nCmdShow);

  HWND hWnd;

  hInst = hInstance; // Store instance handle in our global variable

  hWnd = CreateWindow(szWindowClass, szWindowTitle, WS_OVERLAPPEDWINDOW,
    CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, hInstance, NULL);

  if (!hWnd)
    return FALSE;

  return TRUE;
}

//
//   FUNCTION: StartKeyman(HWND)
//
//   PURPOSE: Initialise the Keyman Engine DLL
//
//   COMMENTS:
//
//        Shows an error message box if it fails, in debug mode.
//        Sends a Sentry error report on failure.  Tries to find
//        the Keyman Engine x86 windows for communications.
//
BOOL StartKeyman(HWND hWnd)
{
  if(!Keyman_ResetInitialisation())  // I3092
    return Fail(hWnd, szError_FailedToInitialise);

  if (!CreateWatcherThread(hWnd))
    return Fail(hWnd, szError_WatcherThreadFailed);

  if(!Keyman_RegisterMasterController(hwndController) ||
      !Keyman_RegisterControllerThread(dwControllerThreadId) ||
      !Keyman_RegisterControllerThread(GetCurrentThreadId()))   // I3758
    return Fail(hWnd, szError_FailedToRegisterController);

  if(!Keyman_Initialise(hWnd, FALSE))
    return Fail(hWnd, szError_FailedToInitialise);

  KeymanStarted = TRUE;

  return TRUE;
}

//
//   FUNCTION: WatcherThreadProc(LPVOID)
//
//   PURPOSE:  Watches for parent process termination
//
//   COMMENTS:
//
//        The watcher thread triggers destruction of this process
//        as soon as the parent process is terminated.
//
DWORD WINAPI WatcherThreadProc(LPVOID lpParameter) {
  HWND hwnd = (HWND)lpParameter;
  HANDLE handles[2] = { hWatcherThreadTerminateEvent, hParentProcessHandle };

  if (WaitForMultipleObjects(2, handles, FALSE, INFINITE) == WAIT_OBJECT_0 + 1) {
    // We'll post a message to our window if the parent process terminated,
    // but not during a shutdown in other circumstances, because that would
    // have been triggered by a shutdown of the process anyway.
    PostMessage(hwnd, WM_CLOSE, 0, 0);
  }
  return 0;
}

//
//   FUNCTION: CreateWatcherThread(HWND)
//
//   PURPOSE:  Creates the Watcher Thread
//
//   COMMENTS:
//
//        Creates the watcher thread which watches for
//        termination of the parent process.
//
BOOL CreateWatcherThread(HWND hWnd) {
  hWatcherThreadTerminateEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
  if (hWatcherThreadTerminateEvent == NULL)
    return FALSE;

  hWatcherThread = CreateThread(NULL, 0, WatcherThreadProc, hWnd, 0, NULL);
  if (hWatcherThread == NULL) {
    CloseHandle(hWatcherThreadTerminateEvent);
    hWatcherThreadTerminateEvent = NULL;
    return FALSE;
  }

  return TRUE;
}

//
//  FUNCTION: Shutdown()
//
//  PURPOSE:  Shutdown tasks including finalising the Keyman Engine x64 DLL
//
//
void Shutdown()
{
  if (KeymanStarted) {
    int wm_keyman = RegisterWindowMessage(L"wm_keyman");
    DWORD_PTR dwResult;

    Keyman_StartExit();  // I3092

    /* Tell all threads that it is time to exit.  This is important to do before we shutdown
       because we want to try and detach from as many processes as possible so we don't
       remain locked in memory. */

    SendMessageTimeout(HWND_BROADCAST, wm_keyman, KM_EXIT, 0, SMTO_NORMAL, 1000, &dwResult);  // I3092

    Keyman_Exit();
  }

  /* Unregister those windows that we registered earlier - don't fail on error though */

  if(hwndController != NULL) Keyman_UnregisterMasterController();   // I3758
  if(dwControllerThreadId != NULL) Keyman_UnregisterControllerThread(dwControllerThreadId);   // I3758
  Keyman_UnregisterControllerThread(GetCurrentThreadId());   // I3758

  PostQuitMessage(0);

  /* Cleanup */

  if (hWatcherThreadTerminateEvent != NULL) {
    // Both of these handles will be valid if one of them is, per CreateWatcherThread
    SetEvent(hWatcherThreadTerminateEvent);
    WaitForSingleObject(hWatcherThread, INFINITE);
    CloseHandle(hWatcherThread);
    CloseHandle(hWatcherThreadTerminateEvent);
  }
  if (hParentProcessHandle != NULL) {
    CloseHandle(hParentProcessHandle);
  }
}

//
//  FUNCTION: WndProc(HWND, UINT, WPARAM, LPARAM)
//
//  PURPOSE:  Processes messages for the main window.
//
//  WM_CREATE 	- initialise
//  WM_DESTROY	- post a quit message and return
//
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch (message)
	{
  case WM_CREATE:
    if(!StartKeyman(hWnd)) return -1;
    return 0;

  case WM_DESTROY:
    Shutdown();
		break;

	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}
