/*
  Name:             keymanx64
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
#include "keymanx64.h"   // I5136
#include "keymansentry.h"

// Forward declarations of functions included in this code module
ATOM             MyRegisterClass(HINSTANCE hInstance);
BOOL             InitInstance(HINSTANCE, int);
LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);
BOOL             Fail(HWND, PWSTR);
BOOL             UniqueInstance();

// Global constants

//#define KMC_GETLOADED 3

#define WM_USER_PlatformComm (WM_USER+103)
#define PC_CLOSE     2
#define PC_GETLOADED 3
#define PC_GETAPPLICATION 4   // I3758

// External functions in Keyman64.dll

extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_Initialise(HWND Handle, BOOL FSingleApp);
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_ResetInitialisation(void);  // I3092
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_StartExit(void);  // I3092
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_Exit(void);
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_RegisterControllerWindow(HWND hwnd);
extern "C" BOOL __declspec( dllimport ) WINAPI Keyman_UnregisterControllerWindow(HWND hwnd);

#define KM_EXIT 4
#define KM_EXITFLUSH 5

// Global variables

HINSTANCE hInst;								// current instance
HWND hwndController = NULL, hwndControllerOwner = NULL;  // keyman x86 controller window handles   // I3758

// Global strings

const PWSTR
  szWindowClass = L"Keymanx64", // Do not localize
  szWindowTitle = L"Keymanx64", // Do not localize

  szTitle = L"Keyman Engine x64",
  szError_Keymanx86NotFound = L"Keyman Engine x86 is not running.  Do not run keymanx64.exe -- it must be started by Keyman Engine x86",
  szError_Keymanx86NotFound_Comms = L"Keyman Engine x86 has closed unexpectedly.  Closing down Keyman Engine x64",
  szError_FailedToRegisterController = L"Failed to register controller window",
  szError_FailedToInitialise = L"Failed to initialise Keyman Engine x64",

  szError_ChangeWindowMessageFilter = L"Failed to prepare message filters",

  szError_CannotRunMultipleInstances = L"Only one instance of Keyman Engine x64 can run at a time",
  szError_FailedToRegister = L"Failed to register window class",
  szError_FailedToInitInstance = L"Failed to initialise application",

  szFail_UnknownError = L"Unknown error %d",
  szFail_ErrorFormat = L"%s: %s (%d)",
  szFail_ErrorFormat_OtherUnknown = L"%s: An unknown error occurred",

  szKeymanX64Mutex = L"KeymanEXEx6470",

  szWindowClass_x86_Wnd = L"TfrmKeyman7Main"; // Do not localize

//const char *szGPA_ChangeWindowMessageFilter = "ChangeWindowMessageFilter"; // Do not localize

#define KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_KEYMANX64 KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE ".keymanx64"

//
//   FUNCTION: _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int
//
//   PURPOSE: Program entry point
//
//   COMMENTS:
//
//        A mutex is used to ensure only one instance of Keyman Engine x64
//
int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);

  keyman_sentry_init(false, KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_KEYMANX64);
  keyman_sentry_setexceptionfilter();

  // We used the 'Started' event when testing Sentry integration in 14.0 alpha
  // but we don't want or need it for stable.
  // keyman_sentry_report_message(KEYMAN_SENTRY_LEVEL_INFO, "Started " KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_KEYMANX64);

  if (!wcscmp(lpCmdLine, L"-sentry-client-test-exception")) {
    keyman_sentry_test_crash();
  }

	MSG msg;

  if (!UniqueInstance()) {
    Fail(0, szError_CannotRunMultipleInstances);
    keyman_sentry_shutdown();
    return 1;
  }

  if (!MyRegisterClass(hInstance)) {
    Fail(0, szError_FailedToRegister);
    keyman_sentry_shutdown();
    return 1;
  }

  if (!InitInstance(hInstance, nCmdShow)) {
    Fail(0, szError_FailedToInitInstance);
    keyman_sentry_shutdown();
    return 1;
  }

	// Main message loop:
	while (GetMessage(&msg, NULL, 0, 0))
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

  keyman_sentry_shutdown();

	return (int) msg.wParam;
}

BOOL UniqueInstance()
{
  HANDLE hMutex = CreateMutex(NULL, FALSE, szKeymanX64Mutex);
  if(!hMutex) return FALSE;

  switch(WaitForSingleObject(hMutex, 0))
  {
    case WAIT_ABANDONED:
    case WAIT_OBJECT_0:
      return TRUE;
    case WAIT_FAILED:
    case WAIT_TIMEOUT:
      return FALSE;
  }
  return FALSE;
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
  WCHAR buf[512], outbuf[1024];
  DWORD err = GetLastError();
  if(err != 0)
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
    keyman_sentry_report_message(KEYMAN_SENTRY_LEVEL_ERROR, buffer, true);
  }

  free(buffer);

  MessageBox(hwnd, outbuf, szTitle, MB_OK | MB_ICONERROR);

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
//        Shows an error message box if it fails.  Tries to find
//        the Keyman Engine x86 windows for communications
//
BOOL StartKeyman(HWND hWnd)
{
  if(!ChangeWindowMessageFilter(WM_USER_PlatformComm, MSGFLT_ADD))
    return Fail(hWnd, szError_ChangeWindowMessageFilter);

  if(!Keyman_ResetInitialisation())  // I3092
    return Fail(hWnd, szError_FailedToInitialise);

  hwndController = FindWindow(szWindowClass_x86_Wnd, NULL);   // I3758
  if(hwndController == NULL)
    return Fail(hWnd, szError_Keymanx86NotFound);

  DWORD_PTR dwResult;

  if(SendMessageTimeout(hwndController, WM_USER_PlatformComm, PC_GETAPPLICATION, 0, SMTO_BLOCK, 5000, &dwResult) == 0)   // I3758
    return Fail(hWnd, szError_Keymanx86NotFound);

  hwndControllerOwner = (HWND) dwResult;   // I3758
  if(hwndControllerOwner == NULL)
    return Fail(hWnd, szError_Keymanx86NotFound);

  if(!Keyman_RegisterControllerWindow(hwndControllerOwner) || !Keyman_RegisterControllerWindow(hwndController))   // I3758
    return Fail(hWnd, szError_FailedToRegisterController);

  if(!Keyman_Initialise(hWnd, FALSE))
    return Fail(hWnd, szError_FailedToInitialise);

  return TRUE;
}

//
//  FUNCTION: SendPlatformComms32(WPARAM, LPARAM)
//
//  PURPOSE:  Post a command to Keyman Engine x86
//
BOOL SendPlatformComms32(WPARAM wParam, LPARAM lParam)
{
  // Search again; lets us reconnect if
  // Keyman.exe crashes and is retarted
  HWND hwndLocalController = FindWindow(szWindowClass_x86_Wnd, NULL);
  if(hwndLocalController == NULL)
  {
    MessageBox(0, szError_Keymanx86NotFound_Comms, szTitle, MB_OK);
    return FALSE;
  }
  PostMessage(hwndLocalController, WM_USER_PlatformComm, wParam, lParam);
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
  int wm_keyman = RegisterWindowMessage(L"wm_keyman");
  DWORD_PTR dwResult;

  Keyman_StartExit();  // I3092

  /* Tell all threads that it is time to exit.  This is important to do before we shutdown
    because we have got a per-thread keyboard hook that needs to be detached before we
    lose our message hooks. */

  SendMessageTimeout(HWND_BROADCAST, wm_keyman, KM_EXIT, 0, SMTO_NORMAL, 1000, &dwResult);  // I3092

  Keyman_Exit();

  /* Unregister those windows that we registered earlier - don't fail on error though */
  if(hwndController != NULL) Keyman_UnregisterControllerWindow(hwndController);   // I3758
  if(hwndControllerOwner != NULL) Keyman_UnregisterControllerWindow(hwndControllerOwner);   // I3758

  PostQuitMessage(0);
}

//
//  FUNCTION: WndProc(HWND, UINT, WPARAM, LPARAM)
//
//  PURPOSE:  Processes messages for the main window.
//
//  WM_CREATE 	- initialise
//  WM_DESTROY	- post a quit message and return
//  WM_USER_PlatformComm - process commands from Keyman Engine x86
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

  case WM_USER_PlatformComm:
    switch(wParam)
    {
      case PC_CLOSE: PostMessage(hWnd, WM_CLOSE, 0, 0); break;
    }

	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}
