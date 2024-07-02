// testhost.cpp : Defines the entry point for the application.
//

#include "framework.h"
#include "testhost.h"
#include <string>
#include <vector>
#include <iostream>
#include <filesystem>
namespace fs = std::filesystem;

#define MAX_LOADSTRING 100

// Global Variables:
HINSTANCE hInst;                                // current instance
HWND hWndEdit;
HWND hWnd;
WCHAR szTitle[MAX_LOADSTRING];        // The title bar text
WCHAR szWindowClass[MAX_LOADSTRING];            // the main window class name

// Forward declarations of functions included in this code module:
ATOM                MyRegisterClass(HINSTANCE hInstance);
BOOL                InitInstance(HINSTANCE, int);
LRESULT CALLBACK    WndProc(HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK    About(HWND, UINT, WPARAM, LPARAM);
void SizeEditWindow();
void StartKeyman();
void StopKeyman();
void Fail(PCWSTR message);

  int APIENTRY wWinMain(_In_ HINSTANCE hInstance,
                     _In_opt_ HINSTANCE hPrevInstance,
                     _In_ LPWSTR    lpCmdLine,
                     _In_ int       nCmdShow)
{
    UNREFERENCED_PARAMETER(hPrevInstance);
    UNREFERENCED_PARAMETER(lpCmdLine);

    // TODO: Place code here.

    // Initialize global strings
    LoadStringW(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING);
    LoadStringW(hInstance, IDC_TESTHOST, szWindowClass, MAX_LOADSTRING);
    MyRegisterClass(hInstance);

    // Perform application initialization:
    if (!InitInstance (hInstance, nCmdShow))
    {
        return FALSE;
    }

    HACCEL hAccelTable = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDC_TESTHOST));

    MSG msg;

    StartKeyman();

    // Main message loop:
    while (GetMessage(&msg, nullptr, 0, 0))
    {
        if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg))
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }

    StopKeyman();

    return (int) msg.wParam;
}



//
//  FUNCTION: MyRegisterClass()
//
//  PURPOSE: Registers the window class.
//
ATOM MyRegisterClass(HINSTANCE hInstance)
{
    WNDCLASSEXW wcex;

    wcex.cbSize = sizeof(WNDCLASSEX);

    wcex.style          = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc    = WndProc;
    wcex.cbClsExtra     = 0;
    wcex.cbWndExtra     = 0;
    wcex.hInstance      = hInstance;
    wcex.hIcon          = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_TESTHOST));
    wcex.hCursor        = LoadCursor(nullptr, IDC_ARROW);
    wcex.hbrBackground  = (HBRUSH)(COLOR_WINDOW+1);
    wcex.lpszMenuName   = MAKEINTRESOURCEW(IDC_TESTHOST);
    wcex.lpszClassName  = szWindowClass;
    wcex.hIconSm        = LoadIcon(wcex.hInstance, MAKEINTRESOURCE(IDI_SMALL));

    return RegisterClassExW(&wcex);
}

//
//   FUNCTION: InitInstance(HINSTANCE, int)
//
//   PURPOSE: Saves instance handle and creates main window
//
//   COMMENTS:
//
//        In this function, we save the instance handle in a global variable and
//        create and display the main program window.
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
   hInst = hInstance; // Store instance handle in our global variable
   hWnd = CreateWindowW(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW,
      CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, nullptr, nullptr, hInstance, nullptr);

   if (!hWnd)
   {
      return FALSE;
   }


   hWndEdit = CreateWindowW(L"EDIT", L"", WS_CHILDWINDOW | ES_MULTILINE | WS_VISIBLE, 0, 0, 10, 10, hWnd, nullptr, hInstance, nullptr);
   SizeEditWindow();

   ShowWindow(hWnd, nCmdShow);
   UpdateWindow(hWnd);

   return TRUE;
}

void
SizeEditWindow() {
  RECT rc;
  GetClientRect(hWnd, &rc);
  SetWindowPos(hWndEdit, 0, 0, 0, rc.right, rc.bottom, SWP_NOOWNERZORDER);
}

//
//  FUNCTION: WndProc(HWND, UINT, WPARAM, LPARAM)
//
//  PURPOSE: Processes messages for the main window.
//
//  WM_COMMAND  - process the application menu
//  WM_PAINT    - Paint the main window
//  WM_DESTROY  - post a quit message and return
//
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    switch (message)
    {
    case WM_SIZE:
      SizeEditWindow();
      break;
    case WM_COMMAND:
        {
            int wmId = LOWORD(wParam);
            // Parse the menu selections:
            switch (wmId)
            {
            case IDM_ABOUT:
                DialogBox(hInst, MAKEINTRESOURCE(IDD_ABOUTBOX), hWnd, About);
                break;
            case IDM_EXIT:
                DestroyWindow(hWnd);
                break;
            default:
                return DefWindowProc(hWnd, message, wParam, lParam);
            }
        }
        break;
    case WM_PAINT:
        {
            PAINTSTRUCT ps;
            HDC hdc = BeginPaint(hWnd, &ps);
            // TODO: Add any drawing code that uses hdc here...
            EndPaint(hWnd, &ps);
        }
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    case WM_SETFOCUS:
        SetFocus(hWndEdit);
        break;
    default:
        return DefWindowProc(hWnd, message, wParam, lParam);
    }
    return 0;
}

// Message handler for about box.
INT_PTR CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
    UNREFERENCED_PARAMETER(lParam);
    switch (message)
    {
    case WM_INITDIALOG:
        return (INT_PTR)TRUE;

    case WM_COMMAND:
        if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
        {
            EndDialog(hDlg, LOWORD(wParam));
            return (INT_PTR)TRUE;
        }
        break;
    }
    return (INT_PTR)FALSE;
}

HMODULE hlibKeyman32 = nullptr;

/* From appint.h */

typedef struct
{
  int ItemType;
  DWORD dwData;
} APPACTIONQUEUEITEM;

// QueueAction ItemTypes
#define QIT_VKEYDOWN	0
#define QIT_VKEYUP		1
#define QIT_VSHIFTDOWN	2
#define QIT_VSHIFTUP	3
#define QIT_CHAR		4
#define QIT_DEADKEY		5
#define QIT_BELL		6
#define QIT_BACK		7

#define QVK_EXTENDED 0x00010000 // Flag for QIT_VKEYDOWN to indicate an extended key
#define QVK_KEYMASK  0x0000FFFF
#define QVK_FLAGMASK 0xFFFF0000



typedef BOOL(WINAPI* CUSTOMPOSTKEYCALLBACKPROC)(APPACTIONQUEUEITEM* Queue, int QueueSize);

typedef BOOL(WINAPI* KEYMAN_INITIALISE)(HWND hwnd, BOOL singleApp);
typedef BOOL(WINAPI* KEYMAN_EXIT)();
typedef BOOL(WINAPI* KEYMAN_REGISTERMASTERCONTROLLER)(HWND hwnd);
typedef void(WINAPI* SETCUSTOMPOSTKEYCALLBACK)(CUSTOMPOSTKEYCALLBACKPROC proc);

KEYMAN_INITIALISE Keyman_Initialise = nullptr;
KEYMAN_EXIT Keyman_Exit             = nullptr;
KEYMAN_REGISTERMASTERCONTROLLER Keyman_RegisterMasterController = nullptr;
SETCUSTOMPOSTKEYCALLBACK SetCustomPostKeyCallback = nullptr;

BOOL WINAPI PostKeyCallback(APPACTIONQUEUEITEM* Queue, int QueueSize) {
  // Note: copied from aiWin2000Unicode and dumbed down for test purposes
  for (int n = 0; n < QueueSize; n++) {
    switch (Queue[n].ItemType) {
    case QIT_VKEYDOWN:
      if ((Queue[n].dwData & QVK_KEYMASK) == 0x05) Queue[n].dwData = (Queue[n].dwData & QVK_FLAGMASK) | VK_RETURN; // I649  // I3438
      if ((Queue[n].dwData & QVK_KEYMASK) <= 255) {
        PostMessage(hWndEdit, WM_KEYDOWN, Queue[n].dwData & 0xFF, 0); // TODO: lparam
        //pInputs[i].ki.wScan = SCAN_FLAG_KEYMAN_KEY_EVENT;
        //pInputs[i].ki.dwFlags = ((Queue[n].dwData & QVK_EXTENDED) ? KEYEVENTF_EXTENDEDKEY : 0);
      }
      break;
    case QIT_VKEYUP:
      if ((Queue[n].dwData & QVK_KEYMASK) == 0x05) Queue[n].dwData = (Queue[n].dwData & QVK_FLAGMASK) | VK_RETURN; // I649  // I3438
      if ((Queue[n].dwData & QVK_KEYMASK) <= 255) {
        PostMessage(hWndEdit, WM_KEYUP, Queue[n].dwData & 0xFF, 0); // TODO: lparam
        //pInputs[i].ki.wScan = SCAN_FLAG_KEYMAN_KEY_EVENT;
        //pInputs[i].ki.dwFlags = KEYEVENTF_KEYUP | ((Queue[n].dwData & QVK_EXTENDED) ? KEYEVENTF_EXTENDEDKEY : 0);  // I3438
      }
      break;
    case QIT_VSHIFTDOWN:
      break;
    case QIT_VSHIFTUP:
      break;
    case QIT_CHAR:
      // TODO: surrogate pairs
      PostMessage(hWndEdit, WM_CHAR, (WORD)Queue[n].dwData, 0);
      break;
    case QIT_DEADKEY:
      break;
    case QIT_BELL:
      MessageBeep(MB_ICONASTERISK);
      break;
    case QIT_BACK:
#define BK_DEADKEY		1
      if (Queue[n].dwData & BK_DEADKEY) break;
      PostMessage(hWndEdit, WM_CHAR, 8, 0);
      break;
    }
  }

  return TRUE;
}


void
Fail(PCWSTR message) {
  const DWORD bufferLength = 1024;
  std::vector<wchar_t> err(bufferLength);
  std::wstring buf;
  DWORD dwError = GetLastError();
  if (dwError == ERROR_SUCCESS) {
    buf = std::wstring(message) + L": no error code was returned"; // Don't append "The operation completed successfully."
  } else if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nullptr, dwError, 0, err.data(), bufferLength, nullptr) != 0) {
    std::wstring errb(err.data());
    buf = std::wstring(message) + L":" + err.data() + L"[" + std::to_wstring(dwError) + L"]";
  } else {
    buf = std::wstring(message) + L": Unknown error [" + std::to_wstring(dwError) + L"]";
  }
  MessageBox(0, buf.c_str(), L"Error starting Keyman", MB_OK | MB_ICONERROR);
}

#ifdef _WIN64
#define KEYMAN32 "keyman64.dll"
#define KEYMAN32_DEBUG "keyman32" / "bin" / "x64" / "Debug" / "keyman64.dll"
#define KEYMAN32_RELEASE "keyman32" / "bin" / "x64" / "Release" / "keyman64.dll"
#else
#define KEYMAN32 "keyman32.dll"
#define KEYMAN32_DEBUG "keyman32" / "bin" / "Win32" / "Debug" / "keyman32.dll"
#define KEYMAN32_RELEASE "keyman32" / "bin" / "Win32" / "Release" / "keyman32.dll"
#endif

bool
beginsWith(std::wstring strHaystack, std::wstring strNeedle) {
  std::transform(strHaystack.begin(), strHaystack.end(), strHaystack.begin(), ::tolower);
  std::transform(strNeedle.begin(), strNeedle.end(), strNeedle.begin(), ::tolower);
  return strHaystack.rfind(strNeedle, 0) == 0;
}

void
StartKeyman() {
  const int bufferLength = MAX_PATH + 1;
  std::vector<wchar_t> keyman_root(bufferLength), app_path(bufferLength);
  fs::path keyman32 = "";

  if (GetEnvironmentVariable(L"KEYMAN_ROOT", keyman_root.data(), bufferLength)) {
    GetModuleFileName(nullptr, app_path.data(), bufferLength);
    if (beginsWith(std::wstring(app_path.data()), std::wstring(keyman_root.data()))) {
      // We are running under KEYMAN_ROOT
      keyman32 = fs::path(keyman_root.data()) / "windows" / "src" / "engine" / KEYMAN32_DEBUG;
      if (!fs::exists(keyman32)) {
        keyman32 = fs::path(keyman_root.data()) / "windows" / "src" / "engine" / KEYMAN32_RELEASE;

        if (!fs::exists(keyman32))
          keyman32 = "";
      }
    }
  }

  if (keyman32 == "") {
    // TODO: Get install folder from Windows
    keyman32 = fs::path("C:\\Program Files (x86)\\Common Files\\Keyman\\Keyman Engine") / KEYMAN32;
    if (!fs::exists(keyman32)) {
      Fail(L"Failed to find keyman32.dll");
    }
  }

    hlibKeyman32 = LoadLibrary(keyman32.c_str());
  if (!hlibKeyman32) {
    Fail(L"Failed to load keyman32.dll");
    return;
  }

  Keyman_Initialise = (KEYMAN_INITIALISE)GetProcAddress(hlibKeyman32, "Keyman_Initialise");
  Keyman_Exit       = (KEYMAN_EXIT)GetProcAddress(hlibKeyman32, "Keyman_Exit");
  Keyman_RegisterMasterController =
      (KEYMAN_REGISTERMASTERCONTROLLER)GetProcAddress(hlibKeyman32, "Keyman_RegisterMasterController");
  SetCustomPostKeyCallback = (SETCUSTOMPOSTKEYCALLBACK)GetProcAddress(hlibKeyman32, "SetCustomPostKeyCallback");
  if (!Keyman_Initialise || !Keyman_Exit || !Keyman_RegisterMasterController || !SetCustomPostKeyCallback) {
    Keyman_Exit = nullptr;
    StopKeyman();
    Fail(L"Failed to get proc addresses");
    return;
  }
  if (!Keyman_Initialise(hWnd, TRUE)) {
    Keyman_Exit = nullptr;
    StopKeyman();
    Fail(L"Failed to initialise Keyman Engine");
    return;
  }
  SetCustomPostKeyCallback(PostKeyCallback);
}

void
StopKeyman() {
  if (Keyman_Exit) {
    Keyman_Exit();
  }
  if (hlibKeyman32) {
    FreeLibrary(hlibKeyman32);
  }
  FreeLibrary(hlibKeyman32);
  hlibKeyman32                    = nullptr;
  Keyman_Initialise               = nullptr;
  Keyman_Exit                     = nullptr;
  Keyman_RegisterMasterController = nullptr;
  SetCustomPostKeyCallback        = nullptr;
  hlibKeyman32 = nullptr;
}
