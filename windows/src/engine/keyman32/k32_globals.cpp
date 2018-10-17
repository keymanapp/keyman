/*
  Name:             k32_globals
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    6 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Add hotkeys globals
                    14 Sep 2006 - mcdurdin - Add IsKeymanSystemWindow and UpdateActiveWindows functions
                    04 Dec 2006 - mcdurdin - Add PostProcessControllers and IsControllerProcess functions
                    16 May 2007 - mcdurdin - I819 - Test if Keyman is functioning - messages
                    05 Nov 2007 - mcdurdin - I1087 - Add language switching hotkeys for Desktop Pro
                    20 Jul 2008 - mcdurdin - I1412 - Keyman Engine starts multiple times in Tutorial
                    16 Jan 2009 - mcdurdin - I1552 - Check if controller windows are still valid before posting/sending messages (e.g. after Keyman exits abnormally)
                    16 Jan 2009 - mcdurdin - I1584 - SendMasterControllers now uses SendMessageTimeout
                    30 Jan 2009 - mcdurdin - I1835 - Improve refresh performance
                    09 Mar 2009 - mcdurdin - I1888 - Keyman Engine can crash with some multithreaded applications
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I1455 - Improve stability of keyboard switching, remove unneeded hooks
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    11 Dec 2009 - mcdurdin - I934 - Add a flag for shutdown so we know when to remove hooks
                    11 Dec 2009 - mcdurdin - I823 - Use GetGUIThreadInfo
                    11 Dec 2009 - mcdurdin - I1455 - Move IsSysTrayWindow to k32_globals
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    29 Mar 2010 - mcdurdin - I1089 - One keyboard - all apps
                    06 Apr 2010 - mcdurdin - I2271 - Select Keyboard tidy up
                    09 Apr 2010 - mcdurdin - I2292 - Keyman was not initialising for multi-thread apps already in memory
                    19 Apr 2010 - mcdurdin - I2297 - Use global cache to remember last active keyboard for all apps
                    04 May 2010 - mcdurdin - I2297 - Work on cross-process sharing of keyboard cache information
                    04 May 2010 - mcdurdin - I2352 - Debug logging not working reliably in some applications
                    04 May 2010 - mcdurdin - I2354 - Race conditions with controller windows
                    28 Jun 2010 - mcdurdin - I2443 - Don't test if system tray is a Keyman-system window (for system-wide language switching)
                    30 Nov 2010 - mcdurdin - I2543 - Support switching to TSF TIPs
                    31 Jan 2011 - mcdurdin - I2686 - Remove assertion in Globals_InitThread to prevent errors on thread/process termination
                    18 Feb 2011 - mcdurdin - I2722 - Fix crash with Adobe Reader and its inability to see Keyman's USER handles when in Sandbox
                    25 Mar 2011 - mcdurdin - I2814 - Global lock was not released in RegisterControllerWindow in failure cases
                    03 May 2011 - mcdurdin - I2866 - If Keyman tray icon is hidden, then Keyman does not switch focus back to active app
                    19 Aug 2011 - mcdurdin - I3040 - keyman32.dll can crash if advapi32.dll is not loaded when first called from a process
                    03 Oct 2011 - mcdurdin - I3092 - Keyman Engine does not restart nicely when shutdown uncleanly
                    02 Dec 2011 - mcdurdin - I3158 - Keyman Engine can crash other applications when multiple OEM products are started with a _tcscpy assertion
                    24 Jan 2012 - mcdurdin - I3143 - Report more useful information when Keyman fails to start
                    02 Feb 2012 - mcdurdin - I3183 - Report clearer error when failure to register controller window
                    04 Nov 2012 - mcdurdin - I3523 - V9.0 - Merge of I3143 - Report more useful information when Keyman fails to start
                    04 Nov 2012 - mcdurdin - I3524 - V9.0 - Merge of I3158 - Keyman Engine can crash other applications when multiple OEM products are started, with a _tcscpy assertion
                    04 Nov 2012 - mcdurdin - I3530 - V9.0 - Merge of I3183 - Report clearer error when failure to register controller window
                    28 Nov 2012 - mcdurdin - I3594 - V9.0 - Remove old SelectKeyboard code and related messages
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    06 Mar 2014 - mcdurdin - I4124 - V9.0 - Language switch dialog is not working in v9
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    25 Sep 2014 - mcdurdin - I4412 - V9.0 - Character Map needs to insert characters using SendInput
                    06 Feb 2015 - mcdurdin - I4583 - V9.0 - Remove altgr lookup test from keyman32 and put it into the registry
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
*/
#include "pch.h"
#include "security.h"

/***************************************************************************/
/*                                                                         */
/* Thread-local storage                                                    */
/*                                                                         */
/***************************************************************************/

//__declspec(align(4)) LONG RefreshTag_Process = 0;
//__declspec(align(4)) LONG FInRefreshKeyboards = 0;

UINT 
  //TODO: consolidate these messages -- they are probably not all required now
  wm_keyman = 0,						// user message - ignore msg   // I3594
  wm_keyman_keyevent = 0,   // for serialized input
	wm_kmdebug = 0,						//  " "  "  "   - debugging

	wm_kmmessage = 0,					// message to Keyman window   // I4412
	wm_keymankeydown = 0,
	wm_keymankeyup = 0,
  wm_keyman_grabwindowproc = 0,
	wm_keyman_refresh = 0,
	wm_kmgetactivekeymanid = 0,
	wm_keymanshift = 0,
	wm_keyman_control = 0,
	wm_keyman_control_internal = 0,

	wm_keymanim_close = 0,
	wm_keymanim_contextchanged = 0,
	
	wm_test_keyman_functioning = 0;

//HWND hwndIM = 0, hwndIMOldFocus = 0;

BOOL
  flag_ShouldSerializeInput = TRUE;

static DWORD dwTlsIndex = TLS_OUT_OF_INDEXES;
static CRITICAL_SECTION csGlobals;

PKEYMAN64THREADDATA Globals_InitThread()
{
  EnterCriticalSection(&csGlobals);
  
  PKEYMAN64THREADDATA lpvData;

  if(Globals::get_Keyman_Shutdown())  // I3092
  {
    // Thread or process is uninitialising, probably receiving a message to a window that is being destroyed
    SetLastError(ERROR_KEYMAN_SHUTTING_DOWN);  // I3143   // I3523
    lpvData = NULL;
  }
  else if(dwTlsIndex == TLS_OUT_OF_INDEXES)  // I2686
  {
    // Thread or process is uninitialising, probably receiving a message to a window that is being destroyed
    SetLastError(ERROR_KEYMAN_TLS_OUT_OF_INDEXES);  // I3143   // I3523
    lpvData = NULL;
  }
  else
  {
    lpvData = (PKEYMAN64THREADDATA) LocalAlloc(LPTR, sizeof(KEYMAN64THREADDATA));
    if(lpvData != NULL)
    {
        TlsSetValue(dwTlsIndex, lpvData);
        lpvData->ActiveKeymanID = KEYMANID_NONKEYMAN;
        lpvData->CurrentAddin = -1;
    }
    else SetLastError(ERROR_KEYMAN_MEMORY_ALLOCATION_FAILED);  // I3143   // I3523
  }
  LeaveCriticalSection(&csGlobals);

#ifdef USE_SERIALKEYEVENTSERVER
  ISerialKeyEventClient::Startup();
#endif
  return lpvData;
}

#ifdef DEBUG_PROCMON_LOGGING
extern BOOL CloseProcMonHandle();
#endif

void Globals_UninitThread()
{
  if(!Globals_ProcessInitialised()) return;

  CloseTSF();   // I3933
#ifdef DEBUG_PROCMON_LOGGING
  CloseProcMonHandle();
#endif

#ifdef USE_SERIALKEYEVENTSERVER
  ISerialKeyEventClient::Startup();
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (_td) {
    delete _td->pSerialKeyEventClient;
  }
  CloseThreadSharedBufferManager();
#endif

  EnterCriticalSection(&csGlobals);
  if(dwTlsIndex != TLS_OUT_OF_INDEXES)
  {
    LPVOID lpvData = TlsGetValue(dwTlsIndex);
    if(lpvData != NULL)
    {
      LocalFree((HLOCAL) lpvData);
    }
    TlsSetValue(dwTlsIndex, NULL);
  }
  LeaveCriticalSection(&csGlobals);
}

BOOL Globals_InitProcess()
{
  InitializeCriticalSection(&csGlobals);

  EnterCriticalSection(&csGlobals);
  if((dwTlsIndex = TlsAlloc()) == TLS_OUT_OF_INDEXES)
  {
    LeaveCriticalSection(&csGlobals);
    return FALSE;
  }
  Globals_InitThread();
  LeaveCriticalSection(&csGlobals);
  return TRUE;
}

void Globals_UninitProcess()
{
  EnterCriticalSection(&csGlobals);
  Globals_UninitThread();
  TlsFree(dwTlsIndex);
  dwTlsIndex = TLS_OUT_OF_INDEXES;
  LeaveCriticalSection(&csGlobals);
}

PKEYMAN64THREADDATA ThreadGlobals()
{
  EnterCriticalSection(&csGlobals);
  
  PKEYMAN64THREADDATA lpvData = NULL;
  if(dwTlsIndex != TLS_OUT_OF_INDEXES)
  {
    lpvData = (PKEYMAN64THREADDATA) TlsGetValue(dwTlsIndex);
  }

  LeaveCriticalSection(&csGlobals);
  return lpvData;
}

BOOL Globals_ProcessInitialised()
{
  EnterCriticalSection(&csGlobals);
  BOOL f = (dwTlsIndex != TLS_OUT_OF_INDEXES && TlsGetValue(dwTlsIndex) != NULL);
  LeaveCriticalSection(&csGlobals);
  return f;
}

/***************************************************************************/
/*                                                                         */
/* System-wide storage                                                     */
/*                                                                         */
/***************************************************************************/

#pragma data_seg(".SHARDATA")

//static INI     // I3158   // I3524
//    f_Ini = {0};								// KEYMAN.INI options

static HHOOK 
    f_hhookGetMessage = NULL,				// GETMESSAGE hook handle
    f_hhookCallWndProc = NULL;			// CALLWNDPROC hook handle

#ifndef _WIN64
static HHOOK
    f_hhookLowLevelKeyboardProc = NULL;   // I4124
#endif

static HWND
	f_hLastActiveWnd = 0,
	f_hLastFocusWnd = 0;
										// of keyboard icon, etc.
static UINT
  f_WindowsVersion = 0;			// Windows Version

static BOOL 
    f_Keyman_Initialised = FALSE,
    f_Keyman_Shutdown = FALSE;

static DWORD
	f_FSingleThread = 0,
  f_InitialisingThread = 0;   // I4326

//static DWORD
//	f_ActiveKeymanID = 0;

static HWND
	f_hwndIM = 0;

static BOOL
	f_hwndIMAlways = 0;

#define MAXCONTROLLERS 128

static HWND
	f_ControllerWindow[MAXCONTROLLERS] = {0};

static int
	f_MaxControllers = 0;

static BOOL
  f_SimulateAltGr = FALSE,   // I4583
  f_MnemonicDeadkeyConversionMode = FALSE;   // I4583   // I4552

static char
  f_BaseKeyboard[16] = "";   // I4583

static wchar_t
  f_BaseKeyboardName[256] = L"",   // I4583
  f_BaseKeyboardNameAlt[64] = L"";   // I4583

__declspec(align(4)) static LONG
  f_RefreshTag = 0;

static BOOL 
  f_debug_KeymanLog = FALSE, 
  f_debug_ToConsole = FALSE;

#pragma data_seg()
/***************************************************************************/

DWORD	//static 
	f_ShiftState = 0;

HANDLE f_hLockMutex = 0;

//INI   *Globals::Ini()                 { return &f_Ini;                }  // I3158   // I3524

HHOOK *Globals::hhookGetMessage()     { return &f_hhookGetMessage;    }
HHOOK *Globals::hhookCallWndProc()    { return &f_hhookCallWndProc;   }

#ifndef _WIN64
HHOOK *Globals::hhookLowLevelKeyboardProc()    { return &f_hhookLowLevelKeyboardProc;   }   // I4124
#endif

HWND  *Globals::hLastActiveWnd()	    { return &f_hLastActiveWnd;     }
HWND  *Globals::hLastFocusWnd()	      { return &f_hLastFocusWnd;      }

HWND  *Globals::hwndIM()              { return &f_hwndIM;             }
BOOL  *Globals::hwndIMAlways()        { return &f_hwndIMAlways;       }

UINT  *Globals::WindowsVersion()      { return &f_WindowsVersion;     }

BOOL  *Globals::Keyman_Initialised()  { return &f_Keyman_Initialised; }
BOOL  *Globals::Keyman_Shutdown()     { return &f_Keyman_Shutdown; }

DWORD *Globals::FSingleThread()       { return &f_FSingleThread;      }
DWORD *Globals::InitialisingThread()  { return &f_InitialisingThread; }   // I4326
//DWORD *Globals::ActiveKeymanID()      { return &f_ActiveKeymanID;     }

DWORD *Globals::ShiftState()          { return &f_ShiftState;         }

LONG *Globals::RefreshTag()           { return &f_RefreshTag;         }

HHOOK Globals::get_hhookCallWndProc()   { return f_hhookCallWndProc;   }
HHOOK Globals::get_hhookGetMessage()    { return f_hhookGetMessage;    }

#ifndef _WIN64
HHOOK Globals::get_hhookLowLevelKeyboardProc()   { return f_hhookLowLevelKeyboardProc;   }   // I4124
#endif

HWND  Globals::get_hLastActiveWnd()     { return f_hLastActiveWnd;     }
HWND  Globals::get_hLastFocusWnd()      { return f_hLastFocusWnd;      }

UINT  Globals::get_WindowsVersion()     { return f_WindowsVersion;     }

BOOL  Globals::get_Keyman_Initialised() { return f_Keyman_Initialised; }
BOOL  Globals::get_Keyman_Shutdown()    { return f_Keyman_Shutdown;    }

DWORD Globals::get_FSingleThread()      { return f_FSingleThread;      }
DWORD Globals::get_InitialisingThread() { return f_InitialisingThread; }   // I4326

//DWORD Globals::get_ActiveKeymanID()  		{ return f_ActiveKeymanID;	   }

DWORD Globals::get_ShiftState()         { return f_ShiftState;         }

char *Globals::get_BaseKeyboard()         { return f_BaseKeyboard;          }   // I4583
BOOL Globals::get_SimulateAltGr()         { return f_SimulateAltGr;         }   // I4583

wchar_t *Globals::get_BaseKeyboardName()     { return f_BaseKeyboardName;      }   // I4583
wchar_t *Globals::get_BaseKeyboardNameAlt()  { return f_BaseKeyboardNameAlt;   }   // I4583

BOOL Globals::get_MnemonicDeadkeyConversionMode() { return f_MnemonicDeadkeyConversionMode; }   // I4583   // I4552

BOOL Globals::get_debug_KeymanLog() { return f_debug_KeymanLog; }
BOOL Globals::get_debug_ToConsole() { return f_debug_ToConsole; }

void Globals::SetBaseKeyboardName(wchar_t *baseKeyboardName, wchar_t *baseKeyboardNameAlt) {   // I4583
  wcscpy_s(f_BaseKeyboardName, baseKeyboardName);
  wcscpy_s(f_BaseKeyboardNameAlt, baseKeyboardNameAlt);
}

void Globals::SetBaseKeyboardFlags(char *baseKeyboard, BOOL simulateAltGr, BOOL mnemonicDeadkeyConversionMode) {   // I4583   // I4552
  strcpy_s(f_BaseKeyboard, baseKeyboard);
  f_SimulateAltGr = simulateAltGr;
  f_MnemonicDeadkeyConversionMode = mnemonicDeadkeyConversionMode;
}

BOOL Globals::Lock()
{
  if(f_hLockMutex == 0)
  {
    f_hLockMutex = CreateMutex(NULL, FALSE, "Tavultesoft_KeymanEngine_GlobalLock");
    if(f_hLockMutex == 0) return FALSE;
  }
    
  if(WaitForSingleObject(f_hLockMutex, 1000) == WAIT_OBJECT_0) return TRUE;  // I3183   // I3530
  SetLastError(ERROR_TIMEOUT);  // I3183   // I3530
  return FALSE;  // I3183   // I3530
}

BOOL Globals::InitHandles()  // I3040
{
  if(f_hLockMutex == 0) 
    f_hLockMutex = CreateMutex(NULL, FALSE, "Tavultesoft_KeymanEngine_GlobalLock");
  if(f_hLockMutex == 0) return FALSE;
  SetObjectToLowIntegrity(f_hLockMutex);
  GrantPermissionToAllApplicationPackages(f_hLockMutex, MUTEX_ALL_ACCESS);

  return TRUE;
}

BOOL Globals::Unlock()
{
  return ReleaseMutex(f_hLockMutex);
}

BOOL Globals::ResetControllers()  // I3092
{
  SendDebugMessage(0, sdmGlobal, 0, "Globals::ResetControllers");

  if(!Globals::Lock()) return FALSE;

  f_MaxControllers = 0;
  /*   I3158   // I3524
  f_Ini.ContextStackSize = 0;
  f_Ini.MaxKeyboards = 0;
  f_Ini.MsgStackSize = 0;
  */
  f_hhookCallWndProc = NULL;
  f_hhookGetMessage = NULL;
#ifndef _WIN64
  f_hhookLowLevelKeyboardProc = NULL;   // I4124
#endif
  f_hLastActiveWnd = 0;
  f_hLastFocusWnd = 0;
  f_WindowsVersion = 0;
  f_Keyman_Shutdown = FALSE;
  f_Keyman_Initialised = FALSE;
  f_FSingleThread = FALSE;
  f_hwndIM = 0;
	f_hwndIMAlways = 0;
  f_RefreshTag = 0;

  Globals::Unlock();

  SendDebugMessage(0, sdmGlobal, 0, "Globals::ResetControllers EXIT");
  return TRUE;
}

BOOL Globals::CheckControllers()
{
  return TRUE;  // I2722
}

BOOL Globals::IsControllerWindow(HWND hwnd)
{
  if(!Lock()) return FALSE;
  CheckControllers();
	for(int i = 0; i < f_MaxControllers; i++)
		if(hwnd == f_ControllerWindow[i])
    {
      Unlock();
      return TRUE;
    }

  Unlock();
	return FALSE;
}

void Globals::PostProcessControllers(UINT msg, WPARAM wParam, LPARAM lParam)
{
  if(!Lock()) return;
  CheckControllers();
  DWORD pid;
	for(int i = 0; i < f_MaxControllers; i++)
  {
    GetWindowThreadProcessId(f_ControllerWindow[i], &pid);
    if(pid == GetCurrentProcessId()) 
      PostMessage(f_ControllerWindow[i], msg, wParam, lParam);
  }
  Unlock();
}

BOOL Globals::IsControllerProcess()
{
  if(!Lock()) return FALSE;
  CheckControllers();
  DWORD pid;
	for(int i = 0; i < f_MaxControllers; i++)
  {
    GetWindowThreadProcessId(f_ControllerWindow[i], &pid);
    if(pid == GetCurrentProcessId())
    { 
      Unlock();
      return TRUE;
    }
  }
  Unlock();
  return FALSE;
}

void Globals::PostControllers(UINT msg, WPARAM wParam, LPARAM lParam)
{
  if(!Lock()) return;
  CheckControllers();
	 SendDebugMessageFormat(GetFocus(), sdmGlobal, 0, "PostControllers [%x : %x, %x]", msg, wParam, lParam);
	for(int i = 0; i < f_MaxControllers; i++) {
  	if(!PostMessage(f_ControllerWindow[i], msg, wParam, lParam)) {
      DebugLastError("PostMessage");
    }
  }
  Unlock();
}

void Globals::PostMasterController(UINT msg, WPARAM wParam, LPARAM lParam)
{
  if(!Lock()) return;
  CheckControllers();
	if(f_MaxControllers == 0)
	{
		SendDebugMessageFormat(0, sdmGlobal, 0, "PostMasterController: no controllers [%x : %x, %x]", msg, wParam, lParam);
    Unlock();
		return;
	}
  
  if(ShouldDebug(sdmGlobal))
  {
  	char buf[64];
	  GetClassName(f_ControllerWindow[0], buf, 64);
	  buf[63] = 0;
	  SendDebugMessageFormat(f_ControllerWindow[0], sdmGlobal, 0, "PostMasterController %s [%x : %x, %x]", buf, msg, wParam, lParam);
  }

	if(!PostMessage(f_ControllerWindow[0], msg, wParam, lParam)) {
    DebugLastError("PostMessage");
  }
  Unlock();
}

LRESULT Globals::SendMasterController(UINT msg, WPARAM wParam, LPARAM lParam)
{
  if(!Lock()) return 0;
  CheckControllers();
	if(f_MaxControllers == 0)
	{
    Unlock();
		SendDebugMessageFormat(0, sdmGlobal, 0, "SendMasterController: no controllers [%x : %x, %x]", msg, wParam, lParam);
		return 0;
	}

  HWND hwnd = f_ControllerWindow[0];
  Unlock();
  
  if(ShouldDebug(sdmGlobal))
  {
  	char buf[64];
	  GetClassName(f_ControllerWindow[0], buf, 64);
	  buf[63] = 0;
	  SendDebugMessageFormat(f_ControllerWindow[0], sdmGlobal, 0, "SendMasterController %s [%x : %x, %x]", buf, msg, wParam, lParam);
  }

  // Window may be taken out of list between Lock and Unlock but let's not worry about this 

  SetLastError(ERROR_SUCCESS);
  DWORD_PTR dwResult = 0;
	if(!SendMessageTimeout(hwnd, msg, wParam, lParam, SMTO_BLOCK, 100, &dwResult)) {
    DebugLastError("SendMessageTimeout");
  } else {
	  SendDebugMessageFormat(hwnd, sdmGlobal, 0, "SendMasterController returned %x", dwResult);
  }
  return dwResult;
}

extern "C" void  _declspec(dllexport) WINAPI Keyman_PostControllers(UINT msg, WPARAM wParam, LPARAM lParam)
{
  Globals::PostControllers(msg, wParam, lParam);
}

extern "C" void  _declspec(dllexport) WINAPI Keyman_PostMasterController(UINT msg, WPARAM wParam, LPARAM lParam)
{
  Globals::PostMasterController(msg, wParam, lParam);
}

extern "C" LRESULT  _declspec(dllexport) WINAPI Keyman_SendMasterController(UINT msg, WPARAM wParam, LPARAM lParam)
{
  return Globals::SendMasterController(msg, wParam, lParam);
}

extern "C" BOOL  _declspec(dllexport) WINAPI Keyman_RegisterControllerWindow(HWND hwnd)
{
  if(!Globals::Lock())
  {
    // last error set by Globals::Lock
    return FALSE;
  }
	if(f_MaxControllers == MAXCONTROLLERS)
  {
    Globals::Unlock(); // I2814
    SetLastError(ERROR_KEYMAN_TOO_MANY_CONTROLLERS);  // I3183   // I3530
    return FALSE;
  }
	if(Globals::IsControllerWindow(hwnd))
  {
    Globals::Unlock(); // I2814
    return TRUE;
  }
	f_ControllerWindow[f_MaxControllers++] = hwnd;
  Globals::Unlock();
  return TRUE;
}

extern "C" BOOL _declspec(dllexport) WINAPI Keyman_UnregisterControllerWindow(HWND hwnd)
{
  if(!Globals::Lock()) return FALSE;
  for(int i = 0; i < f_MaxControllers; i++)
		if(f_ControllerWindow[i] == hwnd)
		{
			for(int j = i+1; j < f_MaxControllers; j++) f_ControllerWindow[j-1] = f_ControllerWindow[j];
			f_MaxControllers--;
      Globals::Unlock();
			return TRUE;
		}
  Globals::Unlock();
	return FALSE;
}

BOOL IsKeymanSystemWindow(HWND hwnd)
{
	if(hwnd == 0 || IsSysTrayWindow(hwnd)) return TRUE;  // I2866


	DWORD pidC, pidHWND;
	GetWindowThreadProcessId(hwnd, &pidHWND);
	
  if(!Globals::Lock()) return FALSE;

  for(int i = 0; i < f_MaxControllers; i++)
	{
		if(f_ControllerWindow[i] == hwnd)
    {
      Globals::Unlock();
      return TRUE; 
    }
		GetWindowThreadProcessId(f_ControllerWindow[i], &pidC);
		if(pidC == pidHWND)
    {
      Globals::Unlock();
      return TRUE;
    }
	}
  Globals::Unlock();
	return FALSE;
}

BOOL IsTooltipWindow(HWND hwnd)
{
  char buf[64];
  if(!GetClassName(hwnd, buf, 64)) return FALSE;
  return !_strcmpi(buf, "tooltips_class32");
}

void UpdateActiveWindows()
{
  GUITHREADINFO gti;
  gti.cbSize = sizeof(GUITHREADINFO);
  GetGUIThreadInfo(0, &gti);
  
	if(IsKeymanSystemWindow(gti.hwndActive) || IsKeymanSystemWindow(gti.hwndFocus)) return;
  if(IsTooltipWindow(gti.hwndActive) || IsTooltipWindow(gti.hwndFocus)) return;
	
	*Globals::hLastActiveWnd() = gti.hwndActive;
	*Globals::hLastFocusWnd() = gti.hwndFocus;

  Globals::PostControllers(wm_keyman_control, KMC_SETFOCUSINFO, 0);
}

BOOL IsSysTrayWindow(HWND hwnd)
{
	char buf[64];
	GetClassName(hwnd, buf, 63);
	return(!strcmp(buf, "SysTabControl32") ||
		!strcmp(buf, "TrayNotifyWnd") ||
		!strcmp(buf, "Shell_TrayWnd") ||
    !strcmp(buf, "NotifyIconOverflowWindow"));  // I2866
}

BOOL Reg_GetDebugFlag(LPSTR pszFlagRegistrySetting, BOOL bDefault) {
  RegistryReadOnly r(HKEY_CURRENT_USER);
  if (r.OpenKeyReadOnly(REGSZ_Keyman_Debug)) {
    if (r.ValueExists(pszFlagRegistrySetting)) {
      return !!r.ReadInteger(pszFlagRegistrySetting);
    }
  }
  return bDefault;
}

void Globals::LoadDebugSettings() {
  RegistryReadOnly reg(HKEY_CURRENT_USER);
  if (reg.OpenKeyReadOnly(REGSZ_KeymanCU)) {
    f_debug_KeymanLog = reg.ValueExists(REGSZ_Debug) && reg.ReadInteger(REGSZ_Debug);
    f_debug_ToConsole = reg.ValueExists(REGSZ_DebugToConsole) && reg.ReadInteger(REGSZ_DebugToConsole);   // I3951
  }
  else {
    f_debug_KeymanLog = FALSE;
    f_debug_ToConsole = FALSE;   // I3951
  }
}
