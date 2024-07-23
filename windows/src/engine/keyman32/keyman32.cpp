/*
  Name:             Keyman32
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Clean up SelectKeyboard and Visual Keyboard updating
                    14 Sep 2006 - mcdurdin - Fix bugs with multiple InitialiseProcess calls causing Keyman to fail to start up correctly
                    14 Sep 2006 - mcdurdin - Fix buffer overflow with Keyman_ForceKeyboard causing shift states to be flushed in Keyman Developer debugger
                    04 Dec 2006 - mcdurdin - Add Vista support
                    04 Dec 2006 - mcdurdin - Add Keyman_IsProductLoaded function
                    04 Jan 2007 - mcdurdin - Fix shutdown order for Keyman Engine
                    16 May 2007 - mcdurdin - I819 - Test if Keyman is functioning
                    16 May 2007 - mcdurdin - I820 - Activate keyboard for first active application after start
                    30 May 2007 - mcdurdin - I863 - Fixed some systems unable to restart Keyman while system is starting up (userinit)
                    04 Jun 2007 - mcdurdin - I819 - Use a timeout to ensure Keyman can shutdown in a timely fashion if another application is not responding
                    19 Jun 2007 - mcdurdin - I819 - Improve Keyman stability
                    13 Jul 2007 - mcdurdin - I910 - Log errors starting Keyman
                    05 Nov 2007 - mcdurdin - I1087 - Add language switching hotkeys for Desktop Pro
                    27 Mar 2008 - mcdurdin - I1287 - Switch keyboard and language togher
                    20 Jul 2008 - mcdurdin - I1546 - Language switch not working with some keyboard IDs
                    10 Sep 2008 - mcdurdin - I1627 - Switching languages with keyboard should be in LIBRARY_NAME
                    19 Sep 2008 - mcdurdin - I1635 - Fix x64 crash due to memory corruption
                    29 Sep 2008 - mcdurdin - I1657 - Fix switching off keyboard bug introduced in build 252
                    16 Jan 2009 - mcdurdin - I1798 - Fix keyboards not always switching correctly first time after startup
                    27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    30 Jan 2009 - mcdurdin - I1835 - Improve refresh performance
                    09 Mar 2009 - mcdurdin - I1888 - Keyman Engine can crash with some multithreaded applications
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    30 Nov 2009 - mcdurdin - I934 - x64 groundwork - registry keys
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    11 Dec 2009 - mcdurdin - I1455 - Remove unneeded hooks
                    11 Dec 2009 - mcdurdin - I934 - x64 - keyboard hook per thread
                    11 Dec 2009 - mcdurdin - I1455 - Update keyboard activation per focused thread
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    29 Mar 2010 - mcdurdin - I1089 - One keyboard - all applications
                    29 Mar 2010 - mcdurdin - I2264 - Keyman Engine should not attach to various processes
                    29 Mar 2010 - mcdurdin - I2265 - Keyman fails to switch keyboard correctly when not associated with a language
                    29 Mar 2010 - mcdurdin - I2255 - Keyman fails to work correctly on restart due to keyman32 or keyman64 being locked by another process
                    29 Mar 2010 - mcdurdin - I2244 - Keyman ceases to accept input with after some time
                    06 Apr 2010 - mcdurdin - I2271 - Select Keyboard tidy up
                    06 Apr 2010 - mcdurdin - I2266 - Reduce chatter when switching keyboards
                    06 Apr 2010 - mcdurdin - I2285 - Stop initial keyboard load from caching and reloading Keyman keyboard
                    06 Apr 2010 - mcdurdin - I2264 - FAIL: Keyman Engine must be allowed to attach to all processes for performance reasons
                    09 Apr 2010 - mcdurdin - I2293 - Keyman thread initialisation was re-entrant
                    04 May 2010 - mcdurdin - I2355 - Resolve deadlocks loading keyman32.dll
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    14 May 2010 - mcdurdin - I2375 - During keyboard refresh, don't call SelectKeyboard because we just need to clear state
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    29 Jun 2010 - mcdurdin - I2435 - Shutdown of Keyman Engine can re-initialise and hang
                    29 Jun 2010 - mcdurdin - I2441 - SelectKeyboard cache needs to be initialised when Keyman starts to prevent wrong keyboard language links
                    29 Jun 2010 - mcdurdin - I2437 - Crash unloading keyboard because keyboard options not initialised when using forced keyboards
                    29 Jun 2010 - mcdurdin - I2448 - Regression - Keyman Engine fails to restart in .285
                    18 Feb 2011 - mcdurdin - I2714 - Leak of registry handle when refreshing keyboards
                    19 Aug 2011 - mcdurdin - I3040 - keyman32.dll can crash if advapi32.dll is not loaded when first called from a process
                    03 Oct 2011 - mcdurdin - I3092 - Keyman Engine does not restart nicely when shutdown uncleanly
                    04 Nov 2011 - mcdurdin - I3108 - Keyman Engine could crash if it shutdown and restarted with keyman32.dll still in memory
                    07 Nov 2011 - mcdurdin - I3127 - If ThreadGlobals is null but Keyman_Shutdown is false, then Keyman can crash on startup
                    02 Dec 2011 - mcdurdin - I3158 - Keyman Engine can crash other applications when multiple OEM products are started, with a _tcscpy assertion
                    23 Dec 2011 - mcdurdin - I3191 - Corruption of keyboard selection cache between x86 and x64
                    24 Jan 2012 - mcdurdin - I3143 - Report more useful information when Keyman fails to start
                    24 Jan 2012 - mcdurdin - I3173 - Keyman can crash when Keyman_StopForcingKeyboard called and keyboard is not already forced
                    04 Nov 2012 - mcdurdin - I3523 - V9.0 - Merge of I3143 - Report more useful information when Keyman fails to start
                    04 Nov 2012 - mcdurdin - I3524 - V9.0 - Merge of I3158 - Keyman Engine can crash other applications when multiple OEM products are started, with a _tcscpy assertion
                    04 Nov 2012 - mcdurdin - I3525 - V9.0 - Merge of I3173 - Keyman can crash when Keyman_StopForcingKeyboard called and keyboard is not already forced
                    04 Nov 2012 - mcdurdin - I3526 - V9.0 - Merge of I3191 - Corruption of keyboard selection cache between x86 and x64
                    05 Nov 2012 - mcdurdin - I3547 - V9.0 Use _countof to tidyup code
                    20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    28 Nov 2012 - mcdurdin - I3594 - V9.0 - Remove old SelectKeyboard code and related messages
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    01 Dec 2012 - mcdurdin - I3616 - V9.0 - Language association obsolete, strip out code
                    17 Jan 2013 - mcdurdin - I3759 - V9.0 - UnderlyingLayout stores a HKL in the registry but expects a KLID in the code
                    17 Jan 2013 - mcdurdin - I3760 - V9.0 - Clean up refresh code to avoid repeated dereferences and indexing
                    17 Jan 2013 - mcdurdin - I3761 - V9.0 - If multiple profiles are enabled for a given keyboard, Keyman32 can crash with memory errors
                    06 Mar 2014 - mcdurdin - I4124 - V9.0 - Language switch dialog is not working in v9
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
                    25 Sep 2014 - mcdurdin - I4412 - V9.0 - Character Map needs to insert characters using SendInput
                    16 Oct 2014 - mcdurdin - I4461 - V9.0 - Keyman_BuildKeyboardList was including keyboards installed but with no profiles
                    23 Oct 2014 - mcdurdin - I4461 - V9.0 - Keyman_BuildKeyboardList was including keyboards installed but with no profiles
                    14 Nov 2014 - mcdurdin - I4516 - V9.0 - Language hotkeys associated with non-primary keyboards do not trigger language change
                    03 Feb 2015 - mcdurdin - I4581 - V9.0 - Underlying layout fails to load and we never know what the AltGr flag is now
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
                    06 Feb 2015 - mcdurdin - I4583 - V9.0 - Remove altgr lookup test from keyman32 and put it into the registry
                    06 Feb 2015 - mcdurdin - I4588 - V9.0 - Support if(&baselayout) with all of the ISO names
                    22 Apr 2015 - mcdurdin - I4660 - V9.0 - underlyinglayout, if not specified, tries to read invalid location in HKLM\System\CurrentControlSet\Keyboard Layouts\\layout file
                    14 May 2015 - mcdurdin - I4714 - V9.0 - Keyboard and language hotkeys don't always work
                    02 Jul 2015 - mcdurdin - I4786 - If no baselayout is specified by the user, default to en-US (kbdus.dll)
                    09 Aug 2015 - mcdurdin - I4844 - Tidy up PostDummyKeyEvent calls
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/

#include "pch.h"
#include "serialkeyeventserver.h"

HINSTANCE g_hInstance;

/*******************************************************************************************/
/*                                                                                         */
/* Keyman Initialisation                                                                   */
/*                                                                                         */
/*******************************************************************************************/

BOOL __stdcall DllMain(HINSTANCE hinstDll, DWORD fdwReason, LPVOID reserved)
{
  UNREFERENCED_PARAMETER(reserved);
  g_hInstance = hinstDll;
	switch(fdwReason)
	{
	case DLL_PROCESS_ATTACH:
    //OutputThreadDebugString("DLL_PROCESS_ATTACH");
    if(!Globals_InitProcess()) return FALSE;
		break;
	case DLL_PROCESS_DETACH:
    if (reserved == NULL) {
      // If reserved == NULL, that means the library is being unloaded, but
      // the process is not terminating.
      //OutputThreadDebugString("DLL_PROCESS_DETACH not terminating");
      UninitialiseProcess(FALSE);
      Globals_UninitProcess();
    }
    else {
      //OutputThreadDebugString("DLL_PROCESS_DETACH terminating");
    }
		break;
	case DLL_THREAD_ATTACH:
    //OutputThreadDebugString("DLL_THREAD_ATTACH");
    Globals_InitThread();
		break;
	case DLL_THREAD_DETACH:
    //OutputThreadDebugString("DLL_THREAD_DETACH");
    UninitialiseProcess(FALSE);
    Globals_UninitThread();
		break;
	}
	return TRUE;
}

void UninitDebuggingEx();

/**
 * Frees memory and cleans up
 *
 * @param Lock    If FALSE, does not attempt to release other DLLs (e.g. call()ed DLLs)
 */
BOOL UninitialiseProcess(BOOL Lock)
{
  if(!Globals_ProcessInitialised()) return TRUE;
  ReleaseKeyboards(Lock);
  UninitDebuggingEx();
  return TRUE;
}

LONG FStartedInitialise = FALSE;

#define MSGFLT_ADD	1

BOOL (WINAPI * PChangeWindowMessageFilter)(UINT, DWORD) = NULL;

void DoCWMF(UINT msg)
{
  if (!(*PChangeWindowMessageFilter)(msg, MSGFLT_ADD)) {
    DebugLastError("ChangeWindowMessageFilter");
  }
}

void DoChangeWindowMessageFilter()
{
  HMODULE hUser32 = LoadLibrary("user32");
  if (!hUser32)
    return;

	PChangeWindowMessageFilter = (BOOL (WINAPI *)(UINT,DWORD))GetProcAddress(hUser32, "ChangeWindowMessageFilter");

  if (!PChangeWindowMessageFilter)
  {
    FreeLibrary(hUser32);
    return;
  }

	DoCWMF(wm_keyman);   // I3594
  DoCWMF(wm_keyman_grabwindowproc);
  DoCWMF(wm_keyman_refresh);
  DoCWMF(wm_kmgetactivekeymanid);
  DoCWMF(wm_keymanim_close);
  DoCWMF(wm_keymanim_contextchanged);
  DoCWMF(wm_keymanshift);
  DoCWMF(wm_keyman_control);   // I4714
  DoCWMF(wm_keyman_control_internal);   // I4714

  FreeLibrary(hUser32);
}

BOOL InitThread(HWND hwnd)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td)
  {
    SetLastError(ERROR_KEYMAN_THREAD_DATA_NOT_READY);  // I3143   // I3523
    return FALSE;
  }

  if(_td->FInitialising)
  {
    SetLastError(ERROR_KEYMAN_CANNOT_REINITIALISE_THREAD);  // I3143   // I3523
    return FALSE;
    // I2435 - Shutdown of Keyman Engine re-initialising and causing a deadlock; moved up above LoadProducts
    // I2448 - Removed "|| _td->FInitialised" -- this should not be included because sometimes it can be re-initialised.
  }

  _td->FInitialising = TRUE;  // Control re-entrancy, this is thread safe because the variable is per-thread

	RefreshKeyboards(TRUE);

	SendDebugMessageFormat(hwnd, sdmGlobal, 0, "--InitialiseProcess LEAVE--");

  _td->FInitialising = FALSE;  // Control re-entrancy, this is thread safe because the variable is per-thread

  return _td->FInitialised = TRUE;
}

/*
  Compatibility flags. Search for the use of these for
  more documentation on why they exist.
*/
void Initialise_Flag_ShouldSerializeInput() {
  flag_ShouldSerializeInput = Reg_GetDebugFlag(REGSZ_Flag_ShouldSerializeInput, TRUE);
}

BOOL InitialiseProcess(HWND hwnd)
{
	if(InterlockedExchange(&FStartedInitialise, TRUE))
	{
		//SendDebugMessage(hwnd, sdmGlobal, 0, "InitialiseProcess: Failed because process is already initialising");
    return InitThread(hwnd);
	}

	SendDebugMessageFormat(hwnd, sdmGlobal, 0, "--InitialiseProcess ENTER--");
	SendDebugMessageFormat(hwnd, sdmGlobal, 0, "ProcessID=%d ThreadID=%d CmdLine=%s", GetCurrentProcessId(), GetCurrentThreadId(),
		GetCommandLine());

  Initialise_Flag_ShouldSerializeInput();

	wm_keyman = RegisterWindowMessage(RWM_KEYMAN);
	wm_keyman_grabwindowproc = RegisterWindowMessage("WM_KEYMAN_GRABWINDOWPROC");
	wm_keyman_refresh = RegisterWindowMessage("WM_KEYMANREFRESH");
	wm_kmgetactivekeymanid = RegisterWindowMessage("WM_KEYMAN_GETACTIVEKEYMANID");
	wm_keymanim_close = RegisterWindowMessage("WM_KEYMANIM_CLOSE");
	wm_keymanim_contextchanged = RegisterWindowMessage("WM_KEYMANIM_CONTEXTCHANGED");
	wm_keymanshift = RegisterWindowMessage("WM_KEYMANSHIFT");


	wm_keyman_control = RegisterWindowMessage(RWM_KEYMAN_CONTROL);
	wm_keyman_control_internal = RegisterWindowMessage("WM_KEYMAN_CONTROL_INTERNAL");

	wm_test_keyman_functioning = RegisterWindowMessage("wm_test_keyman_functioning");

	DoChangeWindowMessageFilter();

  return InitThread(hwnd);
}

extern "C" BOOL _declspec(dllexport) WINAPI Keyman_GetInitialised(BOOL *FSingleApp)
{
	*FSingleApp = Globals::get_FSingleThread();
	return Globals::get_Keyman_Initialised();
}

BOOL InitHooks()
{
  HINSTANCE hinst = GetModuleHandle(LIBRARY_NAME);

	*Globals::hhookGetMessage()   = SetWindowsHookExW(WH_GETMESSAGE,  (HOOKPROC) kmnGetMessageProc, hinst, Globals::get_FSingleThread());
  *Globals::hhookCallWndProc()  = SetWindowsHookExW(WH_CALLWNDPROC, (HOOKPROC) kmnCallWndProc,    hinst, Globals::get_FSingleThread());
#ifndef _WIN64
  *Globals::hhookLowLevelKeyboardProc() = SetWindowsHookExW(WH_KEYBOARD_LL, (HOOKPROC) kmnLowLevelKeyboardProc, hinst, Globals::get_FSingleThread());   // I4124
#endif;

  return
    Globals::get_hhookGetMessage() != NULL
    && Globals::get_hhookCallWndProc() != NULL
#ifndef _WIN64
    && Globals::get_hhookLowLevelKeyboardProc() != NULL   // I4124
#endif
    ;
}

BOOL UninitHooks()
{
  BOOL RetVal = TRUE;

  if(Globals::get_hhookGetMessage() && !UnhookWindowsHookEx(Globals::get_hhookGetMessage()))
      RetVal = FALSE;
  else
      *Globals::hhookGetMessage() = NULL;

	if(Globals::get_hhookCallWndProc() && !UnhookWindowsHookEx(Globals::get_hhookCallWndProc()))
    RetVal = FALSE;
  else
    *Globals::hhookCallWndProc() = NULL;

#ifndef _WIN64
  if(Globals::get_hhookLowLevelKeyboardProc() && !UnhookWindowsHookEx(Globals::get_hhookLowLevelKeyboardProc()))    // I4124
    RetVal = FALSE;
  else
    *Globals::hhookLowLevelKeyboardProc() = NULL;
#endif

	return RetVal;
}

extern "C" BOOL _declspec(dllexport) WINAPI Keyman_ResetInitialisation()  // I3092
{
  if(!Globals::ResetControllers())
  {
    SendDebugMessage(0, sdmGlobal, 0, "Keyman_ResetInitialisation: Failed because controllers and global data could not be reset.");
    return FALSE;
  }
  return TRUE;
}

extern "C" BOOL _declspec(dllexport) WINAPI Keyman_Initialise(HWND Handle, BOOL FSingleApp)
{
	HINSTANCE hinst;

  Globals::LoadDebugSettings();

	if(Globals::get_Keyman_Initialised())
 	{
 	  SendDebugMessage(Handle, sdmGlobal, 0, "Keyman_Initialise: Failed because " LIBRARY_NAME " is already initialised");
    SetLastError(ERROR_ALREADY_INITIALIZED);  // I3143   // I3523
		return FALSE;
	}

  if(Globals::get_Keyman_Shutdown() || !ThreadGlobals())  // I3108   // I3127
  {
    *Globals::Keyman_Shutdown() = FALSE;  // I2255, I2244
    if(!Globals_InitThread())  // I3108 - must re-init thread because it would have failed during the initial startup of the process due to thinking we were shutting down
    {
 	    SendDebugMessage(Handle, sdmGlobal, 0, "Keyman_Initialise: Failed for " LIBRARY_NAME " because Keyman_Shutdown was set and could not be reset.");
      // SetLastError already set by Globals_InitThread  // I3143   // I3523
		  return FALSE;
	  }
  }

	if(FSingleApp)
		*Globals::FSingleThread() = GetWindowThreadProcessId(Handle, NULL);
	else
		*Globals::FSingleThread() = 0;

  *Globals::InitialisingThread() = GetCurrentThreadId();   // I4326

	hinst = GetModuleHandle(LIBRARY_NAME);

	InitDebugging();

  if (!Globals::InitSettings()) {
    SendDebugMessageFormat(Handle, sdmGlobal, 0, "Keyman_Initialise: Failed to initialise global settings.  GetLastError = %d", GetLastError());
    return FALSE;
  }

  if(!Globals::InitHandles())  // I3040
  {
    DebugLastError("Globals::InitHandles");
    // SetLastError: Globals::InitHandles will return a windows error code  // I3143   // I3523
    return FALSE;
  }

	if(!InitialiseProcess(Handle))
 	{
 	  SendDebugMessage(Handle, sdmGlobal, 0, "Keyman_Initialise: Failed to initialise for current process");
    // SetLastError: InitialiseProcess will return an error code  // I3143   // I3523
		return FALSE;	/* Failed to verify certificate */
	}

#ifndef _WIN64
  ISerialKeyEventServer::Startup();
#endif

  InitHooks();

  DWORD dwLastError = GetLastError();
  SendDebugMessageFormat(Handle, sdmGlobal, 0, "GetMessage=%x CallWndProc=%x GetLastError=%d",
    Globals::get_hhookGetMessage(), Globals::get_hhookCallWndProc(), dwLastError);

#ifndef _WIN64
	SendDebugMessageFormat(Handle, sdmGlobal, 0, "Keyboard_LL=%x",    // I4124
    Globals::get_hhookLowLevelKeyboardProc());
#endif

	UpdateActiveWindows();

	*Globals::Keyman_Initialised() = TRUE;
	RefreshKeyboards(TRUE);   // I4786

  SendDebugMessageFormat(Handle, sdmGlobal, 0, "Keyman is now initialised");

	return TRUE;
}

extern "C" BOOL _declspec(dllexport) WINAPI Keyman_StartExit(void)  // I3092
{
  *Globals::Keyman_Shutdown() = TRUE;
  return TRUE;
}

extern "C" BOOL _declspec(dllexport) WINAPI Keyman_Exit(void)
{
  if(Globals::get_InitialisingThread() != GetCurrentThreadId()) {   // I4326
    SendDebugMessageFormat(0, sdmGlobal, 0, "Keyman_Exit called from thread %d that did not initialise (which was %d)", GetCurrentThreadId(), Globals::get_InitialisingThread());
    return FALSE;
  }

#ifndef _WIN64
  Hotkeys::Unload();
#endif

  *Globals::InitialisingThread() = 0;

  BOOL RetVal = TRUE;

  *Globals::Keyman_Shutdown() = TRUE;

  ReleaseKeyboards(TRUE);

  if(!Globals::get_Keyman_Initialised())
  {
	  return TRUE;
  }

  *Globals::Keyman_Initialised() = FALSE;

  DWORD_PTR ret;
	SendMessageTimeout(HWND_BROADCAST, wm_keyman_grabwindowproc, 0, 0, SMTO_ABORTIFHUNG | SMTO_NORMAL, 1000, &ret);

  RetVal = RetVal && UninitHooks();

	UninitDebugging();

#ifndef _WIN64
  ISerialKeyEventServer::Shutdown();
#endif

  //if(RetVal) ;

	return RetVal;
}

extern "C" BOOL _declspec(dllexport) WINAPI Keyman_RestartEngine()
{
  if(Globals::get_Keyman_Initialised())
  {
    UninitHooks();
    InitHooks();
    UpdateActiveWindows(); // I1798
  }

  return TRUE;
}

//---------------------------------------------------------------------------------------------------------
//
// Utility guff functions
//
//---------------------------------------------------------------------------------------------------------

/*
#define MENU_CLASS "#32768"				// Windows Desktop Menu Class

BOOL IsDesktopMenuVisible()
{
    static char str[128];
	static HWND hWndDesktopMenu = NULL;		// Windows Desktop Menu Window

	if(!IsWindow(hWndDesktopMenu))
    {
		hWndDesktopMenu = GetWindow(GetDesktopWindow(), GW_CHILD);
		while(hWndDesktopMenu)
        {
	        GetClassName(hWndDesktopMenu, (LPSTR) str, sizeof(str));
	        if(strcmp((LPSTR) str, MENU_CLASS) == 0)
	        	break;
			hWndDesktopMenu = GetWindow(hWndDesktopMenu, GW_HWNDNEXT);
        }
	}

	return(IsWindowVisible(hWndDesktopMenu));
}
*/

BOOL ReleaseKeyboardMemory(LPKEYBOARD kbd)
{
	if(!kbd) return TRUE;
	delete kbd;
	return TRUE;
}

BOOL ReleaseStateMemoryCore(km_core_state **state) {
  if (!*state) return TRUE;
  km_core_state_dispose(*state);
  *state = NULL;
  return TRUE;
}

BOOL ReleaseKeyboardMemoryCore(km_core_keyboard **kbd) {
  if (!*kbd)
    return TRUE;
  km_core_keyboard_dispose(*kbd);
  *kbd = NULL;
  return TRUE;
}

//---------------------------------------------------------------------------------------------------------
//
// Selecting Keyman Keyboards
//
//---------------------------------------------------------------------------------------------------------


extern "C" DWORD _declspec(dllexport) WINAPI GetActiveKeymanID()
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;
	return _td->ActiveKeymanID;
}

//---------------------------------------------------------------------------------------------------------
//
// Refreshing Keyman's keyboard list
//
//---------------------------------------------------------------------------------------------------------

// This function prevents a refresh event from being processed more than once
// by a thread, for instance if a thread has multiple top-level windows
BOOL UpdateRefreshTag(LONG tag)   // I1835 - Reduce chatter
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;
  if(_td->RefreshTag_Thread != tag)
  {
    _td->RefreshTag_Thread = tag;
    return TRUE;
  }
  return FALSE;
}

void HandleRefresh(int code, LONG tag)
{
  switch (code)
  {
  case KR_REQUEST_REFRESH:
    // This is sent by Keyman COM API, ApplyToRunningKeymanEngine
    SendDebugMessageFormat(0, sdmGlobal, 0, "#### Refresh Requested ####");

    // We ask the master controller window to tell all instances
    // of keyman32/keyman64 that a refresh is coming through
    Globals::PostMasterController(wm_keyman_refresh, KR_PRE_REFRESH, 0);

    // We need to tell the controller window to refresh itself also
    // TODO: eliminate this message (we can hop onto the above message)
    Globals::PostMasterController(wm_keyman_control, KMC_REFRESH, 0);
    break;

  case KR_PRE_REFRESH:
#ifndef _WIN64
    // We only need to broadcast the message from Win32; this avoids
    // a double-broadcast which could happen if both keyman32 and keyman64
    // receive the message, as they have independently managed RefreshTags

    // This RefreshTag is only ever incremented here by the master controller
    // so this is a thread safe operation
    tag = ++(*Globals::RefreshTag());

    if (UpdateRefreshTag(tag)) {
      // The Keyman process gets the update first
      RefreshKeyboards(FALSE);
      PostMessage(HWND_BROADCAST, wm_keyman_refresh, KR_REFRESH, tag);
    }
#endif

    break;

  case KR_REFRESH:
    // All threads need to have their keyboard list
    // refreshed after an update, but only once per
    // refresh request
    if (UpdateRefreshTag(tag)) {
#ifdef _WIN64
      if (Globals::get_InitialisingThread() == GetCurrentThreadId()) {
        // If this is the keymanx64 thread, then we should
        // go ahead and process the refresh immediately so
        // that global settings are updated
        RefreshKeyboards(FALSE);
      }
      else
#endif
        // We'll update when we are called into action
        ScheduleRefresh();
    }
    break;
	}
}

BOOL ConvertStringToGuid(WCHAR *buf, GUID *guid)   // I3581
{
  return IIDFromString(buf, guid) == S_OK;
}

void LoadBaseLayoutSettings() {   // I4552   // I4583
  char underlyingLayout[16] = "";
  wchar_t baseLayout[MAX_PATH];
  DWORD dwUnderlyingLayout = 0;

	RegistryReadOnly *reg = new RegistryReadOnly(HKEY_CURRENT_USER);

	if(reg->OpenKeyReadOnly(REGSZ_KeymanCU)) {
		if(reg->ReadString(REGSZ_UnderlyingLayout, underlyingLayout, 15)) {
      dwUnderlyingLayout = strtoul(underlyingLayout, NULL, 16);   // I4516   // I4581
      wsprintf(underlyingLayout, "%08x", (unsigned int) dwUnderlyingLayout);   // I3759   // I4581
    } else {
			underlyingLayout[0] = 0;
    }

    Globals::SetBaseKeyboardFlags(underlyingLayout, reg->ReadInteger(REGSZ_SimulateAltGr), !reg->ValueExists(REGSZ_DeadkeyConversionMode) || reg->ReadInteger(REGSZ_DeadkeyConversionMode));
	} else {
    Globals::SetBaseKeyboardFlags("", 0, 1);
  }

	delete reg;

  reg = new RegistryReadOnly(HKEY_LOCAL_MACHINE);
  if(underlyingLayout[0] &&   // I4660
      reg->OpenKeyReadOnly(REGSZ_SystemKeyboardLayouts) &&
      reg->OpenKeyReadOnly(underlyingLayout) &&
      reg->ReadString(L"layout file", baseLayout, MAX_PATH)) {
    wchar_t langName[16], countryName[16], baseLayoutAlt[34];

    if(GetLocaleInfoW(LOWORD(dwUnderlyingLayout), LOCALE_SISO639LANGNAME, langName, _countof(langName)) > 0 &&
      GetLocaleInfoW(LOWORD(dwUnderlyingLayout), LOCALE_SISO3166CTRYNAME, countryName, _countof(countryName)) > 0) {   // I4588   // I4786
      wsprintfW(baseLayoutAlt, L"%ls-%ls", langName, countryName);
      Globals::SetBaseKeyboardName(baseLayout, baseLayoutAlt);
    } else {
      Globals::SetBaseKeyboardName(baseLayout, L"en-US");   // I4786
    }
  } else {
    Globals::SetBaseKeyboardName(L"kbdus.dll", L"en-US");   // I4786
  }

  delete reg;
}

// -----------------------------------------------------------------------------------------------
// -                                                                                             -
// - RefreshKeyboards: ....                                                                      -
// -                                                                                             -
// -----------------------------------------------------------------------------------------------

void RefreshKeyboardProfiles(INTKEYBOARDINFO *kp, BOOL isTransient) {
  RegistryReadOnly *reg2 = Reg_GetKeymanInstalledKeyboard(kp->Name);   // I3581
  if (reg2)
  {
    PCSTR RootKey = isTransient ? REGSZ_TransientLanguageProfiles : REGSZ_LanguageProfiles;
    if (reg2->OpenKeyReadOnly(RootKey))   // I3581
    {
      char bufProfile[LOCALE_NAME_MAX_LENGTH];
      int nProfile = 0;
      while (reg2->GetKeyNames(bufProfile, LOCALE_NAME_MAX_LENGTH, nProfile))
      {
        RegistryReadOnly *reg3 = Reg_GetKeymanInstalledKeyboard(kp->Name);
        if (reg3->OpenKeyReadOnly(RootKey) &&
          reg3->OpenKeyReadOnly(bufProfile) &&
          reg3->ValueExists(REGSZ_LanguageProfiles_LangID) &&
          reg3->ValueExists(REGSZ_ProfileGUID))
        {
          kp->nProfiles++;
          if (kp->nProfiles == 1) kp->Profiles = new INTKEYBOARDPROFILE[1];
          else
          {
            INTKEYBOARDPROFILE *pProfiles = new INTKEYBOARDPROFILE[kp->nProfiles];   // I3761
            memcpy(pProfiles, kp->Profiles, sizeof(INTKEYBOARDPROFILE) * (kp->nProfiles - 1));   // I3761
            delete kp->Profiles;
            kp->Profiles = pProfiles;
          }
          LPINTKEYBOARDPROFILE ikp = &kp->Profiles[kp->nProfiles - 1];   // I3761

          WCHAR bufW[40];
          reg3->ReadString(REGWSZ_ProfileGUID, bufW, _countof(bufW));
          ConvertStringToGuid(bufW, &ikp->Guid);
          ikp->LangID = (LANGID)reg3->ReadInteger(REGSZ_LanguageProfiles_LangID);
          SendDebugMessageFormat(0, sdmGlobal, 0, "Found profile %ls, language id %x for keyboard %s", bufW, ikp->LangID, kp->Name);
        }
        delete reg3;
        nProfile++;
      }
    }
    delete reg2;
  }
}

void ScheduleRefresh() {
  if (GetCurrentThreadId() == GetWindowThreadProcessId(GetForegroundWindow(), NULL)) {
    RefreshKeyboards(FALSE);
  }
  else {
    PKEYMAN64THREADDATA _td = ThreadGlobals();
    if (!_td) {
      return;
    }
    _td->RefreshRequired = TRUE;
  }
}

void CheckScheduledRefresh() {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return;
  }
  if (_td->RefreshRequired) {
    RefreshKeyboards(FALSE);
    _td->RefreshRequired = FALSE;
  }
}

void RefreshKeyboards(BOOL Initialising)
{
  OutputThreadDebugString("RefreshKeyboards");
	char sz[_MAX_FNAME];
	char oldname[_MAX_FNAME];

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td || _td->FInRefreshKeyboards) return;
  _td->FInRefreshKeyboards = TRUE;

  // Can happen when multiple top-level windows for one process

	SendDebugMessageFormat(0,sdmGlobal,0,"---ENTER RefreshKeyboards---");
	//FInRefreshKeyboards = TRUE;

	oldname[0] = 0;

	if(!Initialising)
  {
  	if(_td->lpActiveKeyboard)
	  	strcpy(oldname, _td->lpActiveKeyboard->Name);

		if(_td->lpActiveKeyboard) DeactivateDLLs(_td->lpActiveKeyboard);
		_td->lpActiveKeyboard = NULL;
		_td->ActiveKeymanID = KEYMANID_NONKEYMAN;
  }

  ReleaseKeyboards(TRUE);

	/* Read the "keyboard off hotkey", simulate Alt+Gr, Hotkeys-Toggle flags */

#ifndef _WIN64
  Hotkeys::Reload();   // I4326   // I4390
#endif

  // Read global underlying keyboard flags

  if(Globals::get_InitialisingThread() == GetCurrentThreadId()) {
    LoadBaseLayoutSettings();   // I4583
  }

	/* Read the keyboards */

	RegistryReadOnly *reg = new RegistryReadOnly(HKEY_CURRENT_USER);
	if(reg->OpenKeyReadOnly(REGSZ_KeymanActiveKeyboards))
	{
		int i = 0, nk;
		INTKEYBOARDINFO *kp;

		_td->nKeyboards = 0;

		while(reg->GetValueNames(sz, _MAX_FNAME, i))
		{
			/* Read the active keyboards */

			nk = atoi(sz);
			if(nk >= _td->nKeyboards)
			{
				kp = new INTKEYBOARDINFO[nk+1];
				memset(kp, 0, sizeof(INTKEYBOARDINFO)*(nk+1));
				if(_td->lpKeyboards)
				{
					memcpy(kp, _td->lpKeyboards, sizeof(INTKEYBOARDINFO)*_td->nKeyboards);
					delete[] _td->lpKeyboards;
				}
				_td->nKeyboards = nk + 1;
				_td->lpKeyboards = kp;
			}

      kp = &_td->lpKeyboards[nk];   // I3760
			kp->KeymanID = atoi(sz);
			reg->ReadString(sz, kp->Name, 64);

			/* Read the shadow keyboard id */    // I3613

      kp->nProfiles = 0;
      kp->Profiles = NULL;
      RefreshKeyboardProfiles(kp, FALSE); // Read standard profiles
      RefreshKeyboardProfiles(kp, TRUE);  // Read transient profiles

			SendDebugMessageFormat(0,sdmGlobal,0,"RefreshKeyboards: Added keyboard %s, %d",
				kp->Name, kp->KeymanID);
			i++;
		}
	}
  delete reg; // I2714

  if(!Initialising)
	  for(int i = 0; i < _td->nKeyboards; i++)
		  if(!_strcmpi(_td->lpKeyboards[i].Name, oldname))
		  {
			  SelectKeyboard(_td->lpKeyboards[i].KeymanID);   // I3594
			  break;
		  }

	SendDebugMessageFormat(0,sdmGlobal,0,"---LEAVE RefreshKeyboards---");

  _td->FInRefreshKeyboards = FALSE;
}

void ReleaseKeyboards(BOOL Lock)
{
  OutputThreadDebugString("ReleaseKeyboards");
  PKEYMAN64THREADDATA _td = ThreadGlobals();
	if(!_td || !_td->lpKeyboards) return;


  if(Lock) {
    if(_td->lpActiveKeyboard) {
      DeactivateDLLs(_td->lpActiveKeyboard);
    }
  }

	for(int i = 0; i < _td->nKeyboards; i++)
	{
		if(Lock) UnloadDLLs(&_td->lpKeyboards[i]);
    ReleaseStateMemoryCore(&_td->lpKeyboards[i].lpCoreKeyboardState);
    ReleaseKeyboardMemoryCore(&_td->lpKeyboards[i].lpCoreKeyboard);
    if(_td->lpKeyboards[i].Profiles) delete _td->lpKeyboards[i].Profiles;   // I3581
	}

	_td->nKeyboards = 0;
	delete _td->lpKeyboards;

	_td->lpKeyboards = NULL;
}

/**
  Keyman_GetLastActiveWindow and Keyman_GetLastFocusWindow were previously published as part of keymanapi.h.
  They are now used only by the COM API internally.
*/
extern "C" HWND  _declspec(dllexport) WINAPI Keyman_GetLastActiveWindow()
{
  return (HWND) Globals::SendMasterController(wm_keyman_control, KMC_GETLASTACTIVE, 0);
}

extern "C" HWND _declspec(dllexport) WINAPI Keyman_GetLastFocusWindow()
{
  return (HWND) Globals::SendMasterController(wm_keyman_control, KMC_GETLASTFOCUS, 0);
}

/**
  GetSystemStore was previously published as an internal API. It is now available
  only within keyman32/keyman64.
*/
PWSTR GetSystemStore(LPKEYBOARD kb, DWORD SystemID)
{
  for (DWORD i = 0; i < kb->cxStoreArray; i++)
    if (kb->dpStoreArray[i].dwSystemID == SystemID) return kb->dpStoreArray[i].dpString;

  return NULL;
}

void PostDummyKeyEvent() {  // I3301 - Handle I3250 regression with inadvertent menu activation with Alt keys   // I3534   // I4844
  keybd_event((BYTE) Globals::get_vk_prefix(), SCAN_FLAG_KEYMAN_KEY_EVENT, 0, 0); // I3250 - is this unnecessary?
  keybd_event((BYTE) Globals::get_vk_prefix(), SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, 0); // I3250 - is this unnecessary?
}
