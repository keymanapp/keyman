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
                    13 Jul 2007 - mcdurdin - I910 - Log errors starting Keyman Desktop
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
   // I3616   // I4169   // I5136
//
// WARNING: Virtual key output is partially supported in this file, however
//			it has never been fully debugged; several known problems exist:
//				1) Interference with backspace			
//				2) Performance hit
//			some code has been commented out; you may wish to debug and
//			uncomment this code if you find it necessary to support it.
//			(We also ran out of time in testing and debugging it; it is
//			very complex and totally illogical - just to encourage you to
//			start with it!)
//

#include "pch.h"
#include "serialkeyeventserver.h"

/*
 If DEBUG is defined, give information in the map file about PRIVATE (=static)
 functions that don't normally appear in the map file.
*/

//#define CERTCHECK_DISABLE

#define KEYMAN_MSGFLAG 0x4000000L
#define DEBUGLOG

LRESULT CALLBACK kmnKeyboardProc(int nCode, WPARAM wParam, LPARAM lParam);
int MapVirtualKeys(int keyCode, UINT shiftFlags);
int KPostMessage(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

HINSTANCE g_hInstance;

/*******************************************************************************************/ 
/*                                                                                         */ 
/* Keyman Initialisation                                                                   */ 
/*                                                                                         */ 
/*******************************************************************************************/ 

BOOL TestDebugProcess()
{
  /*WCHAR buf[260];
  GetModuleFileNameW(NULL, buf, 260);
  if(wcsicmp(buf, L"c:\\temp\\keymanx64.exe") != 0) return FALSE;*/
  return TRUE;
}

BOOL ShouldAttachToProcess();

BOOL __stdcall DllMain(HINSTANCE hinstDll, DWORD fdwReason, LPVOID reserved) 
{
  UNREFERENCED_PARAMETER(reserved);
  g_hInstance = hinstDll;
	switch(fdwReason)
	{
	case DLL_PROCESS_ATTACH:
    //if(!TestDebugProcess()) return FALSE;
    //if(!ShouldAttachToProcess()) return FALSE;
    if(!Globals_InitProcess()) return FALSE;
		break;
	case DLL_PROCESS_DETACH:
    //if(!TestDebugProcess()) return FALSE;
		UninitialiseProcess(FALSE);
    Globals_UninitProcess();
		break;
	case DLL_THREAD_ATTACH:
    //if(!TestDebugProcess()) return FALSE;
    Globals_InitThread();
		break;
	case DLL_THREAD_DETACH:
    //if(!TestDebugProcess()) return FALSE;
		UninitialiseProcess(FALSE);
    Globals_UninitThread();
		break;
	}
	return TRUE;
}

void UninitDebuggingEx();

BOOL UninitialiseProcess(BOOL Lock) 
{
  //SendDebugMessageFormat(0, sdmGlobal, 0, "DLL_PROCESS_DETACH");
  if(!Globals_ProcessInitialised()) return TRUE;

	ReleaseKeyboards(Lock);

	UninitDebuggingEx();

  PKEYMAN64THREADDATA _td = ThreadGlobals(); // This is safe because of Globals_ProcessInitialised call above
  if(_td)
  {
	  if(_td->msgbuf) delete _td->msgbuf;
	  _td->msgbuf = NULL;

	  if(_td->IndexStack) delete _td->IndexStack;
	  _td->IndexStack = NULL;
  }

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
	PChangeWindowMessageFilter = (BOOL (WINAPI *)(UINT,DWORD))GetProcAddress(LoadLibrary("user32"), "ChangeWindowMessageFilter");

	if(!PChangeWindowMessageFilter)
		return;

	DoCWMF(wm_keyman);   // I3594
  DoCWMF(wm_keyman_keyevent);
    DoCWMF(wm_kmmessage);   // I4412
    DoCWMF(wm_keymankeydown);
    DoCWMF(wm_keymankeyup);
    DoCWMF(wm_keyman_grabwindowproc);
    DoCWMF(wm_keyman_refresh);
    DoCWMF(wm_kmgetactivekeymanid);
    DoCWMF(wm_keymanim_close);
    DoCWMF(wm_keymanim_contextchanged);
    DoCWMF(wm_keymanshift);
    DoCWMF(wm_keyman_control);   // I4714
    DoCWMF(wm_keyman_control_internal);   // I4714
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

  // TODO: Check if we are initialising again and don't reallocate memory.  This can happen if Keyman is restarted

  _td->FInitialising = TRUE;  // Control re-entrancy, this is thread safe because the variable is per-thread
  
	_td->IndexStack = new WORD[GLOBAL_ContextStackSize]; //Globals::Ini()->ContextStackSize];  // I3158   // I3524
	if(!_td->IndexStack)
	{
 	  SendDebugMessage(hwnd, sdmGlobal, 0, "InitialiseProcess: Failed to allocate memory for IndexStack");
    SetLastError(ERROR_KEYMAN_MEMORY_ALLOCATION_FAILED);  // I3143   // I3523
  	return FALSE;
  }

	_td->miniContext = new WCHAR[GLOBAL_ContextStackSize]; //Globals::Ini()->ContextStackSize];  // I3158   // I3524
	if(!_td->miniContext)
	{
 	  SendDebugMessage(hwnd, sdmGlobal, 0, "InitialiseProcess: Failed to allocate memory for miniContext");
    SetLastError(ERROR_KEYMAN_MEMORY_ALLOCATION_FAILED);  // I3143   // I3523
 	  return FALSE;
 	}

	_td->msgbuf = new MSG[GLOBAL_MsgStackSize]; //Globals::Ini()->MsgStackSize];  // I3158   // I3524
	if(!_td->msgbuf)
	{
 	  SendDebugMessage(hwnd, sdmGlobal, 0, "InitialiseProcess: Failed to allocate memory for msgbuf");
    SetLastError(ERROR_KEYMAN_MEMORY_ALLOCATION_FAILED);  // I3143   // I3523
	  return FALSE;
  }
  
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
  wm_keyman_keyevent = RegisterWindowMessage("WM_KEYMAN_KEYEVENT");
	wm_kmmessage = RegisterWindowMessage(RWM_KMMESSAGE);
	wm_keymankeydown = RegisterWindowMessage("WM_KEYMANKEYDOWN");
	wm_keymankeyup = RegisterWindowMessage("WM_KEYMANKEYUP");
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

  //GetINIAdvanced();   /* I163x - Fix crash due to memory corruption - Globals::Ini not initialized before use - only appeared to happen on x64? */  // I3158   // I3524

	SendDebugMessageFormat(hwnd, sdmGlobal, 0, "ContextStackSize: %d", GLOBAL_ContextStackSize); // Globals::Ini()->ContextStackSize); // I3158   // I3524

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

#ifdef USE_SERIALKEYEVENTSERVER
#ifndef _WIN64
  ISerialKeyEventServer::Startup();
#endif
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

/*void GetINIAdvanced(void) // I3158   // I3524
{
	Globals::Ini()->ContextStackSize = 80;
	Globals::Ini()->MsgStackSize = 80;
	Globals::Ini()->MaxKeyboards = 32;
}*/

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

#ifdef USE_SERIALKEYEVENTSERVER
#ifndef _WIN64
  ISerialKeyEventServer::Shutdown();
#endif
#endif

  *Globals::InitialisingThread() = 0;

  BOOL RetVal = TRUE;

  *Globals::Keyman_Shutdown() = TRUE;

  ReleaseKeyboards(TRUE);
  Addin_Release();

  if(!Globals::get_Keyman_Initialised()) 
  {
	  return TRUE;
  }
  
  *Globals::Keyman_Initialised() = FALSE;

  DWORD_PTR ret;
	SendMessageTimeout(HWND_BROADCAST, wm_keyman_grabwindowproc, 0, 0, SMTO_ABORTIFHUNG | SMTO_NORMAL, 1000, &ret);
  
  RetVal = RetVal && UninitHooks();

	UninitDebugging();

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

/*******************************************************************************************/
/*                                                                                         */
/* Keyman Keyboard Override Functions                                                      */
/*                                                                                         */
/*******************************************************************************************/

extern "C" BOOL  _declspec(dllexport) WINAPI Keyman_StopForcingKeyboard();

void RefreshPreservedKeys(BOOL Activating);

extern "C" BOOL  _declspec(dllexport) WINAPI Keyman_ForceKeyboard(PCSTR FileName)
{
	SendDebugMessageFormat(0,sdmGlobal,0,"Keyman_ForceKeyboard: ENTER %s", FileName);

  Keyman_StopForcingKeyboard();		// 7.0.219.0

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

  strncpy(_td->ForceFileName, FileName, MAX_PATH);
	_td->ForceFileName[MAX_PATH-1] = 0;

	if(_td->lpActiveKeyboard)
	{
		DeactivateDLLs(_td->lpActiveKeyboard);
	}


	_td->lpActiveKeyboard = new INTKEYBOARDINFO;
  memset(_td->lpActiveKeyboard, 0, sizeof(INTKEYBOARDINFO));    // I2437 - Crash unloading keyboard due to keyboard options not init
	/*_td->lpActiveKeyboard->KeymanID = 0;
	_td->lpActiveKeyboard->nIMDLLs = 0;
	_td->lpActiveKeyboard->IMDLLs = NULL;
  _td->lpActiveKeyboard->KeyboardOptions = NULL;*/
  _splitpath_s(FileName, NULL, 0, NULL, 0, _td->lpActiveKeyboard->Name, sizeof(_td->lpActiveKeyboard->Name), NULL, 0);

	if(LoadKeyboard(_td->ForceFileName, &_td->lpActiveKeyboard->Keyboard)) 
	{
		SendDebugMessageFormat(0,sdmGlobal,0,"Keyman_ForceKeyboard: %s OK", FileName);
		ResetCapsLock();
		LoadDLLs(_td->lpActiveKeyboard);
		ActivateDLLs(_td->lpActiveKeyboard);
    LoadKeyboardOptions(_td->lpActiveKeyboard);   // I2437 - Crash unloading keyboard due to keyboard options not set
    RefreshPreservedKeys(TRUE);
		return TRUE;
	}

	SendDebugMessageFormat(0,sdmGlobal,0,"Keyman_ForceKeyboard: %s FAIL", FileName);

	delete _td->lpActiveKeyboard;
	_td->lpActiveKeyboard = NULL;

	_td->ForceFileName[0] = 0;
	return FALSE;
}

extern "C" BOOL _declspec(dllexport) WINAPI Keyman_StopForcingKeyboard()
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td)
  {
    SetLastError(ERROR_KEYMAN_THREAD_DATA_NOT_READY); // I3173   // I3525
    return FALSE;
  }

  if(!_td->lpActiveKeyboard)
  {
    SetLastError(ERROR_KEYMAN_KEYBOARD_NOT_ACTIVE); // I3173   // I3525
    return FALSE;
  }
	SendDebugMessageFormat(0,sdmGlobal,0,"Keyman_StopForcingKeyboard");
	if(_td->ForceFileName[0])
	{
		SendDebugMessageFormat(0,sdmGlobal,0,"Keyman_StopForcingKeyboard: Stopping forcing");
		if(!DeactivateDLLs(_td->lpActiveKeyboard)) return FALSE;  // I3173   // I3525
		if(!UnloadDLLs(_td->lpActiveKeyboard)) return FALSE;  // I3173   // I3525
		_td->ForceFileName[0] = 0;
    FreeKeyboardOptions(_td->lpActiveKeyboard);
		ReleaseKeyboardMemory(_td->lpActiveKeyboard->Keyboard);
    RefreshPreservedKeys(FALSE);
    delete _td->lpActiveKeyboard;
		_td->lpActiveKeyboard = NULL;
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

BOOL UpdateRefreshTag(LONG tag)   // I1835 - Reduce chatter
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;
  if(_td->RefreshTag_Process < tag)
  {
    _td->RefreshTag_Process = tag;
    return TRUE;
  }
  return FALSE;
  /*LONG lOriginal, lResult;
  do {
    lOriginal = _td->RefreshTag_Process;
    lResult = max(lOriginal, tag);
  } while(InterlockedCompareExchange(&_td->RefreshTag_Process, lResult, lOriginal) != lOriginal);
    
  return lResult > lOriginal;*/
}

void HandleRefresh(int code, LONG tag)
{
	switch(code)
	{
	case KR_REQUEST_REFRESH: 
		SendDebugMessageFormat(0,sdmGlobal,0,"#### Refresh Requested ####");
		//PostMessage(GetDesktopWindow(), wm_keyman_refresh, KR_PRE_REFRESH, 0);
		Globals::PostControllers(wm_keyman_refresh, KR_PRE_REFRESH, 0);
		Globals::PostControllers(wm_keyman_control, KMC_REFRESH, 0);
		break;

	case KR_PRE_REFRESH:
    tag = InterlockedIncrement(Globals::RefreshTag());
    
    if(UpdateRefreshTag(tag))
  		RefreshKeyboards(FALSE); // The Keyman window gets the update first
    PostMessage(HWND_BROADCAST, wm_keyman_refresh, KR_REFRESH, tag);
		break;

	case KR_REFRESH:
    if(UpdateRefreshTag(tag))
      RefreshKeyboards(FALSE);

    break;
	}
}

BOOL ConvertStringToGuid(WCHAR *buf, GUID *guid)   // I3581
{
  return IIDFromString(buf, guid) == S_OK;
}

void LoadBaseLayoutSettings() {   // I4552   // I4583
  char underlyingLayout[16];
  wchar_t baseLayout[MAX_PATH];
  DWORD dwUnderlyingLayout = 0;

	RegistryReadOnly *reg = new RegistryReadOnly(HKEY_CURRENT_USER);

	if(reg->OpenKeyReadOnly(REGSZ_KeymanCU)) {
		if(reg->ReadString(REGSZ_UnderlyingLayout, underlyingLayout, 15)) {
      dwUnderlyingLayout = strtoul(underlyingLayout, NULL, 16);   // I4516   // I4581
      wsprintf(underlyingLayout, "%08x", dwUnderlyingLayout);   // I3759   // I4581
    } else {
			underlyingLayout[0] = 0;
    }

    Globals::SetBaseKeyboardFlags(underlyingLayout, reg->ReadInteger(REGSZ_SimulateAltGr), !reg->ValueExists(REGSZ_DeadkeyConversionMode) || reg->ReadInteger(REGSZ_DeadkeyConversionMode));
	} else {
    Globals::SetBaseKeyboardFlags("", 0, 0);
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
      wsprintfW(baseLayoutAlt, L"%s-%s", langName, countryName);
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

void RefreshKeyboards(BOOL Initialising)
{
	char sz[_MAX_FNAME];
	char oldname[_MAX_FNAME];
	RegistryReadOnly *reg2;

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(_td->FInRefreshKeyboards) return;
  _td->FInRefreshKeyboards = TRUE;

  // Can happen when multiple top-level windows for one process

	Addin_Refresh();

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
					delete _td->lpKeyboards;
				}
				_td->nKeyboards = nk + 1;
				_td->lpKeyboards = kp;
			}

      kp = &_td->lpKeyboards[nk];   // I3760
			kp->KeymanID = atoi(sz);
			reg->ReadString(sz, kp->Name, 64);

			/* Read the shadow keyboard id */    // I3613

      reg2 = Reg_GetKeymanInstalledKeyboard(kp->Name);   // I3581
      if(reg2)
      {
        if(reg2->OpenKeyReadOnly(REGSZ_LanguageProfiles))   // I3581
        {
          char bufProfile[LOCALE_NAME_MAX_LENGTH];
          int nProfile = 0;
          kp->nProfiles = 0;
          kp->Profiles = NULL;
          while(reg2->GetKeyNames(bufProfile, LOCALE_NAME_MAX_LENGTH, nProfile))
          {
            RegistryReadOnly *reg3 = Reg_GetKeymanInstalledKeyboard(kp->Name);
            if(reg3->OpenKeyReadOnly(REGSZ_LanguageProfiles) && reg3->OpenKeyReadOnly(bufProfile))
            {
              if(reg3->ValueExists(REGSZ_ProfileGUID) && reg3->ValueExists("LangID") && reg3->ValueExists("Locale")) //TODO: constnats
              {
                kp->nProfiles++;
                if(kp->nProfiles == 1) kp->Profiles = new INTKEYBOARDPROFILE[1];
                else
                {
                  INTKEYBOARDPROFILE *pProfiles = new INTKEYBOARDPROFILE[kp->nProfiles];   // I3761
                  memcpy(pProfiles, kp->Profiles, sizeof(INTKEYBOARDPROFILE) * (kp->nProfiles-1));   // I3761
                  delete kp->Profiles;
                  kp->Profiles = pProfiles;
                }
                LPINTKEYBOARDPROFILE ikp = &kp->Profiles[kp->nProfiles-1];   // I3761

                WCHAR bufW[40];
                reg3->ReadString(L"profile guid", bufW, _countof(bufW)); //TODO define string constant
                ConvertStringToGuid(bufW, &ikp->Guid);
                reg3->ReadString(L"Locale", ikp->Locale, _countof(ikp->Locale));
                ikp->LangID = (LANGID) reg3->ReadInteger("LangID");
              }
            }
            delete reg3;
            nProfile++;
          }
        }
        delete reg2;
			}

			kp->Keyboard = NULL;

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
  PKEYMAN64THREADDATA _td = ThreadGlobals();
	if(!_td || _td->lpKeyboards) return;

	if(Lock) if(_td->lpActiveKeyboard && !_td->ForceFileName[0]) DeactivateDLLs(_td->lpActiveKeyboard);

	for(int i = 0; i < _td->nKeyboards; i++) 
	{
		if(Lock) UnloadDLLs(&_td->lpKeyboards[i]);
    FreeKeyboardOptions(&_td->lpKeyboards[i]);
		ReleaseKeyboardMemory(_td->lpKeyboards[i].Keyboard);
    if(_td->lpKeyboards[i].Profiles) delete _td->lpKeyboards[i].Profiles;   // I3581
	}

	_td->nKeyboards = 0;
	delete _td->lpKeyboards;

	_td->lpKeyboards = NULL;
	if(!_td->ForceFileName[0]) _td->lpActiveKeyboard = NULL;
}

#include "keymanapi.h"

extern "C" BOOL _declspec(dllexport) WINAPI Keyman_BuildKeyboardList(LPKEYBOARDINFO kbd, int *n) {   // I4461
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

  int nKeyboards = 0;

	for(int i = 0; i < _td->nKeyboards; i++) {
		if(!_td->lpKeyboards[i].Keyboard) LoadlpKeyboard(i);
    if(_td->lpKeyboards[i].nProfiles > 0) nKeyboards++;
  }

	if(!kbd) {
    *n = nKeyboards;
  }	else {
		for(int i = 0, j = 0; j < *n && i < _td->nKeyboards; i++)	{   // I4461
      if(_td->lpKeyboards[i].nProfiles > 0) {   // I4461
			  memcpy(&kbd[j++], &_td->lpKeyboards[i], sizeof(KEYBOARDINFO));
      }
		}
	}
	return TRUE;
}

extern "C" DWORD _declspec(dllexport) WINAPI Keyman_GetAPIVersion()
{
	return 0x0600;
}

extern "C" HWND  _declspec(dllexport) WINAPI Keyman_GetLastActiveWindow()
{
  return (HWND) Globals::SendMasterController(wm_keyman_control, KMC_GETLASTACTIVE, 0);
}

extern "C" HWND WINAPI Keyman_GetLastFocusWindow()
{
  return (HWND) Globals::SendMasterController(wm_keyman_control, KMC_GETLASTFOCUS, 0);
}

BOOL ShouldAttachToProcess()
{
  return TRUE;
#if 0
  char buf[MAX_PATH];  -- if you return false, the DLL repeatedly attempts to attach to the process for every message received by the process -- oops!

  HINSTANCE hinst = GetModuleHandle(LIBRARY_NAME);

  if(!GetModuleFileName(hinst, buf, MAX_PATH)) return 0;
  
	char drive[_MAX_DRIVE], dir[_MAX_DIR];

	_splitpath_s(buf, drive, _countof(drive), dir, _countof(dir), NULL, 0, NULL, 0);   // I3547
	_makepath_s(buf, _countof(buf), drive, dir, "process", ".cfg");   // I3547

  HANDLE hConfig = CreateFile(buf, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
  if(hConfig != INVALID_HANDLE_VALUE)
  {
    DWORD sz = 0;
    if(ReadFile(hConfig, buf, MAX_PATH-1, &sz, NULL))
    {
      CloseHandle(hConfig);
      assert(sz < MAX_PATH);
      buf[sz] = 0;
      _strlwr(buf);
      if(strstr(buf, "attachtoallprocesses") != NULL) return TRUE;
    }
    else
      CloseHandle(hConfig);
  }

  if(!GetModuleFileName(NULL, buf, MAX_PATH)) return 0;
  
  char *p = buf + lstrlen(buf) - lstrlen("cmd.exe");
  if(p >= buf && !lstrcmpi(p, "cmd.exe")) return 0; /* NEVER, EVER, ATTACH TO CMD */
  if(p >= buf && !lstrcmpi(p, "dwm.exe")) return 0; /* NEVER, EVER, ATTACH TO DWM */

  p = buf + lstrlen(buf) - lstrlen("csrss.exe");
  if(p >= buf && !lstrcmpi(p, "csrss.exe")) return 0; /* NEVER, EVER, ATTACH TO CSRSS */
  if(p >= buf && !lstrcmpi(p, "lsass.exe")) return 0; /* NEVER, EVER, ATTACH TO CSRSS */
  if(p >= buf && !lstrcmpi(p, "mstsc.exe")) return 0; /* NEVER, EVER, ATTACH TO RDPCLIENT */

  p = buf + lstrlen(buf) - lstrlen("ehmsas.exe");
  if(p >= buf && !lstrcmpi(p, "ehmsas.exe")) return 0; /* NEVER, EVER, ATTACH TO ehmsas */

  p = buf + lstrlen(buf) - lstrlen("regedit.exe");
  if(p >= buf && !lstrcmpi(p, "svchost.exe")) return 0; /* NEVER, EVER, ATTACH TO SVCHOST */
  if(p >= buf && !lstrcmpi(p, "wininit.exe")) return 0; /* NEVER, EVER, ATTACH TO WININIT */
  if(p >= buf && !lstrcmpi(p, "rdpclip.exe")) return 0; /* NEVER, EVER, ATTACH TO WININIT */
  if(p >= buf && !lstrcmpi(p, "wuauclt.exe")) return 0; /* NEVER, EVER, ATTACH TO WINDEBUG */

  p = buf + lstrlen(buf) - lstrlen("winlogon.exe");
  if(p >= buf && !lstrcmpi(p, "services.exe")) return 0; /* NEVER, EVER, ATTACH TO SERVICES */
  if(p >= buf && !lstrcmpi(p, "winlogon.exe")) return 0; /* NEVER, EVER, ATTACH TO WINLOGON */
  if(p >= buf && !lstrcmpi(p, "windebug.exe")) return 0; /* NEVER, EVER, ATTACH TO WINDEBUG */

  p = buf + lstrlen(buf) - lstrlen("vmwareuser.exe");
  if(p >= buf && !lstrcmpi(p, "vmwareuser.exe")) return 0; /* NEVER, EVER, ATTACH TO VMWAREUSER */

  return TRUE;
#endif
}


void PostDummyKeyEvent() {  // I3301 - Handle I3250 regression with inadvertent menu activation with Alt keys   // I3534   // I4844
  keybd_event(_VK_PREFIX, SCAN_FLAG_KEYMAN_KEY_EVENT, 0, 0); // I3250 - is this unnecessary?
  keybd_event(_VK_PREFIX, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, 0); // I3250 - is this unnecessary?
}
