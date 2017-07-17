/*
  Name:             globals
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
  History:          01 Aug 2006 - mcdurdin - Add hotkeys
                    14 Sep 2006 - mcdurdin - Add UpdateActiveWindows function
                    16 May 2007 - mcdurdin - I819 - Test if Keyman is functioning
                    05 Nov 2007 - mcdurdin - I1087 - Add language switching hotkeys for Desktop Pro
                    20 Jul 2008 - mcdurdin - I1412 - Keyman Engine starts multiple times in Tutorial
                    16 Jan 2009 - mcdurdin - I1552 - CheckControllers function
                    30 Jan 2009 - mcdurdin - I1835 - Improve refresh performance
                    09 Mar 2009 - mcdurdin - I1888 - Keyman Engine can crash with some multithreaded applications
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    07 Sep 2009 - mcdurdin - I2098 - Some debug messages can be lost in multi-threaded apps
                    07 Sep 2009 - mcdurdin - I2096 - 0x88 fake keystroke
                    11 Dec 2009 - mcdurdin - I1455 - Remove shell and cbt hook
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    11 Dec 2009 - mcdurdin - I934 - x64 support - keyboard hook per thread
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    29 Mar 2010 - mcdurdin - I1089 - One keyboard - all applications
                    06 Apr 2010 - mcdurdin - I2271 - Select Keyboard tidy up
                    19 Apr 2010 - mcdurdin - I2297 - Switching languages automatically was not quite stable
                    04 May 2010 - mcdurdin - I2297 - Work on keyboard switching robustness
                    30 Nov 2010 - mcdurdin - I2543 - Support switching to TSF TIPs
                    01 Dec 2012 - mcdurdin - I3616 - V9.0 - Language association obsolete, strip out code
                    01 Dec 2012 - mcdurdin - I3617 - V9.0 - Keyboard hook obsolete, strip out code
                    01 Dec 2012 - mcdurdin - I3618 - V9.0 - TSF language association obsolete, strip out code
                    07 Nov 2013 - mcdurdin - I3951 - V9.0 - Add debug-to-console hidden option for Keyman32
                    06 Mar 2014 - mcdurdin - I4124 - V9.0 - Language switch dialog is not working in v9
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    26 Jun 2014 - mcdurdin - I4290 - Keys that have rules but are not matched due to context do not generate output
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    25 Sep 2014 - mcdurdin - I4412 - V9.0 - Character Map needs to insert characters using SendInput
                    06 Feb 2015 - mcdurdin - I4583 - V9.0 - Remove altgr lookup test from keyman32 and put it into the registry
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
                    29 Mar 2015 - mcdurdin - I4642 - V9.0 - In Logos, generated backspace receives a scan of 0x00 instead of 0xFF
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/
   // I4287   // I5136
#ifndef _globals_h
#define _globals_h

#define GLOBAL_ContextStackSize 80
#define GLOBAL_MsgStackSize 80
#define GLOBAL_MaxKeyboards 32

class Globals
{
public:
	//static INI *Ini();

	static HHOOK *hhookGetMessage();
	static HHOOK *hhookCallWndProc();

#ifndef _WIN64
  static HHOOK *hhookLowLevelKeyboardProc();   // I4124
#endif

  static HWND *hLastActiveWnd();
	static HWND *hLastFocusWnd();

	static UINT *WindowsVersion();

	static BOOL *Keyman_Initialised();
	static BOOL *Keyman_Shutdown();

	static DWORD *FSingleThread();
  static DWORD *InitialisingThread();   // I4326
	
  //static DWORD *ActiveKeymanID();
  //static HKL *ActiveKeyboardLayout();

	static HWND *hwndIM();
	static BOOL *hwndIMAlways();

	static DWORD *ShiftState();

  static LONG *RefreshTag();

	static HHOOK get_hhookGetMessage();
	static HHOOK get_hhookCallWndProc();
  
#ifndef _WIN64
  static HHOOK get_hhookLowLevelKeyboardProc();   // I4124
#endif

  static HWND get_hLastActiveWnd();
	static HWND get_hLastFocusWnd();

	static UINT get_WindowsVersion();

	static BOOL get_Keyman_Initialised();
	static BOOL get_Keyman_Shutdown();

	static DWORD get_FSingleThread();
  static DWORD get_InitialisingThread();   // I4326

	static DWORD get_ShiftState();

  static char *get_BaseKeyboard();   // I4583
  static BOOL get_SimulateAltGr();   // I4583
  static void SetBaseKeyboardFlags(char *baseKeyboard, BOOL simulateAltGr, BOOL mnemonicDeadkeyConversionMode);   // I4583   // I4552

  static wchar_t *get_BaseKeyboardName();   // I4583
  static wchar_t *get_BaseKeyboardNameAlt();   // I4583

  static BOOL get_MnemonicDeadkeyConversionMode();   // I4583   // I4552

  static void SetBaseKeyboardName(wchar_t *baseKeyboardName, wchar_t *baseKeyboardNameAlt);   // I4583

	static BOOL IsControllerWindow(HWND hwnd);

	static LRESULT SendMasterController(UINT msg, WPARAM wParam, LPARAM lParam);
	static void PostMasterController(UINT msg, WPARAM wParam, LPARAM lParam);
  
  static BOOL ResetControllers();

	static void PostControllers(UINT msg, WPARAM wParam, LPARAM lParam);
  static void PostProcessControllers(UINT msg, WPARAM wParam, LPARAM lParam);
  static BOOL IsControllerProcess();

  static BOOL InitHandles();
  static BOOL Lock();
  static BOOL Unlock();
  static BOOL CheckControllers();
};

/* External interface functions */

typedef HRESULT (WINAPI *PKEYMANPROCESSOUTPUTFUNC)(int n, WCHAR *buf, int nbuf);
typedef HRESULT (WINAPI *PKEYMANGETCONTEXTFUNC)(int n, PWSTR buf);
typedef BOOL (WINAPI *PTIPCALLBACK)();  // Tells the TIP to update its status (used to be done through 0x88)

typedef BOOL (WINAPI *PKeymanOutputBackspace)(HWND hwnd);
typedef BOOL (WINAPI *PKeymanOutputChar)(HWND hwnd, DWORD chr);
typedef BOOL (WINAPI *PKeymanFocusChanged)();
typedef BOOL (WINAPI *PKeymanShouldProcess)(HWND hwnd);
typedef BOOL (WINAPI *PKeymanInit)();

typedef struct tagAddin
{
	char ClassName[128];
	char Application[260];
	char AddinName[260];
	HINSTANCE hAddin;
	PKeymanOutputChar      OutputChar;
	PKeymanOutputBackspace OutputBackspace;
	PKeymanFocusChanged    FocusChanged;
	PKeymanShouldProcess   ShouldProcess;
	PKeymanInit            Initialise;
	PKeymanInit            Uninitialise;
} Addin;


typedef struct tagKEYMANHKLPAIR
{
  DWORD KeymanID;
  DWORD hkl;  // not HKL because upper bytes are unused in Win64 and sign extension is just a hassle
} KEYMANHKLPAIR;

#define MAXCACHEDKEYBOARDLAYOUTS 128

typedef struct tagKEYMAN64THREADDATA
{
  LPINTKEYBOARDINFO lpKeyboards;			// keyboard definitions
  LPINTKEYBOARDINFO lpActiveKeyboard;
   // I3616
  LPMSG msgbuf;						// Message buffer (alloc at runtime)

  int nKeyboards;						// nLoadedKeyboards
  int nLanguages;           // I1087

  LPWORD IndexStack;
  LPWSTR miniContext;

  KMSTATE state;

  AITIP *app;

  BOOL
	  KeymanUIDisabled,
	  FInitialised,
    FInitialising;

  char ForceFileName[MAX_PATH];

  DWORD ActiveKeymanID;

  /* TIP Globals */

  PKEYMANPROCESSOUTPUTFUNC TIPProcessOutput;
  PKEYMANGETCONTEXTFUNC TIPGetContext;
 
  BOOL TIPFUpdateable, TIPFPreserved;   // I4290

  BOOL FInRefreshKeyboards;
  LONG RefreshTag_Process;

  /* Addin Globals */

  Addin *Addins;
  int nAddins;
  char CurrentClassName[128];
  int CurrentAddin;
  HWND CurrenthWnd;

  void *debug_fp;
  HANDLE debug_hLogMailSlot, debug_hLogEvent;
  BOOL debug_DebugInit, debug_KeymanLog, debug_ToConsole;   // I3951
  char debug_buf[64];

   // I3617   // I3618


  /* TSF Manager Globals */

  HMODULE hMSCTF;
  ITfInputProcessorProfiles *pInputProcessorProfiles;
  ITfInputProcessorProfileMgr *pInputProcessorProfileMgr;
  BOOL TSFFailed;
  WPARAM LastKey;   // I4642
  BYTE LastScanCode;   // I4642
} KEYMAN64THREADDATA, *PKEYMAN64THREADDATA;

extern UINT 
  wm_keyman,						// user message - ignore msg
  wm_kmdebug,						//  " "  "  "   - debugging
  wm_kmmessage,					// message to Keyman window
  wm_keyman_control,				// messages to main Keyman window - replaces WM_USER+*
  wm_keyman_control_internal,       // messages to all windows to notify of changes to Keyman   // I4412
  wm_keymankeydown,
  wm_keymankeyup,
  wm_keymandebug,
  wm_keyman_grabwindowproc,
  wm_keyman_refresh,
  wm_kmgetactivekeymanid,
  wm_keymanshift,
  wm_keymanim_close,
  wm_keymanim_contextchanged,
  wm_test_keyman_functioning;

void UpdateActiveWindows();


/* Thread Local Data */


PKEYMAN64THREADDATA Globals_InitThread();
void Globals_UninitThread();
BOOL Globals_InitProcess();
void Globals_UninitProcess();
PKEYMAN64THREADDATA ThreadGlobals();
BOOL Globals_ProcessInitialised();


#endif
