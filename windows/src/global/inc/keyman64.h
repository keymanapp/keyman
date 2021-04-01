/*
  Name:             Keyman64
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Remove HWND parameter from SelectKeyboard
                    23 Aug 2006 - mcdurdin - Add version 7.0 system stores - VISUALKEYBOARD, KMW_RTL, KMW_HELPFILE, KMW_HELPTEXT, KMW_EMBEDJS
                    14 Sep 2006 - mcdurdin - Add IsSysTrayWindow function
                    30 May 2007 - mcdurdin - I864 - Log exceptions in hookprocs
                    13 Jul 2007 - mcdurdin - I934 - Prep for x64
                    05 Nov 2007 - mcdurdin - I1087 - Add hotkeys to switch languages (Pro)
                    27 Mar 2008 - mcdurdin - I1358 - Add TSS_WINDOWSLANGUAGES
                    27 Mar 2008 - mcdurdin - I1287 - switch keyboard and language together
                    14 Jun 2008 - mcdurdin - I1389 - BKSP single backspace for Vista+
                    27 Jan 2009 - mcdurdin - I1797 - AIWin2000 fallback
                    30 Jan 2009 - mcdurdin - I1835 - Improve refresh, debug performance
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    22 Mar 2010 - mcdurdin - Compiler tidyup
                    29 Mar 2010 - mcdurdin - I1089 - One keyboard - all applications
                    29 Mar 2010 - mcdurdin - I2265 - Keyboard switching reliability
                    06 Apr 2010 - mcdurdin - I2271 - Select Keyboard tidy up
                    04 May 2010 - mcdurdin - I2355 - Resolve deadlocks loading keyman32.dll
                    04 May 2010 - mcdurdin - I2349 - Pause key for debug log switch
                    04 May 2010 - mcdurdin - I2353 - DebugLastError function
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    01 Dec 2012 - mcdurdin - I3616 - V9.0 - Language association obsolete, strip out code
                    07 Nov 2013 - mcdurdin - I3949 - V9.0 - Keyboard selection and notification needs consolidation
                    17 Dec 2013 - mcdurdin - I4006 - V9.0 - Remove old aiDefault code
                    06 Mar 2014 - mcdurdin - I4124 - V9.0 - Language switch dialog is not working in v9
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    16 Jun 2014 - mcdurdin - I4271 - V9.0 - Switch language for all applications is not working
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    13 Aug 2014 - mcdurdin - I4370 - Deadkeys are still not working in Winword TIP mode
                    14 Aug 2014 - mcdurdin - I4379 - V9.0 - kmtip should use the Keyman debug logging framework
                    16 Oct 2014 - mcdurdin - I4462 - V9.0 - Keyman had a mismatch between KEYBOARDINFO and INTKEYBOARDINFO
                    03 Feb 2015 - mcdurdin - I4582 - V9.0 - Most underlying layout code in Keyman32 is now obsolete and needs to be removed
                    09 Aug 2015 - mcdurdin - I4844 - Tidy up PostDummyKeyEvent calls
*/
/***************************************************************************/   // I4006   // I4169

#ifndef _KEYMAN64_H
#define _KEYMAN64_H

#ifndef _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES
#define _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES 1
#endif

#ifndef _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT
#define _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT 1
#endif

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif

#ifndef STRICT
#define STRICT
#endif

#include <windows.h>
#include <assert.h>
#include <msctf.h>
#include "compiler.h"

#ifdef _WIN64
#define LIBRARY_NAME "KEYMAN64"
#else
#define LIBRARY_NAME "KEYMAN32"
#endif

#define NOT_IMPLEMENTED assert(FALSE)


#define ERROR_APP_MASK 0x20000000L
#define ERROR_KEYMAN_ALREADY_INIT (ERROR_APP_MASK | 0x00000001L)
#define ERROR_KEYMAN_SHUTTING_DOWN (ERROR_APP_MASK | 0x00000002L)
#define ERROR_KEYMAN_TLS_OUT_OF_INDEXES (ERROR_APP_MASK | 0x00000003L)
#define ERROR_KEYMAN_MEMORY_ALLOCATION_FAILED (ERROR_APP_MASK | 0x00000004L)
#define ERROR_KEYMAN_CORRUPT (ERROR_APP_MASK | 0x00000005L)
#define ERROR_KEYMAN_CANNOT_REINITIALISE_THREAD (ERROR_APP_MASK | 0x00000006L)
#define ERROR_KEYMAN_CANNOT_LOAD_PRODUCTS (ERROR_APP_MASK | 0x00000007L)
#define ERROR_KEYMAN_THREAD_DATA_NOT_READY (ERROR_APP_MASK | 0x00000008L)
#define ERROR_KEYMAN_KEYBOARD_NOT_ACTIVE (ERROR_APP_MASK | 0x00000009L)
#define ERROR_KEYMAN_TOO_MANY_CONTROLLERS (ERROR_APP_MASK | 0x0000000AL)

/* RefreshKeyboards message parameters */

#define KR_REQUEST_REFRESH  0
#define KR_PRE_REFRESH      1
#define KR_REFRESH          2
#define KR_SETTINGS_CHANGED 3   // Broadcast when Keyman Configuration settings change

/* WM_KEY* message analysis */

#define KEYMSG_LPARAM_SCAN(lParam) ((BYTE)(((lParam) & 0xFF0000) >> 16))
#define KEYMSG_FLAG_EXTENDED(lParam) (HIWORD(lParam) & KF_EXTENDED ? 1 : 0)
#define KEYMSG_FLAG_DLGMODE(lParam) (HIWORD(lParam) & KF_DLGMODE ? 1 : 0)
#define KEYMSG_FLAG_MENUMODE(lParam) (HIWORD(lParam) & KF_MENUMODE ? 1 : 0)
#define KEYMSG_FLAG_ALTDOWN(lParam) (HIWORD(lParam) & KF_ALTDOWN ? 1 : 0)
#define KEYMSG_FLAG_REPEAT(lParam) (HIWORD(lParam) & KF_REPEAT ? 1 : 0)
#define KEYMSG_FLAG_UP(lParam) (HIWORD(lParam) & KF_UP ? 1 : 0)

// TODO: Deprecate overloading of scancodes and use dwExtraInfo instead
#define SCAN_FLAG_KEYMAN_KEY_EVENT          0xFF

#define EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT 0x4B4D0000

/***************************************************************************/
// wm_keyman

#define RWM_KEYMAN "wm_keyman"

// wParam for wm_keyman
// These messages should be posted to a window
#define KM_DISABLEUI	1
#define KM_ENABLEUI		2
// These messages should be sent to a window
//#define KM_GETUISTATE	3
#define KM_FOCUSCHANGED	5		// Never use this flag: internal to Keyman
#define KM_ACTIVECHANGED 6  // Never use this flag: internal to Keyman
#define KM_EXITFLUSH  8 // Disconnects GetMessage hook

#define KMF_WINDOWCHANGED 1

/***************************************************************************/

typedef struct tagSTORE
{
	DWORD dwSystemID;
	PWSTR dpName;
	PWSTR dpString;
} STORE, *LPSTORE;


typedef struct tagKEY
{
	WCHAR Key;
	DWORD Line;
	DWORD ShiftFlags;
	PWSTR dpOutput;
	PWSTR dpContext;
} KEY, *LPKEY;


typedef struct tagGROUP
{
	PWSTR dpName;
	LPKEY dpKeyArray;		// [LPKEY] address of first item in key array
	PWSTR dpMatch;
	PWSTR dpNoMatch;
	DWORD cxKeyArray;		// in array entries
	BOOL  fUsingKeys;		// group(xx) [using keys] <-- specified or not
} GROUP, *LPGROUP;


typedef struct tagKEYBOARD
{
	DWORD dwIdentifier;		// Keyman compiled keyboard id

	DWORD dwFileVersion;	// Version of the file - Keyman 4.0 is 0x0400

	DWORD dwCheckSum;		// As stored in keyboard
	DWORD xxkbdlayout;    	// as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
	DWORD IsRegistered;		// layout id, from same registry key
	DWORD version;			// keyboard version

	DWORD cxStoreArray;		// in array entries
	DWORD cxGroupArray;		// in array entries

	LPSTORE dpStoreArray;	// [LPSTORE] address of first item in store array, from start of file
	LPGROUP dpGroupArray;	// [LPGROUP] address of first item in group array, from start of file

	DWORD StartGroup[2];	// index of starting groups [2 of them]
							// Ansi=0, Unicode=1

	DWORD dwFlags;			// Flags for the keyboard file

	DWORD dwHotKey;			// standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

	//PWSTR dpName;			// offset of name
	//PWSTR dpLanguageName;	// offset of language name;
	//PWSTR dpCopyright;		// offset of copyright
	//PWSTR dpMessage;		// offset of message in Keyboard About box

	HBITMAP	hBitmap;		// handle to the bitmap in the file;
} KEYBOARD, *LPKEYBOARD;

typedef BOOL (WINAPI *IMDLLHOOKProc)(HWND hwndFocus, WORD KeyStroke, WCHAR KeyChar, DWORD shiftFlags);

typedef struct tagIMDLLHOOK
{
	char name[32];
	DWORD storeno;
	IMDLLHOOKProc function;
} IMDLLHOOK, *LPIMDLLHOOK;

typedef struct tagIMDLL
{
	char        Filename[256];
	HMODULE		hModule;
	DWORD       nHooks;
	LPIMDLLHOOK Hooks;
} IMDLL, *LPIMDLL;

typedef struct tagINTKEYBOARDOPTIONS
{
  PWCHAR Value;
  PWCHAR OriginalStore;
} INTKEYBOARDOPTIONS, *LPINTKEYBOARDOPTIONS;

typedef struct tagINTKEYBOARDPROFILE
{
  //WCHAR Locale[LOCALE_NAME_MAX_LENGTH]; This is not currently used in keyman32/64 so we won't load it
  LANGID LangID;
  GUID Guid;
} INTKEYBOARDPROFILE, *LPINTKEYBOARDPROFILE;

// The members of this structure, from first through to IMDLLs, must match KEYBOARDINFO from keymanapi.h
typedef struct tagINTKEYBOARDINFO
{
	DWORD      KeymanID;
	DWORD      __filler_Hotkey;
  DWORD      __filler; // makes same as KEYBOARDINFO   // I4462
	char       Name[256];
	LPKEYBOARD Keyboard;
	DWORD      nIMDLLs;
	LPIMDLL    IMDLLs;
	int        __filler2; // makes same as KEYBOARDINFO
  LPINTKEYBOARDOPTIONS KeyboardOptions;
  int        nProfiles;
  LPINTKEYBOARDPROFILE Profiles;
} INTKEYBOARDINFO, *LPINTKEYBOARDINFO;

typedef struct tagINI
{
	int MsgStackSize;
	int MaxKeyboards;
	int ContextStackSize;
} INI;


typedef struct tagKMSTATE
{
	BOOL NoMatches;
	BOOL StopOutput;
	int LoopTimes;
	MSG msg;
	WORD vkey; // I934
	WCHAR charCode;   // I4582
	BOOL windowunicode;   // I4287
	LPKEYBOARD lpkb;
	LPGROUP startgroup;
} KMSTATE;
   // I3616
enum ProcessStringReturn {psrPostMessages, psrCheckMatches};

LRESULT WINAPI kmnGetMessageProc(int nCode, WPARAM wParam, LPARAM lParam);
LRESULT WINAPI kmnCallWndProc(int nCode, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK kmnLowLevelKeyboardProc(   // I4124
  _In_  int nCode,
  _In_  WPARAM wParam,
  _In_  LPARAM lParam
);

BOOL ReleaseKeyboardMemory(LPKEYBOARD kbd);

void PostGETNEXT(HWND hwnd);
BOOL CompareMsg(LPMSG MsgA, LPMSG MsgB);
BOOL ProcessHook();	// returns FALSE on error or key not matched [only for AITip]
BOOL ProcessMessage( LPMSG mp );
BOOL ProcessGroup(LPGROUP gp);
BOOL ContextMatch(LPKEY kkp);
int PostString(PWSTR str, LPMSG mp, LPKEYBOARD lpkb, PWSTR endstr);
//void PostKey( int keystroke, int msgflags, LPMSG msg );
//void GetINIAdvanced( void );
BOOL LoadAllKeymanKeyboards(DWORD layout);
void UpdateKeymanUI();

BOOL IsSysTrayWindow(HWND hwnd);

#define ShowDlgItem(hdlg, id, fShow ) ShowWindow( GetDlgItem( (hdlg), (id) ), (fShow) ? SW_SHOW : SW_HIDE )
#define EnableDlgItem(hdlg, id, fEnable ) EnableWindow( GetDlgItem( (hdlg), (id) ), (fEnable) )

BOOL InitialiseProcess(HWND hwnd);
BOOL UninitialiseProcess(BOOL Lock);
BOOL IsKeyboardUnicode();

BOOL IsFocusedThread();

BOOL SelectKeyboard(DWORD KeymanID);

extern "C" DWORD  _declspec(dllexport) WINAPI GetActiveKeymanID();

BOOL GetKeyboardFileName(LPSTR kbname, LPSTR buf, int nbuf);
BOOL LoadKeyboard(LPSTR fileName, LPKEYBOARD *lpKeyboard);
BOOL LoadlpKeyboard(int i);

PSTR wstrtostr(PWSTR in);
PWSTR strtowstr(PSTR in);

WCHAR MapVirtualKeys(WORD keyCode, UINT shiftFlags);

void SelectApplicationIntegration();   // I4287

void PostDummyKeyEvent();  // I3301 - Handle I3250 regression with inadvertent menu activation with Alt keys   // I3534   // I4844

/* Debugging functions */

#ifndef SendDebugMessage
   // I4379
typedef enum ATSDMState { sdmInternat, sdmAIDefault, sdmMessage, sdmKeyboard, sdmGlobal, sdmMenu, sdmDebug, sdmLoad, sdmOther } TSDMState;

void InitDebugging();
void UninitDebugging();

extern "C" void _declspec(dllexport) WINAPI Keyman_WriteDebugEvent(char *file, int line, PWCHAR msg);

#define SendDebugMessage(hwnd,state,kmn_lineno,msg) (ShouldDebug((state)) ? SendDebugMessage_1((hwnd),(state),(kmn_lineno), __FILE__, __LINE__, (msg)) : 0)
#define SendDebugMessageFormat(hwnd,state,kmn_lineno,msg,...) (ShouldDebug((state)) ? SendDebugMessageFormat_1((hwnd),(state),(kmn_lineno), __FILE__, __LINE__, (msg),__VA_ARGS__) : 0)
#define ShouldDebug(state) ShouldDebug_1()
#define DebugLastError(context) (DebugLastError_1(GetLastError(), (context), __FILE__,__LINE__,__FUNCTION__))
#define DebugLastError0(error, context) (DebugLastError_1((error), (context), __FILE__,__LINE__,__FUNCTION__))
int SendDebugMessage_1(HWND hwnd, TSDMState state, int kmn_lineno, char *file, int line, char *msg);
int SendDebugMessageFormat_1(HWND hwnd, TSDMState state, int kmn_lineno, char *file, int line, char *fmt, ...);
void DebugLastError_1(DWORD err, char *context, char *file, int line, char *func);
void DebugMessage(LPMSG msg, WPARAM wParam);
void DebugShift(char *function, char *point);
BOOL DebugSignalPause(BOOL fIsUp);
char *Debug_VirtualKey(WORD vk);
char *Debug_UnicodeString(PWSTR s, int x = 0);

BOOL ShouldDebug_1(); // TSDMState state);

#endif

#ifdef _DEBUG
#define OutputThreadDebugString(s) _OutputThreadDebugString(s)
void _OutputThreadDebugString(char *s);
#else
#define OutputThreadDebugString(s)
#endif

/* Keyboard selection functions */

void HandleRefresh(int code, LONG tag);
void RefreshKeyboards(BOOL Initialising);
void CheckScheduledRefresh();
void ScheduleRefresh();
void ReleaseKeyboards(BOOL Lock);
void CheckScheduledRefresh();
void ScheduleRefresh();

/* Glossary conversion functions */

HKL ForceKeymanIDToHKL(DWORD KeymanID); // sign extends DWORD to HKL on Win64
DWORD ForceHKLToKeymanID(HKL hkl);      // truncates HKL to DWORD on Win64
   // I4220
DWORD HKLToKeyboardID(HKL hkl);
WORD HKLToLanguageID(HKL hkl);
WORD HKLToLayoutNumber(HKL hkl);
WORD HKLToLayoutID(HKL hkl);
DWORD EthnologueCodeToKeymanID(DWORD EthCode);
DWORD EthnologueStringCodeToDWord(PWSTR EthCode);

PWSTR  GetSystemStore(LPKEYBOARD kb, DWORD SystemID);

DWORD ExceptionMessage(LPSTR Proc, LPEXCEPTION_POINTERS ep);

void keybd_shift(LPINPUT pInputs, int *n, BOOL isReset, LPBYTE const kbd);

//#define KEYEVENT_EXTRAINFO_KEYMAN 0xF00F0000   // I4370

#ifndef _KEYMAN64_LIGHT

#ifndef _WIN64   // I4326
#include "hotkeys.h"
#endif

#include "appint\appint.h"
#include "appint\aitip.h"
#include "appint\aiwin2000unicode.h"
#include "globals.h"

#include "capsstate.h"
#include "keystate.h"
#endif

#include "registry.h"

#ifndef _KEYMAN64_LIGHT
#include "calldll.h"
#include "addins.h"
#include "keymancontrol.h"
#include "keyboardoptions.h"
#endif

#include "unicode.h"
#include "xstring.h"

#ifndef _KEYMAN64_LIGHT
#include "syskbd.h"
#include "vkscancodes.h"
#include "rc4.h"

#include "testkeymanfunctioning.h"
#include "keynames.h"

#include "crc32.h"

#include "k32_tsf.h"

void ReportActiveKeyboard(WORD wCommand);   // I3933   // I3949
void SelectKeyboardHKL(DWORD hkl, BOOL foreground);  // I3933   // I3949   // I4271
BOOL SelectKeyboardTSF(DWORD KeymanID, BOOL foreground);   // I3933   // I3949   // I4271
BOOL ReportKeyboardChanged(WORD wCommand, DWORD dwProfileType, UINT langid, HKL hkl, GUID clsid, GUID guidProfile);
void ProcessModifierChange(UINT key, BOOL isUp, BOOL isExtended);   // I4793

#endif

#endif	// _KEYMAN64_H
