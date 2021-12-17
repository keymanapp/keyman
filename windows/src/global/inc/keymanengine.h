/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Name:             KeymanEngine
 * Description:      Main header file for windows engine
 *
*/
/***************************************************************************/   // I4006   // I4169

#ifndef _KEYMANENGINE_H
#define _KEYMANENGINE_H

#ifndef _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES
#define _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES 1
#endif

#ifndef _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT
#define _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT 1
#endif

// For keyboardprocessor_bits.h
#ifndef KMN_KBP_STATIC
#define KMN_KBP_STATIC
#endif
// For keyboardprocessor_bits.h
#ifndef _WIN32
#define _WIN32 1
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
#include <keyman/keyboardprocessor.h>

/***************************************************************************/

typedef BOOL(WINAPI* IMDLLHOOKProc)(HWND hwndFocus, WORD KeyStroke, WCHAR KeyChar, DWORD shiftFlags);

typedef struct tagIMDLLHOOK
{
  char name[32];
  DWORD storeno;
  IMDLLHOOKProc function;
} IMDLLHOOK, * LPIMDLLHOOK;

typedef struct tagIMDLL
{
  char        Filename[256];
  HMODULE		hModule;
  DWORD       nHooks;
  LPIMDLLHOOK Hooks;
} IMDLL, * LPIMDLL;

typedef struct tagINTKEYBOARDOPTIONS
{
  PWCHAR Value;
  PWCHAR OriginalStore;
} INTKEYBOARDOPTIONS, * LPINTKEYBOARDOPTIONS;

typedef struct tagINTKEYBOARDPROFILE
{
  //WCHAR Locale[LOCALE_NAME_MAX_LENGTH]; This is not currently used in keyman32/64 so we won't load it
  LANGID LangID;
  GUID Guid;
} INTKEYBOARDPROFILE, * LPINTKEYBOARDPROFILE;

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
  km_kbp_keyboard* lpCoreKeyboard;
  km_kbp_option_item* lpCoreKeyboardOptions;
  km_kbp_state* lpCoreKeyboardState;
  km_kbp_keyboard_imx* lpIMXList;
  DWORD nIMDLLHooks;  // TODO Remove as not needed
  LPIMDLLHOOK* lpIMDLLHooks;  // TODO Remove as not needed
} INTKEYBOARDINFO, * LPINTKEYBOARDINFO;

typedef struct tagINI
{
  int MsgStackSize;
  int MaxKeyboards;
  int ContextStackSize;
} INI;

typedef struct tagKMSTATE
{
  BOOL NoMatches;
  MSG msg;
  // TODO: 5442 will remove these once windows core is deprecated
  BOOL StopOutput;
  int LoopTimes;
  // TODO: 5442
  WORD vkey;           // I934
  WCHAR charCode;      // I4582
  BOOL windowunicode;  // I4287
  BOOL isDown;
  LPKEYBOARD lpkb;
  km_kbp_keyboard* lpCoreKb;  //  future use with IMDLL
  LPGROUP startgroup;         // TODO: 5442 will remove this once windows core is deprecated
} KMSTATE;

// I3616
enum ProcessStringReturn { psrPostMessages, psrCheckMatches };

LRESULT WINAPI kmnGetMessageProc(int nCode, WPARAM wParam, LPARAM lParam);
LRESULT WINAPI kmnCallWndProc(int nCode, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK kmnLowLevelKeyboardProc(   // I4124
  _In_  int nCode,
  _In_  WPARAM wParam,
  _In_  LPARAM lParam
);

BOOL ReleaseKeyboardMemory(LPKEYBOARD kbd);
BOOL ReleaseStateMemoryCore(km_kbp_state** state);
BOOL ReleaseKeyboardMemoryCore(km_kbp_keyboard** kbd);

void PostGETNEXT(HWND hwnd);
BOOL CompareMsg(LPMSG MsgA, LPMSG MsgB);
BOOL ProcessHook();	// returns FALSE on error or key not matched [only for AITip]
BOOL ProcessMessage( LPMSG mp );
BOOL ProcessGroup(LPGROUP gp);
BOOL ContextMatch(LPKEY kkp);
int PostString(PWSTR str, LPMSG mp, LPKEYBOARD lpkb, PWSTR endstr);
BOOL LoadAllKeymanKeyboards(DWORD layout);

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

PSTR wstrtostr(PCWSTR in);
PWSTR strtowstr(PSTR in);

WCHAR MapVirtualKeys(WORD keyCode, UINT shiftFlags);

void SelectApplicationIntegration();   // I4287

void PostDummyKeyEvent();  // I3301 - Handle I3250 regression with inadvertent menu activation with Alt keys   // I3534   // I4844

/* Debugging functions */

BOOL IsDebugAssertEnabled();

#ifndef SendDebugMessage
   // I4379
typedef enum ATSDMState { sdmInternat, sdmAIDefault, sdmMessage, sdmKeyboard, sdmGlobal, sdmMenu, sdmDebug, sdmLoad, sdmOther } TSDMState;

void InitDebugging();
void UninitDebugging();

extern "C" void _declspec(dllexport) WINAPI Keyman_WriteDebugEvent(char* file, int line, PWCHAR msg);

#define SendDebugMessage(hwnd,state,kmn_lineno,msg) (ShouldDebug((state)) ? SendDebugMessage_1((hwnd),(state),(kmn_lineno), __FILE__, __LINE__, (msg)) : 0)
#define SendDebugMessageFormat(hwnd,state,kmn_lineno,msg,...) (ShouldDebug((state)) ? SendDebugMessageFormat_1((hwnd),(state),(kmn_lineno), __FILE__, __LINE__, (msg),__VA_ARGS__) : 0)
#define ShouldDebug(state) ShouldDebug_1()
#define DebugLastError(context) (DebugLastError_1(GetLastError(), (context), __FILE__,__LINE__,__FUNCTION__))
#define DebugLastError0(error, context) (DebugLastError_1((error), (context), __FILE__,__LINE__,__FUNCTION__))
// On failed condition log "message", return FALSE
// and assert if a debugger is attached for a debug build.
#define DebugAssert(condition, message) (DebugAssert_1((condition),(message), __FILE__, __LINE__))
int SendDebugMessage_1(HWND hwnd, TSDMState state, int kmn_lineno, char* file, int line, char* msg);
int SendDebugMessageFormat_1(HWND hwnd, TSDMState state, int kmn_lineno, char* file, int line, char* fmt, ...);
void DebugLastError_1(DWORD err, char* context, char* file, int line, char* func);
void DebugMessage(LPMSG msg, WPARAM wParam);
void DebugShift(char* function, char* point);
BOOL DebugSignalPause(BOOL fIsUp);
char* Debug_VirtualKey(WORD vk);
char* Debug_UnicodeString(PWSTR s, int x = 0);
BOOL DebugAssert_1(BOOL condition, char* msg, char* file, int line);
BOOL ShouldDebug_1(); // TSDMState state);

#endif

#ifdef _DEBUG
#define OutputThreadDebugString(s) _OutputThreadDebugString(s)
void _OutputThreadDebugString(char* s);
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

void keybd_shift(LPINPUT pInputs, int* n, BOOL isReset, LPBYTE const kbd);

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

#include "calldll.h"
#include "addins.h"
#include "keymancontrol.h"
#include "keyboardoptions.h"
#include "kmprocessactions.h"

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

#endif  // _KEYMAN64_LIGHT
#endif  // _KEYMANENGINE_H
