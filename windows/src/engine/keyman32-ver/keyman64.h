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
#include "keyman64-build-version.h"
#include "../../../../common/windows/cpp/include/legacy_kmx_file.h"
#include "../../../../common/windows/cpp/include/registry.h"
#include "../../../../common/windows/cpp/include/unicode.h"
#include "../../../../common/windows/cpp/include/xstring.h"
#include "../../../../common/windows/cpp/include/legacy_kmx_memory.h"

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

/*********************************************************************/

#endif	// _KEYMAN64_H
