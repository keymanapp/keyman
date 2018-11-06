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

/* External interface functions */

typedef struct tagKEYMAN64THREADDATA
{
  LPWORD IndexStack;
  LPWSTR miniContext;

  KMSTATE state;

  BOOL
	  FInitialised,
    FInitialising;

  BOOL debug_DebugInit;   // I3951
  char debug_buf[64];

  /* TSF Manager Globals */

  WPARAM LastKey;   // I4642
  BYTE LastScanCode;   // I4642
   
} KEYMAN64THREADDATA, *PKEYMAN64THREADDATA;

extern HINSTANCE g_hInstance;

/* Thread Local Data */

PKEYMAN64THREADDATA ThreadGlobals();

/* Temporary globals */

extern BOOL g_debug_ToConsole, g_debug_KeymanLog;
extern BOOL g_mnemonicDeadkeyConversionMode;
extern char g_baseKeyboard[16];
extern DWORD g_shiftState;
extern BOOL g_simulateAltGr;
extern wchar_t g_baseLayout[260], g_baseLayoutAlt[34];
extern INTKEYBOARDINFO g_keyboard;

#endif
