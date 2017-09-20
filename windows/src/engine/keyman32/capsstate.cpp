/*
  Name:             capsstate
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    31 Dec 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Fix CAPS ON ONLY / CAPS ALWAYS OFF
                    22 Jan 2007 - mcdurdin - Fix CAPS ALWAYS OFF
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    23 Mar 2012 - mcdurdin - I3284 - Fix blocking of Ctrl+Shift passthrough to other hooks
                    04 Nov 2012 - mcdurdin - I3529 - V9.0 - Merge of I3284 - Fix blocking of Ctrl+Shift passthrough to other hooks
                    31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
*/
#include "keyman64.h"

static BYTE kbstate[256];
extern BOOL FShouldIgnoreNextKey[256];

//extern "C" void FAR PASCAL keybd_event(void);


void ResetCapsLock(void)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;

	if(!_td->lpActiveKeyboard) return;

	SendDebugMessageFormat(0, sdmGlobal, 0, "ResetCapsLock: enter");

	if(_td->lpActiveKeyboard->Keyboard->dwFlags & KF_CAPSALWAYSOFF) 
	{
		SendDebugMessageFormat(0, sdmGlobal, 0, "ResetCapsLock: caps lock should be always off");
		if(GetKeyState(VK_CAPITAL) & 1)
		{
			SendDebugMessageFormat(0, sdmGlobal, 0, "ResetCapsLock: caps lock is on, switching off caps lock");
			keybd_event(VK_CAPITAL, 0x3A, 0, 0);
			keybd_event(VK_CAPITAL, 0x3A, 0 | KEYEVENTF_KEYUP, 0);
		}
	}
	SendDebugMessageFormat(0, sdmGlobal, 0, "ResetCapsLock: exit");
}


void KeyCapsLockPress(BOOL FIsUp)  // I3284 - void   // I3529
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;
	if(!_td->lpActiveKeyboard) return;		// pass through to window

	if(_td->lpActiveKeyboard->Keyboard->dwFlags & KF_CAPSONONLY)
	{
		if(FIsUp && !(GetKeyState(VK_CAPITAL) & 1))		// I267 - 24/11/2006 invert GetKeyState test
		{
			keybd_event(VK_CAPITAL, 0xFF, 0, 0);
			keybd_event(VK_CAPITAL, 0xFF, KEYEVENTF_KEYUP, 0);
		}
	}
	else if(_td->lpActiveKeyboard->Keyboard->dwFlags & KF_CAPSALWAYSOFF)
	{
		if(!FIsUp && (GetKeyState(VK_CAPITAL) & 1))  
		{												// I267 - 24/11/2006 invert GetKeyState test
			keybd_event(VK_CAPITAL, 0xFF, KEYEVENTF_KEYUP, 0);
			keybd_event(VK_CAPITAL, 0xFF, 0, 0);
		}
	}
}


void KeyShiftPress(BOOL FIsUp)  // I3284 - void   // I3529
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return;
	if(!_td->lpActiveKeyboard) return;		// pass through to window
			
	if((GetKeyState(VK_CAPITAL) & 1) == 0) return;

	if(_td->lpActiveKeyboard->Keyboard->dwFlags & KF_SHIFTFREESCAPS)
	{
		if(!FIsUp)
		{
			keybd_event(VK_CAPITAL, 0xFF, 0, 0);
			keybd_event(VK_CAPITAL, 0xFF, KEYEVENTF_KEYUP, 0);
		}
	}
}

