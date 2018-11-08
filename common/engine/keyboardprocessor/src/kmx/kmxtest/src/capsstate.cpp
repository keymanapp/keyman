/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "pch.h"

void ResetCapsLock(void)
{
	DebugLog("ResetCapsLock: enter");

	if(g_keyboard.Keyboard->dwFlags & KF_CAPSALWAYSOFF) 
	{
		DebugLog("ResetCapsLock: caps lock should be always off");
		if(GetKeyState(VK_CAPITAL) & 1)
		{
			DebugLog("ResetCapsLock: caps lock is on, switching off caps lock");
      GetApp()->QueueAction(QIT_CAPSLOCK, 0);
		}
	}
	DebugLog("ResetCapsLock: exit");
}


void KeyCapsLockPress(BOOL FIsUp)  // I3284 - void   // I3529
{
	if(g_keyboard.Keyboard->dwFlags & KF_CAPSONONLY)
	{
		if(FIsUp && !(GetKeyState(VK_CAPITAL) & 1))		// I267 - 24/11/2006 invert GetKeyState test
		{
      GetApp()->QueueAction(QIT_CAPSLOCK, 1);
		}
	}
	else if(g_keyboard.Keyboard->dwFlags & KF_CAPSALWAYSOFF)
	{
		if(!FIsUp && (GetKeyState(VK_CAPITAL) & 1))  
		{												// I267 - 24/11/2006 invert GetKeyState test
      GetApp()->QueueAction(QIT_CAPSLOCK, 0);
		}
	}
}


void KeyShiftPress(BOOL FIsUp)  // I3284 - void   // I3529
{
	if((GetKeyState(VK_CAPITAL) & 1) == 0) return;

	if(g_keyboard.Keyboard->dwFlags & KF_SHIFTFREESCAPS)
	{
		if(!FIsUp)
		{
      GetApp()->QueueAction(QIT_CAPSLOCK, 0);
		}
	}
}

