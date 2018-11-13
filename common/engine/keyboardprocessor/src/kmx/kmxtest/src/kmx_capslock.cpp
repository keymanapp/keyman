/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "pch.h"

void KMX_Processor::ResetCapsLock(void)
{
	DebugLog("ResetCapsLock: enter");

	if(g_keyboard.Keyboard->dwFlags & KF_CAPSALWAYSOFF) 
	{
		DebugLog("ResetCapsLock: caps lock should be always off");
    if(g_environment.g_capsLock)
		{
			DebugLog("ResetCapsLock: caps lock is on, switching off caps lock");
      m_actions.QueueAction(QIT_CAPSLOCK, 0);
		}
	}
	DebugLog("ResetCapsLock: exit");
}


void KMX_Processor::KeyCapsLockPress(BOOL FIsUp)  // I3284 - void   // I3529
{
	if(g_keyboard.Keyboard->dwFlags & KF_CAPSONONLY)
	{
		if(FIsUp && !g_environment.g_capsLock)		// I267 - 24/11/2006 invert GetKeyState test
		{
      m_actions.QueueAction(QIT_CAPSLOCK, 1);
		}
	}
	else if(g_keyboard.Keyboard->dwFlags & KF_CAPSALWAYSOFF)
	{
		if(!FIsUp && g_environment.g_capsLock)
		{												// I267 - 24/11/2006 invert GetKeyState test
      m_actions.QueueAction(QIT_CAPSLOCK, 0);
		}
	}
}


void KMX_Processor::KeyShiftPress(BOOL FIsUp)  // I3284 - void   // I3529
{
	if(!g_environment.g_capsLock) return;

	if(g_keyboard.Keyboard->dwFlags & KF_SHIFTFREESCAPS)
	{
		if(!FIsUp)
		{
      m_actions.QueueAction(QIT_CAPSLOCK, 0);
		}
	}
}

