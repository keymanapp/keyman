/*
  Name:             keybd_shift
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      11 Dec 2009

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
                    09 Aug 2015 - mcdurdin - I4844 - Tidy up PostDummyKeyEvent calls
*/
#include "keyman64.h"

void do_keybd_event(LPINPUT pInputs, int *n, BYTE vk, BYTE scan, DWORD flags, ULONG_PTR extraInfo) {   // I4548
  INPUT input;
  input.type = INPUT_KEYBOARD;

  switch(vk) {
  case VK_LCONTROL:
  case VK_RCONTROL:
    vk = VK_CONTROL;
    break;
  case VK_LMENU:
  case VK_RMENU:
    vk = VK_MENU;
    break;
  }

  /*PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return 0;
  if(!_td->app) return 0;
  if(flags & KEYEVENTF_KEYUP) {
    _td->app->QueueAction(QIT_VKEYUP, vk);
  } else {
    _td->app->QueueAction(QIT_VKEYDOWN, vk);
  }*/

  input.ki.wVk = vk;
  input.ki.wScan = scan;
  input.ki.dwFlags = flags;
  input.ki.time = 0;
  input.ki.dwExtraInfo = extraInfo;
  SendDebugMessageFormat(0, sdmAIDefault, 0, "do_keybd_event(n=%d, vk=%s, scan=%x, flags=%x)", *n, Debug_VirtualKey(vk), scan, flags);

  pInputs[*n] = input;
  (*n)++;
  //if(SendInput(1, &input, sizeof(INPUT)) == 0) {
  //  SendDebugMessageFormat(0, sdmAIDefault, 0, "do_keybd_event: SendInput failed = %d", GetLastError());
  //}
  //keybd_event(vk, scan, flags, extraInfo);
}

void keybd_sendprefix(LPINPUT pInputs, int *n, BOOL *FPrefix)
{
  if(*FPrefix)
  {
    do_keybd_event(pInputs, n, _VK_PREFIX, 0xFF, 0, 0);   // I4548   // I4844
    do_keybd_event(pInputs, n, _VK_PREFIX, 0xFF, KEYEVENTF_KEYUP, 0);   // I4548   // I4844
    *FPrefix = FALSE;
  }
}

BOOL keybd_sendshift(LPINPUT pInputs, int *n, BYTE *kbd, int AShiftState, BYTE vkey, int flags, BOOL *FPrefix)
{
	DWORD eventflags = 0;

	/* 29 Jul 2003 - mcdurdin - Handle sending correct Right C/A "extendedkey" bit */
	if(vkey == VK_RMENU || vkey == VK_RCONTROL) eventflags |= KEYEVENTF_EXTENDEDKEY;
	
	if(AShiftState & flags)
	{
		if((kbd[vkey] & 0x80) == 0) 
		{
			SendDebugMessageFormat(0, sdmAIDefault, 0, "keybd_sendshift: sending keydown - vkey=%s", Debug_VirtualKey(vkey));
      keybd_sendprefix(pInputs, n, FPrefix);
			do_keybd_event(pInputs, n, vkey, 0xFF, eventflags, 0);   // I4548
      return TRUE;
		}
	}
	else
	{
		if(kbd[vkey] & 0x80)
		{
			SendDebugMessageFormat(0, sdmAIDefault, 0, "keybd_sendshift: sending keyup - vkey=%s", Debug_VirtualKey(vkey));
      keybd_sendprefix(pInputs, n, FPrefix);
			do_keybd_event(pInputs, n, vkey, 0xFF, KEYEVENTF_KEYUP|eventflags, 0);   // I4548
      return TRUE;
		}
	}
  return FALSE;
}

BYTE MapGetAsyncKeyStateToByte(WORD vk) {
  SHORT r = GetAsyncKeyState(vk);
  return (r&1) | ((r&0x8000)>>8);
}

void GetAsyncKeyboardShiftState(BYTE *kbd) {
  const WORD vks[] = {VK_CONTROL, VK_LCONTROL, VK_RCONTROL, VK_MENU, VK_LMENU, VK_RMENU, VK_SHIFT, 0};
  memset(kbd, 0, 256);
  for(int i = 0; vks[i]; i++) {
    kbd[vks[i]] = MapGetAsyncKeyStateToByte(vks[i]);
  }
}

BOOL keybd_shift(LPINPUT pInputs, int *n, BOOL FReset, LPBYTE kbd) {
  //int AShiftFlags;
  BOOL FPrefix = !FReset;
  BOOL res = FALSE;

  if (!FReset) {
    GetKeyboardState(kbd);

    // Clear all modifiers

    if (kbd[VK_LMENU] & 0x80 || kbd[VK_RMENU] & 0x80 || kbd[VK_MENU] & 0x80) {
      res |= keybd_sendshift(pInputs, n, kbd, 0, VK_LMENU, LALTFLAG | K_ALTFLAG, &FPrefix);
      res |= keybd_sendshift(pInputs, n, kbd, 0, VK_RMENU, RALTFLAG, &FPrefix);
    }

    if (kbd[VK_LCONTROL] & 0x80 || kbd[VK_RCONTROL] & 0x80 || kbd[VK_CONTROL] & 0x80) {
      res |= keybd_sendshift(pInputs, n, kbd, 0, VK_LCONTROL, LCTRLFLAG | K_CTRLFLAG, &FPrefix);
      res |= keybd_sendshift(pInputs, n, kbd, 0, VK_RCONTROL, RCTRLFLAG, &FPrefix);
    }

    if (kbd[VK_SHIFT] & 0x80) {
      res |= keybd_sendshift(pInputs, n, kbd, 0, VK_LSHIFT, K_SHIFTFLAG, &FPrefix);
      res |= keybd_sendshift(pInputs, n, kbd, 0, VK_RSHIFT, K_SHIFTFLAG, &FPrefix);
    }

  } else {

    // Reset modifiers to desired output

    if (kbd[VK_RMENU] & 0x80) {
      kbd[VK_RMENU] = 0;
      res |= keybd_sendshift(pInputs, n, kbd, RALTFLAG, VK_RMENU, RALTFLAG, &FPrefix);
    } else if (kbd[VK_LMENU] & 0x80 || kbd[VK_MENU] & 0x80) {
      kbd[VK_LMENU] = 0;
      res |= keybd_sendshift(pInputs, n, kbd, LALTFLAG, VK_LMENU, LALTFLAG | K_ALTFLAG, &FPrefix);
    }

    if (kbd[VK_RCONTROL] & 0x80) {
      kbd[VK_RCONTROL] = 0;
      res |= keybd_sendshift(pInputs, n, kbd, RCTRLFLAG, VK_RCONTROL, RCTRLFLAG, &FPrefix);
    }
    else if (kbd[VK_LCONTROL] & 0x80 || kbd[VK_CONTROL] & 0x80) {
      kbd[VK_LCONTROL] = 0;
      res |= keybd_sendshift(pInputs, n, kbd, LCTRLFLAG, VK_LCONTROL, LCTRLFLAG | K_CTRLFLAG, &FPrefix);
    }

    if (kbd[VK_RSHIFT] & 0x80) {
      kbd[VK_RSHIFT] = 0;
      res |= keybd_sendshift(pInputs, n, kbd, K_SHIFTFLAG, VK_RSHIFT, K_SHIFTFLAG, &FPrefix);
    }
    else if (kbd[VK_LSHIFT] & 0x80 || kbd[VK_SHIFT] & 0x80) {
      kbd[VK_LSHIFT] = 0;
      res |= keybd_sendshift(pInputs, n, kbd, K_SHIFTFLAG, VK_LSHIFT, K_SHIFTFLAG, &FPrefix);
    }
  }

  // We only want to put out a prefix code if we are at the start of the output
	/*SendDebugMessageFormat(0, sdmAIDefault, 0, "keybd_shift: kbd[CAS]: %x %x %x | %x %x %x | %x %x %x "
		"AShiftFlags[CAS]: %d %d %d | %d %d %d | %d",
		kbd[VK_CONTROL], kbd[VK_LCONTROL], kbd[VK_RCONTROL], kbd[VK_MENU], kbd[VK_LMENU], kbd[VK_RMENU], kbd[VK_SHIFT], kbd[VK_LSHIFT], kbd[VK_RSHIFT],
		AShiftFlags & K_CTRLFLAG ? 1 : 0,
		AShiftFlags & LCTRLFLAG ? 1 : 0,
		AShiftFlags & RCTRLFLAG ? 1 : 0,
		AShiftFlags & K_ALTFLAG ? 1 : 0,
		AShiftFlags & LALTFLAG ? 1 : 0,
		AShiftFlags & RALTFLAG ? 1 : 0,
		AShiftFlags & K_SHIFTFLAG ? 1 : 0);*/

  return res;
}
