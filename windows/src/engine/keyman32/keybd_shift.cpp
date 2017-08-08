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

/**
  do_keybd_event adds a keyboard event into the keyboard event queue.

  Parameters: pInputs  array of INPUT structures which we will fill with our key events.
              n        pointer to current index into pInput, which we increment for each key
                       event we add

  This function mimics the keybd_event Windows API function, but instead of posting the event
  to the system keyboard event queue, it appends it to our internal event queue in pInputs.
  This allows us to send an atomic set of key events with SendInput.
*/
void do_keybd_event(LPINPUT pInputs, int *n, BYTE vk, BYTE scan, DWORD flags, ULONG_PTR extraInfo) {   // I4548
  INPUT input;
  input.type = INPUT_KEYBOARD;

  switch(vk) {
  case VK_RCONTROL:
    flags |= KEYEVENTF_EXTENDEDKEY;
  case VK_LCONTROL:
    vk = VK_CONTROL;
    break;

  case VK_RMENU:
    flags |= KEYEVENTF_EXTENDEDKEY;
  case VK_LMENU:
    vk = VK_MENU;
    break;

  case VK_RSHIFT:
    flags |= KEYEVENTF_EXTENDEDKEY;
  case VK_LSHIFT:
    vk = VK_SHIFT;
    break;
  }

  input.ki.wVk = vk;
  input.ki.wScan = scan;
  input.ki.dwFlags = flags;
  input.ki.time = 0;
  input.ki.dwExtraInfo = extraInfo;
  SendDebugMessageFormat(0, sdmAIDefault, 0, "do_keybd_event(n=%d, vk=%s, scan=%x, flags=%x)", *n, Debug_VirtualKey(vk), scan, flags);

  pInputs[*n] = input;
  (*n)++;
}

/**
  keybd_sendprefix pushes a dummy keystroke into the keyboard event queue.

  Parameters: pInputs  array of INPUT structures which we will fill with our key events.
              n        pointer to current index into pInput, which we increment for each key
                       event we add

  The keybd_sendprefix call is needed, for example, if the user presses Alt+F which matches a rule 
  in the current Keyman keyboard, because without the dummy prefix key, faking the release of the 
  Alt key will activate the mainmenu of the current application.
*/
void keybd_sendprefix(LPINPUT pInputs, int *n, BOOL *FPrefix)
{
  if(*FPrefix)
  {
    do_keybd_event(pInputs, n, _VK_PREFIX, 0xFF, 0, 0);   // I4548   // I4844
    do_keybd_event(pInputs, n, _VK_PREFIX, 0xFF, KEYEVENTF_KEYUP, 0);   // I4548   // I4844
    *FPrefix = FALSE;
  }
}

void keybd_sendshift(LPINPUT pInputs, int *n, BYTE *kbd, BYTE vkey, BOOL FReset, BOOL *FPrefix) {
	SendDebugMessageFormat(0, sdmAIDefault, 0, "keybd_sendshift: sending key%s - vkey=%s", FReset ? "down" : "up", Debug_VirtualKey(vkey));
  keybd_sendprefix(pInputs, n, FPrefix);
	do_keybd_event(pInputs, n, vkey, 0xFF, FReset ? 0 : KEYEVENTF_KEYUP, 0);   // I4548
}

BYTE MapGetAsyncKeyStateToByte(WORD vk) {
  SHORT r = GetAsyncKeyState(vk);
  return (r & 1) | ((r & 0x8000) >> 8);
}

void GetAsyncKeyboardShiftState(BYTE *kbd) {
  const WORD vks[] = { VK_CONTROL, VK_LCONTROL, VK_RCONTROL, VK_MENU, VK_LMENU, VK_RMENU, VK_SHIFT, VK_LSHIFT, VK_RSHIFT, 0 };
  memset(kbd, 0, 256);
  for (int i = 0; vks[i]; i++) {
    kbd[vks[i]] = MapGetAsyncKeyStateToByte(vks[i]);
  }
}

/**
  keybd_shift evaluates the current keyboard modifier state and queues key events in order to
  initially set modifiers to "up" and, after the output key events are queued, resets the modifiers
  to their initial state.

  Parameters: pInputs  array of INPUT structures which we will fill with our key events.
              n        pointer to current index into pInput, which we increment for each key 
                       event we add
              FReset   are we clearing or resetting the modifier state?
              kbd      pointer to keyboard state (256 byte array)
 
  There must be enough space in pInputs to contain 6 x up + 6 x down + 1 prefix-down + 1 prefix-up event = 14 events, 
  to support both the clear and reset calls.

  There is a potential for a race here because the user may release a modifier
  key between our test for the key state and when we SendInput, in which case the key will be 
  incorrectly re-pressed in the reset phase. This window of time is very small; we can improve the
  odds by using GetAsyncKeyboardShiftState (which reads the key state at the current instant, as 
  opposed to at the start of the current key event).
*/
void keybd_shift(LPINPUT pInputs, int *n, BOOL FReset, LPBYTE kbd) {
  const BYTE modifiers[6] = { VK_LMENU, VK_RMENU, VK_LCONTROL, VK_RCONTROL, VK_LSHIFT, VK_RSHIFT };
  BOOL FPrefix = !FReset;

  if (!FReset) {
    GetAsyncKeyboardShiftState(kbd);
  }

  for (int i = 0; i < _countof(modifiers); i++) {
    if (kbd[modifiers[i]] & 0x80) {
      keybd_sendshift(pInputs, n, kbd, modifiers[i], FReset, &FPrefix);
    }
  }
}
