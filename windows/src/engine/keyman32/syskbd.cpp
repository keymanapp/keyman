/*
  Name:             syskbd
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      22 Jan 2007

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          22 Jan 2007 - mcdurdin - Don't translate any number pad keys
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    30 Nov 2009 - mcdurdin - I934 - Prep for x64 - change UINT to WORD for vkeys
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    15 Jun 2012 - mcdurdin - I3358 - Media keys generate characters when positional keyboard is active
                    04 Nov 2012 - mcdurdin - I3535 - V9.0 - Merge of I3358 - Media keys generate characters when positional keyboard is active
                    28 Nov 2012 - mcdurdin - I3597 - V9.0 - Move VK_ defines to global header
                    17 Jan 2013 - mcdurdin - I3762 - V9.0 - underlying layout cannot be controlled cleanly via TSF so support translation
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    03 Feb 2015 - mcdurdin - I4582 - V9.0 - Most underlying layout code in Keyman32 is now obsolete and needs to be removed
*/
   // I4169
////////////////////////////////////////////////////////////////////////////
//
// Interfaces for handling windows system keyboards
//
////////////////////////////////////////////////////////////////////////////

#include "pch.h"

extern WCHAR CharFromVK_NT(WORD *VKey, UINT ShiftFlags);   // I4582

WORD VkToVkByScanCode[256] = {0};
WORD USVkToVkByScanCode[256] = {0};   // I3762

extern BOOL KeyboardGivesCtrlRAltForRAlt_NT();

/************************************************************************/
/* CharFromVK: Return a character code from a virtual key code          */
/************************************************************************/

WCHAR CharFromVK(WORD *VKey, UINT ShiftFlags)   // I4582
{
	if(*VKey > 255) return 0;

  /* Remap the virtual key to US English if using Positional layout */

	*VKey = VKToScanCodeToVK(*VKey);
	return MapVirtualKeys(*VKey, ShiftFlags);

  //return CharFromVK_NT(VKey, ShiftFlags);
}

/* I3358 - reserved keys for mapping.  Note that some of these will not get here in AIWin2000Unicode mode due to being blocked in KeyboardProc.  They'll still get here via TSF though */   // I3535
BYTE PositionalVKMap[256] = {
  1,1,1,1, 1,0,1,1, 1,1,1,1, 1,1,1,1,     // 00 - 0F  VK_XBUTTON1 -> reserved?
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,     // 10 - 0F
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,     // 20 - 0F
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,     // 30 - 0F
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,     // 40 - 0F
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,     // 50 - 0F
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,     // 60 - 0F  VK_NUMPAD0 - VK_NUMPAD9, NP*, NP+, VK_SEPARATOR, NP-, NP., NP/
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,     // 70 - 0F  VK_F1 - VK_F16
  0,0,0,0, 0,0,0,0, 1,1,1,1, 1,1,1,1,     // 80 - 0F  VK_F17 - VK_F24
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,     // 90 - 0F
  1,1,1,1, 1,1,0,0, 0,0,0,0, 0,0,0,0,     // A0 - 0F  VK_BROWSER_*, VK_VOLUME_*
  0,0,0,0, 0,0,0,0, 1,1,1,1, 1,1,1,1,     // B0 - 0F  VK_MEDIA_*, VK_LAUNCH_*
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,     // C0 - 0F
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,     // D0 - 0F
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,     // E0 - 0F
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1      // F0 - FF
};

WORD VKToScanCodeToVK(WORD VKey)
{
	static HKL ActiveHKL = 0;

	if(VKey > 255) return VKey;

	/* Num lock test -- MCD: 02/12/2002 Fix num lock not working bug */
  /* I3358 - Block out some additional VK codes here for media keys */   // I3535

  if(!PositionalVKMap[VKey]) return VKey;

	/* Find the appropriate keyboard tables */

  HKL hkl = GetKeyboardLayout(0);

  if(hkl != ActiveHKL)
	{
		memset(VkToVkByScanCode, 0, sizeof(VkToVkByScanCode));
		ActiveHKL = hkl;
	}

	/* Look for the buffer */

	if(VkToVkByScanCode[VKey] != 0) return VkToVkByScanCode[VKey];

	/* Map the virtual key to the US English character: lookup the scancode from the VKey */

	UINT scancode = MapVirtualKeyEx(VKey, 0, hkl);

	if(scancode == 0)
  {
    //SendDebugMessageFormat("VK=%x  scancode=%x  result=%x", VKey, scancode, VKey);
    return VkToVkByScanCode[VKey] = VKey;
  }

	/* Lookup the US virtual key from the scancode */

	for(WORD i = 0; i < 256; i++)
		if(USVirtualKeyToScanCode[i] == scancode)
    {
      //SendDebugMessageFormat("VK=%x  scancode=%x  result=%x", VKey, scancode, i);
			return VkToVkByScanCode[VKey] = i;
    }

  //SendDebugMessageFormat("VK=%x  scancode=%x  result=%x", VKey, scancode, VKey);
	return VkToVkByScanCode[VKey] = VKey;
}

WORD USVKToScanCodeToLayoutVK(WORD VKey)   // I3762
{
	static HKL ActiveHKL = 0;

	if(VKey > 255) return VKey;

  if(!PositionalVKMap[VKey]) return VKey;

	/* Find the appropriate keyboard tables */

  HKL hkl = GetKeyboardLayout(0);

  if(hkl != ActiveHKL)
	{
		memset(USVkToVkByScanCode, 0, sizeof(USVkToVkByScanCode));
		ActiveHKL = hkl;
	}

	/* Look for the buffer */

	if(USVkToVkByScanCode[VKey] != 0) return USVkToVkByScanCode[VKey];

	/* Map the US virtual key to the scan code: lookup the scancode from the VKey */

  UINT scancode = USVirtualKeyToScanCode[VKey];
	if(scancode == 0)
  {
    //SendDebugMessageFormat("VK=%x  scancode=%x  result=%x", VKey, scancode, VKey);
    return USVkToVkByScanCode[VKey] = VKey;
  }

	/* Lookup the layout virtual key from the scancode */

  return USVkToVkByScanCode[VKey] = (WORD) MapVirtualKeyEx(scancode, MAPVK_VSC_TO_VK_EX, hkl);
}

/////////////////////////////////////////////////////////////////////////////////////////////
//
// MapVirtualKeys codes
//
/////////////////////////////////////////////////////////////////////////////////////////////

   // I3597
WCHAR MapVirtualKeys(WORD keyCode, UINT shiftFlags)
{
	char shiftedDigit[] = ")!@#$%^&*(";
	int n, Shift;

	if(shiftFlags & (LCTRLFLAG|RCTRLFLAG|LALTFLAG|RALTFLAG)) return 0;

	if(keyCode >= '0' && keyCode <= '9')
	{
		n = keyCode - '0';
		return ((shiftFlags & K_SHIFTFLAG) ? shiftedDigit[n] : keyCode);
	}

	if(keyCode >= 'A' && keyCode <= 'Z')
	{
		Shift = (shiftFlags & K_SHIFTFLAG);
		if(shiftFlags & (CAPITALFLAG)) Shift = !Shift;
		return (Shift ? keyCode : keyCode + 32);
	}

	if(keyCode >= VK_NUMPAD0 && keyCode <= VK_NUMPAD9)
	{
		if(!(shiftFlags & NUMLOCKFLAG)) return 0;
		return keyCode - (VK_NUMPAD0 - '0');
	}

	Shift = (shiftFlags & K_SHIFTFLAG);

	switch(keyCode)
	{
	case VK_ACCENT:
		return Shift ? '~' : '`';
	case VK_HYPHEN:
		return Shift ? '_' : '-';
	case VK_EQUAL:
		return Shift ? '+' : '=';
	case VK_BKSLASH:
  case 0xE2:  // I5332
		return Shift ? '|' : 92;
	case VK_LBRKT:
		return Shift ? '{' : '[';
	case VK_RBRKT:
		return Shift ? '}' : ']';
	case VK_COLON:
		return Shift ? ':' : ';';
	case VK_QUOTE:
		return Shift ? '"' : 39;
	case VK_COMMA:
		return Shift ? '<' : ',';
	case VK_PERIOD:
		return Shift ? '>' : '.';
	case VK_SLASH:
		return Shift ? '?' : '/';
	case VK_SPACE:
		return ' ';
	}
	return 0;
	//keyCode;
}

/*
 For some reason, keyboards that use RAlt as a shifter modify it internally to Ctrl+RAlt, which
 just confuses the heck out of keyboards that use RAlt.  So we work around by recognising this and
 adding Ctrl to the tests for these layouts.
*/

BOOL KeyboardGivesCtrlRAltForRAlt()
{
	return KeyboardGivesCtrlRAltForRAlt_NT();
}
