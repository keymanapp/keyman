/*
  Name:             syskbd
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      11 Dec 2009

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          30 Nov 2009 - mcdurdin - I934 - Prep for x64 - change UINT to WORD for vkeys
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    03 Feb 2015 - mcdurdin - I4582 - V9.0 - Most underlying layout code in Keyman32 is now obsolete and needs to be removed
*/
   // I4169

#ifndef __SYSKBD_H
#define __SYSKBD_H

#define VK_COLON	0xBA
#define VK_EQUAL	0xBB
#define VK_COMMA	0xBC
#define VK_HYPHEN	0xBD
#define VK_PERIOD	0xBE
#define	VK_SLASH	0xBF
#define VK_ACCENT	0xC0
#define VK_LBRKT	0xDB
#define VK_BKSLASH	0xDC
#define VK_RBRKT	0xDD
#define VK_QUOTE	0xDE
#define VK_xDF		0xDF

WCHAR CharFromVK(WORD *VKey, UINT ShiftFlags);   // I4582
WORD USVKToScanCodeToLayoutVK(WORD VKey);   // I3762

BOOL KeyboardGivesCtrlRAltForRAlt();

#endif

