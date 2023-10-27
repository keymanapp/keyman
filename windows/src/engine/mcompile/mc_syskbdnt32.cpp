/*
  Name:             syskbdnt32
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      10 Sep 2008

  Modified Date:    4 May 2010
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          10 Sep 2008 - mcdurdin - I1635 - Initial version
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    30 Nov 2009 - mcdurdin - I934 - Prep for x64 - change UINT to WORD for vkeys
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
*/

////////////////////////////////////////////////////////////////////////////
//
// Use Windows NT/2000 keyboard DLLs for mapping virtual keys to characters
//
////////////////////////////////////////////////////////////////////////////

#include "pch.h"
#include "../../engine/keyman32-ver/kbd.h"	/* DDK kbdlayout */


typedef PKBDTABLES (WINAPI *PKBDLAYERDESCRIPTORFUNC)(VOID);

static HMODULE hKbdLibrary = NULL;
static PKBDTABLES KbdTables = NULL;

BOOL LoadNewLibrary_NT(PWSTR filename) {
  if(hKbdLibrary) FreeLibrary(hKbdLibrary);

	hKbdLibrary = LoadLibrary(filename);
	if(!hKbdLibrary) {
	  LogError(L"LoadNewLibrary: Exit -- could not load library");
		return FALSE;
	}

  PKBDLAYERDESCRIPTORFUNC KbdLayerDescriptorFunc = (PKBDLAYERDESCRIPTORFUNC) GetProcAddress(hKbdLibrary, "KbdLayerDescriptor");
	if(KbdLayerDescriptorFunc)
	{
		KbdTables = (*KbdLayerDescriptorFunc)();
		if(KbdTables) {
			return TRUE;
    }
	}

	FreeLibrary(hKbdLibrary);

	hKbdLibrary = NULL;
	KbdTables = NULL;

  LogError(L"LoadNewLibrary: Exit -- failed to find function");
	return FALSE;
}

WCHAR CharFromVK_NT(WORD VKey, UINT ShiftFlags, WCHAR *PDeadKey) {
	PVK_TO_WCHARS1 pv;
	PVK_TO_WCHAR_TABLE pvt;
	int i;

	/* MCD 02/12/2002: Fix bug with number pad keys when numlock is on */

  if(IsNumberPadKey(VKey)) {
    return 0;
  }

	/* Get the appropriate table column for shift state */

	int shift = -1, shiftidx = 0, capslockbit = 0;

	for(PVK_TO_BIT pvtb = KbdTables->pCharModifiers->pVkToBit; pvtb->Vk; pvtb++) {
		switch(pvtb->Vk) {
		case VK_MENU:
			if(ShiftFlags & (LALTFLAG|RALTFLAG)) shiftidx |= pvtb->ModBits; break;
		case VK_SHIFT:
			if(ShiftFlags & CAPITALFLAG) capslockbit = pvtb->ModBits;
			if(ShiftFlags & (K_SHIFTFLAG)) shiftidx |= pvtb->ModBits; break;
		case VK_CONTROL:
			if(ShiftFlags & (LCTRLFLAG|RCTRLFLAG)) shiftidx |= pvtb->ModBits; break;
		}
  }

	/* If the shift modifier is unused, don't produce a character */

	if(shiftidx > KbdTables->pCharModifiers->wMaxModBits ||
			KbdTables->pCharModifiers->ModNumber[shiftidx] == 0xF) {
		return 0;
	}

	shift = KbdTables->pCharModifiers->ModNumber[shiftidx];

	/* Get the appropriate virtual key */

	for(i = 0, pvt = KbdTables->pVkToWcharTable; pvt->pVkToWchars; i++, pvt++) {
		for(pv = pvt->pVkToWchars; pv->VirtualKey;) {
			if(pv->VirtualKey == VKey) {
				if(shift >= pvt->nModifications) {
					return 0;
        }

				/* Check for Caps Lock */
				if(ShiftFlags & CAPITALFLAG) {
					if(pv->Attributes & CAPLOK) {
						/* Invert the state of the SHIFT key */
						shift = KbdTables->pCharModifiers->ModNumber[shiftidx ^ capslockbit];
          } else if(pv->Attributes & SGCAPS) {
						pv++; /* Next keystroke has the appropriate capitalised character */
          }
				}

				/* Found the keystroke combination, return the appropriate character (if existing) */
				switch(pv->wch[shift]) {
				case WCH_DEAD:
					pv = (PVK_TO_WCHARS1) ((BYTE *)pv + pvt->cbSize);
          *PDeadKey = pv->wch[shift];
					return 0xFFFF; /* Don't queue as WM_KEYDOWN -- don't output either */
				case WCH_NONE:
					return 0x0000; /* Not a valid key, queue as WM_KEYDOWN */
				}

				return (pv->wch[shift] > 31 && pv->wch[shift] != 127) ? pv->wch[shift] : 0; /* Don't generate 'special chars'; I911 - fix ctrl+bksp */
			}
			pv = (PVK_TO_WCHARS1) ((BYTE *)pv + pvt->cbSize);
		}
	}

	return 0;
}


WORD ModificationToShift_NT(int mod) {
  WORD ShiftFlags = 0;

  int shiftidx = -1;
  for(int i = 0; i <= KbdTables->pCharModifiers->wMaxModBits; i++) {
    if(KbdTables->pCharModifiers->ModNumber[i] == mod) {
      shiftidx = i;
      break;
    }
  }

  if(shiftidx == -1) return 0xFFFF;

	for(PVK_TO_BIT pvtb = KbdTables->pCharModifiers->pVkToBit; pvtb->Vk; pvtb++) {
    if(shiftidx & pvtb->ModBits) {
      switch(pvtb->Vk) {
        case VK_MENU: ShiftFlags |= RALTFLAG; break;
        case VK_SHIFT: ShiftFlags |= K_SHIFTFLAG; break;
        case VK_CONTROL: ShiftFlags |= LCTRLFLAG; break;
      }
    }
  }

  return ShiftFlags;
}

WORD CharToUSVK_NT(WORD ch, WORD *shift)
{
	PVK_TO_WCHARS1 pv;
	PVK_TO_WCHAR_TABLE pvt;
	int i, j;

	/* Find the appropriate keyboard tables */

	if(!KbdTables) return 0;				/* Could not load keyboard DLL */

	/* Map the character back to the virtual key */

	for(i = 0, pvt = KbdTables->pVkToWcharTable; pvt->pVkToWchars; i++, pvt++)
		for(pv = pvt->pVkToWchars; pv->VirtualKey;)
		{
			for(j = 0; j < pvt->nModifications; j++)
				if(pv->wch[j] == ch)
				{
					/* Return the new virtual key */
          *shift = ModificationToShift_NT(j);
          if(*shift == 0xFFFF) return 0;
          return pv->VirtualKey;
				}
			pv = (PVK_TO_WCHARS1) ((BYTE *)pv + pvt->cbSize);
		}

	/* Remapping not found */
	return 0;
}


int GetDeadkeys_NT(WORD DeadKey, WORD *OutputPairs) {
  WORD *p = OutputPairs, shift;
	for(int i = 0; KbdTables->pDeadKey[i].dwBoth; i++) {
		if(HIWORD(KbdTables->pDeadKey[i].dwBoth) == DeadKey) {
			WORD vk = CharToUSVK_NT(LOWORD(KbdTables->pDeadKey[i].dwBoth), &shift);
      if(vk != 0) {
        *p++ = vk;
        *p++ = shift;
        *p++ = KbdTables->pDeadKey[i].wchComposed;
      } else {
        LogError(L"Warning: complex deadkey not supported.");
      }
		}
	}
  *p = 0;
  return (INT_PTR)(p-OutputPairs);
}

WORD VKUSToVKUnderlyingLayout_NT(WORD VKey) {

	/* Num lock test -- MCD: 02/12/2002 Fix num lock not working bug */

	if((VKey >= VK_NUMPAD0 && VKey <= VK_NUMPAD9) || VKey == VK_DECIMAL) return VKey;

	/* Find the appropriate keyboard tables */

	if(!KbdTables)				/* Could not load keyboard DLL */
		return VKey;

  if(VKey > 255) return VKey;

	UINT scan = USVirtualKeyToScanCode[VKey];
	if(scan == 0 || scan >= KbdTables->bMaxVSCtoVK) return VKey;
	return KbdTables->pusVSCtoVK[scan];
}

WORD VKUnderlyingLayoutToVKUS_NT(WORD VKey) {

	/* Num lock test -- MCD: 02/12/2002 Fix num lock not working bug */

	if((VKey >= VK_NUMPAD0 && VKey <= VK_NUMPAD9) || VKey == VK_DECIMAL) return VKey;

	/* Find the appropriate keyboard tables */

	if(!KbdTables)				/* Could not load keyboard DLL */
		return VKey;

  UINT scan;
  for(scan = 1; scan <= KbdTables->bMaxVSCtoVK; scan++) {
    if(KbdTables->pusVSCtoVK[scan] == VKey) {
      break;
    }
  }
  if(scan > KbdTables->bMaxVSCtoVK || scan > 127) {
    // no translation.
    return VKey;
  }

  return ScanCodeToUSVirtualKey[scan];
}
