/*
  Name:             syskbdnt64
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

typedef struct {
    PVK_TO_BIT pVkToBit;     // Virtual Keys -> Mod bits
    DWORD     filler;
    WORD       wMaxModBits;  // max Modification bit combination value
    BYTE       ModNumber[1];  // Mod bits -> Modification Number
} MODIFIERS_x64, *PMODIFIERS_x64;

typedef struct _VK_TO_WCHAR_TABLE_x64 {
    PVK_TO_WCHARS1 pVkToWchars;
    DWORD filler;
    BYTE           nModifications;
    BYTE           cbSize;
    BYTE filler2[2];
    DWORD filler3;
} VK_TO_WCHAR_TABLE_x64, *PVK_TO_WCHAR_TABLE_x64;

/***************************************************************************\
* VSC_VK     - Associate a Virtual Scancode with a Virtual Key
*  Vsc - Virtual Scancode
*  Vk  - Virtual Key | flags
* Used by VKFromVSC() for scancodes prefixed 0xE0 or 0xE1
\***************************************************************************/
typedef struct _VSC_VK_x64 {
    BYTE Vsc;
    USHORT Vk;
} VSC_VK_x64, *PVSC_VK_x64;

/***************************************************************************\
* VK_VSC     - Associate a Virtual Key with a Virtual Scancode
*  Vk  - Virtual Key
*  Vsc - Virtual Scancode
* Used by MapVirtualKey for Virtual Keys not appearing in ausVK[]
\***************************************************************************/
typedef struct _VK_VSC_x64 {
    BYTE Vk;
    BYTE Vsc;
} VK_VSC_x64, *PVK_VSC_x64;


typedef struct tagKdbLayer_x64
{
    /*
     * Modifier keys
     */
    PMODIFIERS_x64 pCharModifiers;
    DWORD filler1;

    /*
     * Characters
     */
    VK_TO_WCHAR_TABLE_x64 *pVkToWcharTable;  // ptr to tbl of ptrs to tbl
    DWORD filler2;

    /*
     * Diacritics
     */
    PDEADKEY pDeadKey;
    DWORD filler3;

    /*
     * Names of Keys
     */
    VSC_LPWSTR *pKeyNames;
    DWORD filler4;
    VSC_LPWSTR *pKeyNamesExt;
    DWORD filler5;
    LPWSTR     *pKeyNamesDead;
    DWORD filler6;

    /*
     * Scan codes to Virtual Keys
     */
    USHORT *pusVSCtoVK;
    DWORD filler7;
    BYTE    bMaxVSCtoVK;
    DWORD filler8;
    PVSC_VK_x64 pVSCtoVK_E0;  // Scancode has E0 prefix
    DWORD filler9;
    PVSC_VK_x64 pVSCtoVK_E1;  // Scancode has E1 prefix
    DWORD fillerA;

    /*
     * Locale-specific special processing
     */
    DWORD fLocaleFlags;
    DWORD fillerB;
} KBDTABLES_x64, *PKBDTABLES_x64;


typedef PKBDTABLES_x64 (WINAPI *PKBDLAYERDESCRIPTORFUNC)(VOID);

static HMODULE hKbdLibrary_x64 = NULL;
static PKBDTABLES_x64 KbdTables_x64 = NULL;

BOOL LoadNewLibrary_NT_x64(PWSTR filename) {
  if(hKbdLibrary_x64) FreeLibrary(hKbdLibrary_x64);

	hKbdLibrary_x64 = LoadLibrary(filename);
	if(!hKbdLibrary_x64) {
	  LogError(L"LoadNewLibrary_x64: Exit -- could not load library");
		return FALSE;
	}

  PKBDLAYERDESCRIPTORFUNC KbdLayerDescriptorFunc = (PKBDLAYERDESCRIPTORFUNC) GetProcAddress(hKbdLibrary_x64, "KbdLayerDescriptor");
	if(KbdLayerDescriptorFunc)
	{
		KbdTables_x64 = (*KbdLayerDescriptorFunc)();
		if(KbdTables_x64) {
			return TRUE;
    }
	}

	FreeLibrary(hKbdLibrary_x64);

	hKbdLibrary_x64 = NULL;
	KbdTables_x64 = NULL;

  LogError(L"LoadNewLibrary_x64: Exit -- failed to find function");
	return FALSE;
}

WCHAR CharFromVK_NT_x64(WORD VKey, UINT ShiftFlags, WCHAR *PDeadKey) {
	PVK_TO_WCHARS1 pv;
	PVK_TO_WCHAR_TABLE_x64 pvt;
	int i;

	/* MCD 02/12/2002: Fix bug with number pad keys when numlock is on */

  if(IsNumberPadKey(VKey)) {
    return 0;
  }

	/* Get the appropriate table column for shift state */

	int shift = -1, shiftidx = 0, capslockbit = 0;

	for(PVK_TO_BIT pvtb = KbdTables_x64->pCharModifiers->pVkToBit; pvtb->Vk; pvtb++) {
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

	if(shiftidx > KbdTables_x64->pCharModifiers->wMaxModBits ||
			KbdTables_x64->pCharModifiers->ModNumber[shiftidx] == 0xF) {
		return 0;
	}

	shift = KbdTables_x64->pCharModifiers->ModNumber[shiftidx];

	/* Get the appropriate virtual key */

	for(i = 0, pvt = KbdTables_x64->pVkToWcharTable; pvt->pVkToWchars; i++, pvt++) {
		for(pv = pvt->pVkToWchars; pv->VirtualKey;) {
			if(pv->VirtualKey == VKey) {
				if(shift >= pvt->nModifications) {
					return 0;
        }

				/* Check for Caps Lock */
				if(ShiftFlags & CAPITALFLAG) {
					if(pv->Attributes & CAPLOK) {
						/* Invert the state of the SHIFT key */
						shift = KbdTables_x64->pCharModifiers->ModNumber[shiftidx ^ capslockbit];
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


WORD ModificationToShift_NT_x64(int mod) {
  WORD ShiftFlags = 0;

  int shiftidx = -1;
  for(int i = 0; i <= KbdTables_x64->pCharModifiers->wMaxModBits; i++) {
    if(KbdTables_x64->pCharModifiers->ModNumber[i] == mod) {
      shiftidx = i;
      break;
    }
  }

  if(shiftidx == -1) return 0xFFFF;

	for(PVK_TO_BIT pvtb = KbdTables_x64->pCharModifiers->pVkToBit; pvtb->Vk; pvtb++) {
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

WORD CharToUSVK_NT_x64(WORD ch, WORD *shift)
{
	PVK_TO_WCHARS1 pv;
	PVK_TO_WCHAR_TABLE_x64 pvt;
	int i, j;

	/* Find the appropriate keyboard tables */

	if(!KbdTables_x64) return 0;				/* Could not load keyboard DLL */

	/* Map the character back to the virtual key */

	for(i = 0, pvt = KbdTables_x64->pVkToWcharTable; pvt->pVkToWchars; i++, pvt++)
		for(pv = pvt->pVkToWchars; pv->VirtualKey;)
		{
			for(j = 0; j < pvt->nModifications; j++)
				if(pv->wch[j] == ch)
				{
					/* Return the new virtual key */
          *shift = ModificationToShift_NT_x64(j);
          if(*shift == 0xFFFF) return 0;
          return pv->VirtualKey;
				}
			pv = (PVK_TO_WCHARS1) ((BYTE *)pv + pvt->cbSize);
		}

	/* Remapping not found */
	return 0;
}


int GetDeadkeys_NT_x64(WORD DeadKey, WORD *OutputPairs) {
  WORD *p = OutputPairs, shift;
	for(int i = 0; KbdTables_x64->pDeadKey[i].dwBoth; i++) {
		if(HIWORD(KbdTables_x64->pDeadKey[i].dwBoth) == DeadKey) {
			WORD vk = CharToUSVK_NT_x64(LOWORD(KbdTables_x64->pDeadKey[i].dwBoth), &shift);
      if(vk != 0) {
        *p++ = vk;
        *p++ = shift;
        *p++ = KbdTables_x64->pDeadKey[i].wchComposed;
      } else {
        LogError(L"Warning: complex deadkey not supported.");
      }
		}
	}
  *p = 0;
  return (INT_PTR)(p-OutputPairs);
}

WORD VKUSToVKUnderlyingLayout_NT_x64(WORD VKey) {

	/* Num lock test -- MCD: 02/12/2002 Fix num lock not working bug */

	if((VKey >= VK_NUMPAD0 && VKey <= VK_NUMPAD9) || VKey == VK_DECIMAL) return VKey;

	/* Find the appropriate keyboard tables */

	if(!KbdTables_x64)				/* Could not load keyboard DLL */
		return VKey;

  if(VKey > 255) return VKey;

	UINT scan = USVirtualKeyToScanCode[VKey];
	if(scan == 0 || scan >= KbdTables_x64->bMaxVSCtoVK) return VKey;
	return KbdTables_x64->pusVSCtoVK[scan];
}

WORD VKUnderlyingLayoutToVKUS_NT_x64(WORD VKey) {

	/* Num lock test -- MCD: 02/12/2002 Fix num lock not working bug */

	if((VKey >= VK_NUMPAD0 && VKey <= VK_NUMPAD9) || VKey == VK_DECIMAL) return VKey;

	/* Find the appropriate keyboard tables */

	if(!KbdTables_x64)				/* Could not load keyboard DLL */
		return VKey;

  UINT scan;
  for(scan = 1; scan <= KbdTables_x64->bMaxVSCtoVK; scan++) {
    if(KbdTables_x64->pusVSCtoVK[scan] == VKey) {
      break;
    }
  }
  if(scan > KbdTables_x64->bMaxVSCtoVK || scan > 127) {
    // no translation.
    return VKey;
  }

  return ScanCodeToUSVirtualKey[scan];
}


typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);

LPFN_ISWOW64PROCESS
fnIsWow64Process = (LPFN_ISWOW64PROCESS)GetProcAddress(GetModuleHandle(L"kernel32"),"IsWow64Process");
BOOL fStored = FALSE;
BOOL bIsWow64 = FALSE;

BOOL IsWow64() {
  if(fStored) return bIsWow64;
  bIsWow64 = FALSE;

  if(NULL != fnIsWow64Process) {
    if(!fnIsWow64Process(GetCurrentProcess(),&bIsWow64)){
      // handle error
      return FALSE;
    }
  }
  fStored = TRUE;
  return bIsWow64;
}
