/*
  Name:             syskbdnt64
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      10 Sep 2008

  Modified Date:    6 Feb 2015
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
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    03 Feb 2015 - mcdurdin - I4582 - V9.0 - Most underlying layout code in Keyman32 is now obsolete and needs to be removed
                    06 Feb 2015 - mcdurdin - I4583 - V9.0 - Remove altgr lookup test from keyman32 and put it into the registry
*/
   // I4169   // I4582   // I4583
////////////////////////////////////////////////////////////////////////////
//
// Use Windows NT/2000 keyboard DLLs for mapping virtual keys to characters
//
////////////////////////////////////////////////////////////////////////////

#include "pch.h"

// This file is used only in keyman32.dll; it maps syskbd for x64 builds of Windows
#ifndef _WIN64

#include "kbd.h"	/* DDK kbdlayout */

//#pragma warning ( disable : 4200 )

typedef struct {
    PVK_TO_BIT pVkToBit;     // Virtual Keys -> Mod bits
    DWORD     filler;
    WORD       wMaxModBits;  // max Modification bit combination value
    BYTE       ModNumber[1];  // Mod bits -> Modification Number
} MODIFIERS_x64, *PMODIFIERS_x64;

//#pragma warning ( default : 4200 )

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

static HKL ActiveHKL_NT_x64 = 0;

static HMODULE hKbdLibrary_x64 = NULL;
static PKBDTABLES_x64 KbdTables_x64 = NULL;


BOOL LoadNewLibrary_x64()
{
	char buf[128], buf2[KL_NAMELENGTH+1];

	if(hKbdLibrary_x64) FreeLibrary(hKbdLibrary_x64);

	RegistryReadOnly *r = new RegistryReadOnly(HKEY_LOCAL_MACHINE);
	__try
	{
  	GetKeyboardLayoutName(buf2);
		wsprintf(buf, "System\\CurrentControlSet\\Control\\keyboard layouts\\%s", buf2);
		if(r->OpenKeyReadOnly(buf))
		{
			if(r->ReadString("Layout File", buf, 32))
			{
				hKbdLibrary_x64 = LoadLibrary(buf);
				if(!hKbdLibrary_x64)
				{
					SendDebugMessageFormat("Exit -- could not load library");
					return FALSE;
				}
				PKBDLAYERDESCRIPTORFUNC KbdLayerDescriptorFunc = (PKBDLAYERDESCRIPTORFUNC) GetProcAddress(hKbdLibrary_x64, "KbdLayerDescriptor");
				if(KbdLayerDescriptorFunc)
				{
					KbdTables_x64 = (*KbdLayerDescriptorFunc)();
					if(KbdTables_x64)
						return TRUE;
				}

				FreeLibrary(hKbdLibrary_x64);
			}
		}

		hKbdLibrary_x64 = NULL;
		KbdTables_x64 = NULL;
	}
	__finally
	{
		delete r;
	}
	SendDebugMessageFormat("Exit -- failed to find function");
	return FALSE;
}

BOOL KeyboardGivesCtrlRAltForRAlt_NT_x64()
{
	HKL hkl = GetKeyboardLayout(0);

	/* Find the appropriate keyboard tables */

	if(hkl != ActiveHKL_NT_x64)
	{
		ActiveHKL_NT_x64 = hkl;
		LoadNewLibrary_x64();
	}

	if(!KbdTables_x64)				/* Could not load keyboard DLL */
		return FALSE;

	return (KbdTables_x64->fLocaleFlags & 0x1) ? TRUE : FALSE;
}


typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);

LPFN_ISWOW64PROCESS
fnIsWow64Process = (LPFN_ISWOW64PROCESS)GetProcAddress(
GetModuleHandle("kernel32"),"IsWow64Process");
BOOL fStored = FALSE;
BOOL bIsWow64 = FALSE;

BOOL IsWow64()
{
  if(fStored) return bIsWow64;
  bIsWow64 = FALSE;

  if (NULL != fnIsWow64Process)
  {
     if (!fnIsWow64Process(GetCurrentProcess(),&bIsWow64))
      {
            // handle error
     }
  }
  fStored = TRUE;
  return bIsWow64;
}

#endif
