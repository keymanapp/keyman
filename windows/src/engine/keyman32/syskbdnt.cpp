/*
  Name:             syskbdnt
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      22 Jan 2007

  Modified Date:    6 Feb 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          22 Jan 2007 - mcdurdin - Don't translate any number pad keys
                    13 Jul 2007 - mcdurdin - I911 - Fix for Ctrl+BKSP
                    10 Sep 2008 - mcdurdin - I1635 - mnemonic layouts on x64 fail
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    30 Nov 2009 - mcdurdin - I934 - Prep for x64 - change UINT to WORD for vkeys
                    30 Nov 2009 - mcdurdin - I2155 - Fix crash when RALT pressed on wow64
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
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
#include "kbd.h"	/* DDK kbdlayout */

#ifndef _WIN64

extern BOOL KeyboardGivesCtrlRAltForRAlt_NT_x64();
extern BOOL IsWow64();

#endif

typedef PKBDTABLES (WINAPI *PKBDLAYERDESCRIPTORFUNC)(VOID);

static WCHAR PreviousDeadkey_NT = 0;
static HKL ActiveHKL_NT = 0;
static WORD VkToVkByChar_NT[256] = {0};

static HMODULE hKbdLibrary = NULL;
static PKBDTABLES KbdTables = NULL;

BOOL LoadNewLibrary()
{
	char buf[128], buf2[KL_NAMELENGTH+1];

	memset(VkToVkByChar_NT, 0, sizeof(VkToVkByChar_NT));

	if(hKbdLibrary) FreeLibrary(hKbdLibrary);

	PreviousDeadkey_NT = 0;

	RegistryReadOnly *r = new RegistryReadOnly(HKEY_LOCAL_MACHINE);
	__try
	{
    GetKeyboardLayoutName(buf2);
		wsprintf(buf, "System\\CurrentControlSet\\Control\\keyboard layouts\\%s", buf2);
		if(r->OpenKeyReadOnly(buf))
		{
			if(r->ReadString("Layout File", buf, 32))
			{
				hKbdLibrary = LoadLibrary(buf);
				if(!hKbdLibrary)
				{
					SendDebugMessageFormat("Exit -- could not load library");
					return FALSE;
				}
				PKBDLAYERDESCRIPTORFUNC KbdLayerDescriptorFunc = (PKBDLAYERDESCRIPTORFUNC) GetProcAddress(hKbdLibrary, "KbdLayerDescriptor");
				if(KbdLayerDescriptorFunc)
				{
					KbdTables = (*KbdLayerDescriptorFunc)();
					if(KbdTables)
						return TRUE;
				}

				FreeLibrary(hKbdLibrary);
			}
		}

		hKbdLibrary = NULL;
		KbdTables = NULL;
	}
	__finally
	{
		delete r;
	}
	SendDebugMessageFormat("Exit -- failed to find function");
	return FALSE;
}

BOOL KeyboardGivesCtrlRAltForRAlt_NT()
{
#ifndef _WIN64
  if(IsWow64()) return KeyboardGivesCtrlRAltForRAlt_NT_x64();  // I2155
#endif

	HKL hkl = GetKeyboardLayout(0);

	/* Find the appropriate keyboard tables */

	if(hkl != ActiveHKL_NT)
	{
		ActiveHKL_NT = hkl;
		LoadNewLibrary();
	}

	if(!KbdTables)				/* Could not load keyboard DLL */
		return FALSE;

	return (KbdTables->fLocaleFlags & 0x1) ? TRUE : FALSE;
}
