/*
  Name:             glossary
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jul 2008

  Modified Date:    28 May 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jul 2008 - mcdurdin - I1498 - Fix keyboard switching for Shadow keyboards on Vista+
                    20 Jul 2008 - mcdurdin - I1546 - Fix language switch with ids >= x80000000
                    20 Jul 2008 - mcdurdin - I1545 - Fix registry leak
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    23 Dec 2011 - mcdurdin - I3191 - Corruption of keyboard selection cache between x86 and x64
                    04 Nov 2012 - mcdurdin - I3526 - V9.0 - Merge of I3191 - Corruption of keyboard selection cache between x86 and x64
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
*/
#include "pch.h"
   // I4220
#ifndef SM_IMMENABLED
#define SM_IMMENABLED 82
#endif

#pragma warning(disable: 4996)
BOOL HKLIsIME(HKL hkl)  // I1498 - fix keyboard switching for shadow keyboards on Vista+
{
  BOOL r;
  if( (GetVersion() & 0xFF) >= 6 ) return FALSE;
  if( (GetVersion() & 0x8000000) == 0x8000000 || (GetVersion() & 0xFF) == 4 )
    r = GetSystemMetrics(SM_DBCSENABLED);
  else
    r = GetSystemMetrics(SM_IMMENABLED);

  return r && ImmIsIME(hkl);
}
#pragma warning(default: 4996)

HKL ForceKeymanIDToHKL(DWORD KeymanID)  // I3191   // I3526
{
#ifdef _WIN64
  return (HKL) (LONG_PTR) (INT64) (LONG) KeymanID;  // Sign extension for x64
#else
  return (HKL) (DWORD_PTR) KeymanID;
#endif
}

DWORD ForceHKLToKeymanID(HKL hkl)  // I3191   // I3526
{
  return (DWORD) (DWORD_PTR) hkl; // Just truncate for x64
}

DWORD HKLToKeyboardID(HKL hkl)
{
	HKEY hkey, hsubkey;
	char str[16], str2[16];
	DWORD i = 0;
	DWORD n;
	FILETIME ft;
	DWORD len;

  //SendDebugMessageFormat("hkl=%x", hkl);

	// Test for IME, and if IME return LanguageID

	if(HKLIsIME(hkl)) // I1498 - was ImmIsIME
  {
    //SendDebugMessageFormat("return HKLIsIME is true=%x", hkl);
    return ForceHKLToKeymanID(hkl);  // MCD 19/03/02  // I3191   // I3526
  }
	//if((HIWORD(hkl) & 0xF000) == 0xE000) return (DWORD) hkl;

	// Test for standard layouts: 0409041D for instance
	// specialised layout is: F0020409 fr instance [find layout id:0002]

	DWORD LayoutID = HKLToLayoutID(hkl);
	if(LayoutID == 0)
  {
    //SendDebugMessageFormat("no LayoutID, so return HIWORD(hkl)=%x", HIWORD(hkl));
    return HIWORD(hkl);
  }

 //SendDebugMessageFormat("LayoutID=%x", LayoutID);

	// Find the KeyboardID associated with the LayoutID

	if(RegOpenKeyEx(HKEY_LOCAL_MACHINE, REGSZ_SystemKeyboardLayouts, NULL, KEY_READ, &hkey) != ERROR_SUCCESS)
  {
    //SendDebugMessageFormat("fails, return LOWORD(hkl)=%x", LOWORD(hkl));
		return (DWORD) LOWORD(hkl);
  }

	for(len=16, n = i = 0; RegEnumKeyEx(hkey, i, str, &len, 0, NULL, NULL, &ft) == ERROR_SUCCESS;
		len = 16, i++, n=0)
	{
		RegOpenKeyEx(hkey, str, NULL, KEY_READ, &hsubkey);
		len = 16;
		if(RegQueryValueEx(hsubkey, REGSZ_LayoutID, NULL, NULL, (LPBYTE) str2, &len) == ERROR_SUCCESS)
		{
			n = strtoul(str2, NULL, 16);  // I1546
			if(n == LayoutID)
      {
        RegCloseKey(hsubkey); // I1545
        RegCloseKey(hkey);
      	n = strtoul(str, NULL, 16); // I1546
        SendDebugMessageFormat("return keyboard from LayoutID = %x [%s]", n, str);
        return n;
      }
		}
		RegCloseKey(hsubkey);
	}

	RegCloseKey(hkey);

  //SendDebugMessageFormat("fails[2], return LOWORD(hkl)=%x", LOWORD(hkl));
  return (DWORD) LOWORD(hkl);		// should never happen
}


WORD HKLToLanguageID(HKL hkl)
{
	return LOWORD(hkl);
}


WORD HKLToLayoutNumber(HKL hkl)
{
	WORD LayoutID = HKLToLayoutID(hkl);
	HKEY hkey, hsubkey;
	DWORD len, n, i;
	FILETIME ft;
	char str[16], str2[16];

	if(RegOpenKeyEx(HKEY_LOCAL_MACHINE, REGSZ_SystemKeyboardLayouts, NULL, KEY_READ, &hkey) != ERROR_SUCCESS)
		return 0;

	for(len=16, n = i = 0; RegEnumKeyEx(hkey, i, str, &len, 0, NULL, NULL, &ft) == ERROR_SUCCESS; len = 16, i++, n=0)
	{
		RegOpenKeyEx(hkey, str, NULL, KEY_READ, &hsubkey);
		len = 16;
		if(RegQueryValueEx(hsubkey, REGSZ_LayoutID, NULL, NULL, (LPBYTE) str2, &len) == ERROR_SUCCESS)
		{
			if(strtoul(str2, NULL, 16) == LayoutID) break;  // strtoul - I1546
		}
		RegCloseKey(hsubkey);
	}

	RegCloseKey(hkey);

	n = strtoul(str, NULL, 16); // strtoul - I1546
	return HIWORD(n);
}

WORD HKLToLayoutID(HKL hkl)
{
	if((HIWORD(hkl) & 0xF000) != 0xF000) return 0;

	return HIWORD(hkl) & 0x0FFF;
}
