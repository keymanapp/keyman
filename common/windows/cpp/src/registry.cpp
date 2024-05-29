/*
  Name:             registry
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      13 Jul 2007

  Modified Date:    20 Nov 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          13 Jul 2007 - mcdurdin - I934 - Prep for x64
                    30 Nov 2009 - mcdurdin - I934 - Prep for x64 - registry keys
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options - add support for wide registry strings
                    20 Nov 2012 - mcdurdin - I3584 - V9.0 - IntRecursiveDeleteKey needed a buf len reset after each iteration
*/

#include "pch.h"
#include "../include/registry.h"

RegistryReadOnly::RegistryReadOnly(HKEY AhRootKey)
{
	FhRootKey = AhRootKey;
	FhKey = NULL;
}

RegistryReadOnly::~RegistryReadOnly()
{
	if(FhKey != NULL) CloseKey();
}

BOOL RegistryReadOnly::CloseKey(void)
{
	BOOL Result = FALSE;
	if(FhKey != NULL) Result = WrapError(RegCloseKey(FhKey));
	FhKey = NULL;
	return TRUE;
}


BOOL RegistryFullAccess::CreateKey(LPCSTR AKey)
{
	DWORD dwDisposition;
	return WrapError(RegCreateKeyEx(FhKey ? FhKey : FhRootKey, AKey, 0, NULL, 0, KEY_ALL_ACCESS, NULL,
		&FhKey, &dwDisposition));
}

BOOL RegistryFullAccess::RecursiveDeleteKey(LPCSTR AKey)
{
	HKEY hkey;

	if(RegOpenKeyEx(FhKey ? FhKey : FhRootKey, AKey, 0, KEY_ALL_ACCESS, &hkey) != ERROR_SUCCESS)
		return FALSE;

	if(!IntRecursiveDeleteKey(hkey))
	{
		RegCloseKey(hkey);
		return FALSE;
	}

	if(RegCloseKey(hkey) != ERROR_SUCCESS) return FALSE;
	return WrapError(RegDeleteKey(hkey, AKey));
}

BOOL RegistryFullAccess::IntRecursiveDeleteKey(HKEY hkey)
{
	char buf[260];
	DWORD dwlen = 260;
	FILETIME ftime;
	HKEY hsubkey;
	LONG n = RegEnumKeyEx(hkey, 0, buf, &dwlen, NULL, NULL, NULL, &ftime);
	while(n == ERROR_MORE_DATA || n == ERROR_SUCCESS)
	{
		if(RegOpenKeyEx(hkey, buf, 0, KEY_ALL_ACCESS, &hsubkey) != ERROR_SUCCESS) return FALSE;
		IntRecursiveDeleteKey(hsubkey);
		if(RegCloseKey(hsubkey) != ERROR_SUCCESS) return FALSE;
		if(RegDeleteKey(hkey, buf) != ERROR_SUCCESS) return FALSE;
    dwlen = 260;   // I3584
		n = RegEnumKeyEx(hkey, 0, buf, &dwlen, NULL, NULL, NULL, &ftime);
	}
	return TRUE;
}

BOOL RegistryFullAccess::DeleteKey(LPCSTR AKey)
{
	return WrapError(RegDeleteKey(FhKey, AKey));
}

BOOL RegistryFullAccess::DeleteValue(LPCSTR AName)
{
	return WrapError(RegDeleteValue(FhKey, AName));
}

HKEY RegistryFullAccess::GetKey(LPCSTR AKey)
{
	HKEY Result;
	if(RegOpenKeyEx(FhKey ? FhKey : FhRootKey, AKey, 0, KEY_ALL_ACCESS, &Result) != ERROR_SUCCESS)
		return 0;
	return Result;
}

HKEY RegistryReadOnly::GetKeyReadOnly(LPCSTR AKey)
{
	HKEY Result;
	if(RegOpenKeyEx(FhKey ? FhKey : FhRootKey, AKey, 0, KEY_READ, &Result) != ERROR_SUCCESS)
		return 0;
	return Result;
}


BOOL RegistryReadOnly::GetKeyNames(LPSTR AKey, int len, int n)
{
	DWORD dwlen = len;
	FILETIME ftime;
	return WrapError(RegEnumKeyEx(FhKey, n, AKey, &dwlen, NULL, NULL, NULL, &ftime));
}

BOOL RegistryReadOnly::GetValueNames(LPSTR AName, int len, int n)
{
	DWORD dwlen = len;
	return WrapError(RegEnumValue(FhKey, n, AName, &dwlen, NULL, NULL, NULL, NULL));
}

BOOL RegistryReadOnly::GetValueNames(LPWSTR AName, int len, int n)
{
	DWORD dwlen = len;
	return WrapError(RegEnumValueW(FhKey, n, AName, &dwlen, NULL, NULL, NULL, NULL));
}

BOOL RegistryReadOnly::KeyExists(LPCSTR AKey)
{
	HKEY hkey = GetKeyReadOnly(AKey);
	if(hkey != 0) RegCloseKey(hkey);
	return hkey != 0;
}

BOOL RegistryFullAccess::OpenKey(LPCSTR AKey, BOOL ACreate)
{
	if(ACreate)
		if(!KeyExists(AKey))
			return CreateKey(AKey);
		else
			return OpenKey(AKey, FALSE);
	else
	{
		HKEY hkey = GetKey(AKey);
		if(!hkey) return FALSE;
		if(FhKey) CloseKey();
		FhKey = hkey;
		return TRUE;
	}
}


BOOL RegistryReadOnly::OpenKeyReadOnly(LPCSTR AKey)
{
	HKEY hkey = GetKeyReadOnly(AKey);
	if(!hkey) return FALSE;
	if(FhKey) CloseKey();
	FhKey = hkey;
	return TRUE;
}

int RegistryReadOnly::ReadInteger(LPCSTR AName)
{
	int buf = 0;
	DWORD dwlen = sizeof(long);
	if(RegQueryValueEx(FhKey, AName, NULL, NULL, (LPBYTE) &buf, &dwlen) == ERROR_SUCCESS)
		return buf;
	return 0;
	//return WrapError();
}

BOOL RegistryReadOnly::ReadString(LPCSTR AName, LPSTR AValue, int len)
{
	DWORD dwlen = len;
	return WrapError(RegQueryValueEx(FhKey, AName, NULL, NULL, (LPBYTE) AValue, &dwlen));
}

BOOL RegistryReadOnly::ReadString(LPCWSTR AName, LPWSTR AValue, int len)
{
	DWORD dwlen = len * 2; // byte size
	return WrapError(RegQueryValueExW(FhKey, AName, NULL, NULL, (LPBYTE) AValue, &dwlen));
}

BOOL RegistryReadOnly::ValueExists(LPCSTR AName)
{
	DWORD dwType;
	return WrapError(RegQueryValueEx(FhKey,  AName, NULL, &dwType, NULL, NULL));
}

BOOL RegistryReadOnly::ValueExists(LPCWSTR AName)
{
	DWORD dwType;
	return WrapError(RegQueryValueExW(FhKey,  AName, NULL, &dwType, NULL, NULL));
}

BOOL RegistryReadOnly::WrapError(DWORD res)
{
	/*if(res != ERROR_SUCCESS)
	{
		SendDebugMessageFormat(0,KDS_PROGRAM,0,"RegistryFullAccess::WrapError = %d", res);
	}*/
	return res == ERROR_SUCCESS;
}

BOOL RegistryFullAccess::WriteInteger(LPCSTR AName, const int AValue)
{
	return WrapError(RegSetValueEx(FhKey, AName, 0, REG_DWORD, (LPBYTE) &AValue, sizeof(int)));
}

BOOL RegistryFullAccess::WriteString(LPCSTR AName, LPCSTR AValue) {
	return WrapError(RegSetValueEx(FhKey, AName, 0, REG_SZ, (LPBYTE) AValue, (DWORD) strlen(AValue)+1));
}

BOOL RegistryFullAccess::WriteString(LPCWSTR AName, LPCWSTR AValue) {
	return WrapError(RegSetValueExW(FhKey, AName, 0, REG_SZ, (LPBYTE) AValue, (DWORD) wcslen(AValue)*2+2));
}

RegistryReadOnly *Reg_GetKeymanInstalledKeyboard(LPSTR kbname)
{
	RegistryReadOnly *reg = new RegistryReadOnly(HKEY_LOCAL_MACHINE);
	if(!reg->OpenKeyReadOnly(REGSZ_KeymanInstalledKeyboardsLM) || !reg->OpenKeyReadOnly(kbname))
	{
		delete reg;
		reg = new RegistryReadOnly(HKEY_CURRENT_USER);
		if(!reg->OpenKeyReadOnly(REGSZ_KeymanInstalledKeyboardsCU) || !reg->OpenKeyReadOnly(kbname))
		{
			delete reg;
			return NULL;
		}
	}
	return reg;
}

RegistryReadOnly *Reg_GetKeymanActiveKeyboard(LPSTR kbname)
{
	RegistryReadOnly *reg = new RegistryReadOnly(HKEY_CURRENT_USER);
	if(!reg->OpenKeyReadOnly(REGSZ_KeymanActiveKeyboards) || !reg->OpenKeyReadOnly(kbname))
	{
		delete reg;
		return NULL;
	}
	return reg;
}
