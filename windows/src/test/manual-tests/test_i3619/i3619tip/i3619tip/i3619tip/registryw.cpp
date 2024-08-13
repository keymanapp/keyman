/*
  Name:             registryw
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      1 Dec 2012

  Modified Date:    13 Dec 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Dec 2012 - mcdurdin - I3622 - V9.0 - Add Registry*W classes for Unicode
                    13 Dec 2012 - mcdurdin - I3675 - V9.0 - Minor updates to I3622 for consistency
*/

#include "stdafx.h"

RegistryReadOnlyW::RegistryReadOnlyW(HKEY AhRootKey)
{
	FhRootKey = AhRootKey;
	FhKey = NULL;
}

RegistryReadOnlyW::~RegistryReadOnlyW()
{
	if(FhKey != NULL) CloseKey();
}

BOOL RegistryReadOnlyW::CloseKey(void)
{
	BOOL Result = FALSE;
	if(FhKey != NULL) Result = WrapError(RegCloseKey(FhKey));
	FhKey = NULL;
	return TRUE;
}


BOOL RegistryFullAccessW::CreateKey(LPCWSTR AKey)
{
	DWORD dwDisposition;
	return WrapError(RegCreateKeyExW(FhKey ? FhKey : FhRootKey, AKey, 0, NULL, 0, KEY_ALL_ACCESS, NULL,
		&FhKey, &dwDisposition));
}

BOOL RegistryFullAccessW::RecursiveDeleteKey(LPCWSTR AKey)
{
	HKEY hkey;

	if(RegOpenKeyExW(FhKey ? FhKey : FhRootKey, AKey, 0, KEY_ALL_ACCESS, &hkey) != ERROR_SUCCESS)
		return FALSE;

	if(!IntRecursiveDeleteKey(hkey))
	{
		RegCloseKey(hkey);
		return FALSE;
	}

	if(RegCloseKey(hkey) != ERROR_SUCCESS) return FALSE;
	return WrapError(RegDeleteKeyW(hkey, AKey));
}

BOOL RegistryFullAccessW::IntRecursiveDeleteKey(HKEY hkey)
{
	WCHAR buf[260];
	DWORD dwlen = _countof(buf);
	FILETIME ftime;
	HKEY hsubkey;
	LONG n = RegEnumKeyExW(hkey, 0, buf, &dwlen, NULL, NULL, NULL, &ftime);
	while(n == ERROR_MORE_DATA || n == ERROR_SUCCESS)
	{
		if(RegOpenKeyExW(hkey, buf, 0, KEY_ALL_ACCESS, &hsubkey) != ERROR_SUCCESS) return FALSE;
		IntRecursiveDeleteKey(hsubkey);
		if(RegCloseKey(hsubkey) != ERROR_SUCCESS) return FALSE;
		if(RegDeleteKeyW(hkey, buf) != ERROR_SUCCESS) return FALSE;
    dwlen = _countof(buf);
		n = RegEnumKeyExW(hkey, 0, buf, &dwlen, NULL, NULL, NULL, &ftime);
	}
	return TRUE;
}

BOOL RegistryFullAccessW::DeleteKey(LPCWSTR AKey)
{
	return WrapError(RegDeleteKeyW(FhKey, AKey));
}

BOOL RegistryFullAccessW::DeleteValue(LPCWSTR AName)
{
	return WrapError(RegDeleteValueW(FhKey, AName));
}

HKEY RegistryFullAccessW::GetKey(LPCWSTR AKey)
{
	HKEY Result;
	if(RegOpenKeyExW(FhKey ? FhKey : FhRootKey, AKey, 0, KEY_ALL_ACCESS, &Result) != ERROR_SUCCESS)
		return 0;
	return Result;
}

HKEY RegistryReadOnlyW::GetKeyReadOnly(LPCWSTR AKey)
{
	HKEY Result;
	if(RegOpenKeyExW(FhKey ? FhKey : FhRootKey, AKey, 0, KEY_READ, &Result) != ERROR_SUCCESS)
		return 0;
	return Result;
}


BOOL RegistryReadOnlyW::GetKeyNames(LPWSTR AKey, int len, int n)
{
	DWORD dwlen = len;
	FILETIME ftime;
	return WrapError(RegEnumKeyExW(FhKey, n, AKey, &dwlen, NULL, NULL, NULL, &ftime));
}

BOOL RegistryReadOnlyW::GetValueNames(LPWSTR AName, int len, int n)
{
	DWORD dwlen = len;
	return WrapError(RegEnumValueW(FhKey, n, AName, &dwlen, NULL, NULL, NULL, NULL));
}

BOOL RegistryReadOnlyW::KeyExists(LPCWSTR AKey)
{
	HKEY hkey = GetKeyReadOnly(AKey);
	if(hkey != 0) RegCloseKey(hkey);
	return hkey != 0;
}

BOOL RegistryFullAccessW::OpenKey(LPCWSTR AKey, BOOL ACreate)
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


BOOL RegistryReadOnlyW::OpenKeyReadOnly(LPCWSTR AKey)
{
	HKEY hkey = GetKeyReadOnly(AKey);
	if(!hkey) return FALSE;
	if(FhKey) CloseKey();
	FhKey = hkey;
	return TRUE;
}

int RegistryReadOnlyW::ReadInteger(LPCWSTR AName)
{
	int buf = 0;
	DWORD dwlen = sizeof(int);   // I3675
	if(RegQueryValueExW(FhKey, AName, NULL, NULL, (LPBYTE) &buf, &dwlen) == ERROR_SUCCESS)
		return buf;
	return 0;
	//return WrapError();
}

BOOL RegistryReadOnlyW::ReadString(LPCWSTR AName, LPWSTR AValue, int len)
{
	DWORD dwlen = len * sizeof(WCHAR);
	return WrapError(RegQueryValueExW(FhKey, AName, NULL, NULL, (LPBYTE) AValue, &dwlen));
}

BOOL RegistryReadOnlyW::ValueExists(LPCWSTR AName)
{
	DWORD dwType;
	return WrapError(RegQueryValueExW(FhKey,  AName, NULL, &dwType, NULL, NULL));
}

BOOL RegistryReadOnlyW::WrapError(DWORD res)
{
	/*if(res != ERROR_SUCCESS)
	{
		__logerror(0,KDS_PROGRAM,0,"RegistryFullAccessW::WrapError = %d", res);
	}*/
	return res == ERROR_SUCCESS;
}

BOOL RegistryFullAccessW::WriteInteger(LPCWSTR AName, const int AValue)
{
	return WrapError(RegSetValueExW(FhKey, AName, 0, REG_DWORD, (LPBYTE) &AValue, sizeof(int)));
}

BOOL RegistryFullAccessW::WriteString(LPCWSTR AName, LPCWSTR AValue)
{
  DWORD dwLen = (DWORD) (wcslen(AValue)+1) * sizeof(WCHAR);   // I3675
	return WrapError(RegSetValueExW(FhKey, AName, 0, REG_SZ, (LPBYTE) AValue, dwLen));
}

