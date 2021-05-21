#ifndef UTIL_H
#define UTIL_H

#pragma once

#include "stdafx.h"
#include <string>

class WException
{
	std::wstring whatText;

public:
	WException(const std::wstring& error);
	virtual const std::wstring::value_type* what() const;
	virtual ~WException(){}
};

enum GMPP_Flag
{
	FULL_PATH = 0,
	FILE_NAME_ONLY,
	DIRECTORY_ONLY
};

// returns the file or full path name of the module specified
// by the hMod parameter
std::wstring GetModulePathPart(HMODULE hMod, GMPP_Flag operation);
std::wstring BuildAbsolutePath(const std::wstring& addendum);
std::wstring GetFolderPath(REFGUID folderId);
std::wstring GetWin32ErrorText(DWORD err, const std::wstring& prependText = L"");
std::wstring GetLastErrorText(const std::wstring& prependText = L"");
DECLSPEC_NORETURN void ThrowWin32Error(DWORD err, const std::wstring& preErrorText = L"");
bool GetWin32kParticulars(ULONG& checksum, ULONG& timestamp);
std::wstring GetSystem32ModulePath(const std::wstring& module);
BOOL SetProcessPrivilege(HANDLE hProcess, LPCWSTR privilege, BOOL enable);

#endif
