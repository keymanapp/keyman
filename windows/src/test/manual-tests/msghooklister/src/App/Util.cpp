#include "stdafx.h"
#include "Util.h"
#include <shlobj.h>
#include <shlwapi.h>
#include <iostream>
#include <fstream>
#include "Messages.h"

WException::WException(const std::wstring& error)
	: whatText(error)
{}

const std::wstring::value_type* WException::what() const
{
	return whatText.c_str();
}

std::wstring GetWin32ErrorText(DWORD err, const std::wstring& preErrorText)
{
	std::wstring message;
	GetModuleMessage(NULL, err, message);
	std::wostringstream error;
	if(preErrorText.empty())
	{
		error << message;
	}
	else
	{
		error << preErrorText << L' ' << message;
	}
	return error.str();
}

std::wstring GetLastErrorText(const std::wstring& prependText)
{
	return GetWin32ErrorText(GetLastError(), prependText);
}

DECLSPEC_NORETURN void ThrowWin32Error(DWORD err, const std::wstring& preErrorText)
{
	throw WException(GetWin32ErrorText(err, preErrorText));
}

std::wstring GetFolderPath(REFGUID folderId)
{
	LPWSTR folderPath;
	if(SUCCEEDED(SHGetKnownFolderPath(folderId, 0, NULL, &folderPath)))
	{
		std::wstring path(folderPath);
		CoTaskMemFree(folderPath);
		return path;
	}
	return L"";
}

// returns the file or full path name of the module specified
// by the hMod parameter
std::wstring GetModulePathPart(HMODULE hMod, GMPP_Flag operation)
{
	DWORD dwBufferSize = MAX_PATH;
	std::wstring filePath(MAX_PATH, 0);
	for(;;)
	{
		DWORD dwWrittenSize = GetModuleFileName(hMod, &filePath[0], dwBufferSize);
		if(dwWrittenSize == dwBufferSize)
		{
			filePath.resize(dwBufferSize *= 2);
		}
		else
		{
			filePath.resize(dwWrittenSize);
			break;
		}
	}
	if(operation == FILE_NAME_ONLY)
	{
		std::wstring::size_type pos = filePath.find_last_of(L"\\/");
		if(pos == std::wstring::npos)
		{
			pos = 0;
		}
		return pos ? filePath.substr(pos + 1) : filePath;
	}
	else if(operation == DIRECTORY_ONLY)
	{
		std::wstring::size_type pos = filePath.find_last_of(L"\\/");
		if(pos != std::wstring::npos)
		{
			filePath.erase(pos + 1);
		}
		return filePath;
	}
	else
	{
		return filePath;
	}
}

std::wstring BuildAbsolutePath(const std::wstring& addendum)
{
	const std::wstring& appDir = GetModulePathPart(NULL, DIRECTORY_ONLY);
	return appDir + addendum;
}

std::wstring GetSystem32ModulePath(const std::wstring& module)
{
	std::wstring win32kPathString = GetFolderPath(FOLDERID_System);
	if(win32kPathString.empty())
	{
		LPCWSTR win32kEnvPath = L"%systemroot%\\system32\\";
		DWORD bufferRequired = ExpandEnvironmentStrings(win32kEnvPath, NULL, 0);
		if(bufferRequired)
		{
			win32kPathString.resize(bufferRequired);
			bufferRequired = ExpandEnvironmentStrings(win32kEnvPath, &win32kPathString[0], bufferRequired);
			win32kPathString.resize(bufferRequired - 1);
		}
	}
	else
	{
		win32kPathString.push_back(L'\\');
	}
	return win32kPathString += module;
}

bool GetWin32kParticulars(ULONG& checksum, ULONG& timestamp)
{
	checksum = timestamp = 0;
	std::wstring win32kPathString = GetSystem32ModulePath(L"win32k.sys");
	std::ifstream win32kFile(win32kPathString.c_str(), std::ios::binary);
	IMAGE_DOS_HEADER idh;
	if(win32kFile.read(reinterpret_cast<char*>(&idh), sizeof(idh)))
	{
		if(win32kFile.seekg(idh.e_lfanew, std::ios::beg))
		{
			IMAGE_NT_HEADERS inth;
			if(win32kFile.read(reinterpret_cast<char*>(&inth), sizeof(inth)))
			{
				checksum = inth.OptionalHeader.CheckSum;
				timestamp = inth.FileHeader.TimeDateStamp;
			}
		}
	}
	return checksum && timestamp;
}

BOOL SetProcessPrivilege(HANDLE hProcess, LPCTSTR privilege, BOOL enable)
{
	WinType<> hProcToken(NULL, &CloseHandle);
	TOKEN_PRIVILEGES tp;
	LUID luid;

	if(!OpenProcessToken(hProcess, TOKEN_ADJUST_PRIVILEGES, &hProcToken))
	{
		return FALSE;
	}
	if ( !LookupPrivilegeValue( 
		NULL,            // lookup privilege on local system
		privilege,   // privilege to lookup 
		&luid ) )        // receives LUID of privilege
	{
		return FALSE; 
	}

	tp.PrivilegeCount = 1;
	tp.Privileges[0].Luid = luid;
	if(enable)
	{
		tp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
	}
	else
	{
		tp.Privileges[0].Attributes = 0;
	}

	// Enable the privilege or disable all privileges.
	if (!AdjustTokenPrivileges(
		*hProcToken, 
		FALSE, 
		&tp, 
		sizeof(TOKEN_PRIVILEGES), 
		NULL, 
		NULL) )
	{ 
		return FALSE; 
	}
	return GetLastError() != ERROR_NOT_ALL_ASSIGNED;
}