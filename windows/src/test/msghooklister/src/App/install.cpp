#include "stdafx.h"
#include "install.h"
#include <dbghelp.h>
#include <fstream>
#include <shellapi.h>
#include <shlobj.h>
#include <iostream>
#include "dbghelpprotos.h"
#include "Messages.h"
#include "RuntimeLoader.h"
#include "Util.h"
#include "../Drv/resource.h"
#include "../Drv/sharedinfo.h"

#define DRIVER_FILE_NAME L"MsgLister.sys"
static const LPCWSTR g_systemDriverAddendum = L"\\drivers\\" DRIVER_FILE_NAME ;

DWORD CheckPrerequisites()
{
	BOOL isWow64 = FALSE;
	SYSTEM_INFO si = {0};
	GetNativeSystemInfo(&si);
	if(si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_IA64 ||
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_UNKNOWN)
	{
		return MSG_PLATFORM_NOT_SUPPORTED;
	}
	else if(IsWow64Process(GetCurrentProcess(), &isWow64) && isWow64)
	{
		return MSG_NEEDS_NATIVE_VERSION;
	}
	else if(!IsUserAnAdmin())
	{
		return MSG_NEEDS_USER_ADMIN;
	}
	return 0;
}

struct SymbolData
{
	ULONG validateHwndOffset;
	ULONG gpresUserOffset;
	ULONG userGetAtomNameOffset;
	ULONG atomArrayOffset;
	ULONG grpWinstaListOffset;
	ULONG timestamp;
	ULONG checksum;
};

BOOL CALLBACK SymCallback(HANDLE /*hProc*/, ULONG action, ULONG64 data, ULONG64 /*ctx*/)
{
	switch(action)
	{
		case CBA_EVENT:
		{
			IMAGEHLP_CBA_EVENT* pice = reinterpret_cast<IMAGEHLP_CBA_EVENT*>(data);
			std::wcerr << L"Sym Event: " << pice->desc;
			return TRUE;
		}
		break;
	}
	return FALSE;
}

bool SetTempSymPathIfNeeded(HANDLE hDummyProc, RuntimeLoader& dbghelp, std::wstring& tempDir)
{
	SymGetSymPathW symGetPath;
	SymSetSymPathW symSetPath;
	if(!(dbghelp.FindFunction("SymGetSearchPathW", symGetPath) &&
		 dbghelp.FindFunction("SymSetSearchPathW", symSetPath))
	)
	{
		DisplayMessage(MSG_CANT_LOAD_DBGHELP_FUNCS);
		return false;
	}
	WCHAR buffer[3];
	// if the user hasn't set up a search path, or the call fails
	// use the temporary directory to store any downloaded files
	//
	// note this may cause debug output of the 'Invalid parameter' kind.
	// this is because our buffer isn't big enough to hold the full
	// symbol path and wcscpy_s (which dnghelp uses) complains about it
	if(!symGetPath(hDummyProc, buffer, ARRAYSIZE(buffer))
		|| ((buffer[0] == L'.') && (buffer[1] == 0))
	)
	{
		std::wstring fullSymPath(L"srv*");
		DWORD bufferRequired = GetTempPath(0, NULL);
		if(!bufferRequired)
		{
			DisplayMessage(MSG_CANT_LOAD_SYMBOL_INFO, L"GetTempPath", GetLastErrorText().c_str());
			return false;
		}
		std::wstring tempDirectory(bufferRequired, 0);
		GetTempPath(bufferRequired, &tempDirectory[0]);
		// chop off the unneeded terminating null
		// which would otherwise mess up concatenation
		tempDirectory.resize(bufferRequired - 1);
		std::wstring symTemp = tempDirectory + L"sym\\";
		BOOL bRet = CreateDirectory(symTemp.c_str(), NULL);
		DBG_TEXT("CreateDirectory %s returned %d (error %d)", symTemp.c_str(), bRet, GetLastError());
		tempDir = bRet ? symTemp : tempDirectory;
		fullSymPath += tempDir;
		fullSymPath += L"*http://msdl.microsoft.com/download/symbols";
		DBG_TEXT("Full symbol path = %s", fullSymPath.c_str());
		if(!symSetPath(hDummyProc, fullSymPath.c_str()))
		{
			DisplayMessage(MSG_CANT_LOAD_SYMBOL_INFO, L"SymSetSearchPath", GetLastErrorText().c_str());
			return false;
		}
		// cheeky flag so we know whether to delete the directory
		// or merely files
		tempDir.push_back(static_cast<WCHAR>(bRet));
	}
	return true;
}

bool GetSymbolData(SymbolData& symData)
{
	try
	{
		// load dbghelp from the current directory
		// and grab the entrypoints we need
		RuntimeLoader dbghelp(BuildAbsolutePath(L"dbghelp.dll").c_str());
		SetSymOptions setSymOpts;
		if(dbghelp.FindFunction("SymSetOptions", setSymOpts))
		{
			const DWORD options = SYMOPT_AUTO_PUBLICS | SYMOPT_CASE_INSENSITIVE | SYMOPT_OMAP_FIND_NEAREST | SYMOPT_DEBUG;
			setSymOpts(options);
		}

		SymInitW symInit;
		SymLoadModW symLoadModEx;
		SymClose symCleanup;
		SymAtNameW symFromName;
		SymReg registerCallback;
		if(dbghelp.FindFunction("SymInitializeW", symInit) && 
			dbghelp.FindFunction("SymCleanup", symCleanup) &&
			dbghelp.FindFunction("SymFromNameW", symFromName) &&
			dbghelp.FindFunction("SymLoadModuleExW", symLoadModEx) &&
			dbghelp.FindFunction("SymRegisterCallbackW64", registerCallback)
		)
		{
			HANDLE hDummyProc = (HANDLE)1;
			if(symInit(hDummyProc, NULL, FALSE))
			{
				registerCallback(hDummyProc, &SymCallback, 0);
				std::wstring tempDirectory;
				if(!SetTempSymPathIfNeeded(hDummyProc, dbghelp, tempDirectory))
				{
					return false;
				}
				// get the checksum and timestamp from the file
				// then find the offsets to the symbols we need
				GetWin32kParticulars(symData.checksum, symData.timestamp);
				const std::wstring& win32kPathString = GetSystem32ModulePath(L"win32k.sys");
				DBG_TEXT("Path to win32k.sys = %s", win32kPathString.c_str());
				DWORD64 baseAddress = symLoadModEx(hDummyProc, NULL, win32kPathString.c_str(), NULL, 0, 0, NULL, 0);
				DBG_TEXT("SymLoadModuleEx returned 0x%I64x, error %d", baseAddress, GetLastError());
				DWORD lastError = GetLastError();
				if(baseAddress)
				{
					SYMBOL_INFO si = {sizeof(si), 0};
#ifndef _WIN64
					if(symFromName(hDummyProc, L"win32k!@ValidateHwnd@4", &si))
#else
					if(symFromName(hDummyProc, L"win32k!ValidateHwnd", &si))
#endif
					{
						symData.validateHwndOffset = static_cast<ULONG>(si.Address - si.ModBase);
						DBG_TEXT("Found validateHwnd at 0x%I64x, base = 0x%I64x", si.Address, si.ModBase);
#ifndef _WIN64
						if(symFromName(hDummyProc, L"win32k!_gpresUser", &si))
#else
						if(symFromName(hDummyProc, L"win32k!gpresUser", &si))
#endif
						{
							DBG_TEXT("Found _gpresUser at 0x%I64x, base = 0x%I64x", si.Address, si.ModBase);
							symData.gpresUserOffset = static_cast<ULONG>(si.Address - si.ModBase);
							// this isn't a typo, we do want the internal UserGetAtomName and not the external NtUserGetAtomName
#ifndef _WIN64
							if(symFromName(hDummyProc, L"win32k!_UserGetAtomName@12", &si))
#else
							if(symFromName(hDummyProc, L"win32k!UserGetAtomName", &si))
#endif
							{
								DBG_TEXT("Found UserGetAtomName at 0x%I64x, base = 0x%I64x", si.Address, si.ModBase);
								symData.userGetAtomNameOffset = static_cast<ULONG>(si.Address - si.ModBase);
#ifndef _WIN64
								if(symFromName(hDummyProc, L"win32k!_aatomSysLoaded", &si))
#else
								if(symFromName(hDummyProc, L"win32k!aatomSysLoaded", &si))
#endif
								{
									DBG_TEXT("Found aatomSysLoaded at 0x%I64x, base = 0x%I64x", si.Address, si.ModBase);
									symData.atomArrayOffset = static_cast<ULONG>(si.Address - si.ModBase);
#ifndef _WIN64
									if(symFromName(hDummyProc, L"win32k!_grpWinStaList", &si))
#else
									if(symFromName(hDummyProc, L"win32k!grpWinStaList", &si))
#endif
									{
										DBG_TEXT("Found grpWinStaList at 0x%I64x, base = 0x%I64x", si.Address, si.ModBase);
										symData.grpWinstaListOffset = static_cast<ULONG>(si.Address - si.ModBase);
									}
								}
							}
						}						
					}					
					lastError = GetLastError();
				}
				if(!(symData.gpresUserOffset && 
					symData.validateHwndOffset && 
					symData.userGetAtomNameOffset && 
					symData.atomArrayOffset &&
					symData.grpWinstaListOffset))
				{
					ULONG index = baseAddress ? 1 : 0;
					static LPCWSTR functions[2] = {L"SymLoadModuleExW", L"SymFromNameW"};
					DisplayMessage(MSG_CANT_LOAD_SYMBOL_INFO, functions[index], GetWin32ErrorText(lastError).c_str());
				}
				// delete the directory or any files we created
				if(!tempDirectory.empty())
				{
					WCHAR flag = tempDirectory[tempDirectory.length() - 1];
					// remove the flag char
					tempDirectory.erase(tempDirectory.end() - 1);
					SHFILEOPSTRUCT shop = {0};
					shop.wFunc = FO_DELETE;
					shop.fFlags = FOF_NO_UI;
					shop.hwnd = NULL;
					// if we didn't create a directory, try and delete the symbol file
					if(!flag)
					{
						tempDirectory += L"\\win32k.pdb";
					}
					// add on a terminating NULs (c_str() will add the other)
					tempDirectory.push_back(0);
					shop.pFrom = tempDirectory.c_str();
					int ret = SHFileOperation(&shop);
					DBG_TEXT("SHFileOperation deleting %s returned %d", tempDirectory.c_str(), ret);
				}
				symCleanup(hDummyProc);
			}
			else
			{
				DisplayMessage(MSG_CANT_LOAD_SYMBOL_INFO, L"SymInitialize", GetLastErrorText().c_str());
			}
		}
		else
		{
			DisplayMessage(MSG_CANT_LOAD_DBGHELP_FUNCS);
		}
	}
	catch(const WException& ex)
	{
		DisplayMessage(MSG_GENERIC_INSTALL_FAILURE, ex.what());
	}
	return symData.gpresUserOffset && 
		symData.validateHwndOffset && 
		symData.userGetAtomNameOffset &&
		symData.atomArrayOffset &&
		symData.timestamp && 
		symData.checksum;
}

bool SetRegistryData(const SymbolData& symData)
{
	DBG_TEXT("Parameter key is %s", g_parameterKey);
	LSTATUS lStat;
	{
		WinType<HKEY, LSTATUS> serviceParamsKey(NULL, &RegCloseKey);
		lStat = RegCreateKeyEx(
			HKEY_LOCAL_MACHINE,
			g_parameterKey,
			0,
			NULL,
			REG_OPTION_NON_VOLATILE,
			KEY_SET_VALUE,
			NULL,
			&serviceParamsKey,
			NULL);
		if(lStat != ERROR_SUCCESS)
		{
			DisplayMessage(MSG_CANT_CREATE_PARAM_KEY, g_parameterKey, GetWin32ErrorText(lStat).c_str());
			return false;
		}
		// ensure that the REG_DWORD specification remains valid in the face
		// of any potential changes
		C_ASSERT(sizeof(symData.validateHwndOffset) == sizeof(DWORD));
		C_ASSERT(sizeof(symData.gpresUserOffset) == sizeof(DWORD));
		C_ASSERT(sizeof(symData.userGetAtomNameOffset) == sizeof(DWORD));
		C_ASSERT(sizeof(symData.timestamp) == sizeof(DWORD));
		C_ASSERT(sizeof(symData.checksum) == sizeof(DWORD));

#define SET_IT(name, value) RegSetValueEx(*serviceParamsKey, (name), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&(value)), sizeof(DWORD))

		lStat = SET_IT(g_validateHwndParameterName, symData.validateHwndOffset);
		if(lStat == ERROR_SUCCESS)
			lStat = SET_IT(g_gpresUserParameterName, symData.gpresUserOffset);
			if(lStat == ERROR_SUCCESS)
				lStat = SET_IT(g_timeStampParameterName, symData.timestamp);
				if(lStat == ERROR_SUCCESS)
					lStat = SET_IT(g_checksumParameterName, symData.checksum);
					if(lStat == ERROR_SUCCESS)
						lStat = SET_IT(g_userGetAtomNameParamName, symData.userGetAtomNameOffset);
						if(lStat == ERROR_SUCCESS)
							lStat = SET_IT(g_atomArrayParamName, symData.atomArrayOffset);
							if(lStat == ERROR_SUCCESS)
								lStat = SET_IT(g_grpWinStaListParamName, symData.grpWinstaListOffset);
	}
	if(lStat)
	{
		DisplayMessage(MSG_CANT_CREATE_PARAM_KEY, g_parameterKey, GetWin32ErrorText(lStat).c_str());
		// don't have to delete the key, uninstalling the service will
		// do it for us
	}
	else
	{
		DBG_TEXT("Successfully set registry parameters");
	}
	return !lStat;
}

bool InstallService()
{
	std::wstring newDriverPath = GetFolderPath(FOLDERID_System) + g_systemDriverAddendum;
	DBG_TEXT("New driver path: %s", newDriverPath.c_str());
	const std::wstring& currentDriverPath = BuildAbsolutePath(DRIVER_FILE_NAME);
	if(!CopyFile(currentDriverPath.c_str(), newDriverPath.c_str(), TRUE))
	{
		DWORD lastError = GetLastError();
		DBG_TEXT("First CopyFile failed with %d", lastError);
		// check if the service is already installed, which is the most
		// likely cause of failure
		HKEY hKey = NULL;
		LSTATUS err;
		if((err = RegOpenKeyEx(HKEY_LOCAL_MACHINE, g_parameterKey, 0, STANDARD_RIGHTS_READ, &hKey)) != ERROR_SUCCESS)
		{
			// service wasn't installed, report the original failure
			DBG_TEXT("Failed to open registry key, %lu", err);
			DisplayMessage(MSG_CANT_COPY_FILE, currentDriverPath.c_str(), newDriverPath.c_str(), GetWin32ErrorText(lastError).c_str());
			return false;
		}
		// service was installed, just overwrite the file
		RegCloseKey(hKey);
		if(!CopyFile(currentDriverPath.c_str(), newDriverPath.c_str(), FALSE))
		{
			lastError = GetLastError();
			DBG_TEXT("Second CopyFile failed with %lu", lastError);
			DisplayMessage(MSG_CANT_COPY_FILE, currentDriverPath.c_str(), newDriverPath.c_str(), GetWin32ErrorText(lastError).c_str());
			return false;
		}
		return true;
	}
	// file didn't already exist, create the service to go with it
	WinType<SC_HANDLE> scm(OpenSCManager(NULL, NULL, SC_MANAGER_CREATE_SERVICE), &CloseServiceHandle);
	if(*scm)
	{
		DBG_TEXT("Opened SCM");
		WinType<SC_HANDLE> service(
			CreateService(*scm,
				g_serviceName,
				g_serviceName,
				SERVICE_CHANGE_CONFIG,
				SERVICE_KERNEL_DRIVER,
				SERVICE_DEMAND_START,
				SERVICE_ERROR_NORMAL,
				newDriverPath.c_str(),
				NULL,
				NULL,
				NULL,
				NULL,
				NULL
			),
			&CloseServiceHandle);
		if(*service)
		{
			// build the path of the description string
			std::wostringstream descriptionLocation;
			descriptionLocation << L'@' << newDriverPath << L",-" << STR_ID_DESCRIPTION << L'\0';
			const std::wstring& description = descriptionLocation.str();
			LPCWSTR pDesc = description.c_str();
			DBG_TEXT("Created service - desc location = %s", pDesc);
			// more const-incorrectness
			SERVICE_DESCRIPTION sd = {const_cast<LPWSTR>(pDesc)};
			if(!ChangeServiceConfig2(*service, SERVICE_CONFIG_DESCRIPTION, &sd))
			{
				DBG_TEXT("ChangeServiceConfig2 failed %d", GetLastError());
			}
			return true;
		}
		else
		{
			DisplayMessage(MSG_CANT_CREATE_SERVICE, g_serviceName, GetLastErrorText().c_str());
		}
	}
	else
	{
		DisplayMessage(MSG_CANT_OPEN_SCM, GetLastErrorText().c_str());
	}
	if(!DeleteFile(newDriverPath.c_str()))
	{
		DisplayMessage(MSG_CANT_DELETE_FILE, newDriverPath.c_str(), GetLastErrorText().c_str());
	}
	return false;
}

bool UninstallService()
{
	WinType<SC_HANDLE> scm(OpenSCManager(NULL, NULL, 0), &CloseServiceHandle);
	if(*scm)
	{
		DBG_TEXT("Opened SCM");
		WinType<SC_HANDLE> service(OpenService(*scm, g_serviceName, DELETE | SERVICE_STOP), &CloseServiceHandle);
		if(*service)
		{
			DBG_TEXT("Opened Service, stopping and deleting");
			SERVICE_STATUS stat;
			ControlService(*service, SERVICE_CONTROL_STOP, &stat);
			return ((DeleteService(*service)) || (GetLastError() == ERROR_SERVICE_MARKED_FOR_DELETE));
		}
		else
		{
			DBG_TEXT("OpenService returned %d", GetLastError());
		}
	}
	else
	{
		DBG_TEXT("OpenSCManager returned %d", GetLastError());
	}
	return false;
}

bool Install()
{
	DBG_TEXT("Starting install");
	if(DWORD errorMessage = CheckPrerequisites())
	{
		DisplayMessage(errorMessage);
		return false;
	}
	SymbolData symData = {0};
	if(!GetSymbolData(symData))
	{
		return false;
	}
	if(!InstallService())
	{
		return false;
	}
	if(!SetRegistryData(symData))
	{
		Uninstall();
		return false;
	}
	return true;
}

bool Uninstall()
{
	if(DWORD errorMessage = CheckPrerequisites())
	{
		DisplayMessage(errorMessage);
		return false;
	}
	const std::wstring& driverFile = (GetFolderPath(FOLDERID_System) + g_systemDriverAddendum);
	bool ret;
	if(!(ret = UninstallService()))
	{
		DisplayMessage(MSG_CANT_DELETE_SERVICE, g_serviceName, GetLastErrorText().c_str(), driverFile.c_str());
		return false;
	}
	if(ret)
	{
		if(!DeleteFile(driverFile.c_str()))
		{
			DisplayMessage(MSG_CANT_DELETE_FILE, driverFile.c_str(), GetLastErrorText().c_str());
			return false;
		}
	}
	return true;
}
