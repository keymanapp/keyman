#include "stdafx.h"
#include "init.h"
#include "sharedinfo.h"
#include <aux_klib.h>
#include "util.h"

NTSTATUS InitializeEnvironment(PUNICODE_STRING pusRegPath)
{
	ASSERT(pusRegPath && "Invalid registry path");
	PAGED_CODE();
	// figure out how much buffer we need
	UNICODE_STRING usParamString = RTL_CONSTANT_STRING(g_parameterSubKey);
	USHORT wBytesRequired = (pusRegPath->Length + usParamString.Length) + sizeof(WCHAR);
	ScopedArray<WCHAR> wszParamKeyPathBuffer(new WCHAR[wBytesRequired / sizeof(WCHAR)]);
	if(!*wszParamKeyPathBuffer)
	{
		DBG_TEXT("No memory for registry key buffer");
		return STATUS_INSUFFICIENT_RESOURCES;
	}
	else
	{
		// do the concat
		UNICODE_STRING usParamKeyPath = {0, wBytesRequired, *wszParamKeyPathBuffer};
		RtlAppendUnicodeStringToString(&usParamKeyPath, pusRegPath);
		RtlAppendUnicodeStringToString(&usParamKeyPath, &usParamString);

		// null terminate
		(*wszParamKeyPathBuffer)[(wBytesRequired / sizeof(WCHAR)) - 1] = 0;
		DBG_TEXT("Full path to registry key is %wZ", &usParamKeyPath);

		// get the offsets from the registry
		RegData regData = {0};
		NTSTATUS stat = QueryRegistryParams(usParamKeyPath, regData);
		if(!NT_SUCCESS(stat))
		{
			return stat;
		}
		// then the base address of win32k.sys
		STRING sWin32k = RTL_CONSTANT_STRING("win32k.sys");
		ULONG moduleSize = 0;
		PUCHAR pUserAddress = static_cast<PUCHAR>(GetModuleAddress(sWin32k, moduleSize));
		if(!pUserAddress)
		{
			return STATUS_UNSUCCESSFUL;
		}
		// check all the offsets fall within win32k's address range
		if(
			regData.gpresUserOffset >= moduleSize ||
			regData.validateHwndOffset >= moduleSize ||
			regData.userGetAtomNameOffset >= moduleSize ||
			regData.atomArrayOffset >= moduleSize ||
			regData.grpWinstaListOffset >= moduleSize
		)
		{
			return STATUS_UNSUCCESSFUL;
		}
		// finally, just resolve the offsets
		g_ppResUser = reinterpret_cast<PERESOURCE*>(pUserAddress + regData.gpresUserOffset);
		g_pValidateHwnd = reinterpret_cast<pfnValidateHwnd>(pUserAddress + regData.validateHwndOffset);
		g_pUserGetAtomName = reinterpret_cast<pfnUserGetAtomName>(pUserAddress + regData.userGetAtomNameOffset);
		g_pAtomArray = reinterpret_cast<ATOM*>(pUserAddress + regData.atomArrayOffset);
		g_ppWinstaList = reinterpret_cast<PVOID*>(pUserAddress + regData.grpWinstaListOffset);
		return STATUS_SUCCESS;
	}
}

NTSTATUS QueryRegistryParams(const UNICODE_STRING& usRegPath, RegData& regData)
{
	ASSERT(usRegPath.Buffer && "Invalid string buffer");
	ASSERT((usRegPath.Buffer[(usRegPath.Length / sizeof(WCHAR))] == WCHAR(0)) && "Registry key buffer not null terminated");
	PAGED_CODE();

	static const ULONG dwNumberOfRegistryParameters = 5;
	RTL_QUERY_REGISTRY_TABLE rqrtQueryTable[dwNumberOfRegistryParameters + 1] = {{0}};

	// make sure the reg_dword specifications are obeyed
	C_ASSERT(sizeof(regData.validateHwndOffset) == sizeof(ULONG));
	C_ASSERT(sizeof(regData.gpresUserOffset) == sizeof(ULONG));
	C_ASSERT(sizeof(regData.userGetAtomNameOffset) == sizeof(ULONG));
	C_ASSERT(sizeof(regData.atomArrayOffset) == sizeof(ULONG));
	// ValidateHWNDOffset	
	rqrtQueryTable[0].Flags = RTL_QUERY_REGISTRY_DIRECT | RTL_QUERY_REGISTRY_REQUIRED;
	rqrtQueryTable[0].Name = const_cast<PWSTR>(g_validateHwndParameterName);
	rqrtQueryTable[0].DefaultLength = sizeof(regData.validateHwndOffset);
	rqrtQueryTable[0].DefaultType = REG_DWORD;
	rqrtQueryTable[0].EntryContext = &regData.validateHwndOffset;

	// gpresUserOffset
	rqrtQueryTable[1].Flags = RTL_QUERY_REGISTRY_DIRECT | RTL_QUERY_REGISTRY_REQUIRED;
	rqrtQueryTable[1].Name = const_cast<PWSTR>(g_gpresUserParameterName);
	rqrtQueryTable[1].DefaultLength = sizeof(regData.gpresUserOffset);
	rqrtQueryTable[1].DefaultType = REG_DWORD;
	rqrtQueryTable[1].EntryContext = &regData.gpresUserOffset;

	// userGetAtomNameOffset
	rqrtQueryTable[2].Flags = RTL_QUERY_REGISTRY_DIRECT | RTL_QUERY_REGISTRY_REQUIRED;
	rqrtQueryTable[2].Name = const_cast<PWSTR>(g_userGetAtomNameParamName);
	rqrtQueryTable[2].DefaultLength = sizeof(regData.userGetAtomNameOffset);
	rqrtQueryTable[2].DefaultType = REG_DWORD;
	rqrtQueryTable[2].EntryContext = &regData.userGetAtomNameOffset;

	// atomArrayOffset
	rqrtQueryTable[3].Flags = RTL_QUERY_REGISTRY_DIRECT | RTL_QUERY_REGISTRY_REQUIRED;
	rqrtQueryTable[3].Name = const_cast<PWSTR>(g_atomArrayParamName);
	rqrtQueryTable[3].DefaultLength = sizeof(regData.atomArrayOffset);
	rqrtQueryTable[3].DefaultType = REG_DWORD;
	rqrtQueryTable[3].EntryContext = &regData.atomArrayOffset;

	// atomArrayOffset
	rqrtQueryTable[4].Flags = RTL_QUERY_REGISTRY_DIRECT | RTL_QUERY_REGISTRY_REQUIRED;
	rqrtQueryTable[4].Name = const_cast<PWSTR>(g_grpWinStaListParamName);
	rqrtQueryTable[4].DefaultLength = sizeof(regData.grpWinstaListOffset);
	rqrtQueryTable[4].DefaultType = REG_DWORD;
	rqrtQueryTable[4].EntryContext = &regData.grpWinstaListOffset;
	return RtlQueryRegistryValues(RTL_REGISTRY_ABSOLUTE, usRegPath.Buffer, rqrtQueryTable, NULL, NULL);
}

PVOID GetModuleAddress(const STRING& sModuleToFile, ULONG& modSize)
{
	NTSTATUS stat;
	PAGED_CODE();
	if(!NT_SUCCESS(stat = AuxKlibInitialize()))
	{
		DBG_TEXT("AuxKlibInitialize failed with error 0x%x", stat);
		return NULL;
	}
	ULONG bufSize = 0;
	stat = AuxKlibQueryModuleInformation(&bufSize, sizeof(AUX_MODULE_EXTENDED_INFO), NULL);
	if(!bufSize)
	{
		DBG_TEXT("AuxKlibQueryModuleInformation failed with error 0x%x", stat);
		return NULL;
	}
	ScopedArray<UCHAR> saModules(new UCHAR[bufSize]);
	if(!*saModules)
	{
		DBG_TEXT("Couldn't allocate %lu bytes for module list", bufSize);
		return NULL;
	}
	if(!NT_SUCCESS(stat = AuxKlibQueryModuleInformation(&bufSize, sizeof(AUX_MODULE_EXTENDED_INFO), *saModules)))
	{
		DBG_TEXT("AuxKlibQueryModuleInformation2 failed with error 0x%x", stat);
		return NULL;
	}
	// see how many modules were returned and if any of them match what we're after
	const ULONG numModules = bufSize / sizeof(AUX_MODULE_EXTENDED_INFO);
	AUX_MODULE_EXTENDED_INFO* pModules = reinterpret_cast<AUX_MODULE_EXTENDED_INFO*>(*saModules);
	for(ULONG i = 0; i < numModules; ++i)
	{
		UCHAR* szStartOfModString = pModules[i].FullPathName;
		UCHAR* szFileName = szStartOfModString + pModules[i].FileNameOffset;
		STRING sModName;
		RtlInitString(&sModName, reinterpret_cast<PCSZ>(szFileName));
		DBG_TEXT("Found module: %Z", &sModName);
		if(RtlEqualString(&sModName, &sModuleToFile, TRUE))
		{
			modSize = pModules[i].ImageSize;
			return pModules[i].BasicInfo.ImageBase;
		}
	}
	return NULL;
}
