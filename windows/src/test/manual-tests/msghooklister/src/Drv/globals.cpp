#include "stdafx.h"
#include "sharedinfo.h"

#pragma code_seg("PAGE")

// names of the links are the same as those from the original version
UNICODE_STRING g_usSymName = RTL_CONSTANT_STRING(L"\\DosDevices\\" g_serviceName);

// current os version in same format as the NTDDI values
ULONG g_dwOSVersion = 0;
// win32k/gdi resource
PERESOURCE* g_ppResUser = NULL;
// address of validatehwnd
pfnValidateHwnd g_pValidateHwnd = NULL;
// address of UserGetAtomName
pfnUserGetAtomName g_pUserGetAtomName = NULL;
// the array of atoms that return hook lib names
// when passed to UserGetAtomName
ATOM* g_pAtomArray = NULL;
// list of session WindowStations
PVOID* g_ppWinstaList;

void* __cdecl operator new(size_t size)
{
	PAGED_CODE();
	return ExAllocatePoolWithTag(PagedPool, size, g_heapTag);
}

void __cdecl operator delete(PVOID pData)
{
	PAGED_CODE();
	ExFreePoolWithTag(pData, g_heapTag);
}

void* __cdecl operator new[](size_t size)
{
	PAGED_CODE();
	return operator new(size);
}

void __cdecl operator delete[](PVOID pData)
{
	PAGED_CODE();
	return operator delete(pData);
}

static char* ConvertUnicodeToMultiByte(PCWSTR wszText, ULONG dwTextByteLength, PULONG pdwReturnLength)
{
	PAGED_CODE();
	if(!dwTextByteLength)
	{
		// including the NULL in the size will null terminate the
		// resultant multi-byte string
		dwTextByteLength = static_cast<ULONG>((wcslen(wszText) + 1) * sizeof(WCHAR));
	}
	ULONG dwMultiLen = 0;
	char* szMultiText = NULL;
	// doesn't return an error
	RtlUnicodeToMultiByteSize(&dwMultiLen, wszText, dwTextByteLength);
	if(dwMultiLen != 0)
	{
		szMultiText = new char[dwMultiLen];
		if(szMultiText)
		{
			// also doesn't set an error return code
			RtlUnicodeToMultiByteN(szMultiText, dwMultiLen, &dwMultiLen, wszText, dwTextByteLength);
			if(dwMultiLen)
			{
				if(pdwReturnLength)
				{
					*pdwReturnLength = dwMultiLen;
				}
			}
			else
			{
				delete [] szMultiText;
				szMultiText = NULL;
			}
		}
	}
	return szMultiText;
}

void PrintUnicode(PCWSTR wszText, ULONG dwTextByteLength)
{
	PAGED_CODE();
	ScopedArray<char> saszMultiText(ConvertUnicodeToMultiByte(wszText, dwTextByteLength, NULL));
	if(*saszMultiText)
	{
		DbgPrintEx(DPFLTR_IHVDRIVER_ID, DPFLTR_ERROR_LEVEL, "%s\n", *saszMultiText);
	}
	else
	{
		DbgPrintEx(DPFLTR_IHVDRIVER_ID, DPFLTR_ERROR_LEVEL, "MsgLister: Failed to convert unicode text to multibyte\n");
	}
}

void PrintUnicode(PCUNICODE_STRING pusText)
{
	PAGED_CODE();
	PrintUnicode(pusText->Buffer, pusText->MaximumLength);
}

#pragma code_seg()
