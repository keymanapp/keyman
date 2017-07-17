#include "stdafx.h"
#include "util.h"
#include "imports.h"
#include <cwchar>

NTSTATUS GetRunningProcesses(UCHAR** ppProcessBuffer)
{
	ASSERT(ppProcessBuffer && "Invalid output buffer");
	PAGED_CODE();
	NTSTATUS stat = STATUS_SUCCESS;
	ULONG dwInSize = 32000, dwOutSize = dwInSize;
	UCHAR* pBuffer = new UCHAR[dwInSize];
	// loop around until we get a big enough buffer, 
	// we succeed, or fail with a non-length related error
	while(pBuffer && (stat = ZwQuerySystemInformation(SystemProcessInformation, pBuffer, dwInSize, &dwOutSize)) == STATUS_INFO_LENGTH_MISMATCH)
	{
		dwInSize = dwOutSize;
		delete [] pBuffer;
		pBuffer = new UCHAR[dwInSize];
	}
	if(!NT_SUCCESS(stat))
	{
		if(pBuffer)
		{
			delete [] pBuffer;
		}
	}
	else
	{
		*ppProcessBuffer = pBuffer;
	}
	return stat;
}

NTSTATUS ReportException(PEXCEPTION_POINTERS pExp)
{
	PVOID pAddress = pExp->ExceptionRecord->ExceptionAddress;
	NTSTATUS code = pExp->ExceptionRecord->ExceptionCode;
	ERROR_TEXT("Caught exception of type %#08x from 0x%p", code, pAddress);
	return code;
}

//
// The code below is left over from MS's Desktop Heap Monitor that I ported to Vista/7
// If MS's original code did this, it may be needed.
// Since we use the same resource, I've left it in
//
// disable the warning
// C4509: nonstandard extension used: 'CheckPtrPtrValidity' uses SEH and 'mdlWrap' has destructor
// the generated code does the right thing
#pragma warning(push)
#pragma warning(disable : 4509)
BOOLEAN CheckPtrPtrValidity(PVOID* ppAddress)
{
	PAGED_CODE();
	ASSERT(ppAddress && "Invalid pointer passed");
	BOOLEAN bIsValid = FALSE;
	PMDL pMdl = IoAllocateMdl(ppAddress, sizeof(PVOID), FALSE, FALSE, NULL);
	if(pMdl)
	{
		WinType<PMDL, VOID> mdlWrap(pMdl, &IoFreeMdl);
		__try
		{
			MmProbeAndLockPages(pMdl, KernelMode, IoReadAccess);
			// if we're here a first dereference was successful
			DBG_TEXT("First dereference, %p can be referenced", ppAddress);
			MmUnlockPages(pMdl);
			MmPrepareMdlForReuse(pMdl);
			MmInitializeMdl(pMdl, *ppAddress, sizeof(PVOID));
			MmProbeAndLockPages(pMdl, KernelMode, IoReadAccess);
			// and a second dereference
			MmUnlockPages(pMdl);
			DBG_TEXT("Second dereference, %p can be referenced", *ppAddress);
			bIsValid = TRUE;
		}
		__except(REPORT_EXCEPTION_VOID)
		{
		}
	}
	else
	{
		DBG_TEXT("No memory for MDL");
	}
	return bIsValid;
}
#pragma warning(pop)

BOOLEAN CheckEResourceValidity()
{
	PAGED_CODE();
	BOOLEAN bIsEResourceValid = FALSE;
	if(CheckPtrPtrValidity(reinterpret_cast<PVOID*>(g_ppResUser)))
	{
		PERESOURCE pEResource = *g_ppResUser;

		// Check whether the EResource is correctly
		// inserted into the list of system resources
		// (quick check that should weed out if it's truly a valid eresource)
		PLIST_ENTRY pNextResource = pEResource->SystemResourcesList.Flink;
		PLIST_ENTRY pSecondResource = pNextResource->Flink;
		if(pSecondResource->Blink == pNextResource)
		{
			PLIST_ENTRY pBeforeNextResource = pNextResource->Blink;
			bIsEResourceValid = (pBeforeNextResource->Flink == pNextResource);
		}
	}
	return bIsEResourceValid;
}

NTSTATUS GetObjectName(PVOID pObject, PUNICODE_STRING pusName)
{
	PAGED_CODE();
	NTSTATUS stat;
	UCHAR oniBuffer[1024];
	ULONG dwRealLenRequired;
	POBJECT_NAME_INFORMATION poni = reinterpret_cast<POBJECT_NAME_INFORMATION>(oniBuffer);
	if(NT_SUCCESS(stat = ObQueryNameString(pObject, poni, sizeof(oniBuffer), &dwRealLenRequired)))
	{
		// trim off everything but the actual object name
		// as that is what dheapmon is set to output
		// (the full directory path of is too long to display in its nicely layed out table)
		USHORT wBufferCharLength = poni->Name.Length / sizeof(WCHAR);
		WCHAR* pLastSlashChar = poni->Name.Buffer;
		WCHAR* pNextSlashChar = pLastSlashChar;
		// find the last slash
		// not using wcsrchr since the name string isn't necessarily null terminated
		while((pNextSlashChar = wmemchr(pNextSlashChar, L'\\', wBufferCharLength)) != NULL)
		{
			++pNextSlashChar;
			wBufferCharLength -= static_cast<USHORT>(pNextSlashChar - pLastSlashChar);
			pLastSlashChar = pNextSlashChar;
		}
		USHORT wBytesLeftInName = wBufferCharLength * sizeof(WCHAR);
		UNICODE_STRING usNameStringFromLastSlash = {wBytesLeftInName, wBytesLeftInName, pLastSlashChar};
		RtlCopyUnicodeString(pusName, &usNameStringFromLastSlash);
	}
	return stat;
}
