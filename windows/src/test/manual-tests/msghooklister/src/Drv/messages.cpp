#include "stdafx.h"
#include "messages.h"
#include "sharedinfo.h"
#include "structs.h"
#include "init.h"
#include "imports.h"
#include "util.h"

// disable the warning
// C4509: nonstandard extension used: 'GetWindowMessageQueue' uses SEH and 'userResLock' has destructor
// the generated code does the right thing
#pragma warning(push)
#pragma warning(disable : 4509)
NTSTATUS GetWindowMessageQueue(PIRP pIrp, ULONG_PTR* pulpBytesWritten)
{
	PAGED_CODE();
	*pulpBytesWritten = 0;
	PIO_STACK_LOCATION pStack = IoGetCurrentIrpStackLocation(pIrp);
	const ULONG dwInLen = pStack->Parameters.DeviceIoControl.InputBufferLength;
	const WndInfo* pWndInf = static_cast<WndInfo*>(pIrp->AssociatedIrp.SystemBuffer);
	const ULONG dwOutLen = pStack->Parameters.DeviceIoControl.OutputBufferLength;
	WndThreadInfo* pOutBuffer = static_cast<WndThreadInfo*>(pIrp->AssociatedIrp.SystemBuffer);
	NTSTATUS stat = STATUS_SUCCESS;

	// first validate the 'in' parameters
	// and filter out 'special' HWND values
	if((dwInLen < sizeof(*pWndInf)) ||
	   (IsSpecialHWND(pWndInf->hwnd))
	)
	{
		DBG_TEXT("input buffer is too small or an invalid HWND was specified");
		return STATUS_INVALID_PARAMETER_1;
	}
	// next the 'out' parameter
	if(dwOutLen < sizeof(*pOutBuffer))
	{
		DBG_TEXT("Output buffer is too small (%lu vs %lu required)", dwOutLen, sizeof(*pOutBuffer));
		return STATUS_INFO_LENGTH_MISMATCH;
	}
	HWND hwnd = pWndInf->hwnd;
	pOutBuffer->pTMI = NULL;

	if(!CheckEResourceValidity())
	{
		return STATUS_UNSUCCESSFUL;
	}
	// acquire the user/gdi resource
	ExclusiveResourceLock userResLock(*g_ppResUser);
	__try
	{
		// translate the hwnd to a pwnd
		PWND pWnd = g_pValidateHwnd(hwnd);
		if(!pWnd)
		{
			DBG_TEXT("ValidateWnd on 0x%p returned NULL", hwnd);
			return STATUS_INVALID_PARAMETER_2;
		}
		// check the sessions are the same
		PEPROCESS pWndProcess = IoThreadToProcess(*reinterpret_cast<PETHREAD*>(pWnd->head.pti));
		ULONG wndSession = PsGetProcessSessionIdEx(pWndProcess);
		ULONG currentSession = 0;
		if((!NT_SUCCESS(stat = IoGetRequestorSessionId(pIrp, &currentSession))) || (wndSession != currentSession))
		{
			DBG_TEXT("IoGetRequestorSessionId failed (0x%x) or pWnd 0x%p is from session %lu but requesting thread is in session %lu", stat, pWnd, wndSession, currentSession);
			return STATUS_INVALID_PARAMETER_3;
		}
		if(pWnd->head.pti)
		{
			// currently, this is enough
			switch(g_dwOSVersion & 0xFFFF0000)
			{
				case NTDDI_VISTA:
				{
					stat = ReadMessages<THREADINFO6>(pWnd, pOutBuffer);
				}
				break;
				case NTDDI_WIN7:
				{
					stat = ReadMessages<THREADINFO7>(pWnd, pOutBuffer);
				}
				break;
			}
		}
	}
	__except(REPORT_EXCEPTION)
	{
		DBG_TEXT("Caught exception 0x%x", stat);
		if(pOutBuffer->pTMI)
		{
			SIZE_T regionSize = 0;
			ZwFreeVirtualMemory(ZwCurrentProcess(), reinterpret_cast<PVOID*>(&pOutBuffer->pTMI), &regionSize, MEM_RELEASE);
		}
	}
	if(NT_SUCCESS(stat))
	{
		*pulpBytesWritten = sizeof(*pOutBuffer);
	}
	return stat;
}
#pragma warning(pop)

BOOLEAN IsSpecialHWND(HWND hwnd)
{
	PAGED_CODE();
	return hwnd == HWND_TOPMOST ||
		   hwnd == HWND_TOP ||
		   hwnd == HWND_BOTTOM ||
		   hwnd == HWND_BROADCAST ||
		   hwnd == HWND_NOTOPMOST ||
		   hwnd == HWND_MESSAGE;
}

ULONG CountSentMessages(PSMSMSG psmsMsg)
{
	PAGED_CODE();
	ULONG count = 0;
	while(psmsMsg)
	{
		++count;
		psmsMsg = psmsMsg->pSmsReceiveNext;
	}
	return count;
}