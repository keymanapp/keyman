#include "stdafx.h"
#include "structs.h"
#include "hooks.h"
#include "sharedinfo.h"
#include "imports.h"
#include "util.h"
#include "hookint.h"
#include <climits>
#include <malloc.h>

void CopyThreadProcessName(PETHREAD pThread, char* pNameBuffer, ULONG bufLen)
{
	// this doesn't need releasing
	PEPROCESS pProc = IoThreadToProcess(pThread);
	strncpy(pNameBuffer, (char*)PsGetProcessImageFileName(pProc), bufLen);
	pNameBuffer[bufLen - 1] = 0;
}

NTSTATUS GetHooksFromChain(HOOK** ppHookArray, ULONG fsHooks, const WCHAR* pWinstaDeskName, HookListHead& listHead, PPAGED_LOOKASIDE_LIST pList)
{
	PAGED_CODE();
	NTSTATUS stat = STATUS_SUCCESS;
	ULONG bitIndex = 0;
	while(_BitScanForward(&bitIndex, fsHooks))
	{
		// turn off the bit we just found
		fsHooks &= (~(1 << bitIndex));
		// do the hustle!
		HOOK* pHookIter = *(ppHookArray + bitIndex);
		while(pHookIter)
		{
			if(HookEntry* pNext = static_cast<HookEntry*>(ExAllocateFromPagedLookasideList(pList)))
			{
				// copy the bits
				InternalHookInfo& hook = pNext->hook;
				hook.hHook = pHookIter->head.h;
				hook.dllAtomIndex = pHookIter->ihmod;
				hook.hookType = pHookIter->iHook;
				hook.flags = pHookIter->flags;
				hook.lastHookHung = pHookIter->fLastHookHung;
				hook.timeout = pHookIter->nTimeout;
				hook.hookFnOffset = pHookIter->offPfn;
				hook.sessionId = PsGetCurrentProcessSessionId();
				wcsncpy(hook.winstaDeskName, pWinstaDeskName, MAX_WINSTA_DESK_NAME_WCHARS - 1);
				hook.winstaDeskName[MAX_WINSTA_DESK_NAME_WCHARS - 1] = 0;
				// the EThread offset is the same either way, 
				// so it's OK to leave this as THREADINFO7 even on Vista
				THREADINFO7* pSettingThread = static_cast<THREADINFO7*>(pHookIter->head.pti);
				if(pSettingThread)
				{
					hook.dwSettingThread = HandleToULong(PsGetThreadId(pSettingThread->pEThread));
					CopyThreadProcessName(pSettingThread->pEThread, hook.settingProcess, ARRAYSIZE(hook.settingProcess));
				}
				else
				{
					hook.dwSettingThread = 0;
					hook.settingProcess[0] = 0;
				}
				THREADINFO7* pHookedThread = pHookIter->ptiHooked;
				if(pHookedThread)
				{
					hook.dwHookedThread = HandleToULong(PsGetThreadId(pHookedThread->pEThread));
					CopyThreadProcessName(pHookedThread->pEThread, hook.hookedProcess, ARRAYSIZE(hook.hookedProcess));
				}
				else
				{
					hook.dwHookedThread = 0;
					hook.hookedProcess[0] = 0;
				}
				PushEntryList(&listHead.firstEntry, &pNext->nextEntry);
				++listHead.count;
				DBG_TEXT(
					"Found hook (%p)with info, hooker %lu, hooked %lu, name atom %i, "
					 "type %i, flags %lu, timeout %lu, lastTimeHung %d, hookOffset %Ix\n",
					 pHookIter, hook.dwSettingThread, hook.dwHookedThread, hook.dllAtomIndex,
					 hook.hookType, hook.flags, hook.timeout, hook.lastHookHung, hook.hookFnOffset
				);
			}
			else
			{
				stat = STATUS_INSUFFICIENT_RESOURCES;
				DBG_TEXT("Failed to allocate from hook lookaside list");
				goto leaveFunction;
			}
			pHookIter = pHookIter->phkNext;
		}
	}
leaveFunction:
	return stat;
}

// disable the warning
// C4509: nonstandard extension used: 'GetSessionHooks' uses SEH and 'lock' has destructor
// the generated code does the right thing
#pragma warning(push)
#pragma warning(disable : 4509)
NTSTATUS GetSessionHooks(HookListHead& listHead, PPAGED_LOOKASIDE_LIST pList)
{
	PAGED_CODE();
	// the actual ERESOURCE at gpresUser is different for each session
	// hence why this is here and not in EnumSystemHooks
	if(!CheckEResourceValidity())
	{
		return STATUS_UNSUCCESSFUL;
	}
	NTSTATUS stat = STATUS_SUCCESS;
	// take Win32k's lock to increase the chances that none of these change while we're iterating
	ExclusiveResourceLock lock(*g_ppResUser);
	__try
	{
		PWINDOWSTATION pWinSta = static_cast<PWINDOWSTATION>(*g_ppWinstaList);
		while(pWinSta)
		{
			DBG_TEXT("Enumerating Winsta %p", pWinSta);
			// get the winsta name
			WCHAR winstaDeskBuffer[MAX_WINSTA_DESK_NAME_WCHARS] = {0};
			UNICODE_STRING winstaDeskName = {0, sizeof(winstaDeskBuffer), winstaDeskBuffer};
			if(!NT_SUCCESS(stat = GetObjectName(pWinSta, &winstaDeskName)))
			{
				DBG_TEXT("Failed to put a name to the window station (error %#08x)", stat);
			}
			USHORT deskStartNameIndex = 0;
			if(winstaDeskName.Length)
			{
				// stick in the seperating \ 
				deskStartNameIndex = winstaDeskName.Length / sizeof(WCHAR);
				winstaDeskBuffer[deskStartNameIndex] = L'\\';
				++deskStartNameIndex;
			}
			// create a unicode string that maps to the space after
			// the windowstation name. When it's filled in we get auto 
			// concatenation of winstaDeskBuffer
			USHORT deskNameBufferSizeMax = sizeof(winstaDeskBuffer) - (deskStartNameIndex * sizeof(WCHAR));
			UNICODE_STRING deskNameString = {0, deskNameBufferSizeMax, winstaDeskBuffer + deskStartNameIndex};
			PDESKTOP pDesk = pWinSta->rpDeskList;
			while(pDesk)
			{
				// clear the string, the next desktop name might be shorter
				memset(deskNameString.Buffer, 0, deskNameString.MaximumLength);
				deskNameString.Length = 0;
				DBG_TEXT("Enumerating Desk %p", pDesk);
				if(!NT_SUCCESS(stat = GetObjectName(pDesk, &deskNameString)))
				{
					DBG_TEXT("Failed to put a name to the desktop (error %#08x)", stat);
				}
				PDESKTOPINFO pDeskInfo = pDesk->pDeskInfo;
				if(pDeskInfo)
				{
					if(!NT_SUCCESS(stat = GetHooksFromChain(pDeskInfo->aphkStart, pDeskInfo->fsHooks, winstaDeskBuffer, listHead, pList)))
					{
						// it's goto, but not as we know it
						__leave;
					}
				}
				pDesk = pDesk->rpDeskNext;
			}
			pWinSta = pWinSta->rpWinstaNext;
		}
	}
	__except(REPORT_EXCEPTION)
	{
		;
	}
	return stat;
}
#pragma warning(pop)

void MoveListEntries(HANDLE hHeap, HookInfo*& pWritePointer, PSINGLE_LIST_ENTRY listHead, PPAGED_LOOKASIDE_LIST pLookaside)
{
	PAGED_CODE();
	PSINGLE_LIST_ENTRY pEntry = NULL;
	while(pEntry = PopEntryList(listHead))
	{
		HookEntry* pHookEntry = CONTAINING_RECORD(pEntry, HookEntry, nextEntry);
		// copy the bulk of the data
		*static_cast<SharedHookInfo*>(pWritePointer) = *static_cast<SharedHookInfo*>(&pHookEntry->hook);
		// then turn the atom index into a dllname
		pWritePointer->pDllName = GetDllName(hHeap, pHookEntry->hook.dllAtomIndex);
		ExFreeToPagedLookasideList(pLookaside, pHookEntry);
		++pWritePointer;
	}
}

// disable the warning
// C4509: nonstandard extension used: 'MarshalToUserBufferAndFree' uses SEH and 'hMemHeap' has destructor
// the generated code does the right thing
#pragma warning(push)
#pragma warning(disable : 4509)
NTSTATUS MarshalToUserBufferAndFree(HookListHead& hooks, ClientHookArray*& pWinHookInf, PPAGED_LOOKASIDE_LIST pLookaside)
{
	PAGED_CODE();
	NTSTATUS stat = STATUS_SUCCESS;
	// first figure out how much memory is required and allocate enough for the structs
	// and that many MAX_PATH dllName strings (522 is Unicode (MAX_PATH + 1))
	// plus a 1/2K for overhead
	ULONG count = hooks.count;
	SIZE_T initialAlloc = (sizeof(*pWinHookInf) + (sizeof(HookInfo) * count));
	SIZE_T totalAlloc = initialAlloc + (522 * count) + 512;
	// create a heap for the module names
	WinType<PVOID, PVOID> hMemHeap(RtlCreateHeap(HEAP_NO_SERIALIZE | HEAP_GROWABLE, NULL, 0, totalAlloc, NULL, NULL), &RtlDestroyHeap);
	if(!*hMemHeap)
	{
		return STATUS_INSUFFICIENT_RESOURCES;
	}
	DBG_TEXT("Created heap of %Iu bytes at %p", totalAlloc, *hMemHeap);
	__try
	{
		ClientHookArray* pTempOut = static_cast<ClientHookArray*>(RtlAllocateHeap(*hMemHeap, 0, initialAlloc));
		if(!pTempOut)
		{
			return STATUS_INSUFFICIENT_RESOURCES;
		}
		// then do all the copying
		pTempOut->numHooks = count;
		pTempOut->hNamesHeap = *hMemHeap;
		HookInfo* pWritePointer = pTempOut->hooks;
		MoveListEntries(*hMemHeap, pWritePointer, &hooks.firstEntry, pLookaside);
		// success, assign the output and release the heap autodestruct
		pWinHookInf = pTempOut;
		hMemHeap.Release();
	}
	__except(REPORT_EXCEPTION)
	{
		pWinHookInf = NULL;
	}
	return stat;
}
#pragma warning(pop)

void FreeList(PPAGED_LOOKASIDE_LIST pList, HookListHead& listHead)
{
	PAGED_CODE();
	PSINGLE_LIST_ENTRY pEntry = NULL;
	while(pEntry = PopEntryList(&listHead.firstEntry))
	{
		HookEntry* pHookEntry = CONTAINING_RECORD(pEntry, HookEntry, nextEntry);
		ExFreeToPagedLookasideList(pList, pHookEntry);
	}
	listHead.count = 0;
}

WCHAR* GetDllName(PVOID pHeap, int atomIndex)
{
	PAGED_CODE();
	// negative indexes are seen for LowLevel hooks
	// where there isn't an injected dll
	if(atomIndex < 0) return NULL;
	ULONG maxPathBytes = ((g_dllMaxPath + 1) * sizeof(WCHAR));
	WCHAR* pNameBuf = NULL;
	ATOM actualAtom = g_pAtomArray[atomIndex];
	// there's no method to get the length of the returned string
	// cap attempts at determining the size or we could be here all day
	for(int i = 1; i < 3; ++i)
	{
		ULONG toAllocate = maxPathBytes * i;
		pNameBuf = static_cast<WCHAR*>(RtlAllocateHeap(pHeap, HEAP_ZERO_MEMORY, toAllocate));
		if(pNameBuf)
		{
			ULONG charsNeeded = g_pUserGetAtomName(actualAtom, pNameBuf, toAllocate / sizeof(WCHAR));
			if(charsNeeded != 0)
			{
				DBG_TEXT("Translated atom %hu to %S", actualAtom, pNameBuf);
				// this is just a normal atom that's being stringified
				// ignore it
				if(pNameBuf[0] == L'#')
				{
					RtlFreeHeap(pHeap, 0, pNameBuf);
					pNameBuf = NULL;
				}
				break;
			}
			else
			{
				// not big enough or some other failure, try again
				RtlFreeHeap(pHeap, 0, pNameBuf);
				pNameBuf = NULL;
			}
		}
		else
		{
			DBG_TEXT("Failed to allocate %lu bytes for atom %hu", toAllocate, actualAtom);
			break;
		}
	}
	return pNameBuf;
}

void GetWinstaDeskName(PDESKTOP pDesk, WCHAR (&winstaDeskName)[MAX_WINSTA_DESK_NAME_WCHARS])
{
	PAGED_CODE();
	// constructing a Windowstation\desktop string without additional allocs
	PWINDOWSTATION pWinSta = pDesk->rpwinstaParent;
	UNICODE_STRING winstaName = {0, sizeof(winstaDeskName), winstaDeskName};
	USHORT deskNameStartIndex = 0;
	USHORT deskBufferSize = sizeof(winstaDeskName);
	NTSTATUS stat;
	if(NT_SUCCESS(stat = GetObjectName(pWinSta, &winstaName)))
	{
		if((deskNameStartIndex = winstaName.Length / sizeof(WCHAR)))
		{
			winstaDeskName[deskNameStartIndex] = L'\\';
			++deskNameStartIndex;
			deskBufferSize -= (deskNameStartIndex * sizeof(WCHAR)); 
		}
	}
	UNICODE_STRING deskNameString = {0, deskBufferSize, &winstaDeskName[deskNameStartIndex]};
	GetObjectName(pDesk, &deskNameString);
}

#pragma code_seg("PAGE")

template<class ThreadInfoType>
NTSTATUS GetWin32ThreadHooks(PVOID pW32Thread, HookListHead& hookList, PPAGED_LOOKASIDE_LIST pList)
{
	PAGED_CODE();
	ThreadInfoType* pThreadInf = static_cast<ThreadInfoType*>(pW32Thread);
	// we get the name of the desktop here rather than at a wider scope
	// because threads in the same process can have different desktops
	// (SetThreadDesktop)
	WCHAR winstaDeskName[MAX_WINSTA_DESK_NAME_WCHARS] = {0};
	if(pThreadInf->pDesktop)
	{
		GetWinstaDeskName(pThreadInf->pDesktop, winstaDeskName);
	}
	return GetHooksFromChain(pThreadInf->aphkStart, pThreadInf->fsHooks, winstaDeskName, hookList, pList);
}

#pragma code_seg()

NTSTATUS GetProcessHooks(
	const SYSTEM_PROCESS_INFORMATION& process,
	pfnGetThreadHooks getWin32ThreadHooks,
	HookListHead& listHead,
	PPAGED_LOOKASIDE_LIST pList
)
{
	PAGED_CODE();
	NTSTATUS stat = STATUS_SUCCESS;

	const SYSTEM_THREAD_INFORMATION* pThreadInfo = process.Threads, *pEnd = pThreadInfo + process.ThreadCount;
	while(pThreadInfo != pEnd)
	{
		PETHREAD pThread = NULL;
		if(NT_SUCCESS(PsLookupThreadByThreadId(pThreadInfo->ClientId.UniqueThread, &pThread)))
		{
			__try
			{
				PVOID pWin32Thread = PsGetThreadWin32Thread(pThread);
				if(pWin32Thread)
				{
					stat = getWin32ThreadHooks(pWin32Thread, listHead, pList);
				}
				else
				{
					DBG_TEXT("Thread %p doesn't have a Win32Thread!", pThread);
				}
			}
			__finally
			{
				ObDereferenceObject(pThread);
			}
		}
		else
		{
			DBG_TEXT("Failed to lookup thread %lu.%lu", HandleToUlong(pThreadInfo->ClientId.UniqueProcess), HandleToULong(pThreadInfo->ClientId.UniqueThread));
		}
		++pThreadInfo;
	}
	return stat;
}

BOOLEAN AttachToProcessId(HANDLE processId, BOOLEAN attached, KAPC_STATE& attachedState)
{
	if(attached)
	{
		KeUnstackDetachProcess(&attachedState);
		attached = FALSE;
	}
	PEPROCESS pProcToAttach = NULL;
	NTSTATUS stat = STATUS_SUCCESS;
	if(!NT_SUCCESS(stat = PsLookupProcessByProcessId(processId, &pProcToAttach)))
	{
		DBG_TEXT("Couldn't open process %lu, error %#08x", HandleToULong(processId), stat);
		return FALSE;
	}
	// if the process doesn't have a win32 process component. 
	// it isn't really likely to have any win32 threads
	PVOID pWin32Proc = PsGetProcessWin32Process(pProcToAttach);
	if(pWin32Proc)
	{
		KeStackAttachProcess(pProcToAttach, &attachedState);
		attached = TRUE;
	}
	else
	{
		DBG_TEXT("%lu doesn't have a Win32 Process", HandleToULong(processId));
	}	
	ObDereferenceObject(pProcToAttach);	
	return attached;
}

// disable the warning
// C4509: nonstandard extension used: 'EnumSystemHooks' uses SEH and 'processInfo' has destructor
// the generated code does the right thing
#pragma warning(push)
#pragma warning(disable : 4509)
VOID EnumSystemHooks(PDEVICE_OBJECT /*pDev*/, PVOID pCtx)
{
	PAGED_CODE();
	WorkItemContext* pWorkCtx = static_cast<WorkItemContext*>(pCtx);
	NTSTATUS& stat = pWorkCtx->stat;
	ScopedArray<UCHAR> processInfo(NULL);

	// Good job Windows isn't Chrome and we can rely on the version being the same as long as we're running
	const pfnGetThreadHooks hookRetrievalFns[] = {
		&GetWin32ThreadHooks<THREADINFO6>,
		&GetWin32ThreadHooks<THREADINFO7>
	};
	pfnGetThreadHooks threadHookGetterFn = hookRetrievalFns[OSVER(g_dwOSVersion) != NTDDI_VISTA];

	// whether we're attached, and who to
	BOOLEAN attached = FALSE;
	KAPC_STATE attachedProcState = {0};

	// bitfield we'll use to keep track of which sessions we've got the global hooks from
	ULONG sessionBits[4] = {0};
	const ULONG sessionsAllowed = (CHAR_BIT * sizeof(sessionBits));
	const ULONG sessionsInOneUlong = (CHAR_BIT * sizeof(*sessionBits));

	__try
	{
		// the initial process we start in doesn't have a session
		ULONG currentlyAttachedSession = MAXULONG;
		if(NT_SUCCESS(stat = GetRunningProcesses(&processInfo)))
		{
			// we don't want the first entry (the idle process)
			UCHAR* pIter = *processInfo;
			pIter += reinterpret_cast<SYSTEM_PROCESS_INFORMATION*>(pIter)->NextEntryDelta;
			// but we do the rest
			for(SYSTEM_PROCESS_INFORMATION* pProcInfo = reinterpret_cast<SYSTEM_PROCESS_INFORMATION*>(pIter);
				NT_SUCCESS(stat) && pProcInfo->NextEntryDelta;
				pIter += pProcInfo->NextEntryDelta, pProcInfo = reinterpret_cast<SYSTEM_PROCESS_INFORMATION*>(pIter)
			)
			{
				// change session if we have to
				// Win32k stuff is in session space which is easiest read by being a part of that session
				ULONG procSession = pProcInfo->SessionId;
				if(procSession != currentlyAttachedSession)
				{
					if((attached = AttachToProcessId(pProcInfo->ProcessId, attached, attachedProcState)))
					{
						currentlyAttachedSession = procSession;
					}
					else
					{
						currentlyAttachedSession = MAXULONG;
						continue;
					}
				}
				// check it's within recordable range
				// the limit is currently 128 sessions which should be enough 
				// for everthing but the most terminal of servers
				ULONG whichSessionUlong = procSession / sessionsInOneUlong;
				ULONG whichSessionInUlong = procSession % sessionsInOneUlong;
				if(procSession < sessionsAllowed)
				{
					if(!BIT_TEST(sessionBits[whichSessionUlong], whichSessionInUlong))
					{
						if(NT_SUCCESS(stat = GetSessionHooks(pWorkCtx->hookList, pWorkCtx->pLookaside)))
						{
							BIT_SET(sessionBits[whichSessionUlong], whichSessionInUlong);
						}
					}
				}
				stat = GetProcessHooks(*pProcInfo, threadHookGetterFn, pWorkCtx->hookList, pWorkCtx->pLookaside);
			}
		}
	}
	__except(REPORT_EXCEPTION)
	{
	}
	if(attached)
	{
		KeUnstackDetachProcess(&attachedProcState);
	}
	KeSetEvent(pWorkCtx->pFinishedEvent, 0, FALSE);
}
#pragma warning(pop)

NTSTATUS GetWindowsHookInfo(PIRP pIrp, ULONG_PTR* pulpBytesWritten)
{
	PAGED_CODE();
	*pulpBytesWritten = 0;
	PIO_STACK_LOCATION pStack = IoGetCurrentIrpStackLocation(pIrp);
	const ULONG dwOutLen = pStack->Parameters.DeviceIoControl.OutputBufferLength;
	WindowsHookInfo* pOutBuffer = static_cast<WindowsHookInfo*>(pIrp->AssociatedIrp.SystemBuffer);

	NTSTATUS stat = STATUS_SUCCESS;
	// validate the 'out' buffer
	if(dwOutLen < sizeof(*pOutBuffer))
	{
		DBG_TEXT("Output buffer is too small (%lu vs %lu required)", dwOutLen, sizeof(*pOutBuffer));
		return STATUS_INFO_LENGTH_MISMATCH;
	}
	// and the kernel buffer for the hook structures
	PAGED_LOOKASIDE_LIST hookStructLookaside;
	ExInitializePagedLookasideList(&hookStructLookaside, NULL, NULL, 0, sizeof(HookEntry), g_heapTag, 0);

	__try
	{
		// lots o' initializing
		KEVENT finishedEvent;
		KeInitializeEvent(&finishedEvent, NotificationEvent, FALSE);
		// current values of IoSizeOfWorkItem is 0x20 bytes (x86)
		// I think we can manage that on a 12k stack
		PIO_WORKITEM pWorkItem = static_cast<PIO_WORKITEM>(_alloca(IoSizeofWorkItem()));
		IoInitializeWorkItem(pStack->DeviceObject, pWorkItem);

		WorkItemContext workCtx(&finishedEvent, &hookStructLookaside);
		// send it on its way and wait
		IoQueueWorkItem(pWorkItem, &EnumSystemHooks, DelayedWorkQueue, &workCtx);
		KeWaitForSingleObject(workCtx.pFinishedEvent, Executive, KernelMode, FALSE, NULL);
		// free the item
		IoUninitializeWorkItem(pWorkItem);
		// and copy the hook info to user mode, assuming things went peachy
		if(NT_SUCCESS(stat = workCtx.stat))
		{
			// and copy the kernel data into a user mode buffer
			ClientHookArray* pReturnedHookInfo = NULL;
			if(NT_SUCCESS(stat = MarshalToUserBufferAndFree(workCtx.hookList, pReturnedHookInfo, &hookStructLookaside)))
			{
				pOutBuffer->pHookInfoArray = pReturnedHookInfo;
				*pulpBytesWritten = sizeof(*pOutBuffer);
			}
		}
		FreeList(&hookStructLookaside, workCtx.hookList);
	}
	__except(REPORT_EXCEPTION)
	{
	}
	// kill things we no longer need
	ExDeletePagedLookasideList(&hookStructLookaside);
	return stat;
}
