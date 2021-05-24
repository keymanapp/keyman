#ifndef MSGLISTER_HOOKINTERNAL_H
#define MSGLISTER_HOOKINTERNAL_H

#pragma once

// internal stuff

static const ULONG g_dllMaxPath = 260;

struct InternalHookInfo : SharedHookInfo
{
	int dllAtomIndex; // Should be passed to UserGetAtomName(int atom, WCHAR* pBuffer, int arrayCharSize) if exist
};

struct HookEntry
{
	SINGLE_LIST_ENTRY nextEntry;
	InternalHookInfo hook;
};

struct HookListHead
{
	SINGLE_LIST_ENTRY firstEntry;
	ULONG count;
};

#pragma code_seg("PAGE")

struct WorkItemContext
{
	HookListHead hookList;
	PKEVENT pFinishedEvent;
	PPAGED_LOOKASIDE_LIST pLookaside;
	NTSTATUS stat;

	WorkItemContext(PKEVENT pFinishedEvent, PPAGED_LOOKASIDE_LIST pLookaside)
		: hookList(),
		pFinishedEvent(pFinishedEvent),
		pLookaside(pLookaside),
		stat(STATUS_SUCCESS)
	{}
};

#pragma code_seg()

typedef NTSTATUS (*pfnGetThreadHooks)(PVOID, HookListHead&, PPAGED_LOOKASIDE_LIST);

#pragma intrinsic(_bittest)
#pragma intrinsic(_bittestandset)
#pragma intrinsic(_bittestandreset)
#pragma intrinsic(_bittestandcomplement)

#define BIT_TEST(value, index) (_bittest(reinterpret_cast<LONG*>(&value), static_cast<LONG>(index)))
#define BIT_SET(value, index) (_bittestandset(reinterpret_cast<LONG*>(&value), static_cast<LONG>(index)))
#define BIT_TEST_AND_RESET(value, index) (_bittestandreset(reinterpret_cast<LONG*>(&value), static_cast<LONG>(index)))
#define BIT_FLIP(value, index) (_bittestandcomplement(reinterpret_cast<LONG*>(&value), static_cast<LONG>(index)))

#pragma intrinsic(_BitScanForward)

// internal functions
extern "C"
{
	static NTSTATUS GetSessionHooks(HookListHead& listHead, PPAGED_LOOKASIDE_LIST pList);
	static NTSTATUS GetHooksFromChain(HOOK** ppHookArray, ULONG fsHooks, const WCHAR* pWinStaDeskName, HookListHead& listHead, PPAGED_LOOKASIDE_LIST pList);
	static NTSTATUS MarshalToUserBufferAndFree(HookListHead& hooks, ClientHookArray*& pWinHookInf, PPAGED_LOOKASIDE_LIST pLookaside);
	static void FreeList(PPAGED_LOOKASIDE_LIST pList, HookListHead& listHead);
	static WCHAR* GetDllName(PVOID pHeap, int atom);
	static void MoveListEntries(HANDLE hHeap, HookInfo*& pWritePointer, PSINGLE_LIST_ENTRY listHead, PPAGED_LOOKASIDE_LIST pLookasideList);
	static NTSTATUS GetProcessHooks(const SYSTEM_PROCESS_INFORMATION& process, pfnGetThreadHooks getWin32HooksFn, HookListHead& listHead, PPAGED_LOOKASIDE_LIST pList);
	static void GetWinstaDeskName(PDESKTOP pDesk, WCHAR (&winstaDeskName)[MAX_WINSTA_DESK_NAME_WCHARS]);
	static VOID EnumSystemHooks(PDEVICE_OBJECT /*pDev*/, PVOID pCtx);
	static BOOLEAN AttachToProcessId(HANDLE processId, BOOLEAN attached, KAPC_STATE& attachedState);
	static void CopyThreadProcessName(PETHREAD pThread, char* pNameBuffer, ULONG bufLen);
}

#ifdef ALLOC_PRAGMA
#pragma alloc_text(PAGE, GetSessionHooks)
#pragma alloc_text(PAGE, GetHooksFromChain)
#pragma alloc_text(PAGE, MarshalToUserBufferAndFree)
#pragma alloc_text(PAGE, FreeList)
#pragma alloc_text(PAGE, GetDllName)
#pragma alloc_text(PAGE, MoveListEntries)
#pragma alloc_text(PAGE, GetProcessHooks)
#pragma alloc_text(PAGE, EnumSystemHooks)
#pragma alloc_text(PAGE, AttachToProcessId)
#pragma alloc_text(PAGE, GetWinstaDeskName)
#pragma alloc_text(PAGE, CopyThreadProcessName)
#endif

#endif
