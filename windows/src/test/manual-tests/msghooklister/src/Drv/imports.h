#ifndef IMPORTS_H
#define IMPORTS_H

#pragma once

extern "C"
{
	__if_not_exists(PsGetProcessSessionIdEx)
	{
		NTSYSAPI ULONG NTAPI PsGetProcessSessionIdEx(PEPROCESS);
	}
	__if_not_exists(PsGetProcessWin32Process)
	{
		NTSYSAPI PVOID NTAPI PsGetProcessWin32Process(PEPROCESS pProcess);
	}
	__if_not_exists(PsGetThreadWin32Thread)
	{
		NTSYSAPI PVOID NTAPI PsGetThreadWin32Thread(PETHREAD pThread);
	}
	__if_not_exists(PsGetCurrentThreadWin32Thread)
	{
		NTSYSAPI PVOID /*struct tagTHREADINFO* */ NTAPI PsGetCurrentThreadWin32Thread();
	}
	__if_not_exists(PsGetCurrentProcessSessionId)
	{
		NTSYSAPI ULONG NTAPI PsGetCurrentProcessSessionId();
	}
	__if_not_exists(PsGetProcessImageFileName)
	{
		NTSYSAPI UCHAR* NTAPI PsGetProcessImageFileName(PEPROCESS pProc);
	}
	__if_not_exists(SystemProcessInformation)
	{
		static const ULONG SystemProcessInformation = 5;
	}
	__if_not_exists(ZwQuerySystemInformation)
	{
		NTSYSAPI NTSTATUS NTAPI	ZwQuerySystemInformation(ULONG, PVOID, ULONG, PULONG);
	}
	__if_not_exists(SYSTEM_PROCESS_INFORMATION)
	{
		typedef struct _SYSTEM_THREAD_INFORMATION {
			LARGE_INTEGER KernelTime;
			LARGE_INTEGER UserTime;
			LARGE_INTEGER CreateTime;
			ULONG         WaitTime;
			PVOID         StartAddress;
			CLIENT_ID     ClientId;
			KPRIORITY     Priority;
			KPRIORITY     BasePriority;
			ULONG         ContextSwitchCount;
			LONG          State;
			LONG          WaitReason;
		} SYSTEM_THREAD_INFORMATION, * PSYSTEM_THREAD_INFORMATION;

		typedef struct _SYSTEM_PROCESS_INFORMATION {
			ULONG             NextEntryDelta;
			ULONG             ThreadCount;
			LARGE_INTEGER     Reserved1[3];
			LARGE_INTEGER     CreateTime;
			LARGE_INTEGER     UserTime;
			LARGE_INTEGER     KernelTime;
			UNICODE_STRING    ProcessName;
			KPRIORITY         BasePriority;
			HANDLE            ProcessId;
			HANDLE            InheritedFromProcessId;
			ULONG             HandleCount;
			ULONG             SessionId;
			ULONG_PTR         PageDirectoryBase;
			VM_COUNTERS       VmCounters;
			SIZE_T		      PrivatePageCount;
			IO_COUNTERS       IoCounters;
			SYSTEM_THREAD_INFORMATION Threads[1];
		} SYSTEM_PROCESS_INFORMATION, * PSYSTEM_PROCESS_INFORMATION;
	}
}

#endif
