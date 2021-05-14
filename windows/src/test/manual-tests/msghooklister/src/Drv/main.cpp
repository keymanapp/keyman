#include "stdafx.h"
#include "dispatch.h"
#include "init.h"
#include <winerror.h>
#include "sharedinfo.h"
#include "util.h"

extern "C"
{
	DRIVER_INITIALIZE DriverEntry;
	DRIVER_UNLOAD MsgListerUnload;
}

#ifdef ALLOC_PRAGMA
#pragma alloc_text(INIT, DriverEntry)
#pragma alloc_text(PAGE, MsgListerUnload)
#endif

NTSTATUS DriverEntry(PDRIVER_OBJECT pDrvObj, PUNICODE_STRING pRegPath)
{
	PAGED_CODE();
	MmPageEntireDriver(&DriverEntry);
	DBG_TEXT("Starting up...");

	UNICODE_STRING usDevName = RTL_CONSTANT_STRING(L"\\Device\\" g_serviceName);

	// first, check for compatible OS version
	RTL_OSVERSIONINFOEXW osVersion = {sizeof(osVersion), 0};
	RtlGetVersion(reinterpret_cast<PRTL_OSVERSIONINFOW>(&osVersion));
	g_dwOSVersion = 
		(osVersion.dwMajorVersion << 24) |
		(osVersion.dwMinorVersion << 16) |
		(osVersion.wServicePackMajor << 8) |
		(osVersion.wServicePackMinor);
	
	// if the major version isn't 6, or the minor version is greater than 1 then fail
	if((g_dwOSVersion > g_dwHighestVersion) || (g_dwOSVersion < g_dwLowestVersion))
	{
		DBG_TEXT(
			"Expecting Windows version below %x and above %x "
			"but found version %x",
			g_dwHighestVersion,
			g_dwLowestVersion,
			g_dwOSVersion
			);
		return STATUS_NOT_SUPPORTED;
	}
	if(PsGetVersion(NULL, NULL, NULL, NULL))
	{
		ERROR_TEXT("Msg/Hook lister doesn't work on checked builds. Do your own investigation with the symbols");
		return STATUS_NOT_SUPPORTED;
	}

	NTSTATUS stat;
	if(!NT_SUCCESS(stat = InitializeEnvironment(pRegPath)))
	{
		DBG_TEXT("InitializeEnvironment failed with error 0x%x", stat);
		return stat;
	}

	// init the relevant function pointers
	pDrvObj->DriverUnload = &MsgListerUnload;
	memset(pDrvObj->MajorFunction, 0, sizeof(pDrvObj->MajorFunction));
	pDrvObj->MajorFunction[IRP_MJ_CREATE] = &MsgListerDispatchCreate;
	pDrvObj->MajorFunction[IRP_MJ_CLOSE] = &MsgListerDispatchClose;
	pDrvObj->MajorFunction[IRP_MJ_DEVICE_CONTROL] = &MsgListerDispatchControl;

	// create our device object 
	PDEVICE_OBJECT pDevObj = NULL;
	stat = IoCreateDevice(pDrvObj, 0, &usDevName, FILE_DEVICE_UNKNOWN, FILE_DEVICE_SECURE_OPEN, FALSE, &pDevObj);
	if(!NT_SUCCESS(stat))
	{
		DBG_TEXT("Failed to create device. Error = 0x%x, Name = %wZ\n", stat, &usDevName);
		return stat;
	}

	// and the UM visible symbolic link
	stat = IoCreateSymbolicLink(&g_usSymName, &usDevName);
	if(!NT_SUCCESS(stat))
	{
		DBG_TEXT("Failed to create symbolic link. Error = 0x%x, Name = %wZ\n", stat, &g_usSymName);
		IoDeleteDevice(pDevObj);
		return stat;
	}
	return STATUS_SUCCESS;
}

void MsgListerUnload(PDRIVER_OBJECT pDrvObj)
{
	PAGED_CODE();
	DBG_TEXT("Unloading");
	IoDeleteSymbolicLink(&g_usSymName);
	IoDeleteDevice(pDrvObj->DeviceObject);
}
