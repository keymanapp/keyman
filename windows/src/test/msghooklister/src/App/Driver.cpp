#include "stdafx.h"
#include "Driver.h"
#include "util.h"
#include "../Drv/sharedinfo.h"

Driver::Service::Service(const std::wstring& serviceName)
{
	DBG_TEXT("Starting service %s", serviceName.c_str());
	WinType<SC_HANDLE> scMan(OpenSCManager(NULL, NULL, 0), &CloseServiceHandle);
	if(!*scMan)
	{
		DWORD err = GetLastError();
		DBG_TEXT("Couldn't open SCM (%lu)", err);
		ThrowWin32Error(err);
	}
	scService.Init(OpenService(*scMan, serviceName.c_str(), SERVICE_START | SERVICE_STOP), &CloseServiceHandle);
	if(!*scService)
	{
		DWORD err = GetLastError();
		DBG_TEXT("Couldn't open service %s (%lu)", serviceName.c_str(), err);
		ThrowWin32Error(err);
	}
	if((!StartService(*scService, 0, NULL)) && (GetLastError() != ERROR_SERVICE_ALREADY_RUNNING))
	{
		DWORD err = GetLastError();
		DBG_TEXT("Couldn't start service %s (%lu)", serviceName.c_str(), err);
		ThrowWin32Error(err);
	}
}

Driver::Service::~Service()
{
	SERVICE_STATUS stat;
	if(!ControlService(*scService, SERVICE_CONTROL_STOP, &stat))
	{
		DBG_TEXT("Couldn't stop service because of error %lu", GetLastError());
	}
}

Driver::Driver(const std::wstring& serviceName, const std::wstring& fileObjectName, DWORD attributes) 
	: service(serviceName),
	hDev(CreateFile(fileObjectName.c_str(), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, attributes, NULL), &CloseHandle)
{
	if(*hDev == INVALID_HANDLE_VALUE)
	{
		DWORD err = GetLastError();
		DBG_TEXT("Coudn't open %s (%lu)", fileObjectName.c_str(), err);
		ThrowWin32Error(err);
	}
}

BOOL Driver::Control(ULONG ioctlCode, PVOID inBuf, ULONG inBufSize, PVOID outBuf, ULONG outBufSize, PDWORD bytesWritten, OVERLAPPED* ol)
{
	BOOL ret = DeviceIoControl(*hDev, ioctlCode, inBuf, inBufSize, outBuf, outBufSize, bytesWritten, ol);
	DBG_TEXT("DeviceIoControl returned %d for ioctl 0x%x (error %lu)", ret, ioctlCode, GetLastError());
	return ret;
}

BOOL Driver::OverlappedSuccess(OVERLAPPED& ol)
{
	DWORD bytesReturned;
	return GetOverlappedResult(*hDev, &ol, &bytesReturned, TRUE);
}
