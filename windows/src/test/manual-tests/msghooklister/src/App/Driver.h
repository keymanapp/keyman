#ifndef DRIVER_H
#define DRIVER_H

#pragma once

#include "stdafx.h"

class Driver
{
	class Service
	{
		WinType<SC_HANDLE> scService;
	public:
		Service(const std::wstring& serviceName);
		~Service();
	} service;

	WinType<> hDev;

public:
	Driver(const std::wstring& serviceName, const std::wstring& fileObjectName, DWORD attributes = FILE_ATTRIBUTE_NORMAL);
	BOOL Control(ULONG ioctlCode, PVOID inBuf, ULONG inBufSize, PVOID outBuf, ULONG outBufSize, PDWORD bytesWritten, OVERLAPPED* ol);
	BOOL OverlappedSuccess(OVERLAPPED& ol);
};

#endif
