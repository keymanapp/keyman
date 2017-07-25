#ifndef MSGLISTER_UTIL_H
#define MSGLISTER_UTIL_H

#pragma once

extern "C"
{
	NTSTATUS GetRunningProcesses(UCHAR** ppProcessBuffer);
	NTSTATUS ReportException(PEXCEPTION_POINTERS pExp);
	BOOLEAN CheckPtrPtrValidity(PVOID* ppAddress);
	BOOLEAN CheckEResourceValidity();
	NTSTATUS GetObjectName(PVOID pObject, PUNICODE_STRING pusName);
}

#define REPORT_EXCEPTION ((stat = ReportException(GetExceptionInformation())), EXCEPTION_EXECUTE_HANDLER)
#define REPORT_EXCEPTION_VOID (ReportException(GetExceptionInformation()), EXCEPTION_EXECUTE_HANDLER)

#ifdef ALLOC_PRAGMA
#pragma alloc_text(PAGE, GetRunningProcesses)
#pragma alloc_text(PAGE, ReportException)
#pragma alloc_text(PAGE, CheckEResourceValidity)
#pragma alloc_text(PAGE, CheckPtrPtrValidity)
#pragma alloc_text(PAGE, GetObjectName)
#endif

#endif
