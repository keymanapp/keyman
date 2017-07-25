#include "stdafx.h"
#include "sharedinfo.h"
#include "messages.h"
#include "hooks.h"
#include "dispatch.h"
#include "util.h"

NTSTATUS MsgListerDispatchCreate(PDEVICE_OBJECT /*pDevObj*/, PIRP pIrp)
{
	PAGED_CODE();
	pIrp->IoStatus.Information = 0;
	pIrp->IoStatus.Status = STATUS_SUCCESS;
	IoCompleteRequest(pIrp, IO_NO_INCREMENT);
	return STATUS_SUCCESS;
}

NTSTATUS MsgListerDispatchClose(PDEVICE_OBJECT /*pDevObj*/, PIRP pIrp)
{
	PAGED_CODE();
	pIrp->IoStatus.Information = 0;
	pIrp->IoStatus.Status = STATUS_SUCCESS;
	IoCompleteRequest(pIrp, IO_NO_INCREMENT);
	return STATUS_SUCCESS;
}

NTSTATUS MsgListerDispatchControl(PDEVICE_OBJECT /*pDevObj*/, PIRP pIrp)
{
	PAGED_CODE();
	IO_STATUS_BLOCK& iosb = pIrp->IoStatus;
	PIO_STACK_LOCATION pStack = IoGetCurrentIrpStackLocation(pIrp);
	NTSTATUS stat = STATUS_SUCCESS;
	__try
	{
		switch(pStack->Parameters.DeviceIoControl.IoControlCode)
		{
			case IOCTL_GET_WND_INFO:
			{
				stat = GetWindowMessageQueue(pIrp, &iosb.Information);
			}
			break;
			case IOCTL_GET_HOOK_INFO:
			{
				stat = GetWindowsHookInfo(pIrp, &iosb.Information);
			}
			break;
		}
	}
	__except(REPORT_EXCEPTION)
	{
		iosb.Information = 0;
	}
	iosb.Status = stat;
	IoCompleteRequest(pIrp, IO_NO_INCREMENT);
	return stat;
}
