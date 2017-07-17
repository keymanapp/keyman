#ifndef IOCTLS_H
#define IOCTLS_H

#pragma once

#include "stdafx.h"
#include "structs.h"
#include "sharedinfo.h"

extern "C"
{
	NTSTATUS GetWindowMessageQueue(PIRP pIrp, ULONG_PTR* pulpBytesWritten);
	BOOLEAN IsSpecialHWND(HWND hwnd);
	ULONG CountSentMessages(PSMSMSG pMsg);
}

#ifdef ALLOC_PRAGMA
#pragma alloc_text(PAGE, GetWindowMessageQueue, IsSpecialHWND)
#pragma alloc_text(PAGE, CountSentMessages)
#endif

#pragma code_seg("PAGE")

template<class QMsgType>
void CopyQMsg(QMsgType* pqmsg, MsgInfo* pMsg)
{
	PAGED_CODE();
	pMsg->msg = pqmsg->msg;
	pMsg->posted = TRUE;
	pMsg->extra.posted.ExtraInfo = pqmsg->ExtraInfo;
	pMsg->extra.posted.ptMouseReal = pqmsg->ptMouseReal;
}

template<class QMsg>
void CopyQMsgList(QMsg* pqmsg, MsgInfo*& pMsg, ULONG cMsgs)
{
	PAGED_CODE();
	while(pqmsg && cMsgs)
	{
		DBG_TEXT("QMSG 0x%p, msg=0x%x, time=0x%x, pti=0x%p", pqmsg, pqmsg->msg.message, pqmsg->msg.time, pqmsg->pti);
		CopyQMsg(pqmsg, pMsg);
		pqmsg = pqmsg->pqmsgNext;
		++pMsg;
		--cMsgs;
	}
}

template<class ThreadInfoType>
void CopySMSMessage(HWND hwndDefault, MsgInfo* pMsg, PSMSMSG psmsMsg)
{
	PAGED_CODE();
	MSG& destMsg = pMsg->msg;
	destMsg.hwnd = psmsMsg->spwnd ? static_cast<HWND>(psmsMsg->spwnd->head.h) : hwndDefault;
	destMsg.message = psmsMsg->msg;
	destMsg.lParam = psmsMsg->lParam;
	destMsg.wParam = psmsMsg->wParam;
	destMsg.time = psmsMsg->tSent;
	pMsg->posted = FALSE;
	pMsg->extra.sent.dwData = psmsMsg->dwData;
	pMsg->extra.sent.pCallback = psmsMsg->lpResultCallBack;
	pMsg->extra.sent.lRet = psmsMsg->lRet;
	ULONG& msgFlags = pMsg->extra.sent.flags;
	msgFlags = 0;
	const ULONG& smsFlags = psmsMsg->flags;
	if(psmsMsg->ptiSender)
	{
		msgFlags |= ISMEX_SEND;
		ThreadInfoType* ptiSender = reinterpret_cast<ThreadInfoType*>(psmsMsg->ptiSender);
		pMsg->extra.sent.sendingThreadID = reinterpret_cast<ULONG>(PsGetThreadId(ptiSender->pEThread));
	}
	else
	{
		// Might need to check the following 0x300 value
		// its in NtUserGetThreadState, case 9 in the jump table (InSendMessageEx) 
		C_ASSERT(NTDDI_VERSION <= NTDDI_WIN7);
		msgFlags |= (smsFlags & 0x300) ? ISMEX_CALLBACK : ISMEX_NOTIFY;
	}
	if(smsFlags & 1)
	{
		msgFlags |= ISMEX_REPLIED;
	}
}

template<class ThreadInfoType>
void CopySMSMessageList(HWND hwndThis, MsgInfo*& pMsg, PSMSMSG psmsMsg)
{
	PAGED_CODE();
	while(psmsMsg)
	{
		DBG_TEXT("qsms 0x%p, message 0x%x, tSent 0x%x, ptiSender 0x%p, ptiReceiever 0x%p", psmsMsg, psmsMsg->msg, psmsMsg->tSent, psmsMsg->ptiSender, psmsMsg->ptiReceiver);
		DBG_TEXT("lpResultCallback 0x%p, lRet 0x%x, flags 0x%x, pWnd 0x%p", psmsMsg->lpResultCallBack, psmsMsg->lRet, psmsMsg->flags, psmsMsg->spwnd);
		CopySMSMessage<ThreadInfoType>(hwndThis, pMsg, psmsMsg);
		psmsMsg = psmsMsg->pSmsReceiveNext;
		++pMsg;
	}
	DBG_TEXT("Finished enumerating sent messages");
}

template<class ThreadInfoType>
NTSTATUS ReadMessages(PWND pWnd, WndThreadInfo* pThreadInfo)
{
	// make the typedefs less cumbersome
	typedef typename ThreadInfoType::QPtr QPtr;
	typedef typename ThreadInfoType::MListType MListType;
	typedef MListType::QMSGPtr QMSGPtr;

	PAGED_CODE();

	ThreadInfoType* pti = reinterpret_cast<ThreadInfoType*>(pWnd->head.pti);
	if(pti)
	{
		PSMSMSG psmsMsg = pti->psmsReceiveList;
		// first count the number of messages available
		ULONG numMessages = CountSentMessages(psmsMsg);
		numMessages += pti->mlPost.cMsgs;
		ULONG_PTR extraInfo = 0;
		QPtr pq = pti->pq;
		if(pq)
		{
			numMessages += pq->mlInput.cMsgs;
			extraInfo = pq->ExtraInfo;
		}
		// -1 accounts for the one present in the ThreadMessageInfo structure
		ULONG additionalMessages = numMessages ? numMessages - 1 : 0;
		// allocate the memory
		PVOID pBaseAddress = NULL;
		SIZE_T sizeRequired = sizeof(ThreadMessageInfo) + (sizeof(MsgInfo) * additionalMessages);
		DBG_TEXT("Trying to allocate %Iu bytes (%lu messages)", sizeRequired, numMessages);
		NTSTATUS stat = ZwAllocateVirtualMemory(NtCurrentProcess(), &pBaseAddress, 0, &sizeRequired, MEM_COMMIT, PAGE_READWRITE);
		if(!NT_SUCCESS(stat))
		{
			DBG_TEXT("Failed to allocate memory for results, 0x%x", stat);
			return STATUS_INSUFFICIENT_RESOURCES;
		}
		DBG_TEXT("Allocated at 0x%p, pti = 0x%p", pBaseAddress, pti);
		// try and secure it so the UM app can't free or change its protection etc
		HANDLE hMemSec = MmSecureVirtualMemory(pBaseAddress, sizeRequired, PAGE_READWRITE);
		if(!hMemSec)
		{
			DBG_TEXT("Couldn't secure virtual memory");
		}
		// fill in the struct
		ThreadMessageInfo* pTMI = static_cast<ThreadMessageInfo*>(pBaseAddress);
		pThreadInfo->pTMI = pTMI;
		pTMI->numMessages = numMessages;
		pTMI->threadId = reinterpret_cast<ULONG>(PsGetThreadId(pti->pEThread));
		pTMI->TIF_flags = pti->TIF_Flags;
		pTMI->pendingOperations = 0;
		if(pti->cPaintsReady)
		{
			pTMI->pendingOperations |= WM_PAINT_PENDING;
		}
		if(pti->cTimersReady)
		{
			pTMI->pendingOperations |= WM_TIMER_PENDING;
		}
		if(pti->TIF_Flags & TIF_QUITMSGPOSTED)
		{
			pTMI->pendingOperations |= WM_QUIT_PENDING;
		}
		pTMI->quitMessageExitCode = pti->exitCode;
		pTMI->messageExtraInfo = extraInfo;
		// copy all the message details
		// the order these messages are copied is per the comment in the GetMessage docs
		// Sent messages
		// Posted messages
		// input messages
		//
		MsgInfo* pMsg = pTMI->messages;
		HWND hwndThis = static_cast<HWND>(pWnd->head.h);
		DBG_TEXT("Thread info: cPaints %d, cTimers %d, TIF_Flags 0x%x, QS_Flags 0x%04x", pti->cPaintsReady, pti->cTimersReady, pti->TIF_Flags, pti->pClientThreadInfo->fsWakeBits);
		// sent messages
		DBG_TEXT("Messages on 0x%p->receiveList", pWnd);
		CopySMSMessageList<ThreadInfoType>(hwndThis, pMsg, psmsMsg);
		// posted messages
		const MListType& list = pti->mlPost;
		DBG_TEXT("Messages on pti->mlPost, cMsgs=%d", list.cMsgs);
		QMSGPtr pqmsg = list.pqmsgRead;
		CopyQMsgList(pqmsg, pMsg, list.cMsgs);
		// posted input messages
		if(pq)
		{
			const MListType& list = pq->mlInput;
			DBG_TEXT("Messages on the Q->mlInput, cMsgs=%d", list.cMsgs);
			QMSGPtr pqmsg = list.pqmsgRead;
			CopyQMsgList(pqmsg, pMsg, list.cMsgs);
		}
		// finish up
		if(hMemSec)
		{
			MmUnsecureVirtualMemory(hMemSec);
		}
	}
	else
	{
		pThreadInfo->pTMI = NULL;
	}
	return STATUS_SUCCESS;
}
#pragma code_seg()

#endif
