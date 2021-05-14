#include "stdafx.h"
#include "window.h"
#include "messages.h"
#include "driver.h"
#include "util.h"
#include "../drv/sharedinfo.h"
#include <iostream>
#include "wm_messages.h"
#include "Resource.h"

bool IsWin32kUnchanged()
{
	ULONG checksum = 0, timestamp = 0;
	DWORD err = 0;
	// get the current checksum and timestamp and compare them to the ones from install time
	if(GetWin32kParticulars(checksum, timestamp))
	{
		WinType<HKEY, LSTATUS> hKey(NULL, &RegCloseKey);
		if((err = RegOpenKeyEx(HKEY_LOCAL_MACHINE, g_parameterKey, 0, KEY_QUERY_VALUE, &hKey)) == ERROR_SUCCESS)
		{
			DWORD buffer[2] = {0};
			DWORD bufferSize = sizeof(buffer);
			VALENT vals[2] = {{0}, {0}};
			// lack of const correctness on MS's part
			vals[0].ve_valuename = const_cast<LPWSTR>(g_timeStampParameterName);
			vals[1].ve_valuename = const_cast<LPWSTR>(g_checksumParameterName);
			BOOL bRet = RegQueryMultipleValues(*hKey, vals, ARRAYSIZE(vals), reinterpret_cast<LPWSTR>(buffer), &bufferSize);
			ASSERT(GetLastError() != ERROR_MORE_DATA && "Check for variable size changes in install.cpp - SetRegistryParams");
			DBG_TEXT("RegQueryMultipleValues returned %d (%lu)", bRet, GetLastError());
			return ((*(reinterpret_cast<DWORD*>(vals[0].ve_valueptr)) == timestamp)
				&& (*(reinterpret_cast<DWORD*>(vals[1].ve_valueptr)) == checksum)
				);
		}
		else
		{
			DBG_TEXT("Couldn't open %s registry key (%lu)", g_parameterKey, err);
		}
	}
	else
	{
		err = GetLastError();
		DBG_TEXT("Failed to get win32k timestamp and checksum (%lu)", err);
	}
	DWORD_PTR param = reinterpret_cast<DWORD_PTR>(g_parameterKey);
	std::wstring prependedText;
	GetModuleMessage(GetModuleHandle(NULL), MSG_CANT_CHECK_WIN32K, prependedText, &param);
	ThrowWin32Error(err, prependedText);
}

DWORD GetThreadProcessFileName(DWORD threadId, std::wstring& processName)
{
	DWORD procID = 0;
	WinType<> hThread(OpenThread(THREAD_QUERY_LIMITED_INFORMATION, FALSE, threadId), &CloseHandle);
	if(*hThread)
	{
		procID = GetProcessIdOfThread(*hThread);
		if(procID)
		{
			WinType<> hProcess(OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, procID), &CloseHandle);
			if(*hProcess)
			{
				// QueryFullProcessImageName doesn't return buffer size
				// required so just keep looping until success
				ULONG timesLooped = 0;
				BOOL finished = FALSE;
				while(!finished)
				{
					++timesLooped;
					DWORD size = MAX_PATH * timesLooped;
					processName.resize(size);
					finished = QueryFullProcessImageName(*hProcess, 0, &processName[0], &size);
					if(finished)
					{
						processName.resize(size);
						// return the file name only
						std::wstring::size_type pos = processName.find_last_of(L"\\/");
						if(pos != std::wstring::npos)
						{
							processName.erase(0, pos + 1);
						}
					}
				}
			}
		}
	}
	return procID;
}

std::wstring GetWMMsgName(UINT msg)
{
	if(msg < WM_USER)
	{
		return WM_MESSAGE_STRINGS[msg];
	}
	else if(msg >= 0xC000 && msg < 0xFFFF)
	{
		// the reciprocal of RegisterWindowMessage
		// due to the fact that RWM is the same export
		// as RegisterClipboardFormat
		WCHAR buffer[0x100];
		if(GetClipboardFormatName(msg, buffer, ARRAYSIZE(buffer)))
		{
			return buffer;
		}
	}
	return GetModuleString(STR_UNKNOWN);
}

std::wstring GetWindowInfo(const MsgInfo* pMsg)
{
	std::wostringstream winfo;
	HWND hwnd = pMsg->msg.hwnd;
	if(hwnd)
	{
		WCHAR title[32] = {0};
		DWORD_PTR result = 0;
		// try and get some text for easier identification of the window
		int ret = SendMessageTimeout(
			hwnd,
			WM_GETTEXT,
			ARRAYSIZE(title),
			reinterpret_cast<LPARAM>(title),
			SMTO_ABORTIFHUNG,
			200,
			&result
		);
		winfo << L"0x" << hwnd;
		if(ret)
		{
			winfo << L'(' << title << L')';
		}
	}
	else 
	{
		winfo << L"NULL";
		if(pMsg->posted)
		{
			winfo << L" - " << GetModuleString(STR_THREADMESSAGE);
		}
	}
	return winfo.str();
}

void DisplayMessageInfo(const MsgInfo* pMsg)
{
	bool gotMessageName = false;
	const MSG& msg = pMsg->msg;
	const std::wstring& windowInfo = GetWindowInfo(pMsg);
	DisplayMessage(
		MSG_GENERAL_MESSAGE_INFO,
		windowInfo.c_str(),
		msg.message,
		GetWMMsgName(msg.message).c_str(),
		msg.wParam,
		msg.lParam,
		msg.time,
		msg.pt.x,
		msg.pt.y,
		pMsg->posted);
	if(pMsg->posted)
	{
		const PostedMessageInfo& pmi = pMsg->extra.posted;
		DisplayMessage(MSG_POSTED_MESSAGE_INFO, pmi.ExtraInfo, pmi.ptMouseReal.x, pmi.ptMouseReal.y);
	}
	else
	{
		const SentMessageInfo& smi = pMsg->extra.sent;
		// if a sending thread was specified, try and display tid, pid and process name
		if(smi.sendingThreadID)
		{
			std::wstring processName;
			std::wstringstream processInfo;
			DWORD procID = GetThreadProcessFileName(smi.sendingThreadID, processName);
			if(procID && !processName.empty())
			{
				processInfo << L" (" << processName << L" - " << procID << L')';
			}
			DisplayMessage(MSG_SENT_MESSAGE_INFO_THREAD, smi.sendingThreadID, processInfo.str().c_str());
		}
		if(smi.flags)
		{
			const ULONG flags = smi.flags;
			std::wostringstream flagString;
			if(flags & ISMEX_CALLBACK)
			{
				flagString << L"\tSendMessageCallback - lpCallback = 0x" << smi.pCallback << L", dwData = 0x" << std::hex << smi.dwData << L'\n';
			}
			if(flags & ISMEX_NOTIFY)
			{
				flagString << L"\tSendNotifyMessage\n";
			}
			if(flags & ISMEX_SEND)
			{
				flagString << L"\tSendMessage(Timeout)\n";
			}
			if(flags & ISMEX_REPLIED)
			{
				flagString << L'\t' << GetModuleString(STR_RETURNED) << std::hex << smi.lRet << L'\n';
			}
			DisplayMessage(MSG_SENT_MESSAGE_INFO_FUNCTION, flagString.str().c_str());
		}
	}
}

void DisplayThreadMessageInfo(ThreadMessageInfo* pTMI)
{
	const ULONG tifFlags = pTMI->TIF_flags;
	std::wostringstream tifFlagText;
	std::wstring noneText = GetModuleString(STR_NONE);
	// see if any reportable flags are set
	if(tifFlags & (TIF_VALIDMASK & (~TIF_INITIALIZED)))
	{
		// TIF_INITIALIZED is left out, as it is obviously set, given that the thread has a window
		TEST_SET_AND_PRINT(tifFlagText, tifFlags, TIF_INCLEANUP);
		TEST_SET_AND_PRINT(tifFlagText, tifFlags, TIF_16BIT);
		TEST_SET_AND_PRINT(tifFlagText, tifFlags, TIF_SYSTEMTHREAD);
		TEST_SET_AND_PRINT(tifFlagText, tifFlags, TIF_CSRSSTHREAD);
		TEST_SET_AND_PRINT(tifFlagText, tifFlags, TIF_DISABLEIME);
		TEST_SET_AND_PRINT(tifFlagText, tifFlags, TIF_DISABLEHOOKS);
		TEST_SET_AND_PRINT(tifFlagText, tifFlags, TIF_WAITFORINPUTIDLE);
		TEST_SET_AND_PRINT(tifFlagText, tifFlags, TIF_QUITMSGPOSTED);
	}
	else tifFlagText << noneText << L'\n';
	std::wostringstream pendingOpString;
	ULONG ops = pTMI->pendingOperations;
	if(ops)
	{
		if(ops & WM_TIMER_PENDING)
		{
			pendingOpString << L"\tWM_(SYS)TIMER\n";
		}
		if(ops & WM_PAINT_PENDING)
		{
			pendingOpString << L"\tWM_PAINT\n";
		}
		if(ops & WM_QUIT_PENDING)
		{
			pendingOpString << L"\tWM_QUIT, " << GetModuleString(STR_EXITCODE) << L"0x" << std::hex << pTMI->quitMessageExitCode << L'\n';
		}
	}
	else pendingOpString << GetModuleString(STR_NONE) << L'\n';

	DisplayMessage(
		MSG_THREAD_STATE,
		pTMI->threadId,
		tifFlagText.str().c_str(),
		pTMI->messageExtraInfo, 
		pendingOpString.str().c_str(),
		pTMI->numMessages
	);

	MsgInfo* pMsg = pTMI->messages, *pMsgEnd = pMsg + pTMI->numMessages;
	while(pMsg != pMsgEnd)
	{
		DisplayMessageInfo(pMsg++);
	}
}

void ListWindowMessages(HWND hwnd)
{
	try
	{
		// start the driver service and open a handle to it
		Driver drv(L"MsgLister", CONCAT(L"\\\\.\\Global\\", g_serviceName));
		WndInfo wnd = {hwnd};
		WndThreadInfo wti = {0};
		ULONG bytes = 0;
		// get the info from the driver
		if(!drv.Control(IOCTL_GET_WND_INFO, &wnd, sizeof(wnd), &wti, sizeof(wti), &bytes, NULL))
		{
			DisplayMessage(MSG_DRIVER_COMMUNICATION_FAILURE, WSTRINGIFY(IOCTL_GET_WND_INFO), GetLastErrorText().c_str());
			return;
		}
		DBG_TEXT("Message data at 0x%p", wti.pTMI);
		if(wti.pTMI)
		{
			// display it
			DisplayThreadMessageInfo(wti.pTMI);
			// then free it
			VirtualFree(wti.pTMI, 0, MEM_RELEASE);
		}
		else
		{
			DisplayMessage(MSG_WINDOW_DOESNT_HAVE_A_THREAD, hwnd);
		}
	}
	catch(const WException& ex)
	{
		DisplayMessage(MSG_CANT_OPEN_DRIVER, ex.what());
	}
}

BOOL IsAltTabWindow(HWND hwnd)
{
    LONG_PTR exStyles = GetWindowLongPtr(hwnd, GWL_EXSTYLE);
    // Start at the root owner
    HWND hwndWalk = hwnd;
    // appwindows treated as no owner, so don't try and find one
    if(!(exStyles & WS_EX_APPWINDOW))
    {
        hwndWalk = GetAncestor(hwnd, GA_ROOTOWNER);
    }
    // See if we are the last active visible popup
    HWND hwndTry = hwndWalk;
    while ((hwndTry = GetLastActivePopup(hwndWalk)) != hwndTry) {
        if (IsWindowVisible(hwndTry)) break;
        hwndWalk = hwndTry;
    }
    // tool windows are treated as not visible so they'll never appear in the list 
    // fail them here
    return (hwndWalk == hwnd) && !(exStyles & WS_EX_TOOLWINDOW);
}

BOOL CALLBACK Enum(HWND hwnd, LPARAM lParam)
{
    if(IsWindowVisible(hwnd) && IsAltTabWindow(hwnd))
    {
        WCHAR buf[260];
        GetWindowText(hwnd, buf, ARRAYSIZE(buf));
        std::wcout << L"HWND " << hwnd << L' ' << buf << '\n';
    }
    return TRUE;
}

void ListActiveWindows()
{
	DisplayMessage(MSG_ACTIVE_WINDOW_LIST);
	if(!EnumWindows(&Enum, 0))
	{
		DisplayMessage(MSG_GENERIC_OPERATION_FAILURE, GetLastErrorText().c_str());
	}
	std::wcout << std::flush;
}
