#include "stdafx.h"
#include "hooks.h"
#include "driver.h"
#include "../drv/sharedinfo.h"
#include "messages.h"
#include "resource.h"
#include "util.h"
#include "window.h"

static const DWORD g_dwGlobalHooksOnly = MAXDWORD;
static const LPCWSTR g_hookNames[] = {
	L"WH_MSGFILTER",
	L"WH_JOURNALRECORD",
	L"WH_JOURNALPLAYBACK",
	L"WH_KEYBOARD",
	L"WH_GETMESSAGE",
	L"WH_CALLWNDPROC",
	L"WH_CBT",
	L"WH_SYSMSGFILTER",
	L"WH_MOUSE",
	L"WH_HARDWARE",
	L"WH_DEBUG",
	L"WH_SHELL",
	L"WH_FOREGROUNDIDLE",
	L"WH_CALLWNDPROCRET",
	L"WH_KEYBOARD_LL",
	L"WH_MOUSE_LL"
};

DWORD IsValidPid(const WCHAR* pwszPid)
{
	if(pwszPid)
	{
		if(pwszPid[0] == L'-')
		{
			return g_dwGlobalHooksOnly;
		}
		return wcstoul(pwszPid, NULL, 10);
	}
	else return g_dwGlobalHooksOnly;
}

static std::wstring GetHookFlagString(DWORD flags)
{
	std::wstring flagStr;
	TEST_SET_AND_PRINT_HOOK(flagStr, flags, HF_GLOBAL);
	TEST_SET_AND_PRINT_HOOK(flagStr, flags, HF_ANSI);
	TEST_SET_AND_PRINT_HOOK(flagStr, flags, HF_NEEDHC_SKIP);
	TEST_SET_AND_PRINT_HOOK(flagStr, flags, HF_HUNG);
	TEST_SET_AND_PRINT_HOOK(flagStr, flags, HF_HOOKFAULTED);
	TEST_SET_AND_PRINT_HOOK(flagStr, flags, HF_NOPLAYBACKDELAY);
	TEST_SET_AND_PRINT_HOOK(flagStr, flags, HF_WX86KNOWNDLL);
	TEST_SET_AND_PRINT_HOOK(flagStr, flags, HF_DESTROYED);
	TEST_SET_AND_PRINT_HOOK(flagStr, flags, HF_INCHECKWHF);
	TEST_SET_AND_PRINT_HOOK(flagStr, flags, HF_FREED);
	if(flagStr.empty())
	{
		flagStr = GetModuleString(STR_NONE);
	}
	return flagStr;
}

static void DisplayHookData(const ClientHookArray* pHookArray, Driver& drv)
{
	DisplayMessage(MSG_HOOK_DISPLAY_HEADER, pHookArray->numHooks);
	const HookInfo* pHookData = pHookArray->hooks, *pEnd = pHookData + pHookArray->numHooks;
	std::wstring exeOrNoneStr = GetModuleString(STR_EXEORNONE);
	UINT hookNum = 1;
	while(pHookData != pEnd)
	{
		LPCWSTR pwszHookTypeName = g_hookNames[pHookData->hookType + 1];
		DBG_TEXT("HookData at %p, pwszHookTypeName = %s, type = %d", pHookData, pwszHookTypeName, pHookData->hookType);
		// turn the flags into a string
		std::wstring flagStr = GetHookFlagString(pHookData->flags);
		DisplayMessage(
			MSG_HOOK_DISPLAY_INFO,
			hookNum,
			pHookData->hHook,
			pHookData->sessionId,
			pHookData->winstaDeskName,
			pwszHookTypeName,
			pHookData->pDllName ? pHookData->pDllName : exeOrNoneStr.c_str(),
			pHookData->hookFnOffset,
			pHookData->dwSettingThread,
			pHookData->settingProcess,
			pHookData->dwHookedThread,
			pHookData->hookedProcess,
			pHookData->timeout,
			!!pHookData->lastHookHung,
			flagStr.c_str()
		);
		if(pHookData->pDllName)
		{
			HeapFree(pHookArray->hNamesHeap, 0, pHookData->pDllName);
		}
		++pHookData;
		++hookNum;
	}
}

void ListHooks()
{
	try
	{
		// start the driver service and open a handle to it
		Driver drv(L"MsgLister", CONCAT(L"\\\\.\\Global\\", g_serviceName));
		WindowsHookInfo whi = {0};
		ULONG bytes = 0;
		// get the info from the driver
		if((!drv.Control(IOCTL_GET_HOOK_INFO, NULL, 0, &whi, sizeof(whi), &bytes, NULL)) || (bytes != sizeof(whi)))
		{
			DisplayMessage(MSG_DRIVER_COMMUNICATION_FAILURE, WSTRINGIFY(IOCTL_GET_HOOK_INFO), GetLastErrorText().c_str());
			return;
		}
		DBG_TEXT("Hook data at 0x%p", whi.pHookInfoArray);
		if((bytes == sizeof(whi)) && (whi.pHookInfoArray))
		{
			if(whi.pHookInfoArray->numHooks)
			{
				// display any we got
				DisplayHookData(whi.pHookInfoArray, drv);
			}
			else
			{
				// or a message that there weren't any
				DisplayMessage(MSG_NO_HOOKS_RETURNED);
			}
			// then free it
			HANDLE hHeap = whi.pHookInfoArray->hNamesHeap;
			HeapFree(hHeap, 0, whi.pHookInfoArray);
			HeapDestroy(hHeap);
		}
		else
		{
			DisplayMessage(MSG_FAILED_HOOK_QUERY);
		}
	}
	catch(const WException& ex)
	{
		DisplayMessage(MSG_CANT_OPEN_DRIVER, ex.what());
	}
}