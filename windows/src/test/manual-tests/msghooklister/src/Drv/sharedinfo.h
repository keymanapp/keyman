#ifndef SHARED_INFO_H
#define SHARED_INFO_H

#pragma once

#include <cstddef>

// The UM app need winioctl for CTL_CODE and associated macros
#ifndef CTL_CODE
#include <winioctl.h>
#endif

#ifndef CONCAT
#define CONCAT_INT(a, b) a##b
#define CONCAT(a, b) CONCAT_INT(a, b)
#endif

static const WCHAR* g_userGetAtomNameParamName = L"UserGetAtomNameOffset";
static const WCHAR* g_atomArrayParamName = L"AtomArrayOffset";
static const WCHAR* g_validateHwndParameterName = L"ValidateHwndOffset";
static const WCHAR* g_gpresUserParameterName = L"gpresUserOffset";
static const WCHAR* g_timeStampParameterName = L"Timestamp";
static const WCHAR* g_checksumParameterName = L"Checksum";
static const WCHAR* g_grpWinStaListParamName = L"grpWinStaListOffset";
// this is a define so it can be concatted to the below constants
#define g_serviceName L"MsgLister"
// this is a define so it can be used in the RTL_CONSTANT_STRING macro
// it must start with a slash
#define g_parameterSubKey L"\\Parameters"
#define g_baseServiceKey L"System\\CurrentControlSet\\Services\\" g_serviceName
static const WCHAR* g_parameterKey = g_baseServiceKey g_parameterSubKey;

__if_not_exists(MSG)
{
	typedef struct tagPOINT{
		LONG x;
		LONG y;
	} POINT;

	typedef struct tagMSG {
		HWND        hwnd;
		ULONG        message;
		ULONG_PTR      wParam;
		LONG_PTR      lParam;
		ULONG       time;
		POINT       pt;
	} MSG, *PMSG, *NPMSG, *LPMSG;
}

typedef struct PostedMessageInfo_
{
	LONG_PTR ExtraInfo;
	POINT ptMouseReal;
} PostedMessageInfo;

typedef struct SentMessageInfo_
{
	ULONG sendingThreadID; // id of thread that sent the message (may be 0)
	ULONG flags; // ISMEX_* flags
	ULONG_PTR dwData; // dwData specified in SendMessageCallback
	PVOID pCallback; // SendAsyncProc specified in SendMessageCallback
	LONG_PTR lRet; // only valid if (flags & ISMEX_REPLIED) is non-zero
} SentMessageInfo;

typedef struct MsgInfo_
{
	MSG msg;
	BOOLEAN posted; // TRUE for posted, FALSE for sent
	union
	{
		SentMessageInfo sent;
		PostedMessageInfo posted;
	} extra;
} MsgInfo;

static const ULONG WM_PAINT_PENDING = 1;
static const ULONG WM_TIMER_PENDING = 2;
static const ULONG WM_QUIT_PENDING = 4;

// TIF_flags values
// unless specified, names are from asserts in the Win7 RC checked version
// as some of these have a different value to the list in the ReactOS wiki
// I ain't using it as it obviously isn't wholly correct for Vista/7
static const ULONG TIF_INCLEANUP = 0x1;
static const ULONG TIF_16BIT = 0x2;
static const ULONG TIF_SYSTEMTHREAD = 0x4;
static const ULONG TIF_CSRSSTHREAD = 0x8;
// not used in an assert, but set in xxxWaitForInputIdle
static const ULONG TIF_WAITFORINPUTIDLE = 0x4000;
// own name, used in PtiFromThreadId
static const ULONG TIF_INITIALIZED = 0x1000000;
static const ULONG TIF_DISABLEIME = 0x2000000;
static const ULONG TIF_DISABLEHOOKS = 0x10000000;
// own name, used in IPostQuitMessage
static const ULONG TIF_QUITMSGPOSTED = 0x40000000;

static const ULONG TIF_VALIDMASK = TIF_INCLEANUP | TIF_16BIT | TIF_SYSTEMTHREAD | TIF_CSRSSTHREAD | TIF_INITIALIZED | TIF_WAITFORINPUTIDLE | TIF_DISABLEIME | TIF_DISABLEHOOKS | TIF_QUITMSGPOSTED;

#define TEST_SET_AND_PRINT(stream, value, flag) \
	if((value) & (flag)) \
	{ \
		(stream) << L"\t" CONCAT(L, #flag) L"\n"; \
	}

typedef struct ThreadMessageInfo_
{
	LONG_PTR messageExtraInfo; // Value passed to SetMessageExtraInfo
	ULONG pendingOperations; // WM_PAINT / WM_TIMER / WM_QUIT to be generated when queue is empty
	ULONG quitMessageExitCode; // only valid if (pendingOperations & WM_QUIT_PENDING) is nonzero
	ULONG TIF_flags; // thread flags
	ULONG threadId; // id of thread
	ULONG numMessages; // number of messages in the 'messages' array
	MsgInfo messages[1];
} ThreadMessageInfo;

// ensure that messages is the last member of the ThreadMessageInfo struct
C_ASSERT((offsetof(ThreadMessageInfo, messages) + sizeof(MsgInfo)) == sizeof(ThreadMessageInfo));

typedef struct WndInfo_
{
	HWND hwnd;
} WndInfo;

typedef struct WndThreadInfo_
{
	ThreadMessageInfo* pTMI; // pointer must be freed with VirtualFree
} WndThreadInfo;

// HF_ flags
static const ULONG HF_GLOBAL = 0x1;
static const ULONG HF_ANSI = 0x2;
static const ULONG HF_NEEDHC_SKIP = 0x4;
static const ULONG HF_HUNG = 0x8;
static const ULONG HF_HOOKFAULTED = 0x10;
static const ULONG HF_NOPLAYBACKDELAY = 0x20;
static const ULONG HF_WX86KNOWNDLL = 0x40;
static const ULONG HF_DESTROYED = 0x80;
static const ULONG HF_INCHECKWHF = 0x100;
static const ULONG HF_FREED = 0x200;

#define TEST_SET_AND_PRINT_HOOK(string, value, flag) \
	if((value) & (flag)) \
	{ \
		(string) += CONCAT(L, #flag) L" "; \
	}

static const ULONG MAX_WINSTA_DESK_NAME_WCHARS = 66;

typedef struct SharedHookInfo_
{
	HANDLE hHook; // the HHOOK value
	UINT_PTR hookFnOffset; // RVA offset in dllname to the start of the hook function
	ULONG dwSettingThread; // 0 for global
	ULONG dwHookedThread;
	ULONG sessionId;
	int hookType; // WH_ values
	ULONG flags; // HF_ flags
	ULONG timeout;
	BOOLEAN lastHookHung;
	WCHAR winstaDeskName[MAX_WINSTA_DESK_NAME_WCHARS]; // null terminated name in Winsta\desktop format
	char settingProcess[16]; // null terminated
	char hookedProcess[16]; // null terminated
} SharedHookInfo;

typedef struct ClientHookInfo_ : SharedHookInfo
{
	WCHAR* pDllName;
} HookInfo;

typedef struct _ClientHookArray
{
	ULONG numHooks;
	HANDLE hNamesHeap;
	HookInfo hooks[1];
} ClientHookArray;

typedef struct WindowsHookInfo_
{
	ClientHookArray* pHookInfoArray;
} WindowsHookInfo;

// Input is a pointer to WndInfo_ struct containing the hwnd of interest
// Output is a pointer to a WndThreadInfo struct
// the pThreadInfo member must be freed with VirtualFree after use
static const ULONG IOCTL_GET_WND_INFO = CTL_CODE(FILE_DEVICE_UNKNOWN, 1, METHOD_BUFFERED, FILE_ANY_ACCESS);

// There is no input
// Output is a WindowsHookInfo*
// the pHookInfoArray member must be freed with the HeapFree(pHookInfoArray->hNamesHeap) after use
// with the heap destructed by HeapDestroy
static const ULONG IOCTL_GET_HOOK_INFO = CTL_CODE(FILE_DEVICE_UNKNOWN, 2, METHOD_BUFFERED, FILE_ANY_ACCESS);

#endif
