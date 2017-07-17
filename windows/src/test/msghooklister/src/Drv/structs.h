#ifndef STRUCTS_H
#define STRUCTS_H

#pragma once

#include "stdafx.h"

// for WIN7
// large fs:124h = ETHREAD
// ETHREAD + 50h = EPROCESS
// EPROCESS + 120h = _W32PROCESS

#define DECLARE_GAP(size) UCHAR CONCAT(gap, __COUNTER__)[(size)]
static const ULONG g_numHooks = 16;

__if_not_exists(MSG)
{
	typedef struct tagPOINT
	{
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

#ifndef _WIN64
typedef UCHAR TLSPRITESTATE[0x60];
typedef UCHAR tagCARET[0x38];
#else
typedef UCHAR TLSPRITESTATE[0xA8];
typedef UCHAR tagCARET[0x44];
#endif

typedef struct tagMSGPPINFO
{
	ULONG dwIndexMsgPP;
} MSGPPINFO;

// same for vista / 7, checked versions have two
// extra tagSMS* ptr members in between psmsNext
// and psmsReceiveNext
struct tagSMS
{
	struct tagSMS* psmsNext;
	struct tagSMS* pSmsReceiveNext;
	struct tagTHREADINFO* ptiSender;
	struct tagTHREADINFO* ptiReceiver;
	PVOID lpResultCallBack;
	ULONG_PTR dwData;
	struct tagTHREADINFO* ptiCallBackSender;
	LONG_PTR lRet;
	ULONG tSent;
	ULONG flags;
	ULONG_PTR wParam;
	LONG_PTR lParam;
	ULONG msg;
	struct tagWND* spwnd;
	PVOID pvCapture;
};
typedef tagSMS SMSMSG, *PSMSMSG;

struct tagQMSG7
{
	struct tagQMSG7* pqmsgNext;
	struct tagQMSG7* pqmsgPrev;
	MSG msg;
	LONG_PTR ExtraInfo;
	POINT ptMouseReal;
	ULONG dwQueueEvent : 0x1e;
	ULONG padding : 2;
	INT Wow64Message : 1;
	INT NoCoalesce : 1;
	INT FromTouch : 1;
	INT FromPen : 1;
	struct tagTHREADINFO* pti;
	MSGPPINFO MsgPPInfo;
};
typedef tagQMSG7 QMSG7, *PQMSG7;

struct tagQMSG6
{
	struct tagQMSG6* pqmsgNext;
	struct tagQMSG6* pqmsgPrev;
	MSG msg;
	LONG_PTR ExtraInfo;
	POINT ptMouseReal;
	ULONG dwQueueEvent : 0x1e;
	ULONG padding : 2;
	struct tagTHREADINFO* pti;
};
typedef tagQMSG6 QMSG6, *PQMSG6;

template<class QMSGType>
struct MLIST
{
	typedef QMSGType* QMSGPtr;
	QMSGType* pqmsgRead;
	QMSGType* pqmsgLastWrite;
	ULONG cMsgs;
};

template<class MsgType>
struct QUEUE
{
	MLIST<MsgType> mlInput; // (input messages WM_MOUSE* / WM_KEY*)
	PVOID ptiSysLock;
	ULONG_PTR idSysLock;
	ULONG_PTR idSysPeek;
	struct tagTHREADINFO* ptiMouse;
	struct tagTHREADINFO* ptiKeyboard;
	struct tagWND* spwndCapture;
	struct tagWND* spwndFocus;
	struct tagWND* spwndActive;
	struct tagWND* spwndActivePrev;
	ULONG codeCapture;
	ULONG msgDblClick;
	USHORT xbtnDblClick;
	ULONG timeDblClk;
	PVOID hwndDblClick;
	POINT ptDblClk;
	POINT ptMouseMove;
	UCHAR afKeyDownRecent[0x20];
	UCHAR afKeyState[0x40];
	tagCARET caret;
	PVOID spCurCurrent;
	INT iCursorLevel;
	ULONG QF_Flags;
	USHORT cThreads;
	USHORT cLockCount;
	ULONG msgJournal;
	ULONG_PTR ExtraInfo;
	ULONG ulEtwReserved1;
};

typedef struct tagCLIENTTHREADINFO
{
	ULONG CTIF_flags;
	USHORT fsChangeBits;
	USHORT fsWakeBits;
	USHORT fsWakeBitsJournal;
	USHORT fsWakeMask;
	ULONG tickLastMsgChecked;
} CLIENTTHREADINFO, *PCLIENTTHREADINFO;

#ifdef _X86_
// server 2008 sp1 x86
// functions of interest
// xxxReceiveMessage
// xxxRealInternalGetMessage
// StoreQMessage (struct tagQMSG)
// xxxDoPaint
// DoTimer
// _DecPaintCount
// _DecTimerCount
struct tagTHREADINFO6
{
	// enabler for the template
	typedef MLIST<QMSG6> MListType;
	typedef QUEUE<QMSG6>* QPtr;

	// members
	PETHREAD pEThread; // 0x0
	DECLARE_GAP(0x28); // 0x4
	PVOID pProcessInfo; // 0x2c
	QPtr pq; // 0x30
	ULONG pNonRBRecursionCount; // 0x34
	PCLIENTTHREADINFO pClientThreadInfo; // 0x38
	struct tagDESKTOP* pDesktop; // 0x3c
	struct tagDESKTOPINFO* pDeskInfo; // 0x40
	DECLARE_GAP(0x4); // 0x44
	PVOID pClientInfo; // 0x48
	ULONG TIF_Flags; // 0x4c
	DECLARE_GAP(0x4); // 0x50
	PSMSMSG psmsSent; // 0x54
	PSMSMSG psmsCurrent; // 0x58
	PSMSMSG psmsReceiveList; // 0x5c (sent messages)
	DECLARE_GAP(0x4); // 0x60
	ULONG idLast; // 0x64
	ULONG exitCode; // 0x68
	DECLARE_GAP(0x4); // 0x6c
	ULONG cPaintsReady; // 0x70
	ULONG cTimersReady; // 0x74
	DECLARE_GAP(0x1c); // 0x78
	struct tagTHREADINFO6* ptiSibling; // 0x94
	DECLARE_GAP(0x4); // 0x98
	ULONG fsHooks; // 0x9c
	DECLARE_GAP(0x3c); // 0xa0
	MListType mlPost; // 0xdc (posted messages)
	ULONG fsChangeBitsRemoved;// 0xe8
	DECLARE_GAP(0x10); // 0xec
	ULONG cVisWindows; // 0xfc
	struct tagHOOK* aphkStart[g_numHooks]; // 0x100
};
typedef tagTHREADINFO6 THREADINFO6, *PTHREADINFO6;

C_ASSERT(offsetof(tagTHREADINFO6, ptiSibling) == 0x94);
C_ASSERT(offsetof(tagTHREADINFO6, aphkStart) == 0x100);

#elif defined(AMD64)

struct tagTHREADINFO6
{
	// enabler for the template
	typedef MLIST<QMSG6> MListType;
	typedef QUEUE<QMSG6>* QPtr;

	// members
	PETHREAD pEThread; // 0x0
	DECLARE_GAP(0x68); // 0x8
	QPtr pq; // 0x70
	DECLARE_GAP(0x8); // 0x78
	PCLIENTTHREADINFO pClientThreadInfo; // 0x80
	struct tagDESKTOP* pDesktop; // 0x88
	struct tagDESKTOPINFO* pDeskInfo; // 0x90
	DECLARE_GAP(0x8); // 0x98
	PVOID pClientInfo; // 0xa0
	ULONG TIF_Flags; // 0xa8
	DECLARE_GAP(0xc); // 0xac
	PSMSMSG psmsSent; // 0xb8
	PSMSMSG psmsCurrent; // 0xc0
	PSMSMSG psmsReceiveList; // 0xc8
	DECLARE_GAP(0x8); // 0xd0
	ULONG64 idLast; // 0xd8
	ULONG exitCode; // 0xe0
	DECLARE_GAP(0xc); // 0xe4
	ULONG cPaintsReady; // 0xF0
	ULONG cTimersReady; // 0xf4
	DECLARE_GAP(0x30); // 0xF8
	struct tagTHREADINFO6* ptiSibling; // 0x128
	DECLARE_GAP(0x8); // 0x130
	ULONG fsHooks; // 0x138
	DECLARE_GAP(0x64); // 0x13c
	MListType mlPost; // 0x1a0
	DECLARE_GAP(0x8); // 0x1b8
	USHORT fsChangeBitsRemoved; // 0x1c0
	DECLARE_GAP(0x14); // 0x1c4
	struct tagHOOK* aphkStart[g_numHooks]; // 0x1d8
};
typedef tagTHREADINFO6 THREADINFO6, *PTHREADINFO6;

C_ASSERT(offsetof(tagTHREADINFO6, ptiSibling) == 0x128);
C_ASSERT(offsetof(tagTHREADINFO6, aphkStart) == 0x1d8);

#else

#error "I'm illiterate at Itanium assembly"

#endif

// win7 rtm layout
// note the layout is different for the RC version 
// as well as checked versions so consult the win32k symbols
// and rejig
struct tagTHREADINFO7
{
	// enabler for the template
	typedef MLIST<QMSG7> MListType;
	typedef QUEUE<QMSG7>* QPtr;

	// members
	PETHREAD pEThread;
	ULONG refCount;
	PVOID ptlW32; // struct _TL*
	PVOID pgdiDcAttrs;
	PVOID pgdiBrushAttrs;
	PVOID pUMPDObjs;
	PVOID pUMPDHeap;
	PVOID pUMPDObj;
#ifdef _WIN64
	PVOID pProxyPort;
	PVOID pClientId;
#endif
	LIST_ENTRY GdiTmpTgoList;
	ULONG pRBRecursionCount;
	ULONG pNonRBRecursionCount;
	TLSPRITESTATE tlSpriteState;
	PVOID pSpriteState;
	PVOID pDevHTInfo;
	ULONG ulDevHTInfoUniqueness;
	PVOID pdcoAA;
	PVOID pdcoRender;
	PVOID pdcoSrc;
	UCHAR bEnableEngUpdateDeviceSurface;
	UCHAR bIncludeSprites;
	ULONG ulWindowSystemRendering;
	ULONG iVisRgnUniqueness;
	PVOID ptl; // struct _TL*
	PVOID pProcessInfo; // struct tagPROCESSINFO
	QPtr pq;
	PVOID spklActive;
	PCLIENTTHREADINFO pClientThreadInfo;
	struct tagDESKTOP* pDesktop;
	struct tagDESKTOPINFO* pDeskInfo;
	ULONG_PTR ulClientDelta;
	PVOID pClientInfo;
	ULONG TIF_Flags;
	PUNICODE_STRING pstrAppName;
	PSMSMSG psmsSent;
	PSMSMSG psmsCurrent;
	PSMSMSG psmsReceiveList;
	LONG timeLast;
	ULONG_PTR idLast;
	INT exitCode;
	PVOID hdesk;
	INT cPaintsReady;
	ULONG cTimersReady;
	PVOID pMenuState;
	union
	{
		PVOID ptdb;
		PVOID pWinSta;
	} tdbOrWinsta;
	PVOID psiiList;
	ULONG dwExpWinVer;
	ULONG dwCompatFlags;
	union
	{
		ULONG dwCompatFlags2;
		ULONG64 qwCompatFlags2;
	} compatFlags2;
	struct tagQ* pqAttach;
	struct tagTHREADINFO7* ptiSibling;
	PVOID pmsd;
	ULONG fsHooks;
	PVOID sphkCurrent;
	LONG_PTR lParamHkCurrent;
	ULONG_PTR wParamHkCurrent;
	PVOID pSBTrack;
	PVOID hEventQueueClient;
	KEVENT* pEventQueueServer;
	LIST_ENTRY PtiLink;
	INT iCursorLevel;
	POINT ptLast;
	POINT ptLastReal;
	struct tagWND* spwndDefaultIme;
	PVOID spDefaultImc;
	PVOID hklPrev;
	INT cEnterCount;
	MListType mlPost;
	USHORT fsChangeBitsRemoved;
	wchar_t wchInjected;
	ULONG fsReserveKeys;
	PKEVENT* apEvent;
	ULONG amDesk;
	UINT cWindows;
	UINT cVisWindows;
	struct tagHOOK* aphkStart[g_numHooks];
	CLIENTTHREADINFO cti;
	PVOID hPrevHidData;
	PVOID hTouchInputCurrent; // HTOUCHINPUT
	PVOID hGestureInfoCurrent; // HGESTUREINFO
	MSGPPINFO MsgPPInfo;
#ifdef _WIN64
	UINT cNestedCalls;
#endif
	UINT cNestedStableVisRgn;
	LIST_ENTRY readyHead;
	ULONG fSpecialInitialization :1;
	ULONG fgfSwitchInProgressSeting :1;
	ULONG fPack :22;
	ULONG fThreadCleanupFinished :1;
	ULONG fETWReserved :3;
	ULONG ulThreadFlags2;
};
typedef tagTHREADINFO7 THREADINFO7, *PTHREADINFO7;

#ifdef _WIN64
C_ASSERT(offsetof(tagTHREADINFO7, pDeskInfo) == 0x180);
C_ASSERT(offsetof(tagTHREADINFO7, ptiSibling) == 0x218);
C_ASSERT(offsetof(tagTHREADINFO7, aphkStart) == 0x2e0);
#else
C_ASSERT(offsetof(tagTHREADINFO7, pDeskInfo) == 0xcc);
C_ASSERT(offsetof(tagTHREADINFO7, ptiSibling) == 0x124);
C_ASSERT(offsetof(tagTHREADINFO7, aphkStart) == 0x198);
#endif

typedef struct _THRDESKHEAD
{
	PVOID h; // user handle (hdesk for desktops, hwnd for windows etc)
	ULONG CLockObj;
	PVOID pti;
	PVOID pDesk;
	PVOID pSelf;
} THRDESKHEAD;

typedef struct _LARGE_UNICODE_STRING
{
	ULONG Length;
	ULONG MaximumLength : 0x1F;
	ULONG bAnsi : 1;
	WCHAR* Buffer;
} LARGE_UNICODE_STRING;

struct tagWND
{
	THRDESKHEAD head;
	ULONG state;
	ULONG state2;
	ULONG exStyle;
	ULONG style;
	PVOID hModule;
	USHORT hMod16;
	USHORT fnid;
	struct tagWND* spwndNext;
	struct tagWND* spwndPrev;
	struct tagWND* spwndParent;
	struct tagWND* spwndChild;
	struct tagWND* spwndOwner;
	LONG rcWindow[4]; // is a tagRECT really
	LONG rcClient[4]; // ditto
	PVOID wndProc;
	PVOID pCls;
	PVOID hrgnUpdate;
	PVOID ppropList;
	PVOID pSBInfo;
	PVOID spMenuSys;
	PVOID spMenu;
	PVOID hrgnClip;
	LARGE_UNICODE_STRING strName;
};

typedef tagWND WND, *PWND;

struct tagHOOK
{
	THRDESKHEAD head;
	struct tagHOOK* phkNext;
	int iHook; // WH_ hook type
	UINT_PTR offPfn; // hook fn offset
	UINT flags;
	int ihmod; // UserGetAtomName will get name of hmod
	THREADINFO7* ptiHooked; // the hooked thread, head.pto is the thread that allocated the hook
	PVOID rpDesk;
	ULONG nTimeout :7;
	ULONG fLastHookHung :1;
};
typedef tagHOOK HOOK, *PHOOK;

typedef struct tagUSERSTARTUPINFO
{
	ULONG cb;
	ULONG dwX;
	ULONG dwY;
	ULONG dwXSize;
	ULONG dwYSize;
	USHORT wShowWindow;
	USHORT cbReserved2;
} USERSTARTUPINFO;

struct tagPROCESSINFO6
{
	PEPROCESS pProcess;
#ifdef _WIN64
	DECLARE_GAP(0xd8);
#else
	DECLARE_GAP(0x7c);
#endif
	PTHREADINFO6 ptiList;
	PTHREADINFO6 ptiMainThread;
};

typedef tagPROCESSINFO6 PROCESSINFO6;
typedef PROCESSINFO6* PPROCESSINFO6;

struct tagPROCESSINFO7
{
	PEPROCESS Process;
	ULONG RefCount;
	ULONG W32PF_Flags;
	PKEVENT InputIdleEvent;
	ULONG StartCursorHideTime;
	PVOID NextStart; // struct _W32PROCESS*
	PVOID pDCAttrList;
	PVOID pBrushAttrList;
	ULONG W32Pid;
	LONG GDIHandleCount;
	ULONG GDIHandleCountPeak;
	LONG UserHandleCount;
	ULONG UserHandleCountPeak;
	ULONG_PTR GDIPushLock; //struct EX_PUSH_LOCK
	RTL_AVL_TABLE GDIEngUserMemAllocTable;
	LIST_ENTRY GDIDcAttrFreeList;
	LIST_ENTRY GDIBrushAttrFreeList;
	LIST_ENTRY GDIW32PIDLockedBitmaps;
	PVOID hSecureGdiSharedHandleTable;
	PVOID DxProcess;
	PVOID ptiList; // THREADINFO*
	PVOID ptiMainThread; // THREADINFO*
	PVOID rpdeskStartup; // tagDESKTOP*
	PVOID pclsPrivateList; // tagCLS*
	PVOID pclsPublicList; // tagCLS*
	PVOID pwpi; // WOWPROCESSINFO*
	struct tagPROCESSINFO* ppiNext;
	struct tagPROCESSINFO* ppiNextRunning;
	UINT cThreads;
	PVOID hdeskStartup; // HDESK
	UINT cSysExpunge;
	ULONG dwhmodLibLoadedMask;
	PVOID ahmodLibLoaded[32];
	PVOID rpwinsta; // WINDOWSTATION*
	PVOID hwinsta; // HWINSTA
	ULONG amwinsta;
	ULONG dwHotKey;
	PVOID hMonitor; // HMONITOR
	PVOID pdvList; // DESKTOPVIEW*
	UINT iClipSerialNumber;
	RTL_BITMAP bmHandleFlags;
	PVOID pCursorCache; // CURSOR*
	PVOID pClientBase;
	ULONG dwLpkEntryPoints;
	PVOID pW32Job; // W32JOB*
	ULONG dwImeCompatFlags;
	LUID luidSession;
	USERSTARTUPINFO usi;
	ULONG Flags;
	UINT fHasMagContext :1;
	UINT Unused :31;
	ULONG dwLayout;
	PVOID pHidTable; // PROCESS_HID_TABLE*
	ULONG dwRegisteredClasses;
	PVOID pvwplWndGCList;
};
typedef tagPROCESSINFO7 PROCESSINFO7;
typedef PROCESSINFO7* PPROCESSINFO7;

// not the whole structure, just what we need
struct tagDESKTOPINFO {

    PVOID pvDesktopBase;
    PVOID pvDesktopLimit;
    PVOID spwnd; // tagWND*
    ULONG fsHooks;
    struct tagHOOK *aphkStart[16];
};
typedef tagDESKTOPINFO DESKTOPINFO, *PDESKTOPINFO;

#ifndef _WIN64
C_ASSERT(offsetof(PROCESSINFO7, pvwplWndGCList) == 0x1ac);
#else
C_ASSERT(offsetof(PROCESSINFO7, pvwplWndGCList) == 0x2f8);
#endif

typedef struct tagWINDOWSTATION
{
	ULONG dwSessionId;
	struct tagWINDOWSTATION* rpWinstaNext;
	struct tagDESKTOP* rpDeskList;
	PVOID pTerm; // struct tagTERMINAL*
	ULONG dwWSF_Flags;
	PVOID spklList; // struct tagKL*
	PVOID ptiClipLock; // struct tagTHREADINFO*
	PVOID ptiDrawingClipboard; // struct tagTHREADINFO*
	PVOID spwndClipOpen; // struct tagWND*
	PVOID spwndClipViewer; // struct tagWND*
	PVOID spwndClipOwner; // struct tagWND*
	PVOID pClipBase; // struct tagCLIP*
	ULONG cNumClipFormats;
	ULONG iClipSerialNumber;
	ULONG iClipSequenceNumber;
	PVOID spwndClipboardListener; // struct tagWND*
	PVOID pGlobalAtomTable; // is actually a PVOID :-O
	LUID luidEndSession;
	LUID luidUser;
	struct tagDESKTOP* pDeskCurrent;
} WINDOWSTATION;
typedef WINDOWSTATION* PWINDOWSTATION;

typedef struct tagDESKTOP
{
	ULONG dwSessionId;
	struct tagDESKTOPINFO* pDeskInfo;
	PVOID pDispInfo; // struct tagDISPLAYINFO*
	struct tagDESKTOP* rpDeskNext;
	PWINDOWSTATION rpwinstaParent;
	ULONG dwDTFlags;
	ULONG_PTR dwDesktopId;
	PVOID spmenuSys; // struct tagMENU*
	PVOID spmenuDialogSys; // struct tagMENU*
	PVOID spmenuHScrollSys; // struct tagMENU*
	PVOID spmenuVScrollSys; // struct tagMENU*
	PVOID spwndForeground; // struct tagWND*
	PVOID spwndTray; // struct tagWND*
	PVOID spwndMessage; // struct tagWND*
	PVOID spwndTooltip; // struct tagWND*
	HANDLE hsectionDesktop;
	struct _HEAP* pHeap;
	ULONG ulHeapSize;
	ULONG cciConsole[2]; // _CONSOLE_CARET_INFO
	LIST_ENTRY PtiList;
	PVOID spwndTrack; // struct tagWND*
	LONG htEx;
	LONG rcMouseHover[4]; // tagRECT
	ULONG dwMouseHoverTime;
	PVOID pMagInputTransform;
} DESKTOP;
typedef DESKTOP *PDESKTOP;

// various bits we need from the SDK
static const HWND HWND_BROADCAST = (HWND)0xffff;
static const HWND HWND_TOP = NULL;
static const HWND HWND_BOTTOM = (HWND)1;
static const HWND HWND_TOPMOST = (HWND)-1;
static const HWND HWND_NOTOPMOST = (HWND)-2;
static const HWND HWND_MESSAGE = (HWND)-3;

static const ULONG ISMEX_SEND = 0x00000001;
static const ULONG ISMEX_NOTIFY = 0x00000002;
static const ULONG ISMEX_CALLBACK = 0x00000004;
static const ULONG ISMEX_REPLIED = 0x00000008;

#endif
