/*
  Name:             globals
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    28 Mar 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Removed ARRAYSIZE define
                    05 Nov 2007 - mcdurdin - CHange msctf reference
                    07 Sep 2009 - mcdurdin - I2095 - Threadsafe
                    13 Aug 2014 - mcdurdin - I4375 - V9.0 - Add registry flag deep integration to allow us to disable TIP context
                    14 Aug 2014 - mcdurdin - I4379 - V9.0 - kmtip should use the Keyman debug logging framework
                    28 Mar 2016 - mcdurdin - I4933 - Compat issue with Firefox 42 and IE and Keyman 9 TSF
*/
//
// globals.h
//
// Global variable declarations.
//

#ifndef GLOBALS_H
#define GLOBALS_H

#include "..\..\..\include\kmtip_guids.h"
#define assert(x)

//
// Global DLL functions
//

void DllAddRef();
void DllRelease();

//
// Text input control functions
//

void InsertTextAtSelection(TfEditCookie ec, ITfContext *pContext, const WCHAR *pchText, ULONG cchText);
void DeleteLeftOfSelection(TfEditCookie ec, ITfContext *pContext, LONG n);
BOOL GetLeftOfSelection(TfEditCookie ec, ITfContext *pContext, WCHAR *buf, LONG n, HRESULT *hrGetSelection, ULONG *cFetched, TF_SELECTION *tfSelection);  // I4933
void GetDeadkeyFlags(TfEditCookie ec, ITfContext *pContext, PWSTR buf, int n);

//
// Logging functions
//

// Following macros widen a constant string, ugly it is true
#define __WFILE2__(m) L ## m
#define __WFILE1__(m) __WFILE2__(m)
#define __WFILE__ __WFILE1__(__FILE__)

void Log(WCHAR *fmt, ...);

BOOL ShouldDebug();
int SendDebugMessage_1(wchar_t *file, int line, PWCHAR msg);   // I4379
int SendDebugMessageFormat_1(wchar_t *file, int line, PWCHAR fmt, ...);   // I4379
void DebugLastError_1(wchar_t *file, int line, char *func, PWCHAR msg, DWORD err);

#define SendDebugMessage(msg) (SendDebugMessage_1(__WFILE__, __LINE__, (msg)))
#define SendDebugMessageFormat(msg,...) (SendDebugMessageFormat_1(__WFILE__, __LINE__, (msg),__VA_ARGS__))

#define DebugLastError(msg) (DebugLastError_1(__WFILE__,__LINE__,__FUNCTION__,(msg),GetLastError()))
#define DebugLastError0(msg,err) (DebugLastError_1(__WFILE__,__LINE__,__FUNCTION__,(msg),(err)))

#define LogSUCCEEDED(hr) _LogSUCCEEDED(__WFILE__, __LINE__, __FUNCTIONW__, (#hr), (hr))
BOOL _LogSUCCEEDED(wchar_t *file, int line, PWSTR caller, PSTR callee, HRESULT hr);

//#define LogEnter() (SendDebugMessageFormat_1(__WFILE__, __LINE__, L"%hs ENTER", __FUNCTION__))
//#define LogExit() (SendDebugMessageFormat_1(__WFILE__, __LINE__, L"%hs EXIT", __FUNCTION__))
#define LogEnter()
#define LogExit()

//
// Defines
//

#define KMTIP_DESC_A   "Keyman Text Service"
#define KMTIP_MODEL    TEXT("Apartment")

//#define UC_SENTINEL    0xFFFF

#define SafeRelease(punk)       \
{                               \
    if ((punk) != NULL)         \
    {                           \
        (punk)->Release();      \
    }                           \
}

#define SafeReleaseClear(punk)  \
{                               \
    if ((punk) != NULL)         \
    {                           \
        (punk)->Release();      \
        (punk) = NULL;          \
    }                           \
}

extern HINSTANCE g_hInst;

extern LONG g_cRefDll;

extern CRITICAL_SECTION g_cs;

extern DWORD g_dwDeepIntegration;   // I4375


extern const GUID c_guidKeymanDeadkeysProp;

#endif // GLOBALS_H
