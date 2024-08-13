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
/**
 * Retrieves the text to the left of the current selection in the context.
 * In addition it also returns the output values of the TSF call 'GetSelection'.
 * 
 * @param[in]   ec               The edit cookie.
 * @param[in]   pContext         The text input context.
 * @param[out]  buf              The buffer to store the text.
 * @param[in]   n                The maximum number of characters to retrieve.
 * @param[out]  hrGetSelection   The result of the 'GetSelection' call.
 * @param[out]  cFetched         The number of selections fetched.
 * @param[out]  tfSelection      The selection details.
 * @return                       TRUE if successful, otherwise FALSE.
 */
BOOL GetLeftOfSelection(TfEditCookie ec, ITfContext *pContext, WCHAR *buf, LONG n, HRESULT *hrGetSelection, ULONG *cFetched, TF_SELECTION *tfSelection);  // I4933
void GetDeadkeyFlags(TfEditCookie ec, ITfContext *pContext, PWSTR buf, int n);

//
// Logging functions
//

// Following macros widen a constant string, ugly it is true
#define __WIDEN2__(m)  L ## m
#define __WIDEN1__(m)  __WIDEN2__(m)

#define __WIDEN__(m)  __WIDEN1__(m)
#define __WFILE__ __WIDEN1__(__FILE__)
#define __WFUNCTION__ __WIDEN1__(__FUNCTION__)

BOOL ShouldDebug();
int SendDebugMessage_1(wchar_t *file, int line, wchar_t *function, PWCHAR msg);   // I4379
int SendDebugMessageFormat_1(wchar_t *file, int line, wchar_t *function, PWCHAR fmt, ...);   // I4379
void DebugLastError_1(wchar_t *file, int line, wchar_t *function, PWCHAR msg, DWORD err);

#define SendDebugMessage(msg) (SendDebugMessage_1(__WFILE__, __LINE__, __WFUNCTION__, (msg)))
#define SendDebugMessageFormat(msg,...) (SendDebugMessageFormat_1(__WFILE__, __LINE__, __WFUNCTION__, (msg),__VA_ARGS__))

#define DebugLastError(msg) (DebugLastError_1(__WFILE__,__LINE__,__WFUNCTION__,(msg),GetLastError()))
#define DebugLastError0(msg,err) (DebugLastError_1(__WFILE__,__LINE__,__WFUNCTION__,(msg),(err)))

#define LogSUCCEEDED(hr) _LogSUCCEEDED(__WFILE__, __LINE__, __WFUNCTION__, (#hr), (hr))
BOOL _LogSUCCEEDED(wchar_t *file, int line, PWSTR caller, PSTR callee, HRESULT hr);

#define SendDebugEntry() (ShouldDebug() ? SendDebugEntry_1(__WFILE__, __LINE__, __WFUNCTION__) : 0)
#define SendDebugExit() (ShouldDebug() ? SendDebugExit_1(__WFILE__, __LINE__, __WFUNCTION__) : 0)
#define return_SendDebugExit(v) { SendDebugExit(); return v; }

int SendDebugEntry_1(wchar_t* file, int line, wchar_t* function);
int SendDebugExit_1(wchar_t* file, int line, wchar_t* function);

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
