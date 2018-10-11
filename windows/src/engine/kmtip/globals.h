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
BOOL GetLeftOfSelection(TfEditCookie ec, ITfContext *pContext, WCHAR *buf, LONG n);   // I4933
void GetDeadkeyFlags(TfEditCookie ec, ITfContext *pContext, PWSTR buf, int n);

//
// Logging functions
//

void Log(WCHAR *fmt, ...);
void LogPush(char *p);
void LogPop();

BOOL ShouldDebug_1();   // I4379
int SendDebugMessage_1(char *file, int line, char *msg);   // I4379
int SendDebugMessageFormat_1(char *file, int line, char *fmt, ...);   // I4379
void DebugLastError_1(char *file, int line, char *func, char *date);   // I4379


#define SendDebugMessage(msg) (ShouldDebug() ? SendDebugMessage_1(__FILE__, __LINE__, (msg)) : 0)
#define SendDebugMessageFormat(msg,...) (ShouldDebug() ? SendDebugMessageFormat_1(__FILE__, __LINE__, (msg),__VA_ARGS__) : 0)
#define ShouldDebug() ShouldDebug_1()
#define DebugLastError() (DebugLastError_1(__FILE__,__LINE__,__FUNCTION__,__DATE__))

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

extern const CLSID c_clsidKMTipTextService;

extern const GUID c_guidKeymanDeadkeysProp;

#endif // GLOBALS_H
