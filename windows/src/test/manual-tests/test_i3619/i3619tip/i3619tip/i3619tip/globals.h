/*
  Name:             globals
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    7 Sep 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Removed ARRAYSIZE define
                    05 Nov 2007 - mcdurdin - CHange msctf reference
                    07 Sep 2009 - mcdurdin - I2095 - Threadsafe
*/
//
// globals.h
//
// Global variable declarations.
//

#ifndef GLOBALS_H
#define GLOBALS_H

#include <windows.h>
#include <ole2.h>
#include <olectl.h>
#include <assert.h>
#include <msctf.h>

//
// Global DLL functions
//

void DllAddRef();
void DllRelease();

//
// Text input control functions
//

void InsertTextAtSelection(TfEditCookie ec, ITfContext *pContext, const WCHAR *pchText, ULONG cchText);

//
// Logging functions
//

void Log(WCHAR *fmt, ...);
void LogPush(char *p);
void LogPop();

//
// Defines
//

//#define ARRAYSIZE(a) (sizeof(a)/sizeof(a[0]))  <-- use _countof()

#define KMTIP_LANGID   -1

#define KMTIP_DESC     L"I3619TIP"
#define KMTIP_DESC_A   "I3619TIP"
#define KMTIP_MODEL    TEXT("Apartment")

#define KMTIP_ICON_INDEX  0

#define UC_SENTINEL    0xFFFF

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

extern const CLSID c_clsidI3619TIP;

extern const GUID c_guidKMTipProfile;

extern const GUID c_guidLangBarItemButton;

extern const GUID c_guidKeymanDeadkeysProp;

#endif // GLOBALS_H
