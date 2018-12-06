/*
  Name:             globals
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Jun 2007

  Modified Date:    14 Aug 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Jun 2007 - mcdurdin - Initial comments
                    24 Oct 2012 - mcdurdin - I3481 - V9.0 - Eliminate unsafe calls in C++
                    17 Nov 2012 - mcdurdin - I3563 - V9.0 - Add output to debug window for KMTIP logging
                    10 Jun 2014 - mcdurdin - I4262 - V9.0 - TSF deadkeys do not function correctly
                    14 Aug 2014 - mcdurdin - I4379 - V9.0 - kmtip should use the Keyman debug logging framework
*/
//   // I4262
// globals.cpp
//
// Global variables.
//

#include "pch.h"

HINSTANCE g_hInst;

LONG g_cRefDll = -1; // -1 /w no refs, for win95 InterlockedIncrement/Decrement compat

CRITICAL_SECTION g_cs;

/* FE0420F1-38D1-4B4C-96BF-E7E20A74CFB7 */
const CLSID c_clsidKMTipTextService = { 0xFE0420F1, 0x38D1, 0x4B4C, { 0x96, 0xBF, 0xE7, 0xE2, 0x0A, 0x74, 0xCF, 0xB7 } };

/*
LOGGING
*/

void Log(WCHAR *fmt, ...) {   // I4379
	WCHAR outbuf[264];
	va_list vars;
	va_start(vars, fmt);
  wcscpy_s(outbuf, L"kmtip: ");
	wvsprintfW(wcschr(outbuf,0), fmt, vars);
  SendDebugMessageFormat(L"%s", outbuf);
}
