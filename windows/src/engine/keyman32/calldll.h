/*
  Name:             calldll
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      11 Dec 2009

  Modified Date:    11 Dec 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
*/

#ifndef __CALLDLL_H
#define __CALLDLL_H

#include "IntKeyboardInfo.h"

BOOL LoadDLLs(LPINTKEYBOARDINFO lpkbi);
BOOL UnloadDLLs(LPINTKEYBOARDINFO lpkbi);
BOOL DeactivateDLLs(LPINTKEYBOARDINFO lpkbi);
BOOL ActivateDLLs(LPINTKEYBOARDINFO lpkbi);

BOOL IsIMWindow(HWND hwnd);

void CallDLL(LPINTKEYBOARDINFO lpkbi, DWORD storenum);

extern "C" BOOL _declspec(dllexport) WINAPI KMDisplayIM(HWND hwnd, BOOL FShowAlways);
extern "C" BOOL _declspec(dllexport) WINAPI KMHideIM();
void KMUpdateIM();

#endif
