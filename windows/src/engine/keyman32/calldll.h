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

/**
 * Load the all the dlls used by the current keyboard
 * @param   lpkbi  The keyboard for which to load the dlls
 * @return  BOOL  True on success
 */
BOOL LoadDLLs(LPINTKEYBOARDINFO lpkbi);
BOOL UnloadDLLs(LPINTKEYBOARDINFO lpkbi);
BOOL DeactivateDLLs(LPINTKEYBOARDINFO lpkbi);
BOOL ActivateDLLs(LPINTKEYBOARDINFO lpkbi);


BOOL IsIMWindow(HWND hwnd);

// Callback function used by the core processor to call out to 3rd Party Library functions
extern "C" uint8_t IM_CallBackCore(km_core_state *km_state, uint32_t UniqueStoreNo, void *callbackObject);

extern "C" BOOL _declspec(dllexport) WINAPI KMDisplayIM(HWND hwnd, BOOL FShowAlways);
extern "C" BOOL _declspec(dllexport) WINAPI KMHideIM();
void KMUpdateIM();

#endif
