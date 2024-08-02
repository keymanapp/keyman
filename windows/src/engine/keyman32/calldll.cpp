/*
  Name:             calldll
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      11 Mar 2009

  Modified Date:    13 Oct 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    03 Oct 2011 - mcdurdin - I3094 - IMX DLLs do not support x64
                    03 Oct 2011 - mcdurdin - I3091 - KMGetContext crashes with assertion failure due to buffer size mismatch
                    24 Jan 2012 - mcdurdin - I3173 - Keyman Developer crash when unloading forced keyboard in rare cases
                    04 Nov 2012 - mcdurdin - I3525 - V9.0 - Merge of I3173 - Keyman can crash when Keyman_StopForcingKeyboard called and keyboard is not already forced
                    05 Nov 2012 - mcdurdin - I3547 - V9.0 Use _countof to tidyup code
                    13 Oct 2014 - mcdurdin - I4452 - V9.0 - Chinese keyboard is not working correctly
*/

#include "pch.h"

typedef BOOL (WINAPI *InitDllFunction)(PSTR name);

/* Add a dll to the list of dlls associated with the keyboard */
// core processor implementation also uses this function
static LPIMDLL AddIMDLL(LPINTKEYBOARDINFO lpkbi, LPSTR kbdpath, LPSTR dllfilename)
{
  DWORD j;

	for(j = 0; j < lpkbi->nIMDLLs; j++)
		if(!_stricmp(lpkbi->IMDLLs[j].Filename, dllfilename)) break;

	if(j < lpkbi->nIMDLLs) return &lpkbi->IMDLLs[j];

	char drive[_MAX_DRIVE], dir[_MAX_DIR], fullname[_MAX_PATH], newdllname[_MAX_FNAME];

	strcpy_s(fullname, _countof(fullname), kbdpath);
	_splitpath_s(fullname, drive, _countof(drive), dir, _countof(dir), NULL, 0, NULL, 0);   // I3547

#ifdef _WIN64
  char dllname[_MAX_FNAME], dllext[_MAX_EXT], newdllext[_MAX_EXT];   // I3094
  _splitpath_s(dllfilename, NULL, 0, NULL, 0, dllname, _countof(dllname), dllext, _countof(dllext));   // I3547
  strcpy_s(newdllext, _countof(newdllext), ".x64");   // I3547
  strcat_s(newdllext, _countof(newdllext), dllext);   // I3547
  _makepath_s(fullname, _countof(fullname), drive, dir, dllname, newdllext);   // I3547
  _makepath_s(newdllname, _countof(newdllname), NULL, NULL, dllname, newdllext);   // I3547
#else
	_makepath_s(fullname, _countof(fullname), drive, dir, dllfilename, NULL);   // I3547
  strcpy_s(newdllname, _countof(newdllname), dllfilename);  // I3094   // I3547
#endif

	/* Load the DLL */

	HINSTANCE hModule = LoadLibrary(fullname); // Try keyboard dir first
	if(!hModule) hModule = LoadLibrary(newdllname); // Try anywhere on the path -- LIBRARY_NAME dir first  // I3094
    if (!hModule) {
      SendDebugMessageFormat(0, sdmKeyboard, 0, "AddIMDLL: hModule not loadded fullname[%s] newdllname[%s]", fullname, newdllname);
      return NULL;
    }
	/* Initialise the DLL */

	InitDllFunction idf = (InitDllFunction) GetProcAddress(hModule, "KeymanIMInit");
	if(idf && !(*idf)(lpkbi->Name))
	{
		FreeLibrary(hModule);
		return NULL;
	}

	/* Add the DLL to the list of DLLs */

	LPIMDLL imd = new IMDLL[lpkbi->nIMDLLs + 1];
	if(lpkbi->nIMDLLs > 0)
	{
		memcpy(imd, lpkbi->IMDLLs, sizeof(IMDLL) * lpkbi->nIMDLLs);
		delete lpkbi->IMDLLs;
	}
	lpkbi->IMDLLs = imd;
	strncpy(imd->Filename, dllfilename, 255);
	imd->Filename[255] = 0;
	imd->nHooks = 0;
	imd->Hooks = NULL;
	imd->hModule = hModule;
	lpkbi->nIMDLLs++;
	return imd;
}

static km_core_action_item*
kmnToCoreActionItem(int ItemType, DWORD dwData, WORD wVkey) {

  km_core_action_item *actionItems = new km_core_action_item[2];
  actionItems[0].type             = KM_CORE_IT_END;
  actionItems[1].type             = KM_CORE_IT_END;
  switch (ItemType) {
  case QIT_CHAR:
    actionItems[0].type = KM_CORE_IT_CHAR;
    actionItems[0].character = dwData;
    break;
  case QIT_DEADKEY:
    actionItems[0].type = KM_CORE_IT_MARKER;
    actionItems[0].marker = dwData;
    break;
  case QIT_BELL:
    actionItems[0].type   = KM_CORE_IT_ALERT;
    break;
  case QIT_BACK:
    switch (dwData) {
    case BK_DEFAULT:
      actionItems[0].type      = KM_CORE_IT_BACK;
      actionItems[0].backspace.expected_type = KM_CORE_BT_CHAR;
      break;
    case BK_DEADKEY:
      actionItems[0].type                    = KM_CORE_IT_BACK;
      actionItems[0].backspace.expected_type = KM_CORE_BT_MARKER;
      break;
    }
    break;
  case QIT_VKEYDOWN:
  case QIT_VKEYUP:
    if (dwData == wVkey) {
      SendDebugMessageFormat(0, sdmKeyboard, 0, "kmnToCoreActionItem: Emit Action:[%s] Key:[%x] ", ItemTypes[ItemType], dwData);
      actionItems[0].type      = KM_CORE_IT_EMIT_KEYSTROKE;
      actionItems[0].character = dwData;
    } else{
      SendDebugMessageFormat(
          0, sdmKeyboard, 0, "kmnToCoreActionItem: Attempt to emit key that is NOT current key pressed [%s] [%x] ", ItemTypes[ItemType], dwData);
    }
    break;
  case QIT_CAPSLOCK:
    SendDebugMessageFormat(0, sdmKeyboard, 0, "kmnToCoreActionItem: Unhandled Action: [%s] [%x] ", ItemTypes[ItemType], dwData);
    break;      ;
   case QIT_INVALIDATECONTEXT:
     actionItems[0].type = KM_CORE_IT_INVALIDATE_CONTEXT;
    break;
  }

  return actionItems;
}

// For debuging allow printing of the intermediate context

#define CONTEXT_CORE 0
#define CONTEXT_INT 1
static BOOL
LogContext(km_core_state *lpCoreKeyboardState, uint8_t context_type) {
  if (!lpCoreKeyboardState) {
    return FALSE;
  }

  km_core_cu* buffer = km_core_state_context_debug(
    lpCoreKeyboardState,
    context_type == CONTEXT_CORE ? KM_CORE_DEBUG_CONTEXT_CACHED : KM_CORE_DEBUG_CONTEXT_INTERMEDIATE
  );
  char* const log_str_title = context_type == CONTEXT_CORE ? "Core Context" : "Intermediate Context";

  SendDebugMessageFormat(0, sdmKeyboard, 0, "%s: %ls", log_str_title, buffer);

  km_core_cu_dispose(buffer);

  return TRUE;
}

    /* Call a DLL callback for all DLLs loaded with a given keyboard */

BOOL CallbackDLLs(LPINTKEYBOARDINFO lpkbi, PSTR cmd)
{
	//SendDebugMessageFormat(0, sdmKeyboard, 0, "ActivateDLLs: Enter");

	for(DWORD i = 0; i < lpkbi->nIMDLLs; i++)
	{
		InitDllFunction idf = (InitDllFunction) GetProcAddress(lpkbi->IMDLLs[i].hModule, cmd);
		if(idf) (*idf)(lpkbi->Name);
	}

	return TRUE;
}

// Both Core and Window keyboard processor can use this function
BOOL UnloadDLLs(LPINTKEYBOARDINFO lpkbi)
{
  if(!lpkbi)
  {
    SetLastError(ERROR_INVALID_PARAMETER);  // I3173   // I3525
    return FALSE;
  }

	CallbackDLLs(lpkbi, "KeymanIMDestroy");

	for(DWORD i = 0; i < lpkbi->nIMDLLs; i++)
	{
		FreeLibrary(lpkbi->IMDLLs[i].hModule);
		if(lpkbi->IMDLLs[i].Hooks) delete lpkbi->IMDLLs[i].Hooks;
	}

	delete lpkbi->IMDLLs;
	lpkbi->IMDLLs = NULL;
	lpkbi->nIMDLLs = 0;

  if (lpkbi->lpCoreKeyboardState) {
          km_core_state_imx_deregister_callback(lpkbi->lpCoreKeyboardState);
  }
	return TRUE;
}

BOOL ActivateDLLs(LPINTKEYBOARDINFO lpkbi)
{
	if(!lpkbi)
  {
    SetLastError(ERROR_INVALID_PARAMETER);  // I3173 - not actually used by this issue but added for completeness   // I3525
    return FALSE;
  }
	CallbackDLLs(lpkbi, "KeymanIMActivate");
	return TRUE;
}

BOOL DeactivateDLLs(LPINTKEYBOARDINFO lpkbi)
{
	if(!lpkbi)
  {
    SetLastError(ERROR_INVALID_PARAMETER);  // I3173   // I3525
    return FALSE;
  }
	CallbackDLLs(lpkbi, "KeymanIMDeactivate");
	return TRUE;
}

// The callback function called by the Core Keyboardprocessor
extern "C" uint8_t IM_CallBackCore(km_core_state *km_state, uint32_t UniqueStoreNo, void *callbackObject) {
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "IM_CallBackCore: Enter");
  if (callbackObject == NULL) {
    return FALSE;
  }
  if (km_state == NULL) {
    return FALSE;
  }
  LPINTKEYBOARDINFO lpkbi = (LPINTKEYBOARDINFO)(callbackObject);
  if (!lpkbi->lpCoreKeyboard) {
    return FALSE;
  }
  // Iterate through hooks to find the third party library function to call
  DWORD n = 0;
  LPIMDLLHOOK imdh = nullptr;
  for (DWORD i = 0; i < lpkbi->nIMDLLs; i++) {
    for (DWORD j = 0; j < lpkbi->IMDLLs[i].nHooks; j++) {
      if (lpkbi->IMDLLs[i].Hooks[j].storeno == UniqueStoreNo) {
        imdh  = &lpkbi->IMDLLs[i].Hooks[j];
        break;
      }
      n++;
    }
  }

  if (!imdh) {
    return FALSE;
  }

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td)
    return FALSE;
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "IM_CallBackCore: td loadeded");

  SendDebugMessageFormat(0, sdmKeyboard, 0, "IM_CallBackCore: td TIPFUpdatable about to call function [%s]", imdh->name);
  LogContext(_td->lpActiveKeyboard->lpCoreKeyboardState, CONTEXT_CORE);
  LogContext(_td->lpActiveKeyboard->lpCoreKeyboardState, CONTEXT_INT);
  (*imdh->function)(GetFocus(), _td->state.vkey, _td->state.charCode, Globals::get_ShiftState());

  //SendDebugMessageFormat(0, sdmKeyboard, 0, "IM_CallBackCore: Exit");
  return TRUE;
}

extern "C" BOOL _declspec(dllexport) WINAPI KMSetOutput(PWSTR buf, DWORD backlen) {
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMSetOutput: Enter");
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td)
    return FALSE;
  if (!_td->app)
    return FALSE;


  if (!_td->lpActiveKeyboard || !_td->lpActiveKeyboard->lpCoreKeyboardState) {
    SendDebugMessageFormat(0, sdmKeyboard, 0, "KMSetOutputCore: no active state");
    return FALSE;
  }
  DWORD numActions = backlen + (DWORD)wcslen(buf);
  DWORD idx = 0;
  km_core_action_item *actionItems = new km_core_action_item[numActions + 1];

  // The actions sent to the core processor need to set the expected_type
  // correctly. To do this need to check the context as we process the
  // backspaces.
  km_core_context_item *citems = nullptr;
  if (KM_CORE_STATUS_OK != km_core_state_get_intermediate_context(_td->lpActiveKeyboard->lpCoreKeyboardState, &citems)) {
    delete[] actionItems;
    return FALSE;
  }

  DWORD context_length = (DWORD)km_core_context_item_list_size(citems);
  WCHAR *contextString = new WCHAR[(context_length * 3) + 1];  // *3 if every context item was a deadkey
  if (!ContextItemToAppContext(citems, contextString, context_length)) {
    km_core_context_items_dispose(citems);
    delete[] contextString;
    delete[] actionItems;
    return FALSE;
  }
  km_core_context_items_dispose(citems);
  AppContext context;
  context.Set(contextString);
  delete[] contextString;

  while (backlen-- > 0) {
    actionItems[idx].type = KM_CORE_IT_BACK;
    WCHAR *CodeUnitPtr;
    const int DeadKeyLength = 3;
    const int SurrogateLength = 2;
    const int SingleCharLength = 1;
    if (context.CharIsDeadkey()) {
      CodeUnitPtr = context.BufMax(DeadKeyLength);
      CodeUnitPtr += 2;
      actionItems[idx].backspace.expected_type  = KM_CORE_BT_MARKER;
      actionItems[idx].backspace.expected_value = (uintptr_t)*CodeUnitPtr;
    } else if (context.CharIsSurrogatePair()) {
      CodeUnitPtr = context.BufMax(SurrogateLength);
      actionItems[idx].backspace.expected_type  = KM_CORE_BT_CHAR;
      actionItems[idx].backspace.expected_value = (DWORD)Uni_SurrogateToUTF32(*CodeUnitPtr, *(CodeUnitPtr + 1));
    } else if (!context.IsEmpty()) {
      CodeUnitPtr = context.BufMax(SingleCharLength);
      actionItems[idx].backspace.expected_type  = KM_CORE_BT_CHAR;
      actionItems[idx].backspace.expected_value = (DWORD)*CodeUnitPtr;
    } else {
      actionItems[idx].backspace.expected_type  = KM_CORE_BT_UNKNOWN;
      actionItems[idx].backspace.expected_value = 0;
    }
    context.Delete();
    idx++;
  }

  while (*buf) {
    actionItems[idx].type      = KM_CORE_IT_CHAR;
    if (Uni_IsSurrogate1(*buf) && Uni_IsSurrogate2(*(buf + 1))) {
      actionItems[idx].character = Uni_SurrogateToUTF32(*buf, *(buf + 1));
      buf++;
    } else {
      actionItems[idx].character = (DWORD)(*buf);
    }
    buf++;
    idx++;
  }
  actionItems[idx].type   = KM_CORE_IT_END;
  if (KM_CORE_STATUS_OK != km_core_state_queue_action_items(_td->lpActiveKeyboard->lpCoreKeyboardState, actionItems)) {
    delete[] actionItems;
    return FALSE;
  }
  delete[] actionItems;
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMSetOutputCore: Exit");
  return TRUE;

}

extern "C" BOOL _declspec(dllexport) WINAPI KMQueueAction(int ItemType, DWORD dwData) {
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMQueueAction: Enter Item type[%lu]",ItemType);
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td)
    return FALSE;
  if (!_td->app)
    return FALSE;

  if (!_td->lpActiveKeyboard || !_td->lpActiveKeyboard->lpCoreKeyboardState) {
    return FALSE;
  }

  km_core_action_item *actionItem = kmnToCoreActionItem(ItemType, dwData, _td->state.vkey);
  km_core_status_codes error_status =
      (km_core_status_codes)km_core_state_queue_action_items(_td->lpActiveKeyboard->lpCoreKeyboardState, actionItem);
  if (error_status != KM_CORE_STATUS_OK) {
    delete[] actionItem;
    SendDebugMessageFormat(0, sdmKeyboard, 0, "KMQueueAction: Error core queue_action_items error status:[%lu]",error_status);
    return FALSE;
  }
  delete[] actionItem;
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMQueueActionCore: Exit");
  return TRUE;

}

extern "C" BOOL _declspec(dllexport) WINAPI KMGetContext(PWSTR buf, DWORD len)
{
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMGetContext: Enter");
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) {
    return FALSE;
  }

	if(!_td->app) {
    return FALSE;
  }

  if (!_td->lpActiveKeyboard || !_td->lpActiveKeyboard->lpCoreKeyboardState) {
    return FALSE;
  }

  km_core_context_item *citems = nullptr;
  if (KM_CORE_STATUS_OK != km_core_state_get_intermediate_context(_td->lpActiveKeyboard->lpCoreKeyboardState, &citems)) {
      return FALSE;
  }

  if (!ContextItemToAppContext(citems, buf, len)) {
    km_core_context_items_dispose(citems);
    return FALSE;
  }
  km_core_context_items_dispose(citems);
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMGetContext: Exit");
  return TRUE;

}

extern "C" BOOL _declspec(dllexport) WINAPI KMDisplayIM(HWND hwnd, BOOL FShowAlways)
{
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMDisplayIM: Enter");
  if(hwnd == 0) return KMHideIM();

	HWND hwndFocus = GetFocus();
	if(hwndFocus == 0) return FALSE;

	if(*Globals::hwndIM() == hwnd)
	{
		*Globals::hwndIMAlways() = FShowAlways;
		return TRUE;
	}

	*Globals::hwndIM() = hwnd;
	*Globals::hwndIMAlways() = FShowAlways;

	POINT IMTop, Caret;
	RECT RectIM, RectApp;
	HWND hFocus;
	int n = 0;

	HMONITOR hMonitor = MonitorFromWindow(hwnd, MONITOR_DEFAULTTONEAREST);
	if (hMonitor == NULL) {
		return FALSE;
	}

	MONITORINFO monitor_info;
	monitor_info.cbSize = sizeof(monitor_info);
	if (!GetMonitorInfo(hMonitor, &monitor_info)) {
		return FALSE;
	}

  if (!GetWindowRect(hwnd, &RectIM)) {
    return FALSE;
  }

  // Only adjust the IM window coordinates if it is not going to be visible on the
  // monitor it is currently showing on. Otherwise leave it to the 3rd party app
  // to control the location of the IM Window.
  // If changing the coordinates to place the IM window use the caret postion of the
  // application that has focus.
  if ((RectIM.left < monitor_info.rcMonitor.left) || (RectIM.right > monitor_info.rcMonitor.right) ||
      (RectIM.top < monitor_info.rcMonitor.top) || (RectIM.bottom > monitor_info.rcMonitor.bottom)) {

    if (!GetCaretPos(&Caret)) {
      return FALSE;
    }

    hFocus = GetFocus();
    if (!GetWindowRect(hFocus, &RectApp)) {
      return FALSE;
    }

    IMTop.x = Caret.x + RectApp.left;
    IMTop.y = Caret.y + RectApp.top + 24;

    if (IMTop.x + RectIM.right - RectIM.left > RectApp.right) {
      IMTop.x = RectApp.right - (RectIM.right - RectIM.left);
    }

    if (IMTop.y + RectIM.bottom - RectIM.top > RectApp.bottom) {
      IMTop.y = RectApp.top + Caret.y - (RectIM.bottom - RectIM.top);
    }
    n = 0; // Move to tracked postion
  } else {
    IMTop.x = 0; // will be ignored by SWP_NOMOVE;
    IMTop.y = 0;
    n = SWP_NOMOVE;
  }

	SetWindowPos(hwnd, HWND_TOPMOST, IMTop.x, IMTop.y, 0, 0, n|SWP_NOSIZE|SWP_SHOWWINDOW|SWP_NOACTIVATE);
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMDisplayIM: Exit");
	return TRUE;
}

extern "C" BOOL _declspec(dllexport) WINAPI KMHideIM()
{
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMHideIM: Enter");
  if(*Globals::hwndIM() != 0)
	{
		ShowWindow(*Globals::hwndIM(), SW_HIDE);
		//PostMessage(hwndIM, WM_USER+2134, (WPARAM) hwndIMOldFocus, 0);
		*Globals::hwndIM() = 0;
		*Globals::hwndIMAlways() = FALSE;
	}
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMHideIM: Exit");
	return TRUE;
}

extern "C" BOOL _declspec(dllexport) WINAPI KMGetActiveKeyboard(PSTR buf, int nbuf)
{
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMGetActiveKeyboard: Enter");
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;
  if(!_td->lpActiveKeyboard) return FALSE;
	strncpy_s(buf, nbuf, _td->lpActiveKeyboard->Name, nbuf-1);
	buf[nbuf-1] = 0;
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMGetActiveKeyboard: Exit");
	return TRUE;
}

extern "C" BOOL _declspec(dllexport) WINAPI KMGetKeyboardPath(PSTR keyboardname, PSTR buf, int nbuf)
{
  return GetKeyboardFileName(keyboardname, buf, nbuf);
}

void KMUpdateIM()
{
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMUpdateIM: Enter");
  if(*Globals::hwndIM() != 0)
	{
		ShowWindow(*Globals::hwndIM(), SW_HIDE);
		//PostMessage(hwndIM, WM_USER+2134, (WPARAM) hwndIMOldFocus, 0);
		*Globals::hwndIM() = 0;
		*Globals::hwndIMAlways() = FALSE;
	}
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "KMUpdateIM: Exit");
}


BOOL IsIMWindow(HWND hwnd)
{
	if(hwnd == *Globals::hwndIM()) return TRUE;
	if(!*Globals::hwndIM()) return FALSE;
	return IsChild(*Globals::hwndIM(), hwnd);
}

//------- All the functions bellow are used for interaction with the core processor.

/* Add a dll hook function to the list of hook functions associated with a single dll */

static BOOL
AddIMDLLHook(LPIMDLL imd, LPSTR funcname, DWORD storeno) {
 //SendDebugMessageFormat(0, sdmKeyboard, 0, "AddIMDLLHook: Enter");
  /* Get the procedure address for the function */
  IMDLLHOOKProc dhp = (IMDLLHOOKProc)GetProcAddress(imd->hModule, funcname);
  if (!dhp)
    return FALSE;

  /* Add the function to the list of functions in the DLL */

  LPIMDLLHOOK hooks = new IMDLLHOOK[imd->nHooks + 1];
  if (imd->nHooks > 0) {
    memcpy(hooks, imd->Hooks, sizeof(IMDLLHOOK) * imd->nHooks);
    delete imd->Hooks;
  }
  imd->Hooks = hooks;
  strncpy(imd->Hooks[imd->nHooks].name, funcname, 31);
  imd->Hooks[imd->nHooks].name[31] = 0;
  imd->Hooks[imd->nHooks].storeno  = storeno;
  imd->Hooks[imd->nHooks].function = dhp;
  imd->nHooks++;
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "AddIMDLLHook: Exit");
  return TRUE;
}

/* Load the dlls associated with a keyboard */
BOOL
LoadDLLs(LPINTKEYBOARDINFO lpkbi) {
  char fullname[_MAX_PATH];
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "LoadDLLs: Enter");

  if (lpkbi->nIMDLLs > 0)
    if (!UnloadDLLs(lpkbi))
      return FALSE;

  if (!GetKeyboardFileName(lpkbi->Name, fullname, _MAX_PATH))
    return FALSE;
  if ((!lpkbi->lpCoreKeyboard) || (!lpkbi->lpCoreKeyboard)){
    return FALSE;
  }

  km_core_keyboard_imx *imx_list = lpkbi->lpIMXList;
  BOOL result = false;
  for (; imx_list->library_name; ++imx_list) {
    LPIMDLL imd = AddIMDLL(lpkbi, fullname, wstrtostr(reinterpret_cast<LPCWSTR>(imx_list->library_name)));
    if (imd && AddIMDLLHook(imd, wstrtostr(reinterpret_cast<LPCWSTR>(imx_list->function_name)), imx_list->imx_id)) {
      result = TRUE;
    }
    else {
      SendDebugMessageFormat(0, sdmKeyboard, 0, "LoadDLLs: Error Loading Library name [%s], Function name [%s]",
        wstrtostr(reinterpret_cast<PCWSTR>(imx_list->library_name)),
        wstrtostr(reinterpret_cast<PCWSTR>(imx_list->function_name)));
    }
  }
  // If result is true register a callback with the core
  if (result) {
    km_core_state_imx_register_callback(lpkbi->lpCoreKeyboardState, IM_CallBackCore, (void *)lpkbi);
  }
  //SendDebugMessageFormat(0, sdmKeyboard, 0, "LoadDLLs: Exit");
  return TRUE;
}

