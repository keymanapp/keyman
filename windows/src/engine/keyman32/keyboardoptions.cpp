/*
  Name:             keyboardoptions
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      25 May 2010

  Modified Date:    28 Nov 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    31 May 2010 - mcdurdin - I2397 - Assertion failure - keyboard options loading twice
                    28 Nov 2012 - mcdurdin - I3594 - V9.0 - Remove old SelectKeyboard code and related messages
*/
#include "pch.h"

BOOL IntLoadKeyboardOptionsRegistrytoCore(LPCSTR key, LPINTKEYBOARDINFO kp, km_core_state* const state);
void IntSaveKeyboardOptionCoretoRegistry(LPCSTR REGKey, LPINTKEYBOARDINFO kp, LPCWSTR key, LPCWSTR value);

static km_core_cu* CloneKeymanCoreCP(const km_core_cu* cp) {
  LPCWSTR buf      = reinterpret_cast<LPCWSTR>(cp);
  km_core_cu* clone = new km_core_cu[wcslen(buf) + 1];
  wcscpy_s(reinterpret_cast<LPWSTR>(clone), wcslen(buf) + 1, buf);
  return clone;
}

static km_core_cu* CloneKeymanCoreCPFromWSTR(LPWSTR buf) {
  km_core_cu* clone = new km_core_cu[wcslen(buf) + 1];
  wcscpy_s(reinterpret_cast<LPWSTR>(clone), wcslen(buf) + 1, buf);
  return clone;
}

void SaveKeyboardOptionCoretoRegistry(LPINTKEYBOARDINFO kp, LPCWSTR key, LPCWSTR value) {
  IntSaveKeyboardOptionCoretoRegistry(REGSZ_KeyboardOptions, kp, key, value);
}

void IntSaveKeyboardOptionCoretoRegistry(LPCSTR REGKey, LPINTKEYBOARDINFO kp, LPCWSTR key, LPCWSTR value) {
  assert(REGKey != NULL);
  assert(kp != NULL);
  assert(key);
  assert(value);

  RegistryFullAccess r(HKEY_CURRENT_USER);
  if (r.OpenKey(REGSZ_KeymanActiveKeyboards, TRUE) && r.OpenKey(kp->Name, TRUE) && r.OpenKey(REGKey, TRUE))
  {
    r.WriteString(key, value);
  }
}

void LoadKeyboardOptionsRegistrytoCore(LPINTKEYBOARDINFO kp, km_core_state* const state)
{
  SendDebugEntry();
  IntLoadKeyboardOptionsRegistrytoCore(REGSZ_KeyboardOptions, kp, state);
  SendDebugExit();
}

BOOL IntLoadKeyboardOptionsRegistrytoCore(LPCSTR key, LPINTKEYBOARDINFO kp, km_core_state* const state)
{
  SendDebugEntry();
  assert(key != NULL);
  assert(kp != NULL);

  // Get the list of default options to determine size of list
  const km_core_keyboard_attrs* keyboardAttrs;
  km_core_status err_status = km_core_keyboard_get_attrs(kp->lpCoreKeyboard, &keyboardAttrs);
  if (err_status != KM_CORE_STATUS_OK) {
    SendDebugMessageFormat("km_core_keyboard_get_attrs failed with error status [%d]", err_status);
    return_SendDebugExit(FALSE);
  }

  size_t listSize = km_core_options_list_size(keyboardAttrs->default_options);
  if (listSize == 0){
    return_SendDebugExit(TRUE);
  }
  km_core_option_item* keyboardOpts = new km_core_option_item[listSize + 1];

  RegistryReadOnly r(HKEY_CURRENT_USER);
  BOOL hasData = r.OpenKeyReadOnly(REGSZ_KeymanActiveKeyboards) && r.OpenKeyReadOnly(kp->Name) && r.OpenKeyReadOnly(key);

  int n = 0;
  for (auto kpc = keyboardAttrs->default_options; kpc->key; kpc++) {
    keyboardOpts[n].scope = KM_CORE_OPT_KEYBOARD;
    keyboardOpts[n].key   = kpc->key;
    LPCWSTR coreKey = reinterpret_cast<LPCWSTR>(kpc->key);
    WCHAR val[256];
    if (hasData && r.ReadString(coreKey, val, sizeof(val) / sizeof(val[0])) && val[0]) {
      keyboardOpts[n].value = CloneKeymanCoreCPFromWSTR(val);
    } else {
      keyboardOpts[n].value = CloneKeymanCoreCP(kpc->value);
    }
    n++;
  }
  keyboardOpts[n] = KM_CORE_OPTIONS_END;

  // once we have the option list we can then update the options using the public api call
  err_status = km_core_state_options_update(state, keyboardOpts);
  if (err_status != KM_CORE_STATUS_OK) {
    SendDebugMessageFormat("km_core_state_options_update failed with error status [%d]", err_status);
  }
  for (int i = 0; i < n; i++) {
    delete[] keyboardOpts[i].value;
  }
  delete[] keyboardOpts;
  return_SendDebugExit(TRUE);
}
