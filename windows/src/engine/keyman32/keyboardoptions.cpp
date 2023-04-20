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

BOOL IntLoadKeyboardOptionsCore(LPCSTR key, LPINTKEYBOARDINFO kp, km_kbp_state* const state);
void IntSaveKeyboardOptionREGCore(LPCSTR REGKey, LPINTKEYBOARDINFO kp, LPCWSTR key, LPWSTR value);

static km_kbp_cp* CloneKMKBPCP(const km_kbp_cp* cp) {
  LPCWSTR buf      = reinterpret_cast<LPCWSTR>(cp);
  km_kbp_cp* clone = new km_kbp_cp[wcslen(buf) + 1];
  wcscpy_s(reinterpret_cast<LPWSTR>(clone), wcslen(buf) + 1, buf);
  return clone;
}

static km_kbp_cp* CloneKMKBPCPFromWSTR(LPWSTR buf) {
  km_kbp_cp* clone = new km_kbp_cp[wcslen(buf) + 1];
  wcscpy_s(reinterpret_cast<LPWSTR>(clone), wcslen(buf) + 1, buf);
  return clone;
}

void SaveKeyboardOptionREGCore(LPINTKEYBOARDINFO kp, LPCWSTR key, LPWSTR value)
{
  IntSaveKeyboardOptionREGCore(REGSZ_KeyboardOptions, kp, key, value);
}

void IntSaveKeyboardOptionREGCore(LPCSTR REGKey, LPINTKEYBOARDINFO kp, LPCWSTR key, LPWSTR value)
{
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

void LoadKeyboardOptionsREGCore(LPINTKEYBOARDINFO kp, km_kbp_state* const state)
{
  SendDebugMessageFormat(0, sdmKeyboard, 0, "LoadKeyboardOptionsREGCore: Enter");
  IntLoadKeyboardOptionsCore(REGSZ_KeyboardOptions, kp, state);
}

BOOL IntLoadKeyboardOptionsCore(LPCSTR key, LPINTKEYBOARDINFO kp, km_kbp_state* const state)
{
  assert(key != NULL);
  assert(kp != NULL);

  // Get the list of default options to determine size of list
  const km_kbp_keyboard_attrs* keyboardAttrs;
  km_kbp_status err_status = km_kbp_keyboard_get_attrs(kp->lpCoreKeyboard, &keyboardAttrs);
  if (err_status != KM_KBP_STATUS_OK) {
    SendDebugMessageFormat(
        0, sdmKeyboard, 0, "LoadKeyboardOptionsREGCore: km_kbp_keyboard_get_attrs failed with error status [%d]", err_status);
    return FALSE;
  }

  size_t listSize = km_kbp_options_list_size(keyboardAttrs->default_options);
  if (listSize == 0){
    return TRUE;
  }
  km_kbp_option_item* keyboardOpts = new km_kbp_option_item[listSize + 1];

  RegistryReadOnly r(HKEY_CURRENT_USER);
  BOOL hasData = r.OpenKeyReadOnly(REGSZ_KeymanActiveKeyboards) && r.OpenKeyReadOnly(kp->Name) && r.OpenKeyReadOnly(key);

  int n = 0;
  for (auto kpc = keyboardAttrs->default_options; kpc->key; kpc++) {
    keyboardOpts[n].scope = KM_KBP_OPT_KEYBOARD;
    keyboardOpts[n].key   = kpc->key;
    LPCWSTR coreKey = reinterpret_cast<LPCWSTR>(kpc->key);
    WCHAR val[256];
    if (hasData && r.ReadString(coreKey, val, sizeof(val) / sizeof(val[0])) && val[0]) {
      keyboardOpts[n].value = CloneKMKBPCPFromWSTR(val);
    } else {
      keyboardOpts[n].value = CloneKMKBPCP(kpc->value);
    }
    n++;
  }
  keyboardOpts[n] = KM_KBP_OPTIONS_END;

  // once we have the option list we can then update the options using the public api call
  err_status = km_kbp_state_options_update(state, keyboardOpts);
  if (err_status != KM_KBP_STATUS_OK) {
    SendDebugMessageFormat(
        0, sdmKeyboard, 0, "LoadKeyboardOptionsREGCore: km_kbp_state_options_update failed with error status [%d]", err_status);
  }
  for (int i = 0; i < n; i++) {
    delete[] keyboardOpts[i].value;
  }
  delete[] keyboardOpts;
  return TRUE;
}

BOOL
UpdateKeyboardOptionsCore(
  km_kbp_state* const lpCoreKeyboardState,
  km_kbp_option_item *lpCoreKeyboardOptions) {

  int listSize = (int)km_kbp_options_list_size(lpCoreKeyboardOptions);
  // Create a option list based on this size look up each key and store the return value in it.
  // then at the end return this options list.
  BOOL changed = FALSE;
  km_kbp_cp const* retValue = nullptr;
  for (int i = 0; i < listSize; i++) {
    km_kbp_status err_status = km_kbp_state_option_lookup(lpCoreKeyboardState, lpCoreKeyboardOptions[i].scope, lpCoreKeyboardOptions[i].key,
        &retValue);
    if (err_status != KM_KBP_STATUS_OK) {
      SendDebugMessageFormat(
          0, sdmKeyboard, 0, "UpdateKeyboardOptionsCore: km_kbp_state_option_lookup failed with error status [%d]", err_status);
      continue;
    }
    // compare to see if changed
    if (wcscmp(reinterpret_cast<LPCWSTR>(retValue), reinterpret_cast<LPCWSTR>(lpCoreKeyboardOptions[i].value)) != 0) {
      delete lpCoreKeyboardOptions[i].value;
      lpCoreKeyboardOptions[i].value = CloneKMKBPCP(retValue);
      changed = TRUE;
    }
  }
  return changed;
}

km_kbp_option_item*
SaveKeyboardOptionsCore(LPINTKEYBOARDINFO kp) {

   // Get the list of default options to determine size of list
  const km_kbp_keyboard_attrs* keyboardAttrs;
  km_kbp_status err_status = km_kbp_keyboard_get_attrs(kp->lpCoreKeyboard, &keyboardAttrs);
  if (err_status != KM_KBP_STATUS_OK) {
    SendDebugMessageFormat(
        0, sdmKeyboard, 0, "SaveKeyboardOptionsCore: km_kbp_keyboard_get_attrs failed with error status [%d]", err_status);
    return nullptr;
  }
  int listSize = (int)km_kbp_options_list_size(keyboardAttrs->default_options);
  km_kbp_option_item* savedKeyboardOpts = new km_kbp_option_item[listSize + 1];
  km_kbp_cp const* retValue = nullptr;

  km_kbp_option_item const* kbDefaultOpts = keyboardAttrs->default_options;
  for (int i = 0; i < listSize; i++, ++kbDefaultOpts) {
    if (kbDefaultOpts->scope != KM_KBP_OPT_KEYBOARD)
      continue;
    err_status =
        km_kbp_state_option_lookup(kp->lpCoreKeyboardState, KM_KBP_OPT_KEYBOARD, kbDefaultOpts->key, &retValue);
    if (err_status != KM_KBP_STATUS_OK) {
      SendDebugMessageFormat(
          0, sdmKeyboard, 0, "SaveKeyboardOptionsCore: km_kbp_state_option_lookup failed with error status [%d]", err_status);
      continue;
    }
    savedKeyboardOpts[i].key   = CloneKMKBPCP(kbDefaultOpts->key);
    savedKeyboardOpts[i].value = CloneKMKBPCP(retValue);
    savedKeyboardOpts[i].scope = KM_KBP_OPT_KEYBOARD;
  }
  savedKeyboardOpts[listSize] = KM_KBP_OPTIONS_END;
  return savedKeyboardOpts;
}

BOOL
RestoreKeyboardOptionsCore(
  km_kbp_state* const lpCoreKeyboardState,
  km_kbp_option_item* lpCoreKeyboardOptions) {
  km_kbp_status err_status = km_kbp_state_options_update(lpCoreKeyboardState, lpCoreKeyboardOptions);
  if (err_status != KM_KBP_STATUS_OK) {
    SendDebugMessageFormat(
        0, sdmKeyboard, 0, "RestoreKeyboardOptionsCore: km_kbp_state_options_update failed with error status [%d]", err_status);
    return FALSE;
  }
  return TRUE;
}

void
DisposeKeyboardOptionsCore(km_kbp_option_item** lpCoreKeyboardOptions) {
  size_t listSize          = km_kbp_options_list_size(*lpCoreKeyboardOptions);
  for (int i = 0; i < (int)listSize; i++) {
    delete[] (*lpCoreKeyboardOptions)[i].key;
    delete[] (*lpCoreKeyboardOptions)[i].value;
  }
  delete[] *lpCoreKeyboardOptions;
  *lpCoreKeyboardOptions = NULL;
}
