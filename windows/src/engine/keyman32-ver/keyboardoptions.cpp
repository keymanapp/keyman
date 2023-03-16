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

void IntSaveKeyboardOption(LPCSTR key, LPINTKEYBOARDINFO kp, int nStoreToSave);
BOOL IntLoadKeyboardOptions(LPCSTR key, LPINTKEYBOARDINFO kp);
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

void LoadKeyboardOptions(LPINTKEYBOARDINFO kp)
{   // I3594
  IntLoadKeyboardOptions(REGSZ_KeyboardOptions, kp);
}

void LoadSharedKeyboardOptions(LPINTKEYBOARDINFO kp)
{
  if(!DebugAssert(!Globals::get_CoreIntegration(), "LoadSharedKeyboardOptions: Error called in core integration mode")) {
    return;
  }
  // Called when another thread changes keyboard options and we are sharing keyboard settings
  assert(kp != NULL);
  assert(kp->Keyboard != NULL);

  if(kp->KeyboardOptions != NULL) FreeKeyboardOptions(kp);

  IntLoadKeyboardOptions(REGSZ_SharedKeyboardOptions, kp);
}

void FreeKeyboardOptions(LPINTKEYBOARDINFO kp)
{
  // This is a cleanup routine; we don't want to precondition all calls to it
  // so we do not assert
  if (kp == NULL || kp->Keyboard == NULL || kp->KeyboardOptions == NULL)
    return;

  for(DWORD i = 0; i < kp->Keyboard->cxStoreArray; i++)
    if(kp->KeyboardOptions[i].Value)
    {
      kp->Keyboard->dpStoreArray[i].dpString = kp->KeyboardOptions[i].OriginalStore;
      delete kp->KeyboardOptions[i].Value;
    }
  delete kp->KeyboardOptions;
  kp->KeyboardOptions = NULL;
}

void SetKeyboardOption(LPINTKEYBOARDINFO kp, int nStoreToSet, int nStoreToRead)
{
  if (!DebugAssert(!Globals::get_CoreIntegration(), "SetKeyboardOption: Error called in core integration mode")) {
    return;
  }
  assert(kp != NULL);
  assert(kp->Keyboard != NULL);
  assert(kp->KeyboardOptions != NULL);
  assert(nStoreToSet >= 0);
  assert(nStoreToSet < (int) kp->Keyboard->cxStoreArray);
  assert(nStoreToRead >= 0);
  assert(nStoreToRead < (int) kp->Keyboard->cxStoreArray);

  LPSTORE sp = &kp->Keyboard->dpStoreArray[nStoreToRead];
  if(kp->KeyboardOptions[nStoreToSet].Value)
  {
    delete kp->KeyboardOptions[nStoreToSet].Value;
  }
  else
  {
    kp->KeyboardOptions[nStoreToSet].OriginalStore = kp->Keyboard->dpStoreArray[nStoreToSet].dpString;
  }

  kp->KeyboardOptions[nStoreToSet].Value = new WCHAR[wcslen(sp->dpString)+1];
  wcscpy_s(kp->KeyboardOptions[nStoreToSet].Value, wcslen(sp->dpString)+1, sp->dpString);
  kp->Keyboard->dpStoreArray[nStoreToSet].dpString = kp->KeyboardOptions[nStoreToSet].Value;
}

void ResetKeyboardOption(LPINTKEYBOARDINFO kp, int nStoreToReset)
{
  if (!DebugAssert(!Globals::get_CoreIntegration(), "ResetKeyboardOption: Error called in core integration mode")) {
    return;
  }
  assert(kp != NULL);
  assert(kp->Keyboard != NULL);
  assert(kp->KeyboardOptions != NULL);
  assert(nStoreToReset >= 0);
  assert(nStoreToReset < (int) kp->Keyboard->cxStoreArray);

  if(kp->KeyboardOptions[nStoreToReset].Value)
  {
    kp->Keyboard->dpStoreArray[nStoreToReset].dpString = kp->KeyboardOptions[nStoreToReset].OriginalStore;
    delete kp->KeyboardOptions[nStoreToReset].Value;
    kp->KeyboardOptions[nStoreToReset].Value = NULL;

    if(kp->Keyboard->dpStoreArray[nStoreToReset].dpName == NULL) return;

    RegistryReadOnly r(HKEY_CURRENT_USER);
    if(r.OpenKeyReadOnly(REGSZ_KeymanActiveKeyboards) && r.OpenKeyReadOnly(kp->Name) && r.OpenKeyReadOnly(REGSZ_KeyboardOptions))
    {
      if(r.ValueExists(kp->Keyboard->dpStoreArray[nStoreToReset].dpName))
      {
        WCHAR val[256];
        if(!r.ReadString(kp->Keyboard->dpStoreArray[nStoreToReset].dpName, val, sizeof(val) / sizeof(val[0]))) return;
        if(!val[0]) return;
        val[255] = 0;
        kp->KeyboardOptions[nStoreToReset].Value = new WCHAR[wcslen(val)+1];
        wcscpy_s(kp->KeyboardOptions[nStoreToReset].Value, wcslen(val)+1, val);

        kp->KeyboardOptions[nStoreToReset].OriginalStore = kp->Keyboard->dpStoreArray[nStoreToReset].dpString;
        kp->Keyboard->dpStoreArray[nStoreToReset].dpString = kp->KeyboardOptions[nStoreToReset].Value;
      }
    }
  }
}


void SaveKeyboardOption(LPINTKEYBOARDINFO kp, int nStoreToSave)
{
  if (!DebugAssert(!Globals::get_CoreIntegration(), "SaveKeyboardOption: Error called in core integration mode")) {
    return;
  }
  IntSaveKeyboardOption(REGSZ_KeyboardOptions, kp, nStoreToSave);
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

BOOL IntLoadKeyboardOptions(LPCSTR key, LPINTKEYBOARDINFO kp)
{
  assert(key != NULL);
  assert(kp != NULL);
  assert(kp->Keyboard != NULL);
  assert(kp->KeyboardOptions == NULL);

  kp->KeyboardOptions = new INTKEYBOARDOPTIONS[kp->Keyboard->cxStoreArray];
  memset(kp->KeyboardOptions, 0, sizeof(INTKEYBOARDOPTIONS) * kp->Keyboard->cxStoreArray);
  RegistryReadOnly r(HKEY_CURRENT_USER);
  if(r.OpenKeyReadOnly(REGSZ_KeymanActiveKeyboards) && r.OpenKeyReadOnly(kp->Name) && r.OpenKeyReadOnly(key))
  {
    WCHAR buf[256];
    int n = 0;
    while(r.GetValueNames(buf, sizeof(buf) / sizeof(buf[0]), n))
    {
      buf[255] = 0;
      WCHAR val[256];
      if(r.ReadString(buf, val, sizeof(val) / sizeof(val[0])) && val[0])
      {
        val[255] = 0;
        for(DWORD i = 0; i < kp->Keyboard->cxStoreArray; i++)
        {
          if(kp->Keyboard->dpStoreArray[i].dpName != NULL && _wcsicmp(kp->Keyboard->dpStoreArray[i].dpName, buf) == 0)
          {
            kp->KeyboardOptions[i].Value = new WCHAR[wcslen(val)+1];
            wcscpy_s(kp->KeyboardOptions[i].Value, wcslen(val)+1, val);

            kp->KeyboardOptions[i].OriginalStore = kp->Keyboard->dpStoreArray[i].dpString;
            kp->Keyboard->dpStoreArray[i].dpString = kp->KeyboardOptions[i].Value;

            break;
          }
        }
      }
      n++;
    }
    return TRUE;
  }
  return FALSE;
}

void IntSaveKeyboardOption(LPCSTR key, LPINTKEYBOARDINFO kp, int nStoreToSave)
{
  assert(key != NULL);
  assert(kp != NULL);
  assert(kp->Keyboard != NULL);
  assert(kp->KeyboardOptions != NULL);
  assert(nStoreToSave >= 0);
  assert(nStoreToSave < (int) kp->Keyboard->cxStoreArray);

  if(kp->Keyboard->dpStoreArray[nStoreToSave].dpName == NULL) return;

  RegistryFullAccess r(HKEY_CURRENT_USER);
  if(r.OpenKey(REGSZ_KeymanActiveKeyboards, TRUE) && r.OpenKey(kp->Name, TRUE) && r.OpenKey(key, TRUE))
  {
    r.WriteString(kp->Keyboard->dpStoreArray[nStoreToSave].dpName, kp->Keyboard->dpStoreArray[nStoreToSave].dpString);
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
