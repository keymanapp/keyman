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

void FreeKeyboardOptions(LPINTKEYBOARDINFO kp)
{
  assert(kp != NULL);
  assert(kp->Keyboard != NULL);
  assert(kp->KeyboardOptions != NULL);

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

    // We'll go through the globally set options and populate them
    for (int n = 0; n < g_keyboardOptionCount; n++) {
      if (kp->Keyboard->dpStoreArray[nStoreToReset].dpName != NULL && _wcsicmp(kp->Keyboard->dpStoreArray[nStoreToReset].dpName, g_keyboardOption[n].name) == 0)
      {
        PWCHAR val = g_keyboardOption[n].value;
        kp->KeyboardOptions[nStoreToReset].Value = new WCHAR[wcslen(val) + 1];
        wcscpy_s(kp->KeyboardOptions[nStoreToReset].Value, wcslen(val) + 1, val);

        kp->KeyboardOptions[nStoreToReset].OriginalStore = kp->Keyboard->dpStoreArray[nStoreToReset].dpString;
        kp->Keyboard->dpStoreArray[nStoreToReset].dpString = kp->KeyboardOptions[nStoreToReset].Value;
        return;
      }
    }
  }
}


void SaveKeyboardOption(LPINTKEYBOARDINFO kp, int nStoreToSave)
{
  assert(kp != NULL);
  assert(kp->Keyboard != NULL);
  assert(kp->KeyboardOptions != NULL);
  assert(nStoreToSave >= 0);
  assert(nStoreToSave < (int)kp->Keyboard->cxStoreArray);

  if (kp->Keyboard->dpStoreArray[nStoreToSave].dpName == NULL) return;

  //TODO: GetApp()->QueueAction(QIT_SAVEOPTION, ...);
  /*RegistryFullAccess r(HKEY_CURRENT_USER);
  if(r.OpenKey(REGSZ_KeymanActiveKeyboards, TRUE) && r.OpenKey(kp->Name, TRUE) && r.OpenKey(key, TRUE))
  {
    r.WriteString(kp->Keyboard->dpStoreArray[nStoreToSave].dpName, kp->Keyboard->dpStoreArray[nStoreToSave].dpString);
  }*/
}

void LoadKeyboardOptions(LPINTKEYBOARDINFO kp)
{
  assert(kp != NULL);
  assert(kp->Keyboard != NULL);
  assert(kp->KeyboardOptions == NULL);
  
  kp->KeyboardOptions = new INTKEYBOARDOPTIONS[kp->Keyboard->cxStoreArray];
  memset(kp->KeyboardOptions, 0, sizeof(INTKEYBOARDOPTIONS) * kp->Keyboard->cxStoreArray);

  // We'll go through the globally set options and populate them
  for (int n = 0; n < g_keyboardOptionCount; n++) {
    for(DWORD i = 0; i < kp->Keyboard->cxStoreArray; i++)
    {
      if(kp->Keyboard->dpStoreArray[i].dpName != NULL && _wcsicmp(kp->Keyboard->dpStoreArray[i].dpName, g_keyboardOption[n].name) == 0)
      {
        PWCHAR val = g_keyboardOption[n].value;
        kp->KeyboardOptions[i].Value = new WCHAR[wcslen(val)+1];
        wcscpy_s(kp->KeyboardOptions[i].Value, wcslen(val)+1, val);

        kp->KeyboardOptions[i].OriginalStore = kp->Keyboard->dpStoreArray[i].dpString;
        kp->Keyboard->dpStoreArray[i].dpString = kp->KeyboardOptions[i].Value;
        break;
      }
    }
  }
}
