/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "pch.h"
#include "kmxtest.h" //TODO: refactor this away

void KMX_Processor::FreeKeyboardOptions(LPINTKEYBOARDINFO kp)
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

void KMX_Processor::SetKeyboardOption(LPINTKEYBOARDINFO kp, int nStoreToSet, int nStoreToRead)
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
   
  kp->KeyboardOptions[nStoreToSet].Value = new WCHAR[u16len(sp->dpString)+1];
  u16cpy(kp->KeyboardOptions[nStoreToSet].Value, /*u16len(sp->dpString)+1,*/ sp->dpString);
  kp->Keyboard->dpStoreArray[nStoreToSet].dpString = kp->KeyboardOptions[nStoreToSet].Value;
}

void KMX_Processor::ResetKeyboardOption(LPINTKEYBOARDINFO kp, int nStoreToReset)
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
      if (kp->Keyboard->dpStoreArray[nStoreToReset].dpName != NULL && u16icmp(kp->Keyboard->dpStoreArray[nStoreToReset].dpName, g_keyboardOption[n].name) == 0)
      {
        PWCHAR val = g_keyboardOption[n].value;
        kp->KeyboardOptions[nStoreToReset].Value = new WCHAR[u16len(val) + 1];
        u16cpy(kp->KeyboardOptions[nStoreToReset].Value, /*u16len(val) + 1,*/ val);

        kp->KeyboardOptions[nStoreToReset].OriginalStore = kp->Keyboard->dpStoreArray[nStoreToReset].dpString;
        kp->Keyboard->dpStoreArray[nStoreToReset].dpString = kp->KeyboardOptions[nStoreToReset].Value;
        return;
      }
    }
  }
}


void KMX_Processor::SaveKeyboardOption(LPINTKEYBOARDINFO kp, int nStoreToSave)
{
  assert(kp != NULL);
  assert(kp->Keyboard != NULL);
  assert(kp->KeyboardOptions != NULL);
  assert(nStoreToSave >= 0);
  assert(nStoreToSave < (int)kp->Keyboard->cxStoreArray);

  if (kp->Keyboard->dpStoreArray[nStoreToSave].dpName == NULL) return;

  //TODO: GetActions()->QueueAction(QIT_SAVEOPTION, ...);
  /*RegistryFullAccess r(HKEY_CURRENT_USER);
  if(r.OpenKey(REGSZ_KeymanActiveKeyboards, TRUE) && r.OpenKey(kp->Name, TRUE) && r.OpenKey(key, TRUE))
  {
    r.WriteString(kp->Keyboard->dpStoreArray[nStoreToSave].dpName, kp->Keyboard->dpStoreArray[nStoreToSave].dpString);
  }*/
}

void KMX_Processor::LoadKeyboardOptions(LPINTKEYBOARDINFO kp)
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
      if(kp->Keyboard->dpStoreArray[i].dpName != NULL && u16icmp(kp->Keyboard->dpStoreArray[i].dpName, g_keyboardOption[n].name) == 0)
      {
        PWCHAR val = g_keyboardOption[n].value;
        kp->KeyboardOptions[i].Value = new WCHAR[u16len(val)+1];
        u16cpy(kp->KeyboardOptions[i].Value, /*u16len(val)+1,*/ val);

        kp->KeyboardOptions[i].OriginalStore = kp->Keyboard->dpStoreArray[i].dpString;
        kp->Keyboard->dpStoreArray[i].dpString = kp->KeyboardOptions[i].Value;
        break;
      }
    }
  }
}
