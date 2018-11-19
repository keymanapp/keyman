/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include <kmx/kmx_processor.h>
#include <option.hpp>

void KMX_Options::AddOptionsStoresFromXString(PKMX_WCHAR s) {
  int idx;
  for (; s && *s; s = incxstr(s)) {
    if (*s == UC_SENTINEL) {
      switch (*(s + 1)) {
      case CODE_IFOPT:
      case CODE_SETOPT:
      case CODE_SAVEOPT:
      case CODE_RESETOPT:
        idx = *(s + 2) - 1;
        if (idx >= 0 && idx < _kp->Keyboard->cxStoreArray && _kp->Keyboard->dpStoreArray[idx].dpName != NULL) {
          _kp->KeyboardOptions[idx].OriginalStore = _kp->Keyboard->dpStoreArray[idx].dpString;
        }
        break;
      }
    }
  }
}

void KMX_Options::Load(std::vector<km_kbp_option_item> *opts) {

  opts->clear();

  // Scan all rules to find options references.

  int i, j;
  LPGROUP gp;
  LPKEY kkp;
  for (i = 0, gp = _kp->Keyboard->dpGroupArray; i < _kp->Keyboard->cxGroupArray; i++, gp++) {
    for (j = 0, kkp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kkp++) {
      AddOptionsStoresFromXString(kkp->dpContext);
      AddOptionsStoresFromXString(kkp->dpOutput);
    }
    AddOptionsStoresFromXString(gp->dpMatch);
    AddOptionsStoresFromXString(gp->dpNoMatch);
  }

  LPINTKEYBOARDOPTIONS ko;
  int n = 0;
  for (i = 0, ko = _kp->KeyboardOptions; i < _kp->Keyboard->cxStoreArray; i++, ko++) {
    if (ko->OriginalStore != NULL) {
      n++;
    }
  }

  if (n == 0) {
    km_kbp_option_item opt = KM_KBP_OPTIONS_END;
    opts->emplace_back(opt);
    return;
  }

  // Setup the default options for KPAPI to maintain

  LPSTORE sp;
  for (n = 0, i = 0, ko = _kp->KeyboardOptions, sp = _kp->Keyboard->dpStoreArray; i < _kp->Keyboard->cxStoreArray; i++, sp++, ko++) {
    if (ko->OriginalStore == NULL) continue;
    km_kbp_option_item opt;
    opt.key = sp->dpName;
    opt.value = sp->dpString;
    opt.scope = KM_KBP_OPT_KEYBOARD;
    opts->emplace_back(opt);
    n++;
  }

  km_kbp_option_item opt = KM_KBP_OPTIONS_END;
  opts->emplace_back(opt);
}

/*
  auto p_options = options->get(KM_KBP_OPT_KEYBOARD);

  for (auto it = p_options; it->key != NULL; it++) {
    switch (it->scope) {
    case KM_KBP_OPT_ENVIRONMENT:
      // TODO load env
      break;
    case KM_KBP_OPT_KEYBOARD:
      for (int i = 0; i < kp->Keyboard->cxStoreArray; i++) {
        if (kp->Keyboard->dpStoreArray[i].dpName != NULL && u16icmp(kp->Keyboard->dpStoreArray[i].dpName, it->key) == 0)
        {
          PKMX_WCHAR val = it->value;
          kp->KeyboardOptions[i].Value = new KMX_WCHAR[u16len(val) + 1];
          u16cpy(kp->KeyboardOptions[i].Value, /*u16len(val)+1,* / val);

          kp->KeyboardOptions[i].OriginalStore = kp->Keyboard->dpStoreArray[i].dpString;
          kp->Keyboard->dpStoreArray[i].dpString = kp->KeyboardOptions[i].Value;
          break;
        }
      }
      // Do we log the missing option?
      break;
    }
  }
}
*/

/*km_kbp_option_item & KMX_Options::Get(std::u16string key) {
  km_kbp_option_item x;
  return x;
  //return nullptr;
}*/
//void KMX_Options::Lookup

void KMX_Processor::FreeKeyboardOptions(LPINTKEYBOARDINFO kp)
{
  assert(kp != NULL);
  assert(kp->Keyboard != NULL);
  assert(kp->KeyboardOptions != NULL);

  for(KMX_DWORD i = 0; i < kp->Keyboard->cxStoreArray; i++)
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
   
  kp->KeyboardOptions[nStoreToSet].Value = new KMX_WCHAR[u16len(sp->dpString)+1];
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
        PKMX_WCHAR val = g_keyboardOption[n].value;
        kp->KeyboardOptions[nStoreToReset].Value = new KMX_WCHAR[u16len(val) + 1];
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
    for(KMX_DWORD i = 0; i < kp->Keyboard->cxStoreArray; i++)
    {
      if(kp->Keyboard->dpStoreArray[i].dpName != NULL && u16icmp(kp->Keyboard->dpStoreArray[i].dpName, g_keyboardOption[n].name) == 0)
      {
        PKMX_WCHAR val = g_keyboardOption[n].value;
        kp->KeyboardOptions[i].Value = new KMX_WCHAR[u16len(val)+1];
        u16cpy(kp->KeyboardOptions[i].Value, /*u16len(val)+1,*/ val);

        kp->KeyboardOptions[i].OriginalStore = kp->Keyboard->dpStoreArray[i].dpString;
        kp->Keyboard->dpStoreArray[i].dpString = kp->KeyboardOptions[i].Value;
        break;
      }
    }
  }
}
