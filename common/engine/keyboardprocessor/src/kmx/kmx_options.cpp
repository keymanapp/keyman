/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "kmx_processor.h"
#include <option.hpp>
#include <state.hpp>

using namespace km::kbp::kmx;

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

void KMX_Options::Load(km::kbp::options *options, std::u16string const &key) {
  LPSTORE sp;
  int i;

  assert(options != nullptr);
  assert(!key.empty());

  if (options == nullptr || key.empty()) return;
  
  for (i = 0, sp = _kp->Keyboard->dpStoreArray; i < _kp->Keyboard->cxStoreArray; i++, sp++) {
    if (_kp->KeyboardOptions[i].OriginalStore != NULL && sp->dpName != NULL && u16icmp(sp->dpName, key.c_str()) == 0) {
      Reset(options, i);
      return;
    }
  }

  // No entry was found.
  assert(false);
}

void KMX_Options::Init(std::vector<km_kbp_option_item> *opts) {

  opts->clear();

  _kp->KeyboardOptions = new INTKEYBOARDOPTIONS[_kp->Keyboard->cxStoreArray];
  memset(_kp->KeyboardOptions, 0, sizeof(INTKEYBOARDOPTIONS) * _kp->Keyboard->cxStoreArray);

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

KMX_Options::~KMX_Options()
{
  //TODO: move ownership of _kp->KeyboardOptions into this class (so we control lifetime of it here)
  if (!_kp || !_kp->Keyboard || !_kp->KeyboardOptions) return;

  for(KMX_DWORD i = 0; i < _kp->Keyboard->cxStoreArray; i++)
    if(_kp->KeyboardOptions[i].Value)
    {
      _kp->Keyboard->dpStoreArray[i].dpString = _kp->KeyboardOptions[i].OriginalStore;
      delete _kp->KeyboardOptions[i].Value;
    }
  delete _kp->KeyboardOptions;
  _kp->KeyboardOptions = NULL;
}

void KMX_Options::Set(int nStoreToSet, int nStoreToRead)
{
  assert(_kp != NULL);
  assert(_kp->Keyboard != NULL);
  assert(_kp->KeyboardOptions != NULL);
  assert(nStoreToSet >= 0);
  assert(nStoreToSet < (int) _kp->Keyboard->cxStoreArray);
  assert(nStoreToRead >= 0);
  assert(nStoreToRead < (int) _kp->Keyboard->cxStoreArray);

  LPSTORE sp = &_kp->Keyboard->dpStoreArray[nStoreToRead];
  if(_kp->KeyboardOptions[nStoreToSet].Value)
  {
    delete _kp->KeyboardOptions[nStoreToSet].Value;
  }
   
  _kp->KeyboardOptions[nStoreToSet].Value = new KMX_WCHAR[u16len(sp->dpString)+1];
  u16cpy(_kp->KeyboardOptions[nStoreToSet].Value, /*u16len(sp->dpString)+1,*/ sp->dpString);
  _kp->Keyboard->dpStoreArray[nStoreToSet].dpString = _kp->KeyboardOptions[nStoreToSet].Value;
}

void KMX_Options::Reset(km::kbp::options *options, int nStoreToReset)
{
  assert(_kp != NULL);
  assert(_kp->Keyboard != NULL);
  assert(_kp->KeyboardOptions != NULL);
  assert(nStoreToReset >= 0);
  assert(nStoreToReset < (int) _kp->Keyboard->cxStoreArray);

  if (_kp->KeyboardOptions[nStoreToReset].Value)
  {
    _kp->Keyboard->dpStoreArray[nStoreToReset].dpString = _kp->KeyboardOptions[nStoreToReset].OriginalStore;
    delete _kp->KeyboardOptions[nStoreToReset].Value;
    _kp->KeyboardOptions[nStoreToReset].Value = NULL;
  }

  if(_kp->Keyboard->dpStoreArray[nStoreToReset].dpName == NULL) return;

  // Now we need to go back and get any saved value from KPAPI. internal_value is owned by options api
  km_kbp_cp const *internal_value = options->lookup(km_kbp_option_scope(KM_KBP_OPT_KEYBOARD), _kp->Keyboard->dpStoreArray[nStoreToReset].dpName);
  if(internal_value) {
    // Copy the value from KPAPI
    _kp->KeyboardOptions[nStoreToReset].Value = new KMX_WCHAR[u16len(internal_value) + 1];
    u16cpy(_kp->KeyboardOptions[nStoreToReset].Value, /*u16len(val) + 1,*/ internal_value);
    _kp->Keyboard->dpStoreArray[nStoreToReset].dpString = _kp->KeyboardOptions[nStoreToReset].Value;
  }
}


void KMX_Options::Save(km_kbp_state *state, int nStoreToSave)
{
  assert(_kp != NULL);
  assert(_kp->Keyboard != NULL);
  assert(_kp->KeyboardOptions != NULL);
  assert(nStoreToSave >= 0);
  assert(nStoreToSave < (int)_kp->Keyboard->cxStoreArray);

  if (_kp->Keyboard->dpStoreArray[nStoreToSave].dpName == NULL) return;

  // TSE QUERY: Does this happen here or with an ACTION? Or both?
  state->options().assign(state, KM_KBP_OPT_KEYBOARD, _kp->Keyboard->dpStoreArray[nStoreToSave].dpName, _kp->Keyboard->dpStoreArray[nStoreToSave].dpString);
  //TODO: GetActions()->QueueAction(QIT_SAVEOPTION, ...);
  /*RegistryFullAccess r(HKEY_CURRENT_USER);
  if(r.OpenKey(REGSZ_KeymanActiveKeyboards, TRUE) && r.OpenKey(_kp->Name, TRUE) && r.OpenKey(key, TRUE))
  {
    r.WriteString(_kp->Keyboard->dpStoreArray[nStoreToSave].dpName, _kp->Keyboard->dpStoreArray[nStoreToSave].dpString);
  }*/
}
