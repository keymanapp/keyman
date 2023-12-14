/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "processor.hpp"
#include "kmx_processevent.h"
#include <option.hpp>

using namespace km::core;
using namespace kmx;

int KMX_Options::_GetIndex(std::u16string const &key) const {
  auto i = 0U;
  ;
  for (auto sp = _kp->Keyboard->dpStoreArray;
       i != _kp->Keyboard->cxStoreArray; ++i, ++sp)
  {
    if (sp->dpName && sp->dpName == key) return i;
  }

  return -1;
}


void KMX_Options::AddOptionsStoresFromXString(PKMX_WCHAR s) {
  auto idx = 0U;
  for (; s && *s; s = incxstr(s)) {
    if (*s == UC_SENTINEL) {
      switch (*(s + 1)) {
      case CODE_IFOPT:
      case CODE_SETOPT:
      case CODE_SAVEOPT:
      case CODE_RESETOPT:
        idx = *(s + 2) - 1;
        if (idx < _kp->Keyboard->cxStoreArray && _kp->Keyboard->dpStoreArray[idx].dpName != NULL) {
          _kp->KeyboardOptions[idx].OriginalStore = _kp->Keyboard->dpStoreArray[idx].dpString;
        }
        break;
      }
    }
  }
}

void KMX_Options::Load(abstract_processor & ap, std::u16string const &key) {
  LPSTORE sp;
  auto i = 0U;

  assert(!key.empty());

  if (key.empty()) return;

  for (i = 0, sp = _kp->Keyboard->dpStoreArray; i < _kp->Keyboard->cxStoreArray; i++, sp++) {
    if (_kp->KeyboardOptions[i].OriginalStore != NULL
        && sp->dpName != NULL
        && u16icmp(sp->dpName, key.c_str()) == 0) {
      Reset(ap, i);
      return;
    }
  }

  // No entry was found.
  assert(false);
}

void KMX_Options::Init(std::vector<option> &opts) {

  opts.clear();

  _kp->KeyboardOptions = new INTKEYBOARDOPTIONS[_kp->Keyboard->cxStoreArray];
  memset(_kp->KeyboardOptions, 0, sizeof(INTKEYBOARDOPTIONS) * _kp->Keyboard->cxStoreArray);

  // Scan all rules to find options references.

  auto i = 0U, j = 0U;
  LPGROUP gp;
  LPKEY kkp;
  for (i = 0U, gp = _kp->Keyboard->dpGroupArray; i < _kp->Keyboard->cxGroupArray; i++, gp++) {
    for (j = 0U, kkp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kkp++) {
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
    opts.emplace_back(); // Terminate the options array
    return;
  }

  // Setup the default options for KPAPI to maintain

  LPSTORE sp;
  for (n = 0, i = 0, ko = _kp->KeyboardOptions, sp = _kp->Keyboard->dpStoreArray; i < _kp->Keyboard->cxStoreArray; i++, sp++, ko++) {
    if (ko->OriginalStore == NULL) continue;
    opts.emplace_back(KM_CORE_OPT_KEYBOARD, sp->dpName, sp->dpString);
    n++;
  }

  opts.emplace_back(); // Terminate the options array
}

KMX_Options::~KMX_Options()
{
  //TODO: move ownership of _kp->KeyboardOptions into this class (so we control lifetime of it here)
  if (!_kp || !_kp->Keyboard || !_kp->KeyboardOptions) return;

  for(KMX_DWORD i = 0; i < _kp->Keyboard->cxStoreArray; i++)
    if(_kp->KeyboardOptions[i].Value)
    {
      _kp->Keyboard->dpStoreArray[i].dpString = _kp->KeyboardOptions[i].OriginalStore;
      delete [] _kp->KeyboardOptions[i].Value;
    }
  delete _kp->KeyboardOptions;
  _kp->KeyboardOptions = NULL;
}


char16_t const * KMX_Options::LookUp(std::u16string const &key) const
{
  auto idx = _GetIndex(key);
  if (idx < 0)  return nullptr;

  return _kp->Keyboard->dpStoreArray[idx].dpString;
}


void KMX_Options::Set(int nStoreToSet, std::u16string const & rValueToSet)
{
  assert(_kp != NULL);
  assert(_kp->Keyboard != NULL);
  assert(_kp->KeyboardOptions != NULL);
  assert(nStoreToSet >= 0);
  assert(nStoreToSet < (int) _kp->Keyboard->cxStoreArray);

  LPSTORE optionStore = &_kp->Keyboard->dpStoreArray[nStoreToSet];

  auto & rStoreToSetValue = _kp->KeyboardOptions[nStoreToSet].Value;
  if(rStoreToSetValue)
  {
    delete [] rStoreToSetValue;
  }

  if(m_debug_items) {
    m_debug_items->push_set_option(m_actions.Length(), optionStore, rValueToSet.c_str());
  }

  rStoreToSetValue = new std::u16string::value_type[rValueToSet.size()+1];
  u16cpy(rStoreToSetValue, /*u16len(sp->dpString)+1,*/ rValueToSet.c_str());
  optionStore->dpString = rStoreToSetValue;
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

  std::u16string const & rStoreToReadValue = _kp->Keyboard->dpStoreArray[nStoreToRead].dpString;
  Set(nStoreToSet, rStoreToReadValue);
}

void KMX_Options::Reset(abstract_processor & ap, int nStoreToReset)
{
  assert(_kp != NULL);
  assert(_kp->Keyboard != NULL);
  assert(_kp->KeyboardOptions != NULL);
  assert(nStoreToReset >= 0);
  assert(nStoreToReset < (int) _kp->Keyboard->cxStoreArray);

  auto & rStoreToReset = _kp->Keyboard->dpStoreArray[nStoreToReset];
  auto & rOptionToReset = _kp->KeyboardOptions[nStoreToReset];
  if (rOptionToReset.Value)
  {
    rStoreToReset.dpString = rOptionToReset.OriginalStore;
    delete [] rOptionToReset.Value;
    rOptionToReset.Value = nullptr;
  }

  if(rStoreToReset.dpName == nullptr) return;

  // Now we need to go back and get any saved value from KPAPI. internal_value is owned by options api
  auto i = ap.persisted_store().find(rStoreToReset.dpName);
  if(i != ap.persisted_store().end()) {
    // Copy the value from KPAPI
    rOptionToReset.Value = new KMX_WCHAR[i->second.size() + 1];
    u16cpy(rOptionToReset.Value, /*u16len(val) + 1,*/ i->second.c_str());
    rStoreToReset.dpString = rOptionToReset.Value;
  }
}


void KMX_Options::Save(int nStoreToSave)
{
  assert(_kp != NULL);
  assert(_kp->Keyboard != NULL);
  assert(_kp->KeyboardOptions != NULL);
  assert(nStoreToSave >= 0);
  assert(nStoreToSave < (int)_kp->Keyboard->cxStoreArray);

  auto const & rStoreToSave = _kp->Keyboard->dpStoreArray[nStoreToSave];
  if (rStoreToSave.dpName == nullptr) return;
  m_actions.QueueAction(QIT_SAVEOPT, nStoreToSave);
}
