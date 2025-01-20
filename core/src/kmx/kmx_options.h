
#pragma once

#include <string>
#include <vector>

#include "keyman_core.h"
#include "option.hpp"

#include "kmx_base.h"
#include "kmx_debugger.h"

namespace km {
namespace core {

class abstract_processor;

namespace kmx {

class KMX_Options
{
private:
  LPINTKEYBOARDINFO _kp;
  KMX_DebugItems *m_debug_items;
  KMX_Actions& m_actions;

  void AddOptionsStoresFromXString(PKMX_WCHAR s);

  int _GetIndex(std::u16string const &key) const;

public:
  KMX_Options(LPINTKEYBOARDINFO kp, KMX_Actions& actions) : _kp(kp), m_actions(actions) {
    m_debug_items = nullptr;
  }
  ~KMX_Options();

  void Init(std::vector<option> &opts);
  void Load(abstract_processor &, std::u16string const &key);
  char16_t const * LookUp(std::u16string const &key) const;
  void Set(int nStoreToSet, int nStoreToRead);
  void Set(int nStoreToSet, std::u16string const &value);
  void Set(std::u16string const &key, std::u16string const &value);
  void Reset(abstract_processor &, int nStoreToReset);
  void Save(int nStoreToSave);

  void SetInternalDebugItems(KMX_DebugItems *debug_items) {
    m_debug_items = debug_items;
  }

  STORE const * begin() const;
  STORE const * end() const;
};

inline
void KMX_Options::Set(std::u16string const &key, std::u16string const &value) {
  auto i = _GetIndex(key);
  if (i >= 0)   Set(i, value);
}

inline
STORE const * KMX_Options::begin() const {
  return _kp->Keyboard->dpStoreArray;
}

inline
STORE const * KMX_Options::end() const {
  return _kp->Keyboard->dpStoreArray + _kp->Keyboard->cxStoreArray;
}

} // namespace kmx
} // namespace core
} // namespace km
