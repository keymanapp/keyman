
#pragma once

#include <string>
#include <vector>

#include <keyman/keyboardprocessor.h>
#include "option.hpp"

#include "kmx_base.h"

namespace km {
namespace kbp {
namespace kmx {

class KMX_Options
{
private:
  LPINTKEYBOARDINFO _kp;

  void AddOptionsStoresFromXString(PKMX_WCHAR s);

  int _GetIndex(std::u16string const &key) const;

public:
  KMX_Options(LPINTKEYBOARDINFO kp) : _kp(kp) {}
  ~KMX_Options();

  void Init(std::vector<option> &opts);
  void Load(options *options, std::u16string const &key);
  void Set(int nStoreToSet, int nStoreToRead);
  void Set(int nStoreToSet, std::u16string const &value);
  void Set(std::u16string const &key, std::u16string const &value);
  void Reset(options *options, int nStoreToReset);
  void Save(km_kbp_state *state, int nStoreToSave);

  STORE const * begin() const;
  STORE const * end() const;
};

inline
void KMX_Options::Set(std::u16string const &key, std::u16string const &value) {
  auto i = _GetIndex(key);
  if (i != signed(_kp->Keyboard->cxStoreArray))   Set(i, value);
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
} // namespace kbp
} // namespace km
