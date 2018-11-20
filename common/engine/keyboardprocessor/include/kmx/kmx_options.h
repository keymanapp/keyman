
#pragma once

#include <string>
#include <map>
#include <vector>
#include <keyman/keyboardprocessor.h>

class KMX_Options
{
private:
  //km_kbp_options *m_options;
  //std::map<std::u16string, KMX_Option> m_options;
  //km::kbp::options *_options;
  LPINTKEYBOARDINFO _kp;

  void AddOptionsStoresFromXString(PKMX_WCHAR s);

public:
  KMX_Options(LPINTKEYBOARDINFO kp) : _kp(kp) {}
  ~KMX_Options();

  void Init(std::vector<km_kbp_option_item> *opts);
  void Load(km_kbp_options *options, std::u16string const &key);
  void Set(int nStoreToSet, int nStoreToRead);
  void Reset(km_kbp_options *options, int nStoreToReset);
  void Save(km_kbp_state *state, int nStoreToSave);
};

