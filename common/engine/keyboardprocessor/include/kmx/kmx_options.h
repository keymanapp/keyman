
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
  LPINTKEYBOARDINFO _kp;

  void AddOptionsStoresFromXString(PKMX_WCHAR s);

public:
  KMX_Options(LPINTKEYBOARDINFO kp) : _kp(kp) {}
  ~KMX_Options() {}

  void Load(std::vector<km_kbp_option_item> *opts);
};

