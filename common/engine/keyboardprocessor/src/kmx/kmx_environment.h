#pragma once

#include <string>
#include "option.hpp"
#include "kmx_base.h"

namespace km {
namespace kbp {
namespace kmx {

class KMX_Environment {
private:
  KMX_BOOL _simulateAltGr, _baseLayoutGivesCtrlRAltForRAlt;
  std::u16string _baseLayout, _baseLayoutAlt;
  KMX_BOOL _capsLock;
  std::u16string _platform;
  void InitOption(
    std::vector<option> & default_env,
    km_kbp_cp const * key,
    km_kbp_cp const * default_value);
public:
  KMX_Environment();
  void Load(std::u16string const & key, std::u16string const & value);
  void Init(std::vector<option> &default_env);

  KMX_BOOL capsLock() const noexcept { return _capsLock; }
  KMX_BOOL simulateAltGr() const noexcept { return _simulateAltGr; }
  KMX_BOOL baseLayoutGivesCtrlRAltForRAlt() const noexcept { return _baseLayoutGivesCtrlRAltForRAlt; }
  const std::u16string & baseLayout() const noexcept { return _baseLayout; }
  const std::u16string & baseLayoutAlt() const noexcept { return _baseLayoutAlt; }
  const std::u16string & platform() const noexcept { return _platform; }
};

} // namespace kmx
} // namespace kbp
} // namespace km
