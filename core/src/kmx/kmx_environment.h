#pragma once

#include <string>
#include "option.hpp"
#include "kmx_base.h"

namespace km {
namespace core {
namespace kmx {

class KMX_Environment {
private:
  KMX_BOOL _simulateAltGr, _baseLayoutGivesCtrlRAltForRAlt;
  std::u16string _baseLayout, _baseLayoutAlt;
  std::u16string _platform;
  void InitOption(
    std::vector<option> & default_env,
    km_core_cu const * key,
    km_core_cu const * default_value);
public:
  KMX_Environment();
  void Set(std::u16string const & key, std::u16string const & value);
  char16_t const * LookUp(std::u16string const & key) const;

  KMX_BOOL simulateAltGr() const noexcept { return _simulateAltGr; }
  KMX_BOOL baseLayoutGivesCtrlRAltForRAlt() const noexcept { return _baseLayoutGivesCtrlRAltForRAlt; }
  const std::u16string & baseLayout() const noexcept { return _baseLayout; }
  const std::u16string & baseLayoutAlt() const noexcept { return _baseLayoutAlt; }
  const std::u16string & platform() const noexcept { return _platform; }
};

} // namespace kmx
} // namespace core
} // namespace km
