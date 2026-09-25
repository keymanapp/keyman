/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_core_keyboard_api.cpp
*/

#pragma once

#include <string>
#include <vector>

#include "keyman_core.h"

#include "option.hpp"
#include "path.hpp"

namespace km {
namespace core
{
  class keyboard_attributes : public km_core_keyboard_attrs
  {
    std::u16string      _keyboard_id;
    std::u16string      _version_string;
    std::vector<option> _default_opts;

    void render();

  public:
    using options_store = decltype(_default_opts);

    keyboard_attributes()
    : km_core_keyboard_attrs {nullptr, nullptr, nullptr} {}
    keyboard_attributes(keyboard_attributes const &) = delete;
    keyboard_attributes(keyboard_attributes &&);

    keyboard_attributes(std::u16string const & id,
             std::u16string const & version,
             options_store const &opts);

    keyboard_attributes & operator = (keyboard_attributes const &) = delete;
    keyboard_attributes & operator = (keyboard_attributes &&);

    options_store const &   default_opts_store() const noexcept { return _default_opts; }
    options_store &         default_opts_store() noexcept { return _default_opts; }
  };

} // namespace core
} // namespace km
