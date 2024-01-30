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

// Forward declarations
class json;

namespace km {
namespace core
{
  class keyboard_attributes : public km_core_keyboard_attrs
  {
    std::u16string      _keyboard_id;
    std::u16string      _version_string;
    core::path           _folder_path;
    std::vector<option> _default_opts;

    void render();

  public:
    using options_store = decltype(_default_opts);
    using path_type = decltype(_folder_path);

    keyboard_attributes()
    : km_core_keyboard_attrs {nullptr, nullptr, nullptr, nullptr} {}
    keyboard_attributes(keyboard_attributes const &) = delete;
    keyboard_attributes(keyboard_attributes &&);

    keyboard_attributes(std::u16string const & id,
             std::u16string const & version,
             path_type const & path,
             options_store const &opts);

    keyboard_attributes & operator = (keyboard_attributes const &) = delete;
    keyboard_attributes & operator = (keyboard_attributes &&);

    friend json & operator << (json &, km::core::keyboard_attributes const &);

    options_store const &   default_opts_store() const noexcept { return _default_opts; }
    options_store &         default_opts_store() noexcept { return _default_opts; }

    path_type const     &   path() const noexcept { return _folder_path; }
  };

  json & operator << (json &, km::core::keyboard_attributes const &);

} // namespace core
} // namespace km
