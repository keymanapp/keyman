/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      7 Oct 2018 - TSE - Refactored out of km_core_keyboard_api.cpp
*/
#include "keyboard.hpp"
#include "jsonpp.hpp"

using namespace km::core;


inline
void keyboard_attributes::render()
{
  // Make attributes point to the stored values above.
  id              = _keyboard_id.c_str();
  version_string  = _version_string.c_str();
  default_options = _default_opts.data();
}


keyboard_attributes::keyboard_attributes(std::u16string const & kbid,
    std::u16string const & version,
    options_store const &opts)
: _keyboard_id(kbid),
  _version_string(version),
  _folder_path(""),
  _default_opts(opts)
{
  // Ensure that the default_options array will be properly terminated.
  _default_opts.emplace_back();
  render();
}


keyboard_attributes::keyboard_attributes(keyboard_attributes &&rhs)
: _keyboard_id(std::move(rhs._keyboard_id)),
  _version_string(std::move(rhs._version_string)),
  _folder_path(""),
  _default_opts(std::move(rhs._default_opts))
{
    rhs.id = rhs.version_string = nullptr;
    render();
}


keyboard_attributes & keyboard_attributes::operator = (keyboard_attributes &&rhs)
{
  return *new (this) keyboard_attributes(std::move(rhs));
}


json & km::core::operator << (json & j, km::core::keyboard_attributes const & kb)
{
  j << json::object
      << "id" << kb.id
      << "version" << kb.version_string
      << "rules" << json::array << json::close;

  return j << json::close;
}
