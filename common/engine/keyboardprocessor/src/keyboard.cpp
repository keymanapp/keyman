/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      7 Oct 2018 - TSE - Refactored out of km_kbp_keyboard_api.cpp
*/

#include "keyboard.hpp"
#include "json.hpp"

using namespace km::kbp;

keyboard::keyboard(std::filesystem::path const & path)
: _keyboard_id(path.stem().string()),
  _version_string("3.145"),
  _folder_path(path.parent_path()),
  _default_opts {KM_KBP_OPTIONS_END}
{
  version_string = _version_string.c_str();
  id = _keyboard_id.c_str();
  folder_path = _folder_path.c_str();
  default_options = _default_opts.data();
}


json & km::kbp::operator << (json & j, km::kbp::keyboard const & kb)
{
  j << json::object
      << "id" << kb.id
      << "folder" << kb.folder_path
      << "version" << kb.version_string
      << "rules" << json::array << json::close;

  return j << json::close;
}
