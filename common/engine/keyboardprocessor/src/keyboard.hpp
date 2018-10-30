/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_kbp_keyboard_api.cpp
*/

#pragma once

#include <experimental/filesystem>
#include <string>

#include <keyboardprocessor.h>

namespace std {
  namespace filesystem = std::experimental::filesystem;
}

// Forward declartions
class json;

namespace km {
namespace kbp
{

  class keyboard : public km_kbp_keyboard_attrs
  {
    std::string const _keyboard_id;
    std::string const _version_string;
    std::string const _folder_path;

  public:
    keyboard(std::filesystem::path const &);
  };

  inline
  keyboard::keyboard(std::filesystem::path const & path)
  : _keyboard_id(path.stem().string()),
    _version_string("3.145"),
    _folder_path(path.parent_path().string())
  {
    version_string = _version_string.c_str();
    id = _keyboard_id.c_str();
    folder_path = _folder_path.c_str();
    default_options = nullptr;
  }

}
}

struct km_kbp_keyboard : public km::kbp::keyboard {};

json & operator << (json &, km::kbp::keyboard const &);
