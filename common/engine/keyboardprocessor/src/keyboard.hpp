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

#include <keyman/keyboardprocessor.h>

#include "option.hpp"

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
    std::string const                         _keyboard_id;
    std::string const                         _version_string;
    std::filesystem::path::string_type const  _folder_path;
    std::vector<km_kbp_option_item>           _default_opts;

  public:
    keyboard(std::filesystem::path const &);

    friend json & operator << (json &, km::kbp::keyboard const &);
  };

  json & operator << (json &, km::kbp::keyboard const &);
}
}

struct km_kbp_keyboard : public km::kbp::keyboard {};
