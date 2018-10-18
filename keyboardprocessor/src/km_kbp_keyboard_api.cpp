#include <cassert>
#include <algorithm>
#include <experimental/filesystem>
#include <iterator>
#include <sstream>
#include <unordered_map>
#include <string>
#include <vector>

#include <keyboardprocessor.h>
#include <json.hpp>

#include "keyboard.hpp"
#include "option.hpp"



km_kbp_status km_kbp_keyboard_load(char const *kb_path,
                                   km_kbp_keyboard **keyboard)
{
  if (!keyboard)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  auto stat = std::filesystem::status(kb_path);
  //
  // if (stat.type() != std::filesystem::file_type::regular)
  //   return KM_KBP_STATUS_INVALID_ARGUMENT;

  *keyboard = static_cast<km_kbp_keyboard *>(new km::kbp::keyboard("/dev/null/unk_dummy.kmn"));
  return KM_KBP_STATUS_OK;
}

void km_kbp_keyboard_dispose(km_kbp_keyboard *keyboard)
{
  delete keyboard;
}

km_kbp_keyboard_attrs const *
km_kbp_keyboard_get_attrs(km_kbp_keyboard const *keyboard)
{
  return keyboard;
}

json & operator << (json & j, km::kbp::keyboard const & kb)
{
  j << json::object
      << "id" << kb.id
      << "folder" << kb.folder_path
      << "version" << kb.version_string
      << "options" << kb.default_options->target
      << "rules" << json::array << json::close;

  return j << json::close;
}
