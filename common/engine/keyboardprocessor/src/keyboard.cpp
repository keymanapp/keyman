/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      7 Oct 2018 - TSE - Refactored out of km_kbp_keyboard_api.cpp
*/

#include <codecvt>
#include "keyboard.hpp"
#include "json.hpp"
#include "processor.hpp"
#include "utfcodec.hpp"

using namespace km::kbp;


inline
void keyboard_attributes::render()
{
  // Make attributes point to the stored values above.
  id              = _keyboard_id.c_str();
  version_string  = _version_string.c_str();
  folder_path     = _folder_path.c_str();
  default_options = _default_opts.data();
}


keyboard_attributes::keyboard_attributes(std::u16string const & kbid,
    std::u16string const & version,
    path_type const & path,
    options_store const &opts)
: _keyboard_id(kbid),
  _version_string(version),
  _folder_path(path),
  _default_opts(opts)
{
  // Ensure that the default_options array will be properly terminated.
  _default_opts.push_back(option());
  render();
}


keyboard_attributes::keyboard_attributes(keyboard_attributes &&rhs)
: _keyboard_id(std::move(rhs._keyboard_id)),
  _version_string(std::move(rhs._version_string)),
  _folder_path(std::move(rhs._folder_path)),
  _default_opts(std::move(rhs._default_opts))
{
    rhs.id = rhs.version_string = nullptr;
    render();
}


keyboard_attributes & keyboard_attributes::operator = (keyboard_attributes &&rhs)
{
  return *new (this) keyboard_attributes(std::move(rhs));
}


json & km::kbp::operator << (json & j, km::kbp::keyboard_attributes const & kb)
{
  j << json::object
      << "id" << std::u16string(kb.id)
      << "folder" << kb._folder_path
      << "version" << std::u16string(kb.version_string)
      << "rules" << json::array << json::close;

  return j << json::close;
}

/*
  This function exists because of a bug in Visual Studio 2015 and 2017:
  https://social.msdn.microsoft.com/Forums/en-US/8f40dcd8-c67f-4eba-9134-a19b9178e481/vs-2015-rc-linker-stdcodecvt-error?forum=vcgeneral
  https://stackoverflow.com/a/35103224/1836776
*/

// #if _MSC_VER >= 1900 /* VS 2015 */ && _MSC_VER <= 1916 /* VS 2017 19.16 */
//
// std::string utf16_to_utf8(std::u16string utf16_string)
// {
//   std::wstring_convert<std::codecvt_utf8_utf16<int16_t>, int16_t> convert;
//   auto p = reinterpret_cast<const int16_t *>(utf16_string.data());
//   return convert.to_bytes(p, p + utf16_string.size());
// }
//
// #else
//
// std::string utf16_to_utf8(std::u16string utf16_string)
// {
//   std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;
//   return convert.to_bytes(utf16_string);
// }
//
// #endif
