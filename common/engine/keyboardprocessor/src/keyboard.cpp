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

using namespace km::kbp;

std::string utf16_to_utf8(std::u16string utf16_string);

keyboard::keyboard(std::filesystem::path const & path)
: _keyboard_id(path.stem().u16string()),
  _version_string(u"3.145"),
  _folder_path(path.parent_path()),
  _default_opts {KM_KBP_OPTIONS_END}
{
  version_string = _version_string.c_str();
  id = _keyboard_id.c_str();
  folder_path = _folder_path.native().c_str();
  default_options = _default_opts.data();

  if (path.extension() == ".kmx" ||
      path.extension() == ".KMX") { // Some legacy packages may include upper-case file extensions
    _processor = new kmx_processor(*this);
  }
  else if (path.extension() == ".mock") {
    _processor = new mock_processor(*this);
  }
  else {
    _processor = new null_processor(*this);
  }
}

json & km::kbp::operator << (json & j, km::kbp::keyboard const & kb)
{
  j << json::object
      << "id" << utf16_to_utf8(kb.id)
      << "folder" << kb._folder_path.string()
      << "version" << utf16_to_utf8(kb.version_string)
      << "rules" << json::array << json::close;

  return j << json::close;
}

/*
  This function exists because of a bug in Visual Studio 2015 and 2017:
  https://social.msdn.microsoft.com/Forums/en-US/8f40dcd8-c67f-4eba-9134-a19b9178e481/vs-2015-rc-linker-stdcodecvt-error?forum=vcgeneral
  https://stackoverflow.com/a/35103224/1836776
*/

#if _MSC_VER >= 1900 /* VS 2015 */ && _MSC_VER <= 1915 /* VS 2017 */

std::string utf16_to_utf8(std::u16string utf16_string)
{
  std::wstring_convert<std::codecvt_utf8_utf16<int16_t>, int16_t> convert;
  auto p = reinterpret_cast<const int16_t *>(utf16_string.data());
  return convert.to_bytes(p, p + utf16_string.size());
}

#else

std::string utf16_to_utf8(std::u16string utf16_string)
{
  std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;
  return convert.to_bytes(utf16_string);
}

#endif
