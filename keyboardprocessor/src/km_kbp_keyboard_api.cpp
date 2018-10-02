#include <cassert>
#include <algorithm>
#include <experimental/filesystem>
#include <iterator>
#include <sstream>
#include <unordered_map>
#include <string>
#include <vector>

#include <keyboardprocessor.h>

namespace std {
  using namespace std::experimental;
}

namespace km {
namespace kbp
{

  class keyboard : public km_kbp_keyboard_attrs
  {
    std::string const _keyboard_id;
    std::string const _version_string;
    std::filesystem::path const _folder_path;

  public:
    keyboard(std::filesystem::path const &);
  };

  keyboard::keyboard(std::filesystem::path const & path)
  : _keyboard_id(path.stem()),
    _version_string("3.145"),
    _folder_path(path.parent_path())
  {
    version_string = _version_string.c_str();
    id = _keyboard_id.c_str();
    folder_path = _folder_path.c_str();
  }
}
}

struct km_kbp_keyboard : public km::kbp::keyboard {};


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

void km_kbp_keyboard_dispose(km_kbp_keyboard const *keyboard)
{
  delete keyboard;
}

km_kbp_keyboard_attrs const *
km_kbp_keyboard_get_attrs(km_kbp_keyboard const *keyboard)
{
  return keyboard;
}
