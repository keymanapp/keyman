#include <experimental/filesystem>
#include <string>

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
