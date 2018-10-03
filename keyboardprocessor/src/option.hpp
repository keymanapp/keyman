#include <unordered_map>

#include <keyboardprocessor.h>

namespace km {
namespace kbp
{
  class option_set : public std::unordered_map<std::string, std::string>
  {
    km_kbp_option_scope _scope;

  public:
    option_set(km_kbp_option_scope s) : _scope(s) {}

    option_set(km_kbp_option_scope s, km_kbp_option const * opt);

    void update(km_kbp_option const * opt);

    auto scope() const noexcept { return _scope; }
  };

  option_set::option_set(km_kbp_option_scope s, km_kbp_option const * opt)
  : option_set(s)
  {
    update(opt);
  }

  void option_set::update(km_kbp_option const * opt)
  {
    while(opt->key) emplace(opt->key, opt->value);
  }
}
}
