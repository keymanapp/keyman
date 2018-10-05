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

    auto scope() const noexcept { return _scope; }
  };

}
}


// Adaptor between internal km::kbp::option_set object and API definitiion.
struct km_kbp_option_set
{
  km::kbp::option_set   & target;

  km_kbp_option_set(km::kbp::option_set & target) : target(target) {}

  km_kbp_option const * export_option(char const * k, char const * v) const {
    _last_lookup.key = k;
    _last_lookup.value = v;
    return &_last_lookup;
  }

  void update(km_kbp_option const * opt);

private:
  km_kbp_option mutable  _last_lookup;
};

inline
void km_kbp_option_set::update(km_kbp_option const * opt)
{
  for (;opt->key; ++opt)  target.emplace(opt->key, opt->value);
}
