#pragma once

#include <unordered_map>

#include <keyboardprocessor.h>

// Forward declarations
class json;

namespace km {
namespace kbp
{
  class options_set : public std::unordered_map<std::string, std::string>
  {
    km_kbp_option_scope _scope;

  public:
    options_set(km_kbp_option_scope s) : _scope(s) {}

    auto scope() const noexcept { return _scope; }
  };

}
}

json & operator << (json &, km::kbp::options_set const &);


// Adaptor between internal km::kbp::options_set object and API definitiion.
struct km_kbp_options_set
{
  km::kbp::options_set   & target;

  km_kbp_options_set(km::kbp::options_set & tgt) : target(tgt) {}

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
void km_kbp_options_set::update(km_kbp_option const * opt)
{
  for (;opt->key; ++opt)  target.emplace(opt->key, opt->value);
}
