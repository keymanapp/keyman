/*
  Copyright:    © 2018 SIL International.
  Description:  Internal option key value map class and adaptor class for
                the API.
  Create Date:  7 Oct 2018
  Authors:      Tim Eves (TSE)
  History:       7 Nov 2018 - TSE - Refactored into option.hpp & option.cpp.
*/
#include <algorithm>

#include "option.hpp"
#include "processor.hpp"


using namespace km::core;

namespace
{
  constexpr char const * const scope_names_lut[] = {
    u8"keyboard",
    u8"environment"
  };
}

// Forward declarations


option::option(km_core_option_scope s, char16_t const *k, char16_t const *v)
: option()
{
  if (k && v)
  {
    auto n_k = std::char_traits<char16_t>::length(k)+1,
         n_v = std::char_traits<char16_t>::length(v)+1;
    auto _key = new km_core_cu[n_k],
         _val = new km_core_cu[n_v];
    std::copy_n(k, n_k, _key);
    std::copy_n(v, n_v, _val);

    key = _key;
    value = _val;
    scope = s;
  }
}

km_core_option_item
option::release() {
  km_core_option_item opt = *this;
  key = nullptr;
  value = nullptr;
  return opt;
}

// TODO: Relocate this and fix it
json & km::core::operator << (json &j, abstract_processor const &)
{
  j << json::object;
  // auto n = 0;
  // for (auto scope: opts._scopes)
  // {
  //   j << scope_names_lut[n++] << json::object;
  //   for (auto opt = scope; opt->key; ++opt)
  //   {
  //     j << opt->key << opt->value;
  //   }
  //   j << json::close;
  // }

  j << "saved" << json::object;
  for (auto scope: {KM_CORE_OPT_KEYBOARD, KM_CORE_OPT_ENVIRONMENT})
  {
    j << scope_names_lut[scope-1] << json::object;
    // for (auto & opt: opts._saved)
    // {
    //   if (opt.scope != scope) continue;
    //   j << opt.key << opt.value;
    // }
    j << json::close;
  }
  j << json::close;
  j << json::close;

  return j;
}
