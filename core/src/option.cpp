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
