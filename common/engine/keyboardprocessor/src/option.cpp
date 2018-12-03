/*
  Copyright:    © 2018 SIL International.
  Description:  Internal option key value map class and adaptor class for
                the API.
  Create Date:  7 Oct 2018
  Authors:      Tim Eves (TSE)
  History:       7 Nov 2018 - TSE - Refactored into option.hpp & option.cpp.
*/
#include <algorithm>
#include <memory>

#include "keyboard.hpp"
#include "option.hpp"
#include "json.hpp"
#include "utfcodec.hpp"
#include "state.hpp"

using namespace km::kbp;

namespace
{
  constexpr char const * const scope_names_lut[] = {
    u8"keyboard",
    u8"environment"
  };
}

// Forward declarations


option::option(km_kbp_option_scope s, std::u16string const & k, std::u16string const & v)
: km_kbp_option_item { new km_kbp_cp[k.size()+1], new km_kbp_cp[v.size()+1],
                uint8_t(s) }
{
  std::copy_n(k.c_str(), k.size()+1, const_cast<km_kbp_cp *>(key));
  std::copy_n(v.c_str(), v.size()+1, const_cast<km_kbp_cp *>(value));
}


char16_t const * options::lookup(km_kbp_option_scope scope,
                                     std::u16string const & key) const noexcept
{
  // Search first in the updated values
  for (auto & opt: _saved)
  {
    if (opt.key == key && opt.scope == scope)
      return opt.value;
  }

  // Then in the pristine copies.
  km_kbp_option_item const * opt = _scopes[scope-1];
  while (opt->key && key != opt->key) ++opt;
  return opt->key ? opt->value : nullptr;
}


km_kbp_option_item const * options::assign(km_kbp_state *state, km_kbp_option_scope scope, std::u16string const & key,
                                       std::u16string const & value)
{
  km_kbp_option_item const * opt = _scopes[scope-1];
  while (opt->key && key != opt->key) ++opt;
  if (!opt->key)  return nullptr;

  for (auto & save: _saved)
  {
    if (save.key == key && save.scope == scope)
    {
      save = option(scope, key, value);

      //((km::kbp::state *)state)->keyboard().
      const_cast<km::kbp::abstract_processor &>(static_cast<km::kbp::state *>(state)->keyboard().processor()).update_option(state, scope, key, value);

      return &save;
    }
  }

  _saved.emplace_back(scope, key, value);

  const_cast<km::kbp::abstract_processor &>(static_cast<km::kbp::state *>(state)->keyboard().processor()).update_option(state, scope, key, value);

  return &_saved.back();
}

void options::reset(km_kbp_option_scope scope, std::u16string const & key)
{
  for (auto i = _saved.begin(); i == _saved.end(); ++i)
  {
    if (i->key == key && i->scope == scope)
    {
      _saved.erase(i);
      break;
    }
  }
}



json & operator << (json &j, char16_t const *u16str) {
  char cps[5] = {0,};
  int8_t l;
  std::string u8str;

  for (auto i = utf16::const_iterator(u16str); *i; ++i)
  {
    utf8::codec::put(reinterpret_cast<utf8::codeunit_t *>(cps), *i, l);
    u8str.append(cps, size_t(l));
  }
  j << u8str;
  return j;
}


json & km::kbp::operator << (json &j, options const &opts)
{
  j << json::object;
  auto n = 0;
  for (auto scope: opts._scopes)
  {
    j << scope_names_lut[n++] << json::object;
    for (auto opt = scope; opt->key; ++opt)
    {
      j << opt->key << opt->value;
    }
    j << json::close;
  }

  j << "saved" << json::object;
  for (auto scope: {KM_KBP_OPT_KEYBOARD, KM_KBP_OPT_ENVIRONMENT})
  {
    j << scope_names_lut[scope-1] << json::object;
    for (auto & opt: opts._saved)
    {
      if (opt.scope != scope) continue;
      j << opt.key << opt.value;
    }
    j << json::close;
  }
  j << json::close;
  j << json::close;

  return j;
}
