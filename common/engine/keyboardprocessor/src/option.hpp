/*
  Copyright:    © 2018 SIL International.
  Description:  Internal option key value map class and adaptor class for
                the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:       2 Oct 2018 - TSE - Refactored out of km_kbp_options_api.cpp.
                 7 Nov 2018 - TSE - Refactored into option.hpp & option.cpp.
*/

#pragma once

#include <vector>
#include <string>

#include <keyman/keyboardprocessor.h>

// Forward declarations
class json;

namespace km {
namespace kbp
{
  struct option : public km_kbp_option
  {
    option(): km_kbp_option KM_KBP_OPTIONS_END {}
    option(option &&);
    option(km_kbp_option_scope, std::u16string const &,
           std::u16string const &);

    ~option() noexcept;

    option & operator=(option && rhs);
  };

  inline
  option::option(option && rhs)
  : km_kbp_option { rhs.key, rhs.value, rhs.scope }
  {
    rhs.key = nullptr;
    rhs.value = nullptr;
  }

  inline
  option::~option() noexcept
  {
    delete [] key;
    delete [] value;
  }

  inline
  option & option::operator=(option && rhs)
  {
    delete [] key;
    delete [] value;
    key = rhs.key;
    value = rhs.value; rhs.key = nullptr;
    scope = rhs.scope; rhs.value = nullptr;
    return *this;
  }



  class options_set
  {
    km_kbp_option const * _scopes[KM_KBP_OPT_MAX_SCOPES-1];
    std::vector<option>   _saved;

  public:
    options_set(km_kbp_option const *env, km_kbp_option const *kb_defs);

    char16_t const *      lookup(km_kbp_option_scope scope,
                                 std::u16string const & key) const noexcept;
    km_kbp_option const * assign(km_kbp_option_scope scope, std::u16string const & key,
                                           std::u16string const & value);
    void                  reset(km_kbp_option_scope scope,
                                std::u16string const & key);

    friend json & operator << (json &j, km::kbp::options_set const &opts);
  };

  json & operator << (json &j, km::kbp::options_set const &opts);

  inline
  options_set::options_set(km_kbp_option const *env, km_kbp_option const *kb_defs)
  : _scopes {kb_defs, env}
  {}

} // namespace kbp
} // namespace km


// Adaptor between internal km::kbp::options_set object and API definitiion.
struct km_kbp_options_set: public km::kbp::options_set {};
