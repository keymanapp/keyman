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
  struct option : public km_kbp_option_item
  {
    option(): km_kbp_option_item KM_KBP_OPTIONS_END {}
    option(option const &);
    option(option &&);
    option(km_kbp_option_scope, std::u16string const &,
           std::u16string const &);

    ~option() noexcept;

    option & operator=(option && rhs);
  };

  inline
  option::option(option const & rhs)
  : option(km_kbp_option_scope(rhs.scope), rhs.key, rhs.value)
  {}

  inline
  option::option(option && rhs)
  : km_kbp_option_item { rhs.key, rhs.value, rhs.scope }
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



  class options
  {
    //km_kbp_keyboard_attrs const &_kb;
    km_kbp_option_item const * _scopes[KM_KBP_OPT_MAX_SCOPES-1];
    std::vector<option>   _saved;

  public:
    options(km_kbp_option_item const * kb_default_options);

    void set_default_env(km_kbp_option_item const *env);

    char16_t const *      lookup(km_kbp_option_scope scope,
                                 std::u16string const & key) const noexcept;
    km_kbp_option_item const * assign(km_kbp_state *state, km_kbp_option_scope scope, std::u16string const & key,
                                           std::u16string const & value);
    void                  reset(km_kbp_option_scope scope,
                                std::u16string const & key);

    friend json & operator << (json &j, km::kbp::options const &opts);
  };

  json & operator << (json &j, km::kbp::options const &opts);

  inline
  options::options(km_kbp_option_item const * kb_default_options)
  : _scopes {kb_default_options, nullptr}
  {}

  inline void options::set_default_env(km_kbp_option_item const *env) {
    _scopes[KM_KBP_OPT_ENVIRONMENT - 1] = env;
  }

} // namespace kbp
} // namespace km


// Adaptor between internal km::kbp::options object and API definitiion.
struct km_kbp_options: public km::kbp::options {};
