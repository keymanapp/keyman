/*
  Copyright:    © 2018 SIL International.
  Description:  Internal option key value map class and adaptor class for
                the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:       2 Oct 2018 - TSE - Refactored out of km_core_options_api.cpp.
                 7 Nov 2018 - TSE - Refactored into option.hpp & option.cpp.
*/

#pragma once

#include <string>

#include "keyman_core.h"


// Forward declarations
class json;

namespace km {
namespace core
{
  struct option : public km_core_option_item
  {
    option(): km_core_option_item KM_CORE_OPTIONS_END {}
    option(option const &);
    option(option &&);
    option(km_core_option_scope, char16_t const *, char16_t const *);
    option(km_core_option_scope, std::u16string const &,
           std::u16string const &);

    ~option() noexcept;

    option & operator=(option const & rhs);
    option & operator=(option && rhs);

    /**
     * Returns contents of this object as a C struct, releasing memory
     * management of key and value, and invalidates this object.
     */
    km_core_option_item release();

    bool empty() const;
  };

  inline
  option::option(km_core_option_scope s,
                 std::u16string const & k, std::u16string const & v)
  : option(s, k.c_str(), v.c_str())
  {}


  inline
  option::option(option const & rhs)
  : option(km_core_option_scope(rhs.scope), rhs.key, rhs.value) {}


  inline
  option::option(option && rhs) : option()
  {
    std::swap(key, rhs.key);
    std::swap(value, rhs.value);
    scope = rhs.scope;
  }

  inline
  option::~option() noexcept
  {
    delete [] key;
    delete [] value;
  }


  inline
  option & option::operator=(option && rhs) {
    delete [] key;
    delete [] value;
    return *new (this) option(std::move(rhs));
  }


  inline
  option & option::operator=(option const & rhs)
  {
    delete [] key;
    delete [] value;
    return *new (this) option(rhs);
  }

  inline
  bool option::empty() const {
    return key == nullptr;
  }



} // namespace core
} // namespace km
