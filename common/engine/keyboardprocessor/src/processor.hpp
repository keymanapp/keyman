/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_kbp_keyboard_api.cpp
*/

#pragma once

#include <string>
#include <keyman/keyboardprocessor.h>

namespace km {
namespace kbp
{

  class abstract_processor
  {
  public:
    virtual km_kbp_status process_event(km_kbp_state *state, km_kbp_virtual_key vk, uint16_t modifier_state) const = 0;
    virtual km_kbp_attr const * get_attrs() const = 0;
  };

  class kmx_processor : public abstract_processor
  {
  public:
    km_kbp_status process_event(km_kbp_state *state, km_kbp_virtual_key vk, uint16_t modifier_state) const;
    km_kbp_attr const * get_attrs() const;
  };

  class mock_processor : public abstract_processor
  {
  public:
    km_kbp_status process_event(km_kbp_state *state, km_kbp_virtual_key vk, uint16_t modifier_state) const;
    km_kbp_attr const * get_attrs() const;
  };

  class null_processor : public mock_processor {};

} // namespace kbp
} // namespace km
