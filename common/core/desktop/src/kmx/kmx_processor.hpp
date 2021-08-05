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
#include "kmx/kmx_processevent.h"
#include "keyboard.hpp"
#include "processor.hpp"

namespace km {
namespace kbp
{
  class kmx_processor : public abstract_processor
  {
  private:
    bool               _valid;
    kmx::KMX_ProcessEvent _kmx;
  public:
    kmx_processor(path);

    km_kbp_status
    process_event(
      km_kbp_state *state,
      km_kbp_virtual_key vk,
      uint16_t modifier_state,
      uint8_t is_key_down
    ) override;

    km_kbp_attr const & attributes() const override;
    km_kbp_status       validate() const override;

    char16_t const *
    lookup_option(
      km_kbp_option_scope,
      std::u16string const & key
    )  const override;

    option
    update_option(
      km_kbp_option_scope scope,
      std::u16string const & key,
      std::u16string const & value
    ) override;
  };

} // namespace kbp
} // namespace km
