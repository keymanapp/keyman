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

    km_kbp_status
    internal_process_queued_actions(
      km_kbp_state *state
      );

  public:
    kmx_processor(path);

    km_kbp_status
    process_event(
      km_kbp_state *state,
      km_kbp_virtual_key vk,
      uint16_t modifier_state,
      uint8_t is_key_down,
      uint16_t event_flags
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

    km_kbp_status
    process_queued_actions(
      km_kbp_state *state
      ) override;

    km_kbp_status
    external_event(
      km_kbp_state* state,
      uint32_t event,
      void* data
      ) override;

    bool
    queue_action(
      km_kbp_state * state,
      km_kbp_action_item const* action_item
    ) override;

    km_kbp_context_item * get_intermediate_context() override;

    km_kbp_keyboard_key * get_key_list() const override;

    km_kbp_keyboard_imx * get_imx_list() const override;

  };

} // namespace kbp
} // namespace km
