/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_core_keyboard_api.cpp
*/

#pragma once

#include <string>
#include "keyman_core.h"
#include "kmx/kmx_processevent.h"
#include "keyboard.hpp"
#include "processor.hpp"

namespace km {
namespace core
{
  class kmx_processor : public abstract_processor
  {
  private:
    bool               _valid;
    kmx::KMX_ProcessEvent _kmx;

    km_core_status
    internal_process_queued_actions(
      km_core_state *state
      );

  public:
    kmx_processor(std::u16string const& kb_name, const std::vector<uint8_t>& data);

    km_core_status
    process_event(
      km_core_state *state,
      km_core_virtual_key vk,
      uint16_t modifier_state,
      uint8_t is_key_down,
      uint16_t event_flags
    ) override;

    km_core_attr const & attributes() const override;
    km_core_status       validate() const override;

    char16_t const *
    lookup_option(
      km_core_option_scope,
      std::u16string const & key
    )  const override;

    option
    update_option(
      km_core_option_scope scope,
      std::u16string const & key,
      std::u16string const & value
    ) override;

    km_core_status
    process_queued_actions(
      km_core_state *state
    ) override;

    km_core_status
    external_event(
      km_core_state* state,
      uint32_t event,
      void* data
    ) override;

    bool
    queue_action(
      km_core_state * state,
      km_core_action_item const* action_item
    ) override;

    km_core_context_item * get_intermediate_context() override;

    km_core_keyboard_key * get_key_list() const override;

    km_core_keyboard_imx * get_imx_list() const override;

    bool
    supports_normalization() const override {
      return false;
    }
  };

} // namespace core
} // namespace km
