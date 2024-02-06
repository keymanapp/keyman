/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_core_keyboard_api.cpp
*/

#pragma once

#include <string>
#include <unordered_map>
#include "keyman_core.h"

#include "processor.hpp"
#include "option.hpp"

namespace km {
namespace core
{
  class mock_processor : public abstract_processor
  {
    std::unordered_map<std::u16string, std::u16string> _options;

  public:
    mock_processor(km::core::path const &);
//    ~mock_processor() override;

    km_core_status
    process_event(
      km_core_state *state,
      km_core_virtual_key vk,
      uint16_t modifier_state,
      uint8_t is_key_down,
      uint16_t event_flags
    ) override;

    virtual km_core_attr const & attributes() const override;
    km_core_status               validate() const override;


    char16_t const *
    lookup_option(
      km_core_option_scope,
      std::u16string const & key
    ) const override;

    option
    update_option(
      km_core_option_scope,
      std::u16string const & key,
      std::u16string const & value
    ) override;

    km_core_status process_queued_actions( km_core_state *state) override;

    bool queue_action(
      km_core_state * state,
      km_core_action_item const* action_item
    ) override;

    km_core_context_item * get_intermediate_context() override;

    km_core_keyboard_key  * get_key_list() const override;

    km_core_keyboard_imx  * get_imx_list() const override;

    bool
    supports_normalization() const override {
      return true;
    }
  };

  class null_processor : public mock_processor {
  public:
    null_processor(): mock_processor(path())
    {
      _attributes = keyboard_attributes(u"null", u"0.0", path(), {});
    }

    km_core_status validate() const override;
  };
} // namespace core
} // namespace km
