/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_kbp_keyboard_api.cpp
*/

#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <keyman/keyboardprocessor.h>

#include "processor.hpp"
#include "option.hpp"

namespace km {
namespace kbp {

#define KM_KBP_LDML_VERSION u"1.0"

  class ldml_processor : public abstract_processor {

  public:
    ldml_processor(
      path const & kb_path,
      const std::vector<uint8_t> data
    );

//    ~ldml_processor() override;

    static bool is_kmxplus_file(
      path const & kb_path,
      std::vector<uint8_t>& data
    );

    km_kbp_status
    process_event(
      km_kbp_state *state,
      km_kbp_virtual_key vk,
      uint16_t modifier_state,
      uint8_t is_key_down
    ) override;

    virtual km_kbp_attr const & attributes() const override;
    km_kbp_status               validate() const override;

    char16_t const *
    lookup_option(
      km_kbp_option_scope,
      std::u16string const & key
    ) const override {
      return nullptr;
    }

    option
    update_option(
      km_kbp_option_scope,
      std::u16string const & key,
      std::u16string const & value
    ) override {
      return option();
    }

    km_kbp_status process_queued_actions(km_kbp_state *state) override;

    bool queue_action(
      km_kbp_state * state,
      km_kbp_action_item const* action_item
    ) override;

    km_kbp_context_item * get_intermediate_context() override;

    km_kbp_keyboard_key  * get_key_list() const override;

    km_kbp_keyboard_imx  * get_imx_list() const override;
  };
} // namespace kbp
} // namespace km
