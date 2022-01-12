/*
  Copyright:    © 2021 SIL International.
  Description:  This is an interface to a rust mock implementation.
                TODO: Add a mechanism to trigger output of PERSIST_OPT &
                RESET_OPT actions items, options support and context matching.
  Authors:      Marc Durdin
*/

#include "rust/rust_mock_processor.hpp"
#include "state.hpp"

namespace
{
  // This is metadata about the Rust implementation
  constexpr km_kbp_attr const engine_attrs = {
    256,
    KM_KBP_LIB_CURRENT,
    KM_KBP_LIB_AGE,
    KM_KBP_LIB_REVISION,
    KM_KBP_TECH_RUST_MOCK,
    "SIL International"
  };

}

namespace km {
  namespace kbp
  {
    rust_mock_processor::rust_mock_processor(kbp::path const & path)
    : abstract_processor(
        keyboard_attributes(path.stem(), u"3.145", path.parent(), {
          option{KM_KBP_OPT_KEYBOARD, u"__test_point", u"not triggered"},
        })),
      _options({
          {u"\x01__test_point", u"not triggered"},
          {u"\x02hello", u"-"}
      })
    {
    }

    char16_t const * rust_mock_processor::lookup_option(km_kbp_option_scope scope,
                                    std::u16string const & key) const
    {
      // TODO: move to rust
      auto i = _options.find(char16_t(scope) + key);
      return i != _options.end() ? i->second.c_str() : nullptr;
    }

    option rust_mock_processor::update_option(km_kbp_option_scope scope,
                       std::u16string const & key,
                       std::u16string const & value)
    {
      // TODO: move to rust
      auto i = _options.find(char16_t(scope) + key);
      if (i == _options.end()) return option();

      i->second = value;
      persisted_store()[key] = value;
      return option(scope, key, i->second);
    }

    bool rust_mock_processor::queue_action(
      km_kbp_state * state,
      km_kbp_action_item const* action_item
    )
    {
      return false;
    }

    extern "C" uint32_t rust_mock_process_event(uint16_t vk, uint16_t modifier, uint8_t is_key_down);

    km_kbp_status rust_mock_processor::process_event(km_kbp_state *state,
                                                    km_kbp_virtual_key vk,
                                                    uint16_t modifier_state,
                                                    uint8_t is_key_down) {
      (void)(state);
      // km_kbd_rust_state rust_state(state);
      return rust_mock_process_event(vk, modifier_state, is_key_down);
    }

    km_kbp_attr const & rust_mock_processor::attributes() const {
      return engine_attrs;
    }

    km_kbp_keyboard_key  * rust_mock_processor::get_key_list() const  {
      km_kbp_keyboard_key* key_list = new km_kbp_keyboard_key(KM_KBP_KEYBOARD_KEY_LIST_END);
      return key_list;
    }

    km_kbp_keyboard_imx  * rust_mock_processor::get_imx_list() const  {
       km_kbp_keyboard_imx* imx_list = new km_kbp_keyboard_imx(KM_KBP_KEYBOARD_IMX_END);
      return imx_list;
    }

    km_kbp_context_item * rust_mock_processor::get_intermediate_context() {
      km_kbp_context_item *citems = new km_kbp_context_item(KM_KBP_CONTEXT_ITEM_END);
      return citems;
    }

    km_kbp_status rust_mock_processor::validate() const { return KM_KBP_STATUS_OK; }

    km_kbp_status rust_null_processor::validate() const { return KM_KBP_STATUS_INVALID_ARGUMENT; }
  } // namespace kbp
} // namespace km
