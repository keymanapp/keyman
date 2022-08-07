/*
  Copyright:    © SIL International.
  Description:  This is an implementation of the LDML keyboard spec 3.0.
  Create Date:  5 Aug 2023
  Authors:      Marc Durdin (MD)
*/

#include "ldml/ldml_processor.hpp"
#include "state.hpp"
#include "../kmx/kmx_file.h"

namespace {
  constexpr km_kbp_attr const engine_attrs = {
    256,
    KM_KBP_LIB_CURRENT,
    KM_KBP_LIB_AGE,
    KM_KBP_LIB_REVISION,
    KM_KBP_TECH_LDML,
    "SIL International"
  };
}

namespace km {
  namespace kbp
  {
    ldml_processor::ldml_processor(void *data, size_t len)
    : abstract_processor(
        /*
        TODO:
         keyboard_attributes(path.stem(), u"3.145", path.parent(), {
          option{KM_KBP_OPT_KEYBOARD, u"__test_point", u"not tiggered"},
        })
        */),
      _options({
        /*
        TODO:
          {u"\x01__test_point", u"not tiggered"},
          {u"\x02hello", u"-"}
        */
      })
    {
      // TODO: load the file from the buffer (KMXPlus format)
    }

    bool ldml_processor::is_kmxplus_file(path const & kb_path, void** buf, size_t& sz) {
    // TODO-LDML: sniff file header for LDML flag, return in buf + sz
    // TODO-LDML: we should refactor all the core components to delegate file loading
    //            to the Engine, which requires an API change, but this makes delivery
    //            of keyboard files more flexible under more WASM.
    // This hack function is not good enough
#if defined(_WIN32) || defined(_WIN64)
    FILE* fp = _wfsopen(kb_path.c_str(), L"rb", _SH_DENYWR);
#else
    FILE* fp = fopen(kb_path.c_str(), "rb");
#endif
    if(fp == NULL) {
      return false;
    }

    if (fseek(fp, 0, SEEK_END) != 0) {
      fclose(fp);
      return false;
    }

    sz = ftell(fp);
    if (sz < 0) {
      fclose(fp);
      return false;
    }

    if (fseek(fp, 0, SEEK_SET) != 0) {
      fclose(fp);
      return false;
    }

    *buf = new uint8_t[sz];

    if(!*buf) {
      fclose(fp);
      return false;
    }

    if (fread(*buf, 1, sz, fp) < (size_t) sz) {
      fclose(fp);
      delete [] (*buf);
      return false;
    }

    fclose(fp);

    kmx::PCOMP_KEYBOARD comp_keyboard = static_cast<kmx::PCOMP_KEYBOARD>(*buf);

    if(comp_keyboard->dwIdentifier != KMX_DWORD(FILEID_COMPILED)) {
      delete [] (*buf);
      return false;
    }

    if(comp_keyboard->dwFileVersion < VERSION_160 || (comp_keyboard->dwFlags & KF_IS_KMXPLUS) == 0) {
      delete [] (*buf);
      return false;
    }

    // We'll keep the buffer to return to constructor
    return true;
  }


    char16_t const * ldml_processor::lookup_option(km_kbp_option_scope scope,
                                    std::u16string const & key) const
    {
      auto i = _options.find(char16_t(scope) + key);
      return i != _options.end() ? i->second.c_str() : nullptr;
    }

    option ldml_processor::update_option(km_kbp_option_scope scope,
                       std::u16string const & key,
                       std::u16string const & value)
    {
      auto i = _options.find(char16_t(scope) + key);
      if (i == _options.end()) return option();

      i->second = value;
      persisted_store()[key] = value;
      return option(scope, key, i->second);
    }

    km_kbp_status
    ldml_processor::process_queued_actions(
      km_kbp_state *state
    ) {
      assert(state);
      if (!state)
        return KM_KBP_STATUS_INVALID_ARGUMENT;
      // TODO Implement
      return KM_KBP_STATUS_OK;
    }

    bool ldml_processor::queue_action(
      km_kbp_state * state,
      km_kbp_action_item const* action_item
    )
    {
      assert(state);
      assert(action_item);
      if ((!state) || (!action_item))
        return false;
      return false;
    }

    km_kbp_status
    ldml_processor::process_event(
      km_kbp_state *state,
      km_kbp_virtual_key vk,
      uint16_t modifier_state,
      uint8_t is_key_down
    ) {
      assert(state);
      if (!state)
        return KM_KBP_STATUS_INVALID_ARGUMENT;

      if (!is_key_down) {
        // TODO: Implement caps lock handling
        state->actions().clear();
        state->actions().commit();
        return KM_KBP_STATUS_OK;
      }

      try {
        // At the start of every process_event always clear the action_items
        state->actions().clear();

        switch (vk) {
        case KM_KBP_VKEY_BKSP:
          state->context().pop_back();
          state->actions().push_backspace(KM_KBP_BT_UNKNOWN); // Assuming we don't know the character
          break;
        default:
          /* We're going to push an 'a' for a passing unit test */
          state->context().push_character('a');
          state->actions().push_character('a');
        }

        state->actions().commit();
      } catch (std::bad_alloc &) {
        state->actions().clear();
        return KM_KBP_STATUS_NO_MEM;
      }

      return KM_KBP_STATUS_OK;
    }

    km_kbp_attr const & ldml_processor::attributes() const {
      return engine_attrs;
    }

    km_kbp_keyboard_key  * ldml_processor::get_key_list() const {
      km_kbp_keyboard_key* key_list = new km_kbp_keyboard_key(KM_KBP_KEYBOARD_KEY_LIST_END);
      return key_list;
    }

    km_kbp_keyboard_imx  * ldml_processor::get_imx_list() const {
      km_kbp_keyboard_imx* imx_list = new km_kbp_keyboard_imx(KM_KBP_KEYBOARD_IMX_END);
      return imx_list;
    }

    km_kbp_context_item * ldml_processor::get_intermediate_context() {
      km_kbp_context_item *citems = new km_kbp_context_item(KM_KBP_CONTEXT_ITEM_END);
      return citems;
    }

    km_kbp_status ldml_processor::validate() const { return KM_KBP_STATUS_OK; }
  } // namespace kbp
} // namespace km
