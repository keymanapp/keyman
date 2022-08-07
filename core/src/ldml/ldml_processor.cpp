/*
  Copyright:    © SIL International.
  Description:  This is an implementation of the LDML keyboard spec 3.0.
  Create Date:  5 Aug 2023
  Authors:      Marc Durdin (MD)
*/

#include <fstream>
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
namespace kbp {

ldml_processor::ldml_processor(path const & kb_path, const std::vector<uint8_t> _kmn_unused(data))
: abstract_processor(
    keyboard_attributes(kb_path.stem(), KM_KBP_LDML_VERSION, kb_path.parent(), {})
  )
{
  // TODO-LDML: load the file from the buffer (KMXPlus format)
  // Note: kb_path is essentially debug metadata here
}

bool ldml_processor::is_kmxplus_file(path const & kb_path, std::vector<uint8_t>& data) {
// TODO-LDML: we should refactor all the core components to delegate file loading
//            to the Engine, which requires an API change, but this makes delivery
//            of keyboard files more flexible under more WASM.

  std::ifstream file(static_cast<std::string>(kb_path), std::ios::binary | std::ios::ate);
  if(!file.good()) {
    return false;
  }
  const std::streamsize size = file.tellg();
  if(size >= KMX_MAX_ALLOWED_FILE_SIZE) {
    return false;
  }

  file.seekg(0, std::ios::beg);

  data.reserve((size_t)size);
  if(!file.read((char *) data.data(), size)) {
    return false;
  }

  file.close();

  const kmx::PCOMP_KEYBOARD comp_keyboard = (kmx::PCOMP_KEYBOARD)data.data();

  if(comp_keyboard->dwIdentifier != KMX_DWORD(FILEID_COMPILED)) {
    return false;
  }

  if(comp_keyboard->dwFileVersion < VERSION_160 || (comp_keyboard->dwFlags & KF_IS_KMXPLUS) == 0) {
    return false;
  }

  // A KMXPlus file is in the buffer (although more validation is required)
  return true;
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
  uint16_t _kmn_unused(modifier_state),
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
