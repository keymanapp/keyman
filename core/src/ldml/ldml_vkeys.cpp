/*
  Copyright:    © SIL International.
  Description:  Implementation for LDML Vkeys
  Create Date:  6 Oct 2022
  Authors:      Steven R. Loomis
*/

#include "ldml_vkeys.hpp"
#include "kmx_file.h"

namespace km {
namespace kbp {
namespace ldml {

vkeys::vkeys() : vkey_to_string() {
}

void
vkeys::add(km_core_virtual_key vk, uint16_t modifier_state, std::u16string output) {
  // construct key
  const vkey_id id(vk, modifier_state);
  // assign the string
  vkey_to_string[id] = output;
}

static const uint16_t BOTH_ALT  = LALTFLAG  | RALTFLAG;
static const uint16_t BOTH_CTRL = LCTRLFLAG | RCTRLFLAG;

std::u16string
vkeys::lookup(km_core_virtual_key vk, uint16_t modifier_state) const {
  const vkey_id id(vk, modifier_state);

  // try exact match first
  std::u16string ret = lookup(id);
  if (!ret.empty()) {
    return ret;
  }

  const bool have_ctrl = modifier_state & BOTH_CTRL;
  const bool have_alt  = modifier_state & BOTH_ALT;

  // look for a layer with "alt" (either)
  if (have_alt) {
    const vkey_id id_alt(vk, (modifier_state & ~(BOTH_ALT)) | K_ALTFLAG);
    ret = lookup(id_alt);
    if (!ret.empty()) {
      return ret;
    }
  }
  // look for a layer with "ctrl" (either)
  if (have_ctrl) {
    const vkey_id id_ctrl(vk, (modifier_state & ~(BOTH_CTRL)) | K_CTRLFLAG);
    ret = lookup(id_ctrl);
    if (!ret.empty()) {
      return ret;
    }
  }
  // look for a layer with "alt ctrl" (either)
  if (have_ctrl && have_alt) {
    const vkey_id id_ctrl_alt(vk, (modifier_state & ~(BOTH_ALT | BOTH_CTRL)) | K_CTRLFLAG | K_ALTFLAG);
    ret = lookup(id_ctrl_alt);
    if (!ret.empty()) {
      return ret;
    }
  }

  // default: return failure.
  return ret;
}

std::u16string
vkeys::lookup(const vkey_id& id) const {
  const auto key = vkey_to_string.find(id);
  if (key == vkey_to_string.end()) {
    return std::u16string();  // TODO-LDML: optimize object construction?
  }
  return key->second;
}

}  // namespace ldml
}  // namespace kbp
}  // namespace km
