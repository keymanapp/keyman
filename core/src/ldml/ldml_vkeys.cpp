/*
  Copyright:    © SIL International.
  Description:  Implementation for LDML Vkeys
  Create Date:  6 Oct 2022
  Authors:      Steven R. Loomis
*/

#include "ldml_vkeys.hpp"
#include "kmx_file.h"
#include <ldml/keyman_core_ldml.h>

namespace km {
namespace core {
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
vkeys::lookup(km_core_virtual_key vk, uint16_t modifier_state, bool &found) const {
  const vkey_id id(vk, modifier_state);

  // try exact match first
  std::u16string ret = lookup(id, found);
  if (found) {
    return ret;
  }

  const bool have_ctrl = modifier_state & BOTH_CTRL;
  const bool have_alt  = modifier_state & BOTH_ALT;

  // look for a layer with "alt" (either)
  if (have_alt) {
    const vkey_id id_alt(vk, (modifier_state & ~(BOTH_ALT)) | K_ALTFLAG);
    ret = lookup(id_alt, found);
    if (found) {
      return ret;
    }
  }

  // look for a layer with "ctrl" (either)
  if (have_ctrl) {
    const vkey_id id_ctrl(vk, (modifier_state & ~(BOTH_CTRL)) | K_CTRLFLAG);
    ret = lookup(id_ctrl, found);
    if (found) {
      return ret;
    }
  }

  // look for a layer with "alt ctrl" (either)
  if (have_ctrl && have_alt) {
    const vkey_id id_ctrl_alt(vk, (modifier_state & ~(BOTH_ALT | BOTH_CTRL)) | K_CTRLFLAG | K_ALTFLAG);
    ret = lookup(id_ctrl_alt, found);
    if (found) {
      return ret;
    }
  }

  // look for a layer with "other"
  {
    const vkey_id id_default(vk, (LDML_KEYS_MOD_OTHER));
    ret = lookup(id_default, found);
    if (found) {
      return ret;
    }
  }

  // default: return failure. found=false.
  return ret;
}

std::u16string
vkeys::lookup(const vkey_id& id, bool &found) const {
  const auto key = vkey_to_string.find(id);
  if (key == vkey_to_string.end()) {
    found = false;
    return std::u16string();  // TODO-LDML: optimize object construction?
  }
  found = true;
  return key->second;
}

}  // namespace ldml
}  // namespace core
}  // namespace km
