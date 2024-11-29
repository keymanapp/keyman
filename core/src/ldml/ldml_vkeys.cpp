/*
  Copyright:    © SIL International.
  Description:  Implementation for LDML Vkeys
  Create Date:  6 Oct 2022
  Authors:      Steven R. Loomis
*/

#include "ldml_vkeys.hpp"
#include "kmx_file.h"
#include <ldml/keyman_core_ldml.h>
#include <set>
#include <assert.h>

namespace km {
namespace core {
namespace ldml {

vkeys::vkeys() : vkey_to_string() {
}

void
vkeys::add(km_core_virtual_key vk, km_core_ldml_modifier_state modifier_state, std::u16string output) {
  // construct key
  const vkey_id id(vk, modifier_state);
  // assign the string
  vkey_to_string[id] = output;
  // includes all keys - including gaps.
  all_vkeys.insert(id);
}

km_core_keyboard_key  *
vkeys::get_key_list() const {
  // prescan to find out which modifier flags are used

  std::size_t other_key_count = 0;        // number of 'OTHER' keys, which will need to expand to all of the other_state set ( so other_state.size())
  std::size_t both_alt_key_count = 0;     // number of 'ALT' keys,   which will need to expand to LALT+RALT   (so 2)
  std::size_t both_ctrl_key_count = 0;    // number of 'CTRL' keys,  which will need to expand to LCTRL+RCTRL (so 2)

  std::set<km::core::ldml::km_core_ldml_modifier_state> all_modifiers;
  for (const auto &k : all_vkeys) {
    const auto mod = k.second;
    if (mod == LDML_KEYS_MOD_OTHER) {
      other_key_count++;
    } else if (mod == LDML_KEYS_MOD_ALT) {
      both_alt_key_count++;
    } else if (mod == LDML_KEYS_MOD_CTRL) {
      both_ctrl_key_count++;
    }
    all_modifiers.insert(mod);
  }
  std::set<km_core_modifier_state> other_state;

  // Alt
  if (all_modifiers.count(LDML_KEYS_MOD_ALT) == 0 && all_modifiers.count(LDML_KEYS_MOD_ALTL) == 0 && all_modifiers.count(LDML_KEYS_MOD_ALTR) == 0) {
    // no ALT keys were seen, so OTHER includes ALT
    other_state.insert(KM_CORE_MODIFIER_LALT);
    other_state.insert(KM_CORE_MODIFIER_RALT);
  } else if(all_modifiers.count(LDML_KEYS_MOD_ALTL) == 0) {
    other_state.insert(KM_CORE_MODIFIER_RALT);
  } else if(all_modifiers.count(LDML_KEYS_MOD_ALTR) == 0) {
    other_state.insert(KM_CORE_MODIFIER_LALT);
  }

  // ctrl
  if (all_modifiers.count(LDML_KEYS_MOD_CTRL) == 0 && all_modifiers.count(LDML_KEYS_MOD_CTRLL) == 0 && all_modifiers.count(LDML_KEYS_MOD_CTRLR) == 0) {
    // no CTRL keys were seen, so OTHER includes CTRL
    other_state.insert(KM_CORE_MODIFIER_LCTRL);
    other_state.insert(KM_CORE_MODIFIER_RCTRL);
  } else if(all_modifiers.count(LDML_KEYS_MOD_CTRLL) == 0) {
    other_state.insert(KM_CORE_MODIFIER_RCTRL);
  } else if(all_modifiers.count(LDML_KEYS_MOD_CTRLR) == 0) {
    other_state.insert(KM_CORE_MODIFIER_LCTRL);
  }

  // shift
  if (all_modifiers.count(LDML_KEYS_MOD_SHIFT) == 0) {
    other_state.insert(KM_CORE_MODIFIER_SHIFT);
  }

  // caps
  if (all_modifiers.count(LDML_KEYS_MOD_CAPS) == 0) {
    other_state.insert(KM_CORE_MODIFIER_CAPS);
  }

  // none- it's possible there is no 'none' layer
  if (all_modifiers.count(LDML_KEYS_MOD_NONE) == 0) {
    other_state.insert(KM_CORE_MODIFIER_NONE);
  }

  const std::size_t new_list_size = all_vkeys.size() // original size
    + (other_key_count * (other_state.size() - 1))
    + (both_alt_key_count * 1)
    + (both_ctrl_key_count * 1)
    + 1; // terminator
  km_core_keyboard_key *list = new km_core_keyboard_key[new_list_size];
  std::size_t n = 0;
  for (const auto &k : all_vkeys) {
    const auto vkey = k.first;
    const auto mod  = k.second;

    if (mod == LDML_KEYS_MOD_OTHER) {
      // expand to all of other_state
      for (const auto expanded_mod : other_state) {
        list[n].key             = vkey;
        list[n++].modifier_flag = expanded_mod;
        assert(n <= new_list_size);
      }
    } else if (mod == LDML_KEYS_MOD_ALT) {
      // expand to 'both'
      list[n].key             = vkey;
      list[n++].modifier_flag = KM_CORE_MODIFIER_LALT;
      assert(n <= new_list_size);

      list[n].key             = vkey;
      list[n++].modifier_flag = KM_CORE_MODIFIER_RALT;
      assert(n <= new_list_size);
    } else if (mod == LDML_KEYS_MOD_CTRL) {
      // expand to 'both'
      list[n].key             = vkey;
      list[n++].modifier_flag = KM_CORE_MODIFIER_LCTRL;
      assert(n <= new_list_size);

      list[n].key             = vkey;
      list[n++].modifier_flag = KM_CORE_MODIFIER_RCTRL;
      assert(n <= new_list_size);
    } else {
      assert(mod <= KM_CORE_MODIFIER_MASK_ALL); // that no LDMLisms escape
      list[n].key             = vkey;
      list[n++].modifier_flag = mod;
      assert(n <= new_list_size);
    }
  }
  list[n++] = KM_CORE_KEYBOARD_KEY_LIST_END;
  assert(n == new_list_size);
  return list;
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
