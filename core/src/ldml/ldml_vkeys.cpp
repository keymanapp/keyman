/*
  Copyright:    © SIL International.
  Description:  Implementation for LDML Vkeys
  Create Date:  6 Oct 2022
  Authors:      Steven R. Loomis
*/

#include "ldml_vkeys.hpp"

namespace km {
namespace kbp {
namespace ldml {

vkeys::vkeys() : vkey_to_string() {
}

void
vkeys::add(km_kbp_virtual_key vkey, uint16_t mod, std::u16string str) {
  // construct key
  vkey_id vkey_id(vkey, mod);
  // assign the string
  vkey_to_string[vkey_id] = str;
}

std::u16string
vkeys::lookup(km_kbp_virtual_key vk, uint16_t modifier_state) const {
  const vkey_id id(vk, modifier_state);
  const auto key = vkey_to_string.find(id);
  if (key == vkey_to_string.end()) {
    return std::u16string();
  }
  return key->second;
}

}  // namespace ldml
}  // namespace kbp
}  // namespace km
