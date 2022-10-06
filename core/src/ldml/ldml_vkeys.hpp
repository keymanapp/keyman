/*
  Copyright:    © SIL International.
  Description:  Internal functions for LDML vkeys
  Create Date:  6 Oct 2022
  Authors:      Steven R. Loomis
*/

#pragma once

#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <keyman/keyboardprocessor.h>

namespace km {
namespace kbp {
namespace ldml {

/**
 * identifier for keybag lookup
 */
typedef std::pair<km_kbp_virtual_key, uint16_t> vkey_id;

/**
 * LDML Class to manage all things key related: vkey remapping and vkey to string
 */
class vkeys {
private:
  std::map<vkey_id, std::u16string> vkey_to_string;

public:
  vkeys();

  /**
   * add a vkey to the bag
   */
  void add(km_kbp_virtual_key vkey, uint16_t mod, std::u16string str);

  /**
   * Lookup a vkey, returns an empty string if not found
   */
  std::u16string
  lookup(km_kbp_virtual_key vk, uint16_t modifier_state) const;
};

}  // namespace ldml
}  // namespace kbp
}  // namespace km
