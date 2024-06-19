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

#include "keyman_core.h"

namespace km {
namespace core {
namespace ldml {

/**
 * identifier for keybag lookup
 */
typedef std::pair<km_core_virtual_key, uint16_t> vkey_id;

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
  void add(km_core_virtual_key vk, uint16_t modifier_state, std::u16string output);

  /**
   * Lookup a vkey, returns an empty string if not found
   * @param found on exit: true if found
   */
  std::u16string
  lookup(km_core_virtual_key vk, uint16_t modifier_state, bool &found) const;

private:
  /**
   * Non-recursive internal lookup of a specific ID
   * @param found on exit: true if found
   */
  std::u16string
  lookup(const vkey_id &id, bool &found) const;
};

}  // namespace ldml
}  // namespace core
}  // namespace km
