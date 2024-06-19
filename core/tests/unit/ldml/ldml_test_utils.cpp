/*
  Copyright:    © 2022-2023 SIL International.
  Description:  Internal utils for ldml tests
  Create Date:  16 Mar 2023
  Authors:      Steven R. Loomis (SRL)
  History:      Refactored out of ldml_test_source.cpp
*/

#include <kmx/kmx_processevent.h> // for char to vk mapping tables

#include "ldml_test_utils.hpp"

namespace km {
namespace tests {

km_core_virtual_key
get_vk(std::string const &vk) {
  for (int i = 1; i < 256; i++) {
    if (vk == km::core::kmx::s_key_names[i]) {
      return i;
    }
  }
  return 0;
}

}  // namespace tests
}  // namespace km
