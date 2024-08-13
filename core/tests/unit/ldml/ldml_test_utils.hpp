/*
  Copyright:    © 2022-2023 SIL International.
  Description:  Internal utils for ldml tests
  Create Date:  16 Mar 2023
  Authors:      Steven R. Loomis (SRL)
  History:      Refactored out of ldml_test_source.cpp
*/

#pragma once

#include <iostream>
#include <string>
#include <kmx/kmx_processevent.h> // for char to vk mapping tables
#include <kmx/kmx_xstring.h> // for surrogate pair macros
#include <kmx/kmx_plus.h>

#include "keyman_core.h"

namespace km {
namespace tests {

km_core_virtual_key get_vk(std::string const &vk);

}  // namespace tests
}  // namespace km
