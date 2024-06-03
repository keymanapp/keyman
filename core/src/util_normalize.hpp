/*
  Copyright:    © SIL International.
  Description:  Normalization and Regex utilities
  Create Date:  23 May 2024
  Authors:      Steven R. Loomis
*/

#pragma once

#include <string>
#include "keyman_core.h"

namespace km {
namespace core {
namespace util {

/** Normalize a u32string inplace to NFD. @return false on failure */
bool normalize_nfd(std::u32string &str);

/** Normalize a u16string inplace to NFD. @return false on failure */
bool normalize_nfd(std::u16string &str);

/** normalize src to dst in NFD. @return false on failure */
bool normalize_nfd(km_core_cu const * src, std::u16string &dst);

}
}
}
