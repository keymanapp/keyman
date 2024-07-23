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

/** Normalize a u32string inplace to NFC. @return false on failure */
bool normalize_nfc(std::u32string &str);

/** Normalize a u16string inplace to NFC. @return false on failure */
bool normalize_nfc(std::u16string &str);

/** Normalize a u32string inplace to NFD. @return false on failure */
bool normalize_nfd(std::u32string &str);

/** Normalize a u16string inplace to NFD. @return false on failure */
bool normalize_nfd(std::u16string &str);

/** normalize src to dst in NFD. @return false on failure */
bool normalize_nfd(km_core_cu const * src, std::u16string &dst);

/** normalize (decompose) a single cp to string. @return false on failure */
bool normalize_nfd(km_core_usv cp, std::u32string &dst);

/** @return true if string is already NFD */
bool is_nfd(const std::u16string& str);

/** @return true if string is already NFD */
bool is_nfd(const std::u32string& str);

/** @return true if cp can interacts with prior chars */
bool has_nfd_boundary_before(km_core_usv cp);

/** convenience function, caller owns storage */
km_core_usv *string_to_usv(const std::u32string& src);

}
}
}
