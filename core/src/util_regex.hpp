/*
  Copyright:    © SIL International.
  Description:  Normalization and Regex utilities
  Create Date:  23 May 2024
  Authors:      Steven R. Loomis
*/

#pragma once

#include "core_icu.h"
#include "keyman_core.h"
#include <string>
#include <deque>

namespace km {
namespace core {
namespace util {

class km_regex {
public:
  km_regex();
  km_regex(const km_regex &other);
  km_regex(const std::u32string &pattern);
  ~km_regex();
  bool init(const std::u32string &pattern);

  size_t apply(
      const std::u32string &input,
      std::u32string &output,
      const std::u32string &to,
      const std::deque<std::u32string> &fromList,
      const std::deque<std::u32string> &toList) const;

  bool valid() const;
private:
#if KMN_NO_ICU
  void *stuff;
#else
  std::unique_ptr<icu::RegexPattern> fPattern;
#endif
// utility functions
  public:
  static int32_t findIndex(const std::u32string &match, const std::deque<std::u32string> &list);
};

}  // namespace util
}  // namespace core
}  // namespace km
