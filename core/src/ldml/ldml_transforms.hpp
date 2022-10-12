/*
  Copyright:    © SIL International.
  Description:  Internal functions for LDML transforms
  Create Date:  6 Oct 2022
  Authors:      Steven R. Loomis
*/

#pragma once

#include <deque>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>

namespace km {
namespace kbp {
namespace ldml {

/**
 * An ordered list of strings.
 */
typedef std::deque<std::u16string> string_list;
/**
 * map from transform list to string
 */
typedef std::map<string_list, std::u16string> simple_transforms;

class transforms {
private:
  simple_transforms simple;

public:
  transforms();

  /**
   * Add a simple transform rule
   * @param match list of matching string elements
   * @param output output string for this rule
   */
  void add(const string_list &match, const std::u16string &output);

  /**
   * Check for any matching transform rules
   * @param ctxt context array for matching
   * @param output fillin: will contain the output string for this rule, if there is a match
   * @return 0 if no match, or the length of the ctxt string matched, starting at the end
   */
  size_t matchContext(const string_list &ctxt, std::u16string &output) const;
};

}  // namespace ldml
}  // namespace kbp
}  // namespace km
