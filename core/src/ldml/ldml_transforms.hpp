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
#include "kmx/kmx_plus.h"


namespace km {
namespace kbp {
namespace ldml {


/**
 * Inner element, representing <transform>
*/
class transform_entry {
public:
  transform_entry(
      const std::u16string &from,
      const std::u16string &to
      /*TODO-LDML: mapFrom, mapTo*/
  );

  /**
   * @returns length if it's a match
  */
  size_t match(const std::u16string &input) const;

  /**
   * @returns output string
  */
  std::u16string apply(const std::u16string &input, size_t matchLen) const;

private:
  const std::u16string fFrom;  // TODO-LDML: regex
  const std::u16string fTo;
};

/**
 * An ordered list of strings.
 */
typedef std::deque<std::u16string> string_list;

/**
 * a group of <transform> entries - a <transformGroup>
 */
class transform_group : public std::deque<transform_entry> {
  public:
    transform_group();

    /**
     * Find the first match in the group
     * @param input input string to match
     * @param subMatched on output, the matched length
     * @returns alias to transform_entry or nullptr
    */
    const transform_entry *match(const std::u16string &input, size_t &subMatched) const;
};

/**
 * A list of groups
*/
typedef std::deque<transform_group> transform_group_list;

/**
 * This represents an entire <transforms> element
 */
class transforms {

private:
  transform_group_list transform_groups;

public:
  transforms();

  /**
   * Add a transform group
   */
  void addTransformGroup(const transform_group& s);

  /**
   * Attempt to match and apply a pattern change.
   *
   * @param input original input text. Will not be altered.
   * @param output if matched, contains the replacement output text
   * @return length in chars of the input (counting from the end) which matched context
   */
  size_t apply(const std::u16string &input, std::u16string &output);

  /**
   * For tests
   * @return true if str was altered
  */
  bool apply(std::u16string &str);

public:
  static transforms *
  load(
    const kmx::kmx_plus &kplus,
    const kbp::kmx::COMP_KMXPLUS_TRAN *tran,
    const kbp::kmx::COMP_KMXPLUS_TRAN_Helper &tranHelper);
};
/**
 * Loader for transform groups (from tran or bksp)
 */
}  // namespace ldml
}  // namespace kbp
}  // namespace km
