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

using km::kbp::kmx::USet;

/**
 * Type of a group
*/
enum any_group_type {
  transform = LDML_TRAN_GROUP_TYPE_REORDER,
  reorder   = LDML_TRAN_GROUP_TYPE_REORDER,
};

/**
 * Corresponds to an 'elem' entry
*/
class element {
  public:
    element(const USet &u, KMX_DWORD flags);
    element(const std::u16string &s, KMX_DWORD flags);

    // TODO-LDML: getters that interpret flags
    bool is_uset() const;
    KMX_DWORD get_order() const;
    KMX_DWORD get_flags() const;

  private:
    const std::u16string str;
    const USet           uset;
    const KMX_DWORD      flags;
};

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

typedef std::deque<element> element_list;

class reorder_entry {
  public:
    reorder_entry(const element_list& elements);
    reorder_entry(const element_list& elements, const element_list& before);
  public:
    element_list elements;
    element_list before;
};

typedef std::deque<reorder_entry> reorder_list;

struct reorder_group {
  public:
    reorder_list list;
};

class any_group {
  public:
    any_group(const transform_group& g);
    any_group(const reorder_group& g);
    any_group_type type; // transform or reorder
    transform_group transform;
    reorder_group reorder;
};

/**
 * A list of groups
*/
typedef std::deque<any_group> group_list;

/**
 * This represents an entire <transforms> element
 */
class transforms {

private:
  group_list transform_groups;

public:
  transforms();

  /**
   * Add a transform group
   */
  void addGroup(const transform_group& s);
  /**
   * Add a reorder group
   */
  void addGroup(const reorder_group& s);

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
