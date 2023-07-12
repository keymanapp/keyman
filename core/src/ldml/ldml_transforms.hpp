/*
  Copyright:    © SIL International.
  Description:  Internal functions for LDML transforms
  Create Date:  6 Oct 2022
  Authors:      Steven R. Loomis
*/

#pragma once

#include "kmx/kmx_plus.h"
#include <deque>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>

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
  /** from a USet */
  element(const USet &u, KMX_DWORD flags);
  /** from a single char */
  element(km_kbp_usv ch, KMX_DWORD flags);

  /** @returns true if a USet type */
  bool is_uset() const;
  bool is_prebase() const;
  bool is_tertiary_base() const;
  signed char get_tertiary() const;
  signed char get_order() const;
  /** @returns raw elem flags */
  KMX_DWORD get_flags() const;
  /** @returns true if matches this character*/
  bool matches(km_kbp_usv ch) const;

private:
  // TODO-LDML: support multi-char strings
  const km_kbp_usv chr;
  const USet uset;
  const KMX_DWORD flags;
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

/** a single char, categorized according to reorder rules*/
struct reorder_sort_key {
  km_kbp_usv ch;         // the single char value
  signed char primary;   // primary order value
  size_t secondary;      // index position
  signed char tertiary;  // tertiary value, defaults to 0
  size_t quaternary;     // index again

  /**
   * Return -1, 0, 1 depending on order
  */
  int compare(const reorder_sort_key &other) const;
  bool operator<(const reorder_sort_key &other) const;

  /** create a 'baseline' sort key, all 0 primary weights */
  static std::deque<reorder_sort_key> from(const std::u32string &str);

  /** TODO-LDML: for debugging. */
  void dump() const;
};

/**
 * List of elements.
 */
class element_list : public std::deque<element> {
public:
  /** @returns 0 if no match, or number of chars at end matched */
  size_t match_end(const std::u32string &str) const;

  /**
   * Update the deque (see reorder_sort_key::from()) with the weights from this element list
   * starting at the beginning of this element list
   * @param offset start at this offset in the deque. Still starts at the first element
   * @param the key deque to update
   * @returns the key parameter
  */
  std::deque<reorder_sort_key> &update_sort_key(size_t offset, std::deque<reorder_sort_key> &key) const;
};

class reorder_entry {
public:
  /** construct an entry from elements */
  reorder_entry(const element_list &elements);
  /** construct an element from elements and before */
  reorder_entry(const element_list &elements, const element_list &before);
  /**
   * Does it match?
   * @param str string to match
   * @param offset start matching at this offset
   * @return 0 if no match otherwise length matched
   */
  size_t match_end(std::u32string &str, size_t offset, size_t len) const;

public:
  element_list elements;
  element_list before;
};

/** a list of entries, such as in a group */
typedef std::deque<reorder_entry> reorder_list;

/** subtype that's a list of reorders */
struct reorder_group {
public:
  reorder_list list;
  /** apply this reordering. Return true if changed. */
  bool apply(std::u32string &str) const;
};

/** container for either a transform or a reorder group */
class any_group {
public:
  any_group(const transform_group &g);
  any_group(const reorder_group &g);
  any_group_type type;  // transform or reorder
  transform_group transform;
  reorder_group reorder;
};

/**
 * A list of any_groups
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
  void addGroup(const transform_group &s);
  /**
   * Add a reorder group
   */
  void addGroup(const reorder_group &s);

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

  /**
   * For tests - TODO-LDML only supports reorder
   * @return true if str was altered
   */
  bool apply(std::u32string &str);

public:
  static transforms *
  load(const kmx::kmx_plus &kplus, const kbp::kmx::COMP_KMXPLUS_TRAN *tran, const kbp::kmx::COMP_KMXPLUS_TRAN_Helper &tranHelper);
};
/**
 * Loader for transform groups (from tran or bksp)
 */
}  // namespace ldml
}  // namespace kbp
}  // namespace km
