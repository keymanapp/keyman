/*
  Copyright:    © SIL International.
  Description:  This is an implementation of the LDML keyboard spec 3.0.
  Create Date:  7 Oct 2022
  Authors:      Steven R. Loomis
*/

#include "ldml_transforms.hpp"
#include "debuglog.h"

#ifndef assert
#define assert(x) // TODO-LDML
#endif

namespace km {
namespace kbp {
namespace ldml {


element::element(const USet &u, KMX_DWORD flags) : str(), uset(u), flags(flags) {
}

element::element(const std::u16string &s, KMX_DWORD flags) : str(s), uset(), flags(flags) {
}

bool element::is_uset() const {
  return (flags & LDML_ELEM_FLAGS_TYPE) == LDML_ELEM_FLAGS_TYPE_USET;
}

transform_entry::transform_entry(const std::u16string &from, const std::u16string &to) : fFrom(from), fTo(to) {
}

size_t
transform_entry::match(const std::u16string &input) const {
  if (input.length() < fFrom.length()) {
    return 0;
  }
  // string at end
  auto substr = input.substr(input.length() - fFrom.length(), fFrom.length());
  if (substr != fFrom) {
    return 0;
  }
  return substr.length();
}

std::u16string
transform_entry::apply(const std::u16string &/*input*/, size_t /*matchLen*/) const {
  return fTo;
}

any_group::any_group(const transform_group& g) : type(any_group_type::transform), transform(g), reorder() {

}
any_group::any_group(const reorder_group& g) : type(any_group_type::reorder), transform(), reorder(g) {
}


transforms::transforms() : transform_groups() {
}

void
transforms::addGroup(const transform_group &s) {
  transform_groups.emplace_back(s);
}

void
transforms::addGroup(const reorder_group &s) {
  transform_groups.emplace_back(s);
}


transform_group::transform_group() {
}

/**
 * return the first transform match in this group
 */
const transform_entry *
transform_group::match(const std::u16string &input, size_t &subMatched) const {
  for (auto transform = begin(); (subMatched == 0) && (transform < end()); transform++) {
    // TODO-LDML: non regex implementation
    // is the match area too short?
    subMatched = transform->match(input);
    if (subMatched != 0) {
      return &(*transform);  // return alias to transform
    }
  }
  return nullptr;
}

/**
 * Apply this entire transform set to the input.
 * Example: input "abc" -> output="xyz", return=2:  replace last two chars "bc" with "xyz", so final output = "abxyz";
 * @param input input string, will match at end: unmodified
 * @param output on output: if return>0, contains text to replace
 * @return match length: number of chars at end of input string to modify.  0 if no match.
*/
size_t
transforms::apply(const std::u16string &input, std::u16string &output) {
  /**
   * Example:
   * Group0:   za -> c, a -> bb
   * Group1:   bb -> ccc
   * Group2:   cc -> d
   * Group3:   tcd -> e
   *
   * Initial condition: matched = subMatched = 0. updatedInput = input = 'ta', output = ''
   *
   * Group0:  matches 'a',  subMatched=1, output='bb', updatedInput='tbb', matched=1
   *  Initial match. subMatched=1 which is > the previous output.length,
   *  so matched += (1-0) == 1
   *
   * Group1:  matches 'bb', subMatched=2, output='ccc', updatedInput='tccc', matched=1
   *  Although the transform group matched 2 chars ('bb'), the match in the original is 1
   *  This time subMatched=2 but previous output.length was 2, so matched remains at 1
   *  In other words, we didn't match outside of the existing match boundary.
   *
   * Group2:  matches 'cc', subMatched=2, output='cd', updatedInput='tcd', matched=1
   *  subMatched <= previous output.length, so no change to mathed
   *
   * Group3:  matches 'tcd', subMatched=3, output='e', updatedInput='e', matched=2
   *  Now subMatched=3, but previous output ('cd').length is only 2.
   *  In other words, we matched before the previous output's start - we matched the 't' also
   *  or this reason, matched+=(3-2) == 1.  So matched is now 2.
   */

  /**
   * Accumulate the maximum matched size of the end of 'input' here.
   * It will be replaced with 'output'.
   * Matched can increment.
   */
  size_t matched              = 0;
  /** modified copy of input */
  std::u16string updatedInput = input;
  for (auto group = transform_groups.begin(); group < transform_groups.end(); group++) {
    // for each transform group
    // break out once there's a match
    // TODO-LDML: reorders
    // Assume it's a non reorder group
    /** Length of match within this group*/
    size_t subMatched = 0;

    // find the first match in this group (if present)
    // TODO-LDML: check if reorder
    if (group->type == any_group_type::transform) {
      auto transform = group->transform.match(updatedInput, subMatched);

      if (transform != nullptr) {
        // now apply the found transform

        // update subOutput (string) and subMatched
        std::u16string subOutput = transform->apply(updatedInput, subMatched);

        // remove the matched part of the updatedInput
        updatedInput.resize(updatedInput.length() - subMatched);  // chop of the subMatched part at end
        updatedInput.append(subOutput);                           // subOutput could be empty such as in backspace transform

        if (subMatched > output.size()) {
          // including first time through
          // expand match by amount subMatched prior to output
          matched += (subMatched - output.size());
        }  // else: didn't match prior to the existing output, so don't expand 'match'

        // now update 'output'
        if (subOutput.length() >= output.length() || subMatched > output.length()) {
          output = subOutput;  // replace all output
        } else {
          // replace output with new output
          output.resize(output.length() - subMatched);
          output.append(subOutput);
        }
      }
    } else if (group->type == any_group_type::reorder) {
      // TODO-LDML reorder
    }
    // else: continue to next group
  }
    /**
     * TODO-LDML: optimization to contract 'matched' if possible.
     * We could decrement 'matched' for every char of output
     * which is already in input. Example (regex example):
     * - str = "xxyyzz";
     * - s/zz$/z/ -> match=2, output='z'
     * - s/z$/zz/ -> match=2, output='zz'
     *      (but could contract to match=0, output='' as already present)
     * - s/zz$/zw/ -> match=2, output='zw'
     *      (but could contract to match=1, output='w')
     *
     * could also handle from="x" to="x" as match=0
     */
    return matched;
}

// simple impl
bool
transforms::apply(std::u16string & str) {
  std::u16string output;
  size_t matchLength = apply(str, output);
  if (matchLength == 0) {
    return false;
  }
  str.resize(str.size() - matchLength);
  str.append(output);
  return true;
}


// Loader

transforms *
transforms::load(const kmx::kmx_plus &kplus,
                      const kbp::kmx::COMP_KMXPLUS_TRAN *tran,
                      const kbp::kmx::COMP_KMXPLUS_TRAN_Helper &tranHelper) {
  if (tran == nullptr) {
    DebugLog("for tran: tran is null");
    assert(false);
    return nullptr;
  }
  if (!tranHelper.valid()) {
    DebugLog("for tran: tranHelper is invalid");
    assert(false);
    return nullptr;
  }
  if (nullptr == kplus.elem) {
    DebugLog("for tran: kplus.elem == nullptr");
    assert(false);
    return nullptr;
  }
  if (nullptr == kplus.strs) {
    DebugLog("for tran: kplus.strs == nullptr");  // need a string table to get strings
    assert(false);
    return nullptr;
  }
  if (nullptr == kplus.vars) {
    DebugLog("for tran: kplus.vars == nullptr");  // need a vars table to get maps
    assert(false);
    return nullptr;
  }

  // with that out of the way, let's set it up

  transforms *transforms = new ldml::transforms();

  for (KMX_DWORD groupNumber = 0; groupNumber < tran->groupCount; groupNumber++) {
    const kmx::COMP_KMXPLUS_TRAN_GROUP *group = tranHelper.getGroup(groupNumber);
    // C++20 returns a reference here
    // auto &newGroup = allGroups->emplace_back();

    if (group->type == LDML_TRAN_GROUP_TYPE_TRANSFORM) {
      transform_group newGroup;

      for (KMX_DWORD itemNumber = 0; itemNumber < group->count; itemNumber++) {
        const kmx::COMP_KMXPLUS_TRAN_TRANSFORM *element = tranHelper.getTransform(group->index + itemNumber);
        const std::u16string fromStr                    = kplus.strs->get(element->from);
        const std::u16string toStr                      = kplus.strs->get(element->to);
        std::u16string mapFrom, mapTo;

        if (element->mapFrom && element->mapTo) {
          // strings: variable name
          mapFrom = kplus.strs->get(element->mapFrom);
          mapTo   = kplus.strs->get(element->mapTo);
        }

        newGroup.emplace_back(fromStr, toStr);  // creating a transform_entry
      }
      transforms->addGroup(newGroup);
    } else if(group->type == LDML_TRAN_GROUP_TYPE_REORDER) {
      // TODO-LDML: reorder
      DebugLog("Skipping reorder (for now) TODO-LDML");
    } else {
      // internal error - some other type - should have been caught by validation
      DebugLog("ERROR: some other type");
    }
  }
  return transforms;
}


}  // namespace ldml
}  // namespace kbp
}  // namespace km
