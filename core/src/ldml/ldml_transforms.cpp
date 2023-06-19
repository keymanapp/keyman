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

transforms::transforms() : transform_groups() {
}

void
transforms::addTransformGroup(const transform_group &s) {
  transform_groups.push_back(s);
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

size_t
transforms::apply(const std::u16string &input, std::u16string &output) {
  /**
   * Accumulate the maximum matched size of the end of 'input' here.
   * It will be replaced with 'output'.
   * Matched can increment.
   * TODO-LDML:  As an optimization, we could decrement 'matched' for every char of output
   * which is already in input. Example (regex example):
   * - str = "xxyyzz";
   * - s/zz$/z/ -> match=2, output='z'
   * - s/z$/zz/ -> match=2, output='zz'
   *      (but could contract to match=0, output='' as already present)
   * - s/zz$/zw/ -> match=2, output='zw'
   *      (but could contract to match=1, output='w')
   */
  size_t matched              = 0;
  std::u16string updatedInput = input;  // modified input
  for (auto group = transform_groups.begin(); group < transform_groups.end(); group++) {
    // for each transform group
    // break out once there's a match
    // TODO-LDML: reorders
    // Assume it's a non reorder group
    size_t subMatched = 0;

    // find the first match in this group (if present)
    auto transform = group->match(updatedInput, subMatched);

    if (transform != nullptr) {
      // apply

      // get the updated sub output
      std::u16string subOutput = transform->apply(updatedInput, subMatched);

      // remove the matched part of the updatedInput
      updatedInput.resize(updatedInput.length() - subMatched);
      updatedInput.append(subOutput);  // which could be empty

      if (subMatched > output.size()) {
        // including first time through
        // expand match by amount subMatched prior to output
        matched += (subMatched - output.size());
      } // else: didn't match prior to the existing output, so don't expand 'match'

      // now update 'output'
      if (subOutput.length() >= output.length() || subMatched > output.length()) {
        output = subOutput; // simple: all output
      } else {
        output.resize(output.length() - subMatched);
        output.append(subOutput);
      }
    } // else: continue to next group
  }
  // TODO-LDML: optimization (mentioned above) to contract 'matched' if possible.
  // could also handle from="x" to="x"
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
      transforms->addTransformGroup(newGroup);
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
