/*
  Copyright:    © SIL International.
  Description:  This is an implementation of the LDML keyboard spec 3.0.
  Create Date:  7 Oct 2022
  Authors:      Steven R. Loomis
*/

#include "ldml_transforms.hpp"

namespace km {
namespace kbp {
namespace ldml {

transforms::transforms() : simple() {
}

void
transforms::add(const string_list &list, const std::u16string &tostr) {
  simple.insert({list, tostr});
}

size_t
transforms::matchContext(const string_list &ctxt, std::u16string &outputString) const {
  if (simple.size() == 0) {
    return 0;  // no transforms, no match
  }

  // int longest_index            = -1;  // index to longest match in simple_transforms
  size_t longest_length = 0;  // length of longest match
  // auto i                       = 0;   // counter
  // TODO-LDML: need to handle partial matches.
  // bool partial       = false;      // true if the longest was a partial match
  for (auto it = simple.begin(); it != simple.end(); it++ /*,i++*/) {
    auto list = it->first;
    // see if it matches
    if (longest_length >= list.size()) {
      continue;  // already have a longer match
    }
    if (list.size() > ctxt.size()) {
      continue;  // not enough ctxt for a match (TODO-LDML: partial match.)
    }
    // Now, check for match
    bool is_match = true;
    for (size_t j = 0; j < list.size(); j++) {
      const auto found  = ctxt.at(ctxt.size() - j - 1);
      const auto expect = list.at(list.size() - j - 1);
      if (found != expect) {
        // mismatch, break
        is_match = false;
        break;
      }
    }
    if (!is_match) {
      continue;
    }
    // Found a match
    longest_length = list.size();
    // longest_index  = i;
    outputString = it->second;
  }

  return longest_length;
  // TODO-LDML: reorder
  // TODO-LDML: final
}

}  // namespace ldml
}  // namespace kbp
}  // namespace km
