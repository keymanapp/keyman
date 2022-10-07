/*
  Copyright:    © SIL International.
  Description:  Internal functions for LDML transforms
  Create Date:  6 Oct 2022
  Authors:      Steven R. Loomis
*/

#pragma once

#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace km {
namespace kbp {
namespace ldml {

/**
 * An ordered list of strings.
 */
typedef std::vector<std::u16string> string_list;
/**
 * map from transform list to string
 */
typedef std::map<string_list, std::u16string> simple_transforms;

class transforms {
private:
    simple_transforms  simple;
public:
    transforms();

    /**
     * Add a transform
     */
    void
    add(const string_list &list, const std::u16string &tostr);

    size_t
    matchContext(const string_list &ctxt, std::u16string &outputString) const;

};


}  // namespace ldml
}  // namespace kbp
}  // namespace km
