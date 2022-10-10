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
#include <deque>
#include <vector>

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

class text_node {
public:
    text_node(const std::u16string &output);
    text_node();
    /**
     * add a vertex to another node
     */
    void add(const std::u16string &ch, const text_node* other);

    bool is_only_terminal() const;
    const text_node *get(const std::u16string *ch) const;
private:
    std::u16string output;
    std::map<std::u16string, const text_node*> vertices;
};

class transforms {
private:
    /**
     * TextNodes. First element is the root.
     */
    std::vector<text_node> simple;
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
