/*
  Copyright:    © 2018 SIL International.
  Description:  Internal context class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_kbp_context_api.cpp
*/

#pragma once
#include <list>
#include <vector>
#include <keyman/keyboardprocessor.h>

// Forward declarations
class json;

namespace km {
namespace kbp
{

// This will likely be replaced with a class implementing a more space
// efficient data structure such as a ring buffer or bounded queue.
class context: public std::list<km_kbp_context_item>
{
public:
  void push_character(km_kbp_usv);
  void push_marker(uint32_t);
};


inline
void context::push_character(km_kbp_usv usv) {
  emplace_back(km_kbp_context_item { KM_KBP_CT_CHAR, {0,}, {usv} });
}


inline
void context::push_marker(uint32_t marker) {
  emplace_back(km_kbp_context_item { KM_KBP_CT_MARKER, {0,}, {marker} });
}

} // namespace kbp
} // namespace km

json & operator << (json &, km::kbp::context const &);
json & operator << (json &, km_kbp_context_item const &);


struct km_kbp_context : public km::kbp::context
{
};
