#pragma once
#include <list>
#include <vector>

#include <keyboardprocessor.h>

// Forward declarations
class json;

namespace km {
namespace kbp
{

// This will likely be replaced with a class implementing a more space
// efficient data structure such as a ring buffer or bounded queue.
using context = std::list<km_kbp_context_item>;

}
}

json & operator << (json &, km::kbp::context const &);
json & operator << (json &, km_kbp_context_item const &);


struct km_kbp_context : public km::kbp::context
{
  std::vector<value_type> mutable _pres;

public:
  const_pointer data() const;
};
