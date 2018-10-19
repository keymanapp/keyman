#pragma once
#include <list>
#include <vector>

#include <keyboardprocessor.h>

// Forward declarations
class json;

namespace km {
namespace kbp
{

class context : public std::list<km_kbp_context_item>
{
  std::vector<value_type> mutable _pres;

public:
  const_pointer data() const
  {
    _pres.assign(begin(), end());
    return _pres.data();
  }
};

}
}

json & operator << (json &, km::kbp::context const &);

json & operator << (json &, km_kbp_context_item const &);


struct km_kbp_context : public km::kbp::context {};
