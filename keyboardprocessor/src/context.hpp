#include <list>

#include <keyboardprocessor.h>

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

struct km_kbp_context : public km::kbp::context {};
