#pragma once
#include <vector>

#include <keyboardprocessor.h>

#include "context.hpp"
#include "option.hpp"
#include "keyboard.hpp"


namespace km {
namespace kbp
{
using action = km_kbp_action_item;

class state
{
protected:
    option_set const      _env;
    option_set            _run;
    kbp::keyboard const & _kb;
    kbp::context          _ctxt;

public:
    state(kbp::keyboard const & kb, option_set const & opts);
    state(state const &) = default;
    state(state const &&) = delete;

    kbp::context       &  context() noexcept            { return _ctxt; }
    kbp::context const &  context() const noexcept      { return _ctxt; }

    option_set &          options() noexcept        { return _run; }
    option_set const &    options() const noexcept  { return _run; }

    kbp::keyboard const &  keyboard() const noexcept      { return _kb; }

    option_set const &    environment() const noexcept { return _env; }
};

inline
state::state(kbp::keyboard const & kb, option_set const & opts)
: _env(std::move(opts)), _run(KM_KBP_OPT_UNKNOWN), _kb(kb)
{}

}
}

struct km_kbp_state : public km::kbp::state
{
  template<typename... Args>
  km_kbp_state(Args&&... args)
  : km::kbp::state(std::forward<Args>(args)...),
    run_opts_adaptor(_run) {}

  km_kbp_option_set run_opts_adaptor;
  std::vector<km_kbp_action_item> actions;
};
