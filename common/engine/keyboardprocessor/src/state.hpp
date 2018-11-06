/*
  Copyright:    © 2018 SIL International.
  Description:  Internal context class and adaptor class for the API.
  Create Date:  17 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      17 Oct 2018 - TSE - Refactored out of km_kbp_state_api.cpp
*/

#pragma once
#include <vector>

#include <keyman/keyboardprocessor.h>

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
    options const      _env;
    options            _run;
    kbp::keyboard const & _kb;
    kbp::context          _ctxt;

public:
    state(kbp::keyboard const & kb, options const & opts);
    state(state const &) = default;
    state(state const &&) = delete;

    kbp::context       &  context() noexcept            { return _ctxt; }
    kbp::context const &  context() const noexcept      { return _ctxt; }

    options &          options() noexcept        { return _run; }
    options const &    options() const noexcept  { return _run; }

    kbp::keyboard const &  keyboard() const noexcept      { return _kb; }

    options const &    environment() const noexcept { return _env; }
};

inline
state::state(kbp::keyboard const & kb, options const & opts)
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

  km_kbp_options run_opts_adaptor;
  std::vector<km_kbp_action_item> actions;
};
