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
    kbp::context          _ctxt;
    kbp::options          _options;
    kbp::keyboard const & _kb;

public:
    state(kbp::keyboard const & kb, km_kbp_option_item const * env);
    state(state const &) = default;
    state(state const &&) = delete;

    kbp::context       &  context() noexcept            { return _ctxt; }
    kbp::context const &  context() const noexcept      { return _ctxt; }

    kbp::options &          options() noexcept        { return _options; }
    kbp::options const &    options() const noexcept  { return _options; }

    kbp::keyboard const &  keyboard() const noexcept      { return _kb; }
};

inline
state::state(kbp::keyboard const & kb, km_kbp_option_item const * env)
: _options(kb.default_options, env), _kb(kb)
{}

}
}

struct km_kbp_state : public km::kbp::state
{
  template<typename... Args>
  km_kbp_state(Args&&... args) : km::kbp::state(std::forward<Args>(args)...)
  {}

  std::vector<km_kbp_action_item> actions;
};
