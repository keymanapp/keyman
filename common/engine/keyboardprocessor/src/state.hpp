/*
  Copyright:    © 2018 SIL International.
  Description:  Internal context class and adaptor class for the API.
  Create Date:  17 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      17 Oct 2018 - TSE - Refactored out of km_kbp_state_api.cpp
*/

#pragma once
#include <cassert>
#include <vector>

#include <keyman/keyboardprocessor.h>

#include "context.hpp"
#include "option.hpp"
#include "keyboard.hpp"



namespace km {
namespace kbp
{
//Forward declarations
class abstract_processor;

using action = km_kbp_action_item;

class state
{
protected:
    kbp::context                    _ctxt;
    kbp::options                    _options;
    kbp::abstract_processor &       _processor;
    std::vector<option> _env;

public:
    state(kbp::abstract_processor & kb, km_kbp_option_item const *env);

    state(state const &) = default;
    state(state const &&) = delete;

    kbp::context       &  context() noexcept            { return _ctxt; }
    kbp::context const &  context() const noexcept      { return _ctxt; }

    kbp::options &          options() noexcept        { return _options; }
    kbp::options const &    options() const noexcept  { return _options; }

    kbp::abstract_processor const & processor() const noexcept { return _processor; }
    kbp::abstract_processor & processor() noexcept { return _processor; }
};

} // namespace kbp
} // namespace km

struct km_kbp_state : public km::kbp::state
{
  template<typename... Args>
  km_kbp_state(Args&&... args) : km::kbp::state(std::forward<Args>(args)...)
  {}

  std::vector<km_kbp_action_item> actions;
};
