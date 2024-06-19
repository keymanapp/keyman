/*
  Copyright:    © 2023 SIL International.
  Description:  Implementation of the action API functions using internal
                data structures and functions.
  Create Date:  23 Oct 2023
  Authors:      Marc Durdin (MCD)
  History:      23 Oct 2023 - MCD - Initial implementation.
*/
#include <cassert>
#include <algorithm>
#include <sstream>
#include <cstring>

#include "keyman_core.h"

#include "jsonpp.hpp"

#include "processor.hpp"
#include "state.hpp"
#include "action.hpp"

using namespace km::core;

km_core_actions const * km_core_state_get_actions(
  km_core_state const *state
) {
  assert(state);
  if(!state) {
    return nullptr;
  }

  return static_cast<km_core_actions *>(&(const_cast<km_core_state *>(state)->action_struct()));
}

void km::core::actions_dispose(
  km_core_actions const & actions
) {
  if(actions.output) {
    delete[] actions.output;
  }

  if(actions.deleted_context) {
    delete[] actions.deleted_context;
  }

  if(actions.persist_options) {
    for(auto option = actions.persist_options; option->scope; option++) {
      delete[] option->key;
      delete[] option->value;
    }
    delete[] actions.persist_options;
  }

  memset(const_cast<km_core_actions*>(&actions), 0, sizeof(km_core_actions));
}

km_core_usv const *km::core::get_deleted_context(context const &app_context, unsigned int code_points_to_delete) {
  auto p = app_context.end();
  for(size_t i = code_points_to_delete; i > 0; i--, p--) {
    assert(p != app_context.begin());
  }

  auto deleted_context = new km_core_usv[code_points_to_delete + 1];
  for(size_t i = 0; i < code_points_to_delete; i++) {
    deleted_context[i] = p->character;
    p++;
  }
  deleted_context[code_points_to_delete] = 0;
  return deleted_context;
}
