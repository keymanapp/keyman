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

#include "keyman_core.h"

#include "jsonpp.hpp"

#include "processor.hpp"
#include "state.hpp"
#include "action.hpp"

using namespace km::core;

km_core_usv const *get_deleted_context(context const &app_context, unsigned int code_points_to_delete);

km_core_actions const * km_core_state_get_actions(
  km_core_state const *state
) {
  assert(state);
  if(!state) {
    return nullptr;
  }

  auto action_items = km_core_state_action_items(state, nullptr);
  if(!action_items) {
    return nullptr;
  }

  km_core_actions * result = action_item_list_to_actions_object(action_items);

  // We keep a copy of the app context before normalization, as the
  // code_points_to_delete value can be updated by normalization, but by the
  // time we get it back from actions_normalize or
  // actions_update_app_context_nfu,  app_context has already been updated to
  // remove the necessary codepoints
  context* app_context = km_core_state_app_context(state);
  context app_context_for_deletion;
  std::copy(app_context->begin(), app_context->end(), std::back_inserter(app_context_for_deletion));


  if(state->processor().supports_normalization()) {
    // Normalize to NFC for those keyboard processors that support it
    if(!actions_normalize(km_core_state_context(state), km_core_state_app_context(state), result)) {
      km_core_actions_dispose(result);
      return nullptr;
    }
  } else {
    // For all other keyboard processors, we just copy the cached_context to the app_context
    if(!actions_update_app_context_nfu(km_core_state_context(state), km_core_state_app_context(state))) {
      km_core_actions_dispose(result);
      return nullptr;
    }
  }

  result->deleted_context = get_deleted_context(app_context_for_deletion, result->code_points_to_delete);

  return result;
}

km_core_status km_core_actions_dispose(
  km_core_actions const * actions
) {
  if(actions == nullptr) {
    return KM_CORE_STATUS_OK;
  }

  if(actions->output) {
    delete[] actions->output;
  }

  if(actions->deleted_context) {
    delete[] actions->deleted_context;
  }

  if(actions->persist_options) {
    for(auto option = actions->persist_options; option->scope; option++) {
      delete[] option->key;
      delete[] option->value;
    }
    delete[] actions->persist_options;
  }

  delete actions;

  return KM_CORE_STATUS_OK;
}

km_core_usv const *get_deleted_context(context const &app_context, unsigned int code_points_to_delete) {
  auto p = app_context.end();
  for(size_t i = code_points_to_delete; i > 0; i--, p--);

  auto deleted_context = new km_core_usv[code_points_to_delete + 1];
  for(size_t i = 0; i < code_points_to_delete; i++) {
    deleted_context[i] = p->character;
    p++;
  }
  deleted_context[code_points_to_delete] = 0;
  return deleted_context;
}
