/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Public debug API implementation
 */

#include <cassert>
#include <algorithm>
#include <sstream>

#include "keyman_core.h"

#include "processor.hpp"
#include "state.hpp"

using namespace km::core;

km_core_status
km_core_state_debug_set(
  km_core_state *state,
  int value
) {
  assert(state);
  if(!state) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }
  state->debug_items().set_enabled(value ? true : false);
  return KM_CORE_STATUS_OK;
}

uint8_t
km_core_state_debug_get(
  km_core_state const *state
) {
  assert(state);
  if(!state) {
    return 0;
  }
  return state->debug_items().is_enabled() ? 1 : 0;
}

km_core_state_debug_item const *
km_core_state_debug_items(
  km_core_state const *state,
  size_t *num_items
) {
  assert(state && state->debug_items().size() > 0);
  if (!state || state->debug_items().empty()) {
    if(num_items) {
      *num_items = 0;
    }
    return nullptr;
  }

  if (num_items) {
    *num_items = state->debug_items().size();
  }

  // Process events will ensure that the debug vector is always well
  // terminated
  assert(state->debug_items().back().type == KM_CORE_DEBUG_END);
  return state->debug_items().data();
}
