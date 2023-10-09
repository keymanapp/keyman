/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Public debug API implementation
 */

#include <cassert>
#include <algorithm>
#include <sstream>

#include <keyman/keyman_core_api.h>

#include "processor.hpp"
#include "state.hpp"

using namespace km::kbp;

km_kbp_status
km_kbp_state_debug_set(
  km_kbp_state *state,
  int value
) {
  assert(state);
  if(!state) {
    return KM_KBP_STATUS_INVALID_ARGUMENT;
  }
  state->debug_items().set_enabled(value ? true : false);
  return KM_KBP_STATUS_OK;
}

uint8_t
km_kbp_state_debug_get(
  km_kbp_state const *state
) {
  assert(state);
  if(!state) {
    return 0;
  }
  return state->debug_items().is_enabled() ? 1 : 0;
}

km_kbp_state_debug_item const *
km_kbp_state_debug_items(
  km_kbp_state const *state,
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
  assert(state->debug_items().back().type == KM_KBP_DEBUG_END);
  return state->debug_items().data();
}
