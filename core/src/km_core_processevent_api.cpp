/*
  Copyright:    © 2018 SIL International.
  Description:  This is a wrapper for the process_event API.
                TODO: Add a mecahnism to trigger output of PERSIST_OPT &
                RESET_OPT actions items, options support and context matching.
  Create Date:  17 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      17 Oct 2018 - TSE - Initial implementation.
*/

#include "keyman_core.h"

#include "processor.hpp"
#include "state.hpp"

km_core_status
km_core_event(
  km_core_state *state,
  uint32_t event,
  void* data
) {
  assert(state != nullptr);
  if(state == nullptr) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }

  // event: KM_CORE_EVENT_KEYBOARD_ACTIVATED; data should be nullptr
  // future event: KM_CORE_EVENT_KEYBOARD_DEACTIVATED
  switch(event) {
    case KM_CORE_EVENT_KEYBOARD_ACTIVATED:
      assert(data == nullptr);
      if(data != nullptr) {
        return KM_CORE_STATUS_INVALID_ARGUMENT;
      }
      break;
    default:
      return KM_CORE_STATUS_INVALID_ARGUMENT;
  }

  return state->processor().external_event(state, event, data);
}

km_core_status
km_core_process_event(km_core_state *state,
                     km_core_virtual_key vk,
                     uint16_t modifier_state,
                     uint8_t is_key_down,
                     uint16_t event_flags) {
  assert(state != nullptr);
  if(state == nullptr) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }
  km_core_status status = state->processor().process_event(state, vk, modifier_state, is_key_down, event_flags);

  state->prepare_actions();

  return status;
}

km_core_status
km_core_process_queued_actions(
      km_core_state *state
      ) {
  assert(state != nullptr);
  if(state == nullptr) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }
  return state->processor().process_queued_actions(state);
}

km_core_attr const *
km_core_get_engine_attrs(km_core_state const *state)
{
  assert(state != nullptr);
  if(state == nullptr) {
    return nullptr;
  }
  return &state->processor().attributes();
}
