/*
  Copyright:    © 2018 SIL International.
  Description:  This is a wrapper for the process_event API.
                TODO: Add a mecahnism to trigger output of PERSIST_OPT &
                RESET_OPT actions items, options support and context matching.
  Create Date:  17 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      17 Oct 2018 - TSE - Initial implementation.
*/

#include <keyman/keyboardprocessor.h>
#include "processor.hpp"
#include "state.hpp"

km_kbp_status
km_kbp_event(
  km_kbp_state *state,
  uint32_t event,
  void* data
) {
  assert(state != nullptr);
  if(state == nullptr) {
    return KM_KBP_STATUS_INVALID_ARGUMENT;
  }

  // event: KM_KBP_EVENT_KEYBOARD_ACTIVATED; data should be nullptr
  // future event: KM_KBP_EVENT_KEYBOARD_DEACTIVATED
  switch(event) {
    case KM_KBP_EVENT_KEYBOARD_ACTIVATED:
      assert(data == nullptr);
      if(data != nullptr) {
        return KM_KBP_STATUS_INVALID_ARGUMENT;
      }
      break;
    default:
      return KM_KBP_STATUS_INVALID_ARGUMENT;
  }

  return state->processor().external_event(state, event, data);
}

km_kbp_status
km_kbp_process_event(km_kbp_state *state,
                     km_kbp_virtual_key vk,
                     uint16_t modifier_state,
                     uint8_t is_key_down,
                     uint16_t event_flags) {
  assert(state != nullptr);
  if(state == nullptr) {
    return KM_KBP_STATUS_INVALID_ARGUMENT;
  }
  return state->processor().process_event(state, vk, modifier_state, is_key_down, event_flags);
}

km_kbp_status
km_kbp_process_queued_actions(
      km_kbp_state *state
      ) {
  assert(state != nullptr);
  if(state == nullptr) {
    return KM_KBP_STATUS_INVALID_ARGUMENT;
  }
  return state->processor().process_queued_actions(state);
}

km_kbp_attr const *
km_kbp_get_engine_attrs(km_kbp_state const *state)
{
  assert(state != nullptr);
  if(state == nullptr) {
    return nullptr;
  }
  return &state->processor().attributes();
}
