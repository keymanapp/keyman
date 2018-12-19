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
km_kbp_process_event(km_kbp_state *state,
                          km_kbp_virtual_key vk, uint16_t modifier_state)
{
  return state->processor().process_event(state, vk, modifier_state);
}


km_kbp_attr const * 
km_kbp_get_engine_attrs(km_kbp_state const *state)
{
  return &state->processor().attributes();
}
