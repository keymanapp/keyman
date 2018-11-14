/*
  Copyright:    © 2018 SIL International.
  Description:  This is a test implementation of the keyboard processor API to
                enable testing API clients against a basic keyboard and give
                them something to link against and load.
                TODO: Add a mecahnism to trigger output of PERSIST_OPT &
                RESET_OPT actions items, options support and context matching.
  Create Date:  17 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      17 Oct 2018 - TSE - Initial implementation.
*/

#include <keyman/keyboardprocessor.h>
#include "processor.hpp"
#include "state.hpp"


km_kbp_status km_kbp_process_event(km_kbp_state *state,
                          km_kbp_virtual_key vk, uint16_t modifier_state)
{
  km::kbp::keyboard const & k = state->keyboard();
  return k.processor().process_event(state, vk, modifier_state);
}


km_kbp_attr const * km_kbp_get_engine_attrs(km_kbp_state *state)
{
  return state->keyboard().processor().get_attrs();
}

namespace km {
  namespace kbp
  {

km_kbp_status kmx_processor::process_event(km_kbp_state *state, km_kbp_virtual_key vk, uint16_t modifier_state) const {
  return 0;
}

constexpr km_kbp_attr const engine_attrs = {
  256,
  KM_KBP_LIB_CURRENT,
  KM_KBP_LIB_AGE,
  KM_KBP_LIB_REVISION,
  KM_KBP_TECH_KMX,
  "SIL International"
};

km_kbp_attr const * kmx_processor::get_attrs() const {
  //TODO
  return &engine_attrs;
}

} // namespace kbp
} // namespace km