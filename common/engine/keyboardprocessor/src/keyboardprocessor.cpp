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

#include <keyboardprocessor.h>

#include "state.hpp"


namespace
{

// An table of VKEY to character sequence mappings, one per shift state.
// An empty string inidicates no mapping.
constexpr char const table[2][256][4] = {
  {
    "","","","","","","","","","\t","","","","\015","","",
    "","","","","","","","","","","","\033","","","","",
    " ","","","","","","","","","","","","","","","",
    "0","1","2","3","4","5","6","7","8","9","","","","","","",
    "","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o",
    "p","q","r","s","t","u","v","w","x","y","z","","","","","",
    "0","1","2","3","4","5","6","7","8","9","*","+","","-",".","/",
    "f1","f2","f3","f4","f5","f6","f7","f8","f9","f10","f11","f12","f13","f14","f15","f16",
    "f17","f18","f19","f20","f24","","","","","","","","","","","",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","",";","=",",","-",".","/","`",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","","","[","\\","]","'","","",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","","","","","","","","",
  },
  {
    "","","","","","","","","","\t","","","","\015","","",
    "","","","","","","","","","","","\033","","","","",
    " ","","","","","","","","","","","","","","","",
    ")","!","@","#","$","%","^","&","*","(","","","","","","",
    "","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O",
    "P","Q","R","S","T","U","V","W","X","Y","Z","","","","","",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","",":","+","<","_",">","?","~",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","","","{","|","}","\"","","",
    "","","","","","","","","","","","","","","","",
    "","","","","","","","","","","","","","","","",
  }
};

constexpr km_kbp_attr const engine_attrs = {
  256,
  KM_KBP_LIB_CURRENT,
  KM_KBP_LIB_AGE,
  KM_KBP_LIB_REVISION,
  KM_KBP_TECH_UNSPECIFIED,
  "SIL International"
};

}

km_kbp_status km_kbp_process_event(km_kbp_state *state,
                          km_kbp_virtual_key vk, uint16_t modifier_state)
{
  try
  {
    state->actions.clear();

    switch (vk)
    {
      case KM_KBP_VKEY_BKSP:
        state->context().pop_back();
        state->actions.emplace_back(km_kbp_action_item {KM_KBP_IT_BACK, {0,}, {0}});
        state->actions.emplace_back(km_kbp_action_item {KM_KBP_IT_END, {0,}, {0}});
        break;

      default:
        auto shift_state = modifier_state & KM_KBP_MODIFIER_SHIFT;
        // Only process further one of the shift states has something to output.
        if (table[0][vk][0] || table[1][vk][0])
        {
          auto char_seq = table[shift_state][vk];

          for (auto c = char_seq; *c; ++c)
          {
            km_kbp_usv usv = *c;
            state->context().emplace_back(km_kbp_context_item {KM_KBP_CT_CHAR,{0,},{usv}});
            state->actions.emplace_back(km_kbp_action_item {KM_KBP_IT_CHAR, {0,}, {.character = usv}});
          }
          state->actions.emplace_back(km_kbp_action_item {KM_KBP_IT_END, {0,}, {0}});

          return KM_KBP_STATUS_OK;
        }

        // Both shift states output nothing, generate an alert.
        state->actions.emplace_back(km_kbp_action_item {KM_KBP_IT_ALERT, {0,}, {0}});
        state->actions.emplace_back(km_kbp_action_item {KM_KBP_IT_END, {0,}, {0}});
        break;
    }
  }
  catch (std::bad_alloc)
  {
    state->actions.clear();
    return KM_KBP_STATUS_NO_MEM;
  }

  return KM_KBP_STATUS_OK;
}


km_kbp_attr const * km_kbp_get_engine_attrs()
{
  return &engine_attrs;
}
