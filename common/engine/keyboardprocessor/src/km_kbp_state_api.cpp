/*
  Copyright:    © 2018 SIL International.
  Description:  Implementation of the state API functions using internal
                data structures and functions.
  Create Date:  5 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      5  Oct 2018 - TSE - Initial implementation.
                18 Oct 2018 - TSE - Refactor out adaptor and internal classes
                                    into state.hpp
*/
#include <cassert>
#include <algorithm>
#include <sstream>

#include <keyman/keyboardprocessor.h>
#include "json.hpp"

#include "processor.hpp"
#include "state.hpp"

using namespace km::kbp;

// Forward declarations
class context;

km_kbp_status km_kbp_state_create(km_kbp_keyboard * keyboard,
                                  km_kbp_option_item const *env,
                                  km_kbp_state ** out)
{
  assert(keyboard); assert(env); assert(out);
  if (!keyboard || !env || !out)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  try
  {
    *out = new km_kbp_state(static_cast<abstract_processor&>(*keyboard), env);
  }
  catch (std::bad_alloc)
  {
    return KM_KBP_STATUS_NO_MEM;
  }
  return KM_KBP_STATUS_OK;
}


km_kbp_status km_kbp_state_clone(km_kbp_state const *state,
                                 km_kbp_state ** out)
{
  assert(state); assert(out);
  if (!state || !out)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  *out = new km_kbp_state(*state);
  return KM_KBP_STATUS_OK;
}


void km_kbp_state_dispose(km_kbp_state *state)
{
  delete state;
}


km_kbp_context *km_kbp_state_context(km_kbp_state *state)
{
  assert(state);
  if (!state) return nullptr;

  return static_cast<km_kbp_context *>(&state->context());
}


km_kbp_action_item const * km_kbp_state_action_items(km_kbp_state const *state,
                                                     size_t *num_items)
{
  assert(state && state->actions().size() > 0);
  if (!state || state->actions().empty()) return nullptr;

  if (num_items)
    *num_items = state->actions().size();

  // Process events will ensure that the actions vector is always well
  // teminated
  assert(state->actions().back().type == KM_KBP_IT_END);
  return state->actions().data();
}

namespace {
  char const * action_item_name_lut[] = {
    "",
    "character",
    "marker",
    "alert",
    "back",
    "persist",
    "reset",
    "vkeydown",
    "vkeyup",
    "vshiftdown",
    "vshiftup"
  };

  constexpr char const * const scope_names_lut[] = {
    u8"unspecified",
    u8"keyboard",
    u8"environment"
  };
}


json & operator << (json & j, km_kbp_action_item const &act)
{
  j << json::flat << json::object;
  if (act.type >= KM_KBP_IT_MAX_TYPE_ID)
  {
    j << "invalid" << json::null << json::close;
    return j;
  }

  j << action_item_name_lut[act.type];
  switch (act.type)
  {
    case KM_KBP_IT_END:
    case KM_KBP_IT_ALERT:
    case KM_KBP_IT_BACK:
      j << json::null;
      break;
    case KM_KBP_IT_CHAR:
    case KM_KBP_IT_MARKER:
      j << km_kbp_context_item {act.type, {0,}, {act.character}}; // TODO: is act.type correct here? it may map okay but this is bad practice to mix constants across types. Similarly using act.character instead of act.type
      break;
    case KM_KBP_IT_PERSIST_OPT:
      j << json::object
          << scope_names_lut[act.option->scope]
          << json::flat << json::object
            << act.option->key << act.option->value
          << json::close
        << json::close;
      break;
  }
  j << json::close;

  return j;
}


json & operator << (json & j, actions const & acts)
{
    j << json::array;
    for (auto & act: acts)
    {
      if (act.type != KM_KBP_IT_END)
      {
        j << act;
      }
    }
    j << json::close;
    return j;
}


km_kbp_status km_kbp_state_to_json(km_kbp_state const *state,
                                        char *buf,
                                        size_t *space)
{
  assert(state);
  if (!state)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  std::stringstream _buf;
  json jo(_buf);

  try
  {
    // Pretty print the document.
    jo << json::object
        << "$schema" << "keyman/keyboardprocessor/doc/introspection.schema"
        << "keyboard" << state->processor().keyboard()
//        << "options" << state->options()  TODO: Fix
        << "context" << state->context()
        << "actions" << state->actions()
        << json::close;
  }
  catch (std::bad_alloc)
  {
    *space = 0;
    return KM_KBP_STATUS_NO_MEM;
  }

  // Fetch the finished doc and copy it to the buffer if there enough space.
  auto const doc = _buf.str();
  if (buf && *space > doc.size())
  {
    std::copy(doc.begin(), doc.end(), buf);
    buf[doc.size()] = 0;
  }

  // Return space needed/used.
  *space = doc.size()+1;
  return KM_KBP_STATUS_OK;

}
