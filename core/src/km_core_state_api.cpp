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
#include <memory>

#include <keyman/keyman_core_api.h>
#include "jsonpp.hpp"

#include "processor.hpp"
#include "state.hpp"

using namespace km::kbp;

// Forward declarations
class context;

km_core_status km_core_state_create(km_core_keyboard * keyboard,
                                  km_core_option_item const *env,
                                  km_core_state ** out)
{
  assert(keyboard); assert(env); assert(out);
  if (!keyboard || !env || !out)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  try
  {
    *out = new km_core_state(static_cast<abstract_processor&>(*keyboard), env);
  }
  catch (std::bad_alloc &)
  {
    return KM_CORE_STATUS_NO_MEM;
  }
  return KM_CORE_STATUS_OK;
}


km_core_status km_core_state_clone(km_core_state const *state,
                                 km_core_state ** out)
{
  assert(state); assert(out);
  if (!state || !out)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  *out = new km_core_state(*state);
  return KM_CORE_STATUS_OK;
}


void km_core_state_dispose(km_core_state *state)
{
  delete state;
}


km_core_context *km_core_state_context(km_core_state *state)
{
  assert(state);
  if (!state) return nullptr;

  return static_cast<km_core_context *>(&state->context());
}

km_core_status km_core_state_get_intermediate_context(
  km_core_state *state,
  km_core_context_item ** context_items
) {
  assert(state);
  assert(context_items);
  if (!state || !context_items) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }
  auto & processor = state->processor();
  *context_items = processor.get_intermediate_context();

  return KM_CORE_STATUS_OK;
}

km_core_action_item const * km_core_state_action_items(km_core_state const *state,
                                                     size_t *num_items)
{
  assert(state && state->actions().size() > 0);
  if (!state || state->actions().empty()) return nullptr;

  if (num_items)
    *num_items = state->actions().size();

  // Process events will ensure that the actions vector is always well
  // teminated
  assert(state->actions().back().type == KM_CORE_IT_END);
  return state->actions().data();
}

km_core_status km_core_state_queue_action_items(
  km_core_state *state,
  km_core_action_item const *action_items
) {
  assert(state);
  assert(action_items);
  if (!state|| !action_items) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }

  auto & processor = state->processor();

  for (; action_items->type != KM_CORE_IT_END; ++action_items) {
    if (action_items->type >= KM_CORE_IT_MAX_TYPE_ID) {
      return KM_CORE_STATUS_INVALID_ARGUMENT;
    }

    if (!processor.queue_action(state, action_items)) {
      return KM_CORE_STATUS_KEY_ERROR;
    }
  }

  return KM_CORE_STATUS_OK;
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


json & operator << (json & j, km_core_action_item const &act)
{
  j << json::flat << json::object;
  if (act.type >= KM_CORE_IT_MAX_TYPE_ID)
  {
    j << "invalid" << json::null << json::close;
    return j;
  }

  j << action_item_name_lut[act.type];
  switch (act.type)
  {
    case KM_CORE_IT_END:
    case KM_CORE_IT_ALERT:
    case KM_CORE_IT_BACK:
      j << json::null;
      break;
    case KM_CORE_IT_CHAR:
    case KM_CORE_IT_MARKER:
      j << km_core_context_item {act.type, {0,}, {act.character}}; // TODO: is act.type correct here? it may map okay but this is bad practice to mix constants across types. Similarly using act.character instead of act.type
      break;
    case KM_CORE_IT_PERSIST_OPT:
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
      if (act.type != KM_CORE_IT_END)
      {
        j << act;
      }
    }
    j << json::close;
    return j;
}


km_core_status km_core_state_to_json(km_core_state const *state,
                                        char *buf,
                                        size_t *space)
{
  assert(state);
  if (!state)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  std::stringstream _buf;
  json jo(_buf);

  try
  {
    // Pretty print the document.
    jo << json::object
        << "$schema" << "keyman/core/doc/introspection.schema"
        << "keyboard" << state->processor().keyboard()
//        << "options" << state->options()  TODO: Fix
        << "context" << state->context()
        << "actions" << state->actions()
        << json::close;
  }
  catch (std::bad_alloc &)
  {
    *space = 0;
    return KM_CORE_STATUS_NO_MEM;
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
  return KM_CORE_STATUS_OK;

}

void km_core_state_imx_register_callback(
  km_core_state *state,
  km_core_keyboard_imx_platform imx_callback,
  void *callback_object
) {
  assert(state);
  if (!state) {
    return;
  }
  state->imx_register_callback(imx_callback, callback_object);
}

void km_core_state_imx_deregister_callback(km_core_state *state)
{
  assert(state);
  if (!state) {
    return;
  }
  state->imx_deregister_callback();
}

bool is_context_valid(km_core_cp const * context, km_core_cp const * cached_context) {
  km_core_cp const* context_p = context;
  while(*context_p) {
    context_p++;
  }

  km_core_cp const* cached_context_p = cached_context;
  while(*cached_context_p) {
    cached_context_p++;
  }

  // we need to compare from the end of the cached context
  for(; context_p >= context && cached_context_p >= cached_context; context_p--, cached_context_p--) {
    if(*context_p != *cached_context_p) {
      // The cached context doesn't match the application context, so it is
      // invalid
      return false;
    }
  }

  if(cached_context_p > cached_context) {
    // if the cached context is longer than the application context, then we also
    // assume that it is invalid
    return false;
  }

  // It's acceptable for the application context to be longer than the cached
  // context, so if we match the whole cached context, we can safely return true
  return true;
}

km_core_context_status km_core_state_context_set_if_needed(
  km_core_state *state,
  km_core_cp const *application_context
) {
  assert(state != nullptr);
  assert(application_context != nullptr);
  if(state == nullptr || application_context == nullptr) {
    return KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT;
  }

  size_t buf_size;
  km_core_context_item* context_items = nullptr;

  auto context = km_core_state_context(state);
  if(km_core_context_get(context, &context_items) != KM_CORE_STATUS_OK) {
    return KM_CORE_CONTEXT_STATUS_ERROR;
  }

  if(km_core_context_items_to_utf16(context_items, nullptr, &buf_size) != KM_CORE_STATUS_OK) {
    km_core_context_items_dispose(context_items);
    return KM_CORE_CONTEXT_STATUS_ERROR;
  }

  std::unique_ptr<km_core_cp[]> cached_context(new km_core_cp[buf_size]);

  km_core_status status = km_core_context_items_to_utf16(context_items, cached_context.get(), &buf_size);
  km_core_context_items_dispose(context_items);

  if(status != KM_CORE_STATUS_OK) {
    return KM_CORE_CONTEXT_STATUS_ERROR;
  }

  bool is_valid = is_context_valid(application_context, cached_context.get());

  if(is_valid) {
    // We keep the context as is
    return KM_CORE_CONTEXT_STATUS_UNCHANGED;
  }

  km_core_context_item* new_context_items = nullptr;

  // We replace the cached context with the current application context
  status = km_core_context_items_from_utf16(application_context, &new_context_items);
  if (status != KM_CORE_STATUS_OK) {
    km_core_context_clear(context);
    return KM_CORE_CONTEXT_STATUS_CLEARED;
  }

  km_core_context_set(context, new_context_items);
  km_core_context_items_dispose(new_context_items);
  return KM_CORE_CONTEXT_STATUS_UPDATED;
}


km_core_status km_core_state_context_clear(
  km_core_state *state
) {
  assert(state != nullptr);
  if(state == nullptr) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }
  km_core_context_clear(km_core_state_context(state));
  return KM_CORE_STATUS_OK;
}