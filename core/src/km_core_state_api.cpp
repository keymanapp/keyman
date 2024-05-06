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
#include <iomanip>
#include <codecvt>

#include "keyman_core.h"
#include "jsonpp.hpp"

#include "processor.hpp"
#include "state.hpp"
#include "vkey_to_contextreset.hpp"
#include "kmx_file.h"

using namespace km::core;

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


km_core_context *km_core_state_context(km_core_state const *state)
{
  assert(state);
  if (!state) return nullptr;

  return static_cast<km_core_context *>(&(const_cast<km_core_state *>(state)->context()));
}

km_core_context *km_core_state_app_context(km_core_state const *state)
{
  assert(state);
  if (!state) return nullptr;

  return static_cast<km_core_context *>(&(const_cast<km_core_state *>(state)->app_context()));
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

km_core_status km_core_state_context_clear(
  km_core_state *state
) {
  assert(state != nullptr);
  if(state == nullptr) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }
  km_core_context_clear(km_core_state_context(state));
  km_core_context_clear(km_core_state_app_context(state));
  return KM_CORE_STATUS_OK;
}

void km_core_cp_dispose(
  km_core_cp *cp
) {
  if(cp != nullptr) {
    delete [] cp;
  }
}

km_core_cp * _new_error_string(std::u16string const str) {
  km_core_cp* result = new km_core_cp[str.size()+1];
  str.copy(result, str.size());
  result[str.size()] = 0;
  return result;
}

km_core_cp * km_core_state_context_debug(
  km_core_state *state,
  km_core_debug_context_type context_type
) {
  km_core_context_item * context_items = nullptr;

  if(context_type == KM_CORE_DEBUG_CONTEXT_INTERMEDIATE) {
    if(km_core_state_get_intermediate_context(state, &context_items) != KM_CORE_STATUS_OK) {
      return _new_error_string(u"<error retrieving intermediate context>");
    }
  } else if(context_type == KM_CORE_DEBUG_CONTEXT_CACHED) {
    if(km_core_context_get(km_core_state_context(state), &context_items) != KM_CORE_STATUS_OK) {
      return _new_error_string(u"<error retrieving cached context>");
    }
  } else if(context_type == KM_CORE_DEBUG_CONTEXT_APP) {
    if(km_core_context_get(km_core_state_app_context(state), &context_items) != KM_CORE_STATUS_OK) {
      return _new_error_string(u"<error retrieving app context>");
    }
  } else {
    return _new_error_string(u"<invalid context type>");
  }

  size_t buf_size;
  if(context_items_to_utf8(context_items, nullptr, &buf_size) != KM_CORE_STATUS_OK) {
    km_core_context_items_dispose(context_items);
    return _new_error_string(u"<could not retrieve context buffer size>");
  }

  std::vector<char> context_buffer(buf_size);
  if(context_items_to_utf8(context_items, &context_buffer[0], &buf_size) != KM_CORE_STATUS_OK) {
    km_core_context_items_dispose(context_items);
    return _new_error_string(u"<could not retrieve context buffer>");
  }

  // construct the log message

  std::stringstream buffer;

  int context_item_length = 0;
  for(auto cp = context_items; cp->type != KM_CORE_CT_END; cp++, context_item_length++);

  buffer << "|" << std::string(&context_buffer[0]) << "| (len: " << context_item_length << ") [";
  for(auto cp = context_items; cp->type != KM_CORE_CT_END; cp++) {
    auto flags = buffer.flags();
    if(cp->type == KM_CORE_CT_CHAR) {
      // A single Unicode codepoint
      buffer << " U+" << std::setfill('0') << std::setw(4) << std::hex << cp->character;
    } else {
      // A marker
      buffer << " M(" << cp->marker << ")";
    }
    buffer.flags(flags);
  }
  buffer << " ]";

  km_core_context_items_dispose(context_items);

  std::u16string s = std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t>{}.from_bytes(buffer.str());

  km_core_cp* result = new km_core_cp[s.size() + 1];
  s.copy(result, s.size());
  result[s.size()] = 0;

  return result;
}

static bool
state_has_action_type(km_core_state *state, uint8_t type) {
  return std::any_of(
      state->actions().begin(), state->actions().end(),
        [type](const km::core::action &a) { return a.type == type; });
}

bool
state_should_invalidate_context(km_core_state *state,
                     km_core_virtual_key vk,
                     uint16_t modifier_state,
                     uint8_t is_key_down,
                     uint16_t _kmn_unused(event_flags)) {
  if (!is_key_down) {
    return false;  // don't invalidate on keyup
  }
  // if emit_keystroke is present, check if a context reset is needed
  if (state_has_action_type(state, KM_CORE_IT_EMIT_KEYSTROKE)) {
    if (
        // when a backspace keystroke is emitted, it is because we are at the start of 
        // context, and we want to give the application the chance to process it, e.g. 
        // by moving to previous field. Note that context manipulation does not result 
        // in an emit_keystroke backspace action, as this is handled through the 
        // `code_points_to_delete` field. So we always invalidate context when a 
        // processor emits a backspace.
        vk == KM_CORE_VKEY_BKSP ||
        // certain modifiers invalidate context
        modifier_should_contextreset(modifier_state) ||
        // most frame keys invalidate context
        vkey_should_contextreset(vk)) {
      return true;
    }
  }
  return false;
}
