#include <cassert>
#include <algorithm>
#include <iterator>
#include <list>
#include <utility>
#include <vector>

#include <keyboardprocessor.h>
#include <utfcodec.hpp>
#include <json.hpp>

#include "context.hpp"
#include "option.hpp"
#include "keyboard.hpp"
#include "state.hpp"

km_kbp_status km_kbp_state_create(km_kbp_keyboard const * keyboard,
                                  km_kbp_option const *env,
                                  km_kbp_state ** out)
{
  assert(keyboard && out);
  if (!keyboard || !out)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  constexpr km_kbp_option const end = KM_KBP_OPTIONS_END;
  auto opts = km::kbp::options_set(KM_KBP_OPT_ENVIRONMENT);
  km_kbp_options_set {opts}.update(env ? env : &end);
  *out = new km_kbp_state(static_cast<km::kbp::keyboard const &>(*keyboard),
                            opts);
  return KM_KBP_STATUS_OK;
}

km_kbp_status km_kbp_state_clone(km_kbp_state const *state,
                                 km_kbp_state ** out)
{
  assert(state && out);
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

km_kbp_options_set *km_kbp_state_options(km_kbp_state *state)
{
  assert(state);
  if (!state) return nullptr;

  return &state->run_opts_adaptor;
}

km_kbp_action_item const * km_kbp_state_action_items(km_kbp_state const *state,
                                                     size_t *num_items)
{
  assert(state);
  if (!state) return nullptr;

  if (num_items)
    *num_items = state->actions.size();

  // Process events will ensure that the actions vector is always well
  // teminated
  return state->actions.data();
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
        << "keyboard" << state->keyboard()
        << "options" << json::object
          << "environment" << state->environment()
          << "dynamic" <<  state->options()
          << json::close
        << "context" << state->context()
// TODO:        << "actions" << state->actions()
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
  *space = doc.size();
  return KM_KBP_STATUS_OK;

}
