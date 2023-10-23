/*
  Copyright:    © 2023 SIL International.
  Description:  Implementation of the action API functions using internal
                data structures and functions.
  Create Date:  23 Oct 2023
  Authors:      Marc Durdin (MCD)
  History:      23 Oct 2023 - MCD - Initial implementation from #9720
*/
#include <cassert>
#include <algorithm>
#include <sstream>

#include <keyman/keyman_core_api.h>

#include "action.hpp"
#include "state.hpp"
#include "option.hpp"

km_core_actions * km::kbp::action_items_to_actions(
  km_core_action_item const *action_items
) {
  assert(action_items != nullptr);
  if(action_items == nullptr) {
    return nullptr;
  }

  km_core_status status = KM_CORE_STATUS_OK;

  km_core_actions* actions = new km_core_actions;

  // Set actions defaults
  std::vector<km_core_context_item> output;
  std::vector<km_core_option_item> options;
  actions->delete_back = 0;
  actions->output = nullptr;
  actions->persist_options = nullptr;
  actions->do_alert = false;
  actions->emit_keystroke = false;
  actions->new_caps_lock_state = -1;

  for (; action_items->type != KM_CORE_IT_END; ++action_items) {
    assert(action_items->type < KM_CORE_IT_MAX_TYPE_ID);

    switch(action_items->type) {
      case KM_CORE_IT_ALERT:
        actions->do_alert = true;
        break;
      case KM_CORE_IT_BACK:
        switch(action_items->backspace.expected_type) {
          case KM_CORE_BT_UNKNOWN:
            // this is equivalent to emit_keystroke, because the only time we
            // are allowed to do an unknown bksp is when a bksp is passed in
            actions->emit_keystroke = true;
            break;
          case KM_CORE_BT_CHAR:
            if(output.empty()) {
              actions->delete_back++;
            } else {
              auto last_context_item = output.back();
              output.pop_back();
              assert(last_context_item.type == KM_CORE_CT_CHAR);
              assert(last_context_item.character == action_items->backspace.expected_value);
            }
            break;
          case KM_CORE_BT_MARKER:
            if(output.empty()) {
              // deleting a marker has no effect on the application
            } else {
              auto last_context_item = output.back();
              output.pop_back();
              assert(last_context_item.type == KM_CORE_CT_MARKER);
              assert(last_context_item.marker == action_items->backspace.expected_value);
            }
            break;
          default:
            assert(false);
        }
        break;
      case KM_CORE_IT_CAPSLOCK:
        actions->new_caps_lock_state = action_items->capsLock;
        break;
      case KM_CORE_IT_CHAR:
        output.push_back({KM_CORE_CT_CHAR,{0},action_items->character});
        break;
      case KM_CORE_IT_EMIT_KEYSTROKE:
        actions->emit_keystroke = true;
        break;
      case KM_CORE_IT_INVALIDATE_CONTEXT:
        // no-op
        break;
      case KM_CORE_IT_MARKER:
        {
         km_core_context_item ci = {0};
          ci.type = KM_CORE_CT_MARKER;
          ci.marker = action_items->marker;
          output.push_back(ci);
        }
        break;
      case KM_CORE_IT_PERSIST_OPT:
        // TODO: lowpri: replace existing item if already present in options vector?
        options.push_back(km::kbp::option(
          static_cast<km_core_option_scope>(action_items->option->scope),
          action_items->option->key,
          action_items->option->value
        ));
        break;
      default:
        assert(false);
    }
  }

  output.push_back({KM_CORE_CT_END});

  // Strip the markers from the output to be passed to the app

  size_t buf_size;

  if((status = km_core_context_items_to_utf32(output.data(), nullptr, &buf_size)) != KM_CORE_STATUS_OK) {
    delete actions;
    return nullptr;
  }

  actions->output = new km_core_usv[buf_size];

  if((status = km_core_context_items_to_utf32(output.data(), actions->output, &buf_size)) != KM_CORE_STATUS_OK) {
    delete[] actions->output;
    delete actions;
    return nullptr;
  }

  // Create an array of the persisted options

  if(!options.empty()) {
    options.push_back(KM_CORE_OPTIONS_END);
    actions->persist_options = new km_core_option_item[options.size()];
    std::copy(options.begin(), options.end(), actions->persist_options);
  }

  return actions;
}
