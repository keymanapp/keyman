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
#include <memory>


#include "action.hpp"
#include "state.hpp"
#include "option.hpp"

bool km::core::action_item_list_to_actions_object(
  km_core_action_item const *action_items,
  km_core_actions *actions
) {
  assert(action_items != nullptr);
  assert(actions != nullptr);
  if(action_items == nullptr || actions == nullptr) {
    return false;
  }


  // Set actions default values
  std::vector<km_core_context_item> output;
  std::vector<km_core_option_item> options;
  actions->code_points_to_delete = 0;
  actions->do_alert = KM_CORE_FALSE;
  actions->emit_keystroke = KM_CORE_FALSE;
  actions->new_caps_lock_state = KM_CORE_CAPS_UNCHANGED;

  // deleted_context data will be set in state::apply_actions_and_merge_app_context
  // because it needs access to the state's app context
  actions->deleted_context = nullptr;

  // Clear output pointers, will be set later once we have sizes
  actions->output = nullptr;
  actions->persist_options = nullptr;

  for (; action_items->type != KM_CORE_IT_END; ++action_items) {
    assert(action_items->type < KM_CORE_IT_MAX_TYPE_ID);

    switch(action_items->type) {
      case KM_CORE_IT_ALERT:
        actions->do_alert = KM_CORE_TRUE;
        break;
      case KM_CORE_IT_BACK:
        switch(action_items->backspace.expected_type) {
          case KM_CORE_BT_UNKNOWN:
            // this is equivalent to emit_keystroke, because the only time we
            // are allowed to do an unknown bksp is when a bksp is passed in
            actions->emit_keystroke = KM_CORE_TRUE;
            break;
          case KM_CORE_BT_CHAR:
            if(output.empty()) {
              actions->code_points_to_delete++;
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
        actions->new_caps_lock_state = action_items->capsLock ? KM_CORE_CAPS_ON : KM_CORE_CAPS_OFF;
        break;
      case KM_CORE_IT_CHAR:
        output.push_back({KM_CORE_CT_CHAR,{0},{action_items->character}});
        break;
      case KM_CORE_IT_EMIT_KEYSTROKE:
        actions->emit_keystroke = KM_CORE_TRUE;
        break;
      case KM_CORE_IT_INVALIDATE_CONTEXT:
        // no-op
        break;
      case KM_CORE_IT_MARKER:
        output.push_back({KM_CORE_CT_MARKER,{0},{action_items->marker}});
        break;
      case KM_CORE_IT_PERSIST_OPT:
      {
        // TODO: lowpri: replace existing item if already present in options vector?
        km::core::option opt(static_cast<km_core_option_scope>(action_items->option->scope),
          action_items->option->key,
          action_items->option->value
        );
        options.push_back(opt.release()); // hand over memory management of the option item to the action struct
        break;
      }
      default:
        assert(false);
    }
  }


  // Strip the markers from the output, and convert to an string of UTF-32

  output.push_back(KM_CORE_CONTEXT_ITEM_END);

  size_t buf_size;

  if(context_items_to_utf32(output.data(), nullptr, &buf_size) != KM_CORE_STATUS_OK) {
    return false;
  }

  std::unique_ptr<km_core_usv[]> output_usv(new km_core_usv[buf_size]);

  if(context_items_to_utf32(output.data(), output_usv.get(), &buf_size) != KM_CORE_STATUS_OK) {
    return false;
  }

  actions->output = output_usv.release();

  // Create an array of the persisted options

  options.push_back(KM_CORE_OPTIONS_END);
  actions->persist_options = new km_core_option_item[options.size()];
  std::copy(options.begin(), options.end(), actions->persist_options);

  // We now have a complete set of actions

  return true;
}


// TODO: this is effectively the inverse of action_item_list_to_actions_object,
//       and perhaps we should consider changing that function to be a member
//       of state also, so that we can move memory management into state?
bool km::core::state::set_actions(
  km_core_actions const &actions
) {
  _actions.clear();

  // number of codepoints (not codeunits!) to delete from app context.

  for(unsigned int i = 0; i < actions.code_points_to_delete; i++) {
    _actions.push_backspace(KM_CORE_BT_CHAR, 0); // expected value is not known
  }

  for(auto output = actions.output; *output; output++) {
    _actions.push_character(*output);
  }

  for(auto opt = actions.persist_options; opt->scope; opt++) {
    km::core::option opt0(static_cast<km_core_option_scope>(opt->scope), opt->key, opt->value);
    _actions.push_persist(opt0);
  }

  if(actions.do_alert) {
    _actions.push_alert();
  }

  if(actions.emit_keystroke) {
    _actions.push_emit_keystroke();
  }

  if(actions.new_caps_lock_state != KM_CORE_CAPS_UNCHANGED) {
    _actions.push_capslock(actions.new_caps_lock_state == KM_CORE_CAPS_ON);
  }

  return true;
}
