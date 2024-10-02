#include "keyman_core.h"
#include "state.hpp"
#include "kmx/kmx_processor.hpp"
#include <map>

using namespace km::core;
using namespace kmx;


static KMX_BOOL ContextItemsFromAppContext(KMX_WCHAR *buf, km_core_context_item** outPtr)
{
  assert(buf);
  assert(outPtr);
  km_core_context_item* context_items  = new km_core_context_item[xstrlen(buf) + 1];
  PKMX_WCHAR p = buf;
  *outPtr = context_items;
  while (*p) {
    if (*p == UC_SENTINEL) {
      assert(*(p + 1) == CODE_DEADKEY);
      // we know the only uc_sentinel code in the context is code_deadkey, which has only 1 parameter: uc_sentinel code_deadkey <deadkey_id>
      // setup dead key context item
      *context_items = km_core_context_item{ KM_CORE_CT_MARKER, {0,}, {*(p+2)} };
    } else if (Uni_IsSurrogate1(*p) && Uni_IsSurrogate2(*(p + 1))) {
      // handle surrogate
      *context_items = km_core_context_item{ KM_CORE_CT_CHAR, {0,}, {(char32_t)Uni_SurrogateToUTF32(*p, *(p + 1))} };
    } else {
      *context_items = km_core_context_item{ KM_CORE_CT_CHAR, {0,}, {*p} };
    }
    p = incxstr(p);
    context_items++;
  }
  // terminate the context_items array.
  *context_items = km_core_context_item KM_CORE_CONTEXT_ITEM_END;
  return true;
}

km_core_status kmx_processor::validate() const {
  return _valid ? KM_CORE_STATUS_OK : KM_CORE_STATUS_INVALID_KEYBOARD;
}

kmx_processor::kmx_processor(std::u16string const& kb_name, const std::vector<uint8_t>& data) {
  _valid = bool(_kmx.Load((PKMX_BYTE)data.data(), data.size()));

  if (!_valid)
    return;

  keyboard_attributes::options_store defaults;
  _kmx.GetOptions()->Init(defaults);

  for (auto const & opt: defaults)
  {
    if (!opt.empty() && opt.scope == KM_CORE_OPT_KEYBOARD  )
      persisted_store()[opt.key] = opt.value;
  }
  // Fill out attributes
  auto v = _kmx.GetKeyboard()->Keyboard->version;
  auto vs = std::to_string(v >> 16) + "." + std::to_string(v & 0xffff);

  _attributes = keyboard_attributes(kb_name, std::u16string(vs.begin(), vs.end()), defaults);
}

char16_t const *
kmx_processor::lookup_option(
  km_core_option_scope scope,
  std::u16string const &key
) const {
  char16_t const *pValue = nullptr;
  switch (scope) {
  case KM_CORE_OPT_KEYBOARD:
    pValue = _kmx.GetOptions()->LookUp(key);
    break;
  case KM_CORE_OPT_ENVIRONMENT:
    pValue = _kmx.GetEnvironment()->LookUp(key);
    break;
  default:
    break;
  }

  return pValue ? pValue : nullptr;
}

option
kmx_processor::update_option(
  km_core_option_scope scope,
  std::u16string const &key,
  std::u16string const &value
) {
  switch (scope) {
  case KM_CORE_OPT_KEYBOARD:
    _kmx.GetOptions()->Set(key, value);
    persisted_store()[key] = value;
    break;
  case KM_CORE_OPT_ENVIRONMENT:
    _kmx.GetEnvironment()->Set(key, value);
    break;
  default:
    return option();
    break;
  }

  return option(scope, key, value);
}

km_core_status
kmx_processor::external_event(
  km_core_state* state,
  uint32_t event,
  void* _kmn_unused(data)
) {

  switch (event) {
    case KM_CORE_EVENT_KEYBOARD_ACTIVATED:
      // reset any current actions in the queue as a new keyboard
      // has been activated
      _kmx.GetActions()->ResetQueue();
      state->actions().clear();
      if (_kmx.GetKeyboard()->Keyboard->dwFlags & KF_CAPSALWAYSOFF) {
        KMX_DWORD dummy_modifiers = 0;
        _kmx.SetCapsLock(dummy_modifiers, FALSE, TRUE);
      }
      break;
    default:
      return KM_CORE_STATUS_INVALID_ARGUMENT;
  }
  return internal_process_queued_actions(state);
}

bool
kmx_processor::queue_action(
  km_core_state * state,
  km_core_action_item const * action_item
) {
  switch (action_item->type) {
  case KM_CORE_IT_END:
    DebugLog("kmx_processor::queue_action: Error attempt to queue KM_CORE_IT_END action type\n");
    return false;
  case KM_CORE_IT_CHAR:
   _kmx.GetActions()->QueueAction(QIT_CHAR, action_item->character);
    break;
  case KM_CORE_IT_MARKER:
   _kmx.GetActions()->QueueAction(QIT_DEADKEY, (KMX_DWORD)action_item->marker);
    break;
  case KM_CORE_IT_ALERT:
    _kmx.GetActions()->QueueAction(QIT_BELL, 0);
    break;
  case KM_CORE_IT_BACK:
  {
    // If the context is already empty, we want to emit the backspace for application to use
    PKMX_WCHAR p_last_item_context = _kmx.GetContext()->Buf(1);

    if ((!p_last_item_context || *p_last_item_context == 0) ||
      (action_item->backspace.expected_type == KM_CORE_BT_UNKNOWN)) {
      _kmx.GetActions()->QueueAction(QIT_INVALIDATECONTEXT, 0);
      _kmx.GetActions()->QueueAction(QIT_BACK, BK_DEFAULT);
    } else if (action_item->backspace.expected_type == KM_CORE_BT_MARKER) {
      _kmx.GetActions()->QueueAction(QIT_BACK, BK_DEADKEY);
    } else /* KM_CORE_BT_CHAR, KM_CORE_BT_UNKNOWN */ {
      _kmx.GetActions()->QueueAction(QIT_BACK, BK_DEFAULT);
    }
    break;
  }
  case KM_CORE_IT_PERSIST_OPT:
  case KM_CORE_IT_CAPSLOCK:
    DebugLog("kmx_processor::queue_action: Warning handling of action type [%d] not implemented\n", action_item->type);
    return false;
    break;
  case KM_CORE_IT_EMIT_KEYSTROKE:
    // By pass the kmx processor and put this action into the core queue immediately
    // This has to be done as the kmx processor QueAction does not have an
    // emit key stroke type.
    // currently only supporting emitting the current key stroke
    state->actions().push_emit_keystroke();
    break;
  case KM_CORE_IT_INVALIDATE_CONTEXT:
    _kmx.GetActions()->QueueAction(QIT_INVALIDATECONTEXT, 0);
    break;
  }
  return true;
}

km_core_status
kmx_processor::internal_process_queued_actions(km_core_state *state) {

  for (auto i = 0; i < _kmx.GetActions()->Length(); i++) {
    auto a = _kmx.GetActions()->Get(i);
    switch (a.ItemType) {
    case QIT_CAPSLOCK:
      state->actions().push_capslock(a.dwData);
      break;
    case QIT_SAVEOPT:
      {
        // NOTE: `save(foo='1') save(foo='0')` will do `save(foo='0')` twice as
        // dpString would have been overwritten on the second save() call before
        // this queue reprocessing happens. This is not an issue from a process
        // point-of-view because the event result data is only guaranteed on
        // exit of process_event.
        auto const & rStoreToSave = _kmx.GetKeyboard()->Keyboard->dpStoreArray[a.dwData];
        state->processor().persisted_store()[rStoreToSave.dpName] = rStoreToSave.dpString;
        state->actions().push_persist(
          option{KM_CORE_OPT_KEYBOARD, rStoreToSave.dpName, rStoreToSave.dpString});
      }
      break;
    case QIT_EMIT_KEYSTROKE:
      state->actions().push_emit_keystroke();
      break;
    case QIT_CHAR:
      state->context().push_character(a.dwData);
      state->actions().push_character(a.dwData);
      break;
    case QIT_DEADKEY:
      state->context().push_marker(a.dwData);
      state->actions().push_marker(a.dwData);
      break;
    case QIT_BELL:
      state->actions().push_alert();
      break;
    case QIT_BACK:
      switch (a.dwData) {
      case BK_DEFAULT:
        assert(state->context().back().type != KM_CORE_IT_MARKER);
        if(!state->context().empty()) {
          auto item = state->context().back();
          state->context().pop_back();
          state->actions().push_backspace(KM_CORE_BT_CHAR, item.character);
        } else {
          // Note: runs on non-debug build or if IMX callback has requested it in both cases uses fail safe
          state->actions().push_backspace(KM_CORE_BT_UNKNOWN);
        }
        break;
      case BK_DEADKEY:
        // This only happens if we know we have context to delete. Last item must be a deadkey
        assert(!state->context().empty());
        assert(state->context().back().type == KM_CORE_IT_MARKER);
        if(!state->context().empty()) {
          auto item = state->context().back();
          state->context().pop_back();
          state->actions().push_backspace(KM_CORE_BT_MARKER, item.marker);
        } else {
          // Note: only runs on non-debug build, fail safe
          state->actions().push_backspace(KM_CORE_BT_UNKNOWN);
        }
        break;
      default:
        assert(false);
      }
      break;
    case QIT_INVALIDATECONTEXT:
      state->context().clear();
      state->actions().push_invalidate_context();
      break;
    default:
      // std::cout << "Unexpected item type " << a.ItemType << ", " << a.dwData << std::endl;
      assert(false);
    }
  }

  state->actions().commit();
  // Queue should be cleared to allow testing if external actions have
  // been added to the keyboard action queue (currently IMX interaction)
  _kmx.GetActions()->ResetQueue();
  return KM_CORE_STATUS_OK;
}

km_core_status
kmx_processor::process_event(
  km_core_state *state,
  km_core_virtual_key vk,
  uint16_t modifier_state,
  uint8_t is_key_down,
  uint16_t /* event_flags */
) {
  // Construct a context buffer from the items
  std::u16string ctxt;
  auto cp = state->context();
  for (auto c = cp.begin(); c != cp.end(); c++) {
    switch (c->type) {
    case KM_CORE_CT_CHAR:
      {
        km::core::kmx::char16_single buf;
        const int len = km::core::kmx::Utf32CharToUtf16(c->character, buf);
        ctxt.append(buf.ch, len);
      }
      break;
    case KM_CORE_CT_MARKER:
      assert(c->marker > 0);
      ctxt += UC_SENTINEL;
      ctxt += CODE_DEADKEY;
      ctxt += c->marker;
      break;
    }
  }

  _kmx.GetContext()->Set(ctxt.c_str());
  _kmx.GetActions()->ResetQueue();
  state->actions().clear();

  if (!_kmx.ProcessEvent(state, vk, modifier_state, is_key_down)) {
    // We need to output the default keystroke
    _kmx.GetActions()->QueueAction(QIT_EMIT_KEYSTROKE, 0);
  }

  return internal_process_queued_actions(state);
}

km_core_status
kmx_processor::process_queued_actions (km_core_state *state) {
  state->actions().clear();
  return internal_process_queued_actions(state);
}

constexpr km_core_attr const engine_attrs = {
  256,
  KM_CORE_LIB_CURRENT,
  KM_CORE_LIB_AGE,
  KM_CORE_LIB_REVISION,
  KM_CORE_TECH_KMX,
  "SIL International"
};

km_core_attr const & kmx_processor::attributes() const {
  return engine_attrs;
}

km_core_context_item * kmx_processor::get_intermediate_context() {
  KMX_WCHAR *buf = _kmx.GetContext()->BufMax(MAXCONTEXT);
  km_core_context_item *citems = nullptr;
  if (!ContextItemsFromAppContext(buf, &citems)) {
    citems = new km_core_context_item(KM_CORE_CONTEXT_ITEM_END);
  }
  return citems;
}

km_core_keyboard_key * kmx_processor::get_key_list() const  {
  // Iterate through the groups and get the rules with virtual keys
  // and store the key along with the modifer.
  const uint32_t group_cnt = _kmx.GetKeyboard()->Keyboard->cxGroupArray;
  const LPGROUP group_array = _kmx.GetKeyboard()->Keyboard->dpGroupArray;
  GROUP *p_group;

  std::map<std::pair<km_core_virtual_key,uint32_t>, uint32_t> map_rules;
  km_core_virtual_key v_key;
  uint32_t modifier_flag;
// Use hash map to get the unique list
  for(auto i = decltype(group_cnt){0}; i < group_cnt; i++) {
    p_group = &group_array[i];
    if(p_group->fUsingKeys) {
      for(auto j = decltype(p_group->cxKeyArray){0}; j < p_group->cxKeyArray; j++) {
        v_key = p_group->dpKeyArray[j].Key;
        modifier_flag = p_group->dpKeyArray[j].ShiftFlags;
        if(modifier_flag == 0) {
          // This must be a ASCII character corresponding US Keyboard key cap
          if(!MapUSCharToVK(v_key, &v_key, &modifier_flag)) continue;
        }
        map_rules[std::make_pair(v_key,modifier_flag)] = (modifier_flag & K_MODIFIERFLAG); // Clear kmx special flags
      }
    }
  }
  // Now convert to the keyboard key array
  km_core_keyboard_key *rules = new km_core_keyboard_key[map_rules.size() + 1];
  std::map<std::pair<km_core_virtual_key,uint32_t>, uint32_t>::iterator it = map_rules.begin();
  int n = 0;
  while (it != map_rules.end()){
    auto pair = it->first;
    rules[n].key = pair.first;
    rules[n].modifier_flag = it->second;
    it++;
    n++;
  }
  // Insert list termination
  rules[n] =  KM_CORE_KEYBOARD_KEY_LIST_END;
  return rules;
}

km_core_keyboard_imx * kmx_processor::get_imx_list() const  {

  const uint32_t store_cnt = _kmx.GetKeyboard()->Keyboard->cxStoreArray;
  const LPSTORE store_array = _kmx.GetKeyboard()->Keyboard->dpStoreArray;
  uint16_t fn_count = 0;
  uint16_t fn_idx = 0;

  for(uint32_t i = 0; i < store_cnt; i++) {
    LPSTORE p_store = &store_array[i];
    if(p_store->dwSystemID == TSS_CALLDEFINITION) {
      fn_count++;
    }
  }

  km_core_keyboard_imx *imx_list = new km_core_keyboard_imx[fn_count + 1];

  for(uint32_t i = 0; i < store_cnt; i++) {
    LPSTORE p_store = &store_array[i];
    if(p_store->dwSystemID == TSS_CALLDEFINITION) {
      /* Break the store string into components */

      PKMX_WCHAR full_fn_name = u16dup(p_store->dpString);
      PKMX_WCHAR pt = NULL;
      PKMX_WCHAR lib_name = u16tok(full_fn_name, u':', &pt);
      PKMX_WCHAR fn_name = u16tok(NULL, u':', &pt);

      if(!lib_name || !fn_name){
        continue;
      }

      imx_list[fn_idx].library_name = lib_name;
      imx_list[fn_idx].function_name = u16dup(fn_name);
      imx_list[fn_idx].imx_id = i;
      fn_idx++;
    }
  }
  // Insert list termination
  imx_list[fn_idx] =  KM_CORE_KEYBOARD_IMX_END;
  return imx_list;
}

/**
 * Returns true the data is a KMX file, i.e. starts with 'KXTS'.
 *
 * @param data  the keyboard blob
 * @return true if the processor can handle the keyboard, otherwise false.
 */
bool kmx_processor::is_handled(const std::vector<uint8_t>& data) {
  if (data.empty()) {
    return false;
  }

  if (data.size() < 64) {  // a KMX file is at least 64 bytes (KMX header)
    return false;
  }

  if (data.size() >= KMX_MAX_ALLOWED_FILE_SIZE) {
    return false;
  }

  return ((kmx::PCOMP_KEYBOARD)data.data())->dwIdentifier == KMX_DWORD(FILEID_COMPILED);  // 'KXTS'
}
