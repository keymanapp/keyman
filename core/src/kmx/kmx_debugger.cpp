/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - KMX Debugger
 *
 * Functions for preparing kmx debug events
 */

#include <kmx/kmx_processevent.h>

using namespace km::core;
using namespace kmx;

void KMX_DebugItems::push_item(
  uint8_t type,
  uint16_t first_action,
  uint32_t flags,
  LPGROUP group,
  LPKEY key,
  PKMX_WCHAR context,
  PKMX_WORD index_stack
) {
  _items->assert_push_entry();
  km_core_state_debug_item item = {type, flags, {}, {}};
  item.kmx_info.rule = key;
  if(item.kmx_info.rule && index_stack) {
    this->fill_store_offsets(&item.kmx_info, index_stack);
  }

  if(context != nullptr) {
    u16ncpy(item.kmx_info.context, context, DEBUG_MAX_CONTEXT - 1);
  }
  item.kmx_info.context[DEBUG_MAX_CONTEXT-1] = 0;
  item.kmx_info.group = group;
  item.kmx_info.first_action = first_action;

  _items->emplace_back(item);
}

void KMX_DebugItems::push_set_option(
  uint16_t first_action,
  LPSTORE option_store,
  KMX_WCHAR const * value
) {
  _items->assert_push_entry();
  km_core_state_debug_item item = {KM_CORE_DEBUG_SET_OPTION, 0, {}, {}};
  item.kmx_info.first_action = first_action;
  item.kmx_info.rule = nullptr;
  item.kmx_info.group = nullptr;
  item.kmx_info.context[0] = 0;
  item.key_info.character = 0;
  item.key_info.modifier_state = 0;
  item.key_info.vk = 0;
  item.kmx_info.store_offsets[0] = 0xFFFF;
  item.kmx_info.option.store = option_store;
  u16ncpy(item.kmx_info.option.value, value, DEBUG_MAX_CONTEXT - 1);
  _items->emplace_back(item);
}

void KMX_DebugItems::fill_store_offsets(km_core_state_debug_kmx_info *info, PKMX_WORD index_stack) {

  int i, n;

  km_core_cu *p;

  // TODO turn this into a struct rather than interwoven values
  for(i = n = 0, p = static_cast<LPKEY>(info->rule)->dpContext; p && *p; p = incxstr(p), i++) {
    if(*p == UC_SENTINEL && (*(p+1) == CODE_ANY || *(p+1) == CODE_NOTANY)) {
      info->store_offsets[n++] = *(p+2) - 1;
      info->store_offsets[n++] = index_stack[i];
    }
    if(*p == UC_SENTINEL && *(p+1) == CODE_INDEX) {
      info->store_offsets[n++] = *(p+2) - 1;
      info->store_offsets[n++] = index_stack[*(p+3) - 1];
    }
    if(n == DEBUG_MAX_STORE_OFFSETS*2) {
      break;
    }
  }

  // TODO split this into a separate variable
  // TODO turn this into a struct rather than interwoven values
  if(n < DEBUG_MAX_STORE_OFFSETS*2 - 1) {
    for(p = static_cast<LPKEY>(info->rule)->dpOutput; *p; p = incxstr(p)) {
      if(*p == UC_SENTINEL && *(p+1) == CODE_INDEX) {
        info->store_offsets[n++] = *(p+2) - 1;
        info->store_offsets[n++] = index_stack[*(p+3) - 1];
      }
      if(n == DEBUG_MAX_STORE_OFFSETS*2) {
        break;
      }
    }
  }

  info->store_offsets[n] = 0xFFFF;
}
