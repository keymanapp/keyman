/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - KMX Debugger header
 *
 * Wrapper class for debug_items for kmx debugger, with
 * helper functions for pushing different types of debug
 * events.
 */

#pragma once

#include "kmx_base.h"
#include <debug.hpp>

namespace km {
namespace core {
namespace kmx {

class KMX_DebugItems
{
private:
  debug_items *_items;
  void push_item(
    uint8_t type,
    uint16_t first_action,
    uint32_t flags = 0,
    LPGROUP group = nullptr,
    LPKEY key = nullptr,
    PKMX_WCHAR context = nullptr,
    PKMX_WORD index_stack = nullptr
  );
  void fill_store_offsets(
    km_core_state_debug_kmx_info *info,
    PKMX_WORD index_stack
  );
public:
  KMX_DebugItems(debug_items *items);
  void push_begin(km_core_state_debug_key_info *key_info, uint32_t flags);
  void push_end(uint16_t first_action, uint32_t flags);
  void push_group_enter(uint16_t first_action, LPGROUP group);
  void push_group_exit(uint16_t first_action, uint32_t flags, LPGROUP group);
  void push_nomatch_enter(uint16_t first_action, LPGROUP group);
  void push_nomatch_exit(uint16_t first_action, LPGROUP group);
  void push_match_enter(uint16_t first_action, LPGROUP group);
  void push_match_exit(uint16_t first_action, LPGROUP group);
  void push_rule_enter(
    uint16_t first_action,
    LPGROUP group,
    LPKEY key,
    PKMX_WCHAR context,
    PKMX_WORD index_stack
  );
  void push_rule_exit(
    uint16_t first_action,
    LPGROUP group,
    LPKEY key,
    PKMX_WCHAR context,
    PKMX_WORD index_stack
  );
  void push_set_option(
    uint16_t first_action,
    LPSTORE option_store,
    KMX_WCHAR const * value
  );
};

inline
KMX_DebugItems::KMX_DebugItems(debug_items *items) {
  assert(items);
  _items = items;
}

inline void
KMX_DebugItems::push_begin(
  km_core_state_debug_key_info *key_info,
  uint32_t flags
) {
  // As the vector always starts with a single KM_CORE_DEBUG_END entry,
  // we clear that and any other potential events first before starting a new event sequence
  _items->clear();
  _items->push_begin(key_info, flags);
}

inline void
KMX_DebugItems::push_end(
  uint16_t first_action,
  uint32_t flags
) {
  _items->push_end(first_action, flags);
}

inline void
KMX_DebugItems::push_group_enter(uint16_t first_action, LPGROUP group) {
  push_item(KM_CORE_DEBUG_GROUP_ENTER, first_action, 0, group);
}

inline void
KMX_DebugItems::push_group_exit(uint16_t first_action, uint32_t flags, LPGROUP group) {
  push_item(KM_CORE_DEBUG_GROUP_EXIT, first_action, flags, group);
}

inline void
KMX_DebugItems::push_nomatch_enter(uint16_t first_action, LPGROUP group) {
  push_item(KM_CORE_DEBUG_NOMATCH_ENTER, first_action, 0, group);
}

inline void
KMX_DebugItems::push_nomatch_exit(uint16_t first_action, LPGROUP group) {
  push_item(KM_CORE_DEBUG_NOMATCH_EXIT, first_action, 0, group);
}

inline void
KMX_DebugItems::push_match_enter(uint16_t first_action, LPGROUP group) {
  push_item(KM_CORE_DEBUG_MATCH_ENTER, first_action, 0, group);
}

inline void
KMX_DebugItems::push_match_exit(uint16_t first_action, LPGROUP group) {
  push_item(KM_CORE_DEBUG_MATCH_EXIT, first_action, 0, group);
}

inline void
KMX_DebugItems::push_rule_enter(
  uint16_t first_action,
  LPGROUP group,
  LPKEY key,
  PKMX_WCHAR context,
  PKMX_WORD index_stack
) {
  push_item(KM_CORE_DEBUG_RULE_ENTER, first_action, 0, group, key, context, index_stack);
}

inline void
KMX_DebugItems::push_rule_exit(
  uint16_t first_action,
  LPGROUP group,
  LPKEY key,
  PKMX_WCHAR context,
  PKMX_WORD index_stack
) {
  push_item(KM_CORE_DEBUG_RULE_EXIT, first_action, 0, group, key, context, index_stack);
}

} // namespace kmx
} // namespace core
} // namespace km
