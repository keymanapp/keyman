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
namespace kbp {
namespace kmx {

class KMX_DebugItems
{
private:
  debug_items *_items;
  void push_item(
    uint8_t type,
    uint32_t flags = 0,
    LPGROUP group = nullptr,
    LPKEY key = nullptr,
    PKMX_WCHAR context = nullptr,
    PKMX_WORD index_stack = nullptr,
    int first_action = 0
  );
  void fill_store_offsets(
    km_kbp_state_debug_kmx_info *info,
    PKMX_WORD index_stack
  );
public:
  KMX_DebugItems(debug_items *items);
  void push_begin(km_kbp_state_debug_key_info *key_info, uint32_t flags);
  void push_end(uint32_t flags);
  void push_group_enter(LPGROUP group);
  void push_group_exit(LPGROUP group, uint32_t flags);
  void push_nomatch_enter(LPGROUP group, int first_action);
  void push_nomatch_exit(LPGROUP group);
  void push_match_enter(LPGROUP group, int first_action);
  void push_match_exit(LPGROUP group);
  void push_rule_enter(
    LPGROUP group,
    LPKEY key,
    PKMX_WCHAR context,
    PKMX_WORD index_stack,
    int first_action
  );
  void push_rule_exit(
    LPGROUP group,
    LPKEY key,
    PKMX_WCHAR context,
    PKMX_WORD index_stack
  );
};

inline
KMX_DebugItems::KMX_DebugItems(debug_items *items) {
  assert(items);
  _items = items;
}

inline void
KMX_DebugItems::push_begin(
  km_kbp_state_debug_key_info *key_info,
  uint32_t flags
) {
  // As the vector always starts with a single KM_KBP_DEBUG_END entry,
  // we clear that and any other potential events first before starting a new event sequence
  _items->clear();
  _items->push_begin(key_info, flags);
}

inline void
KMX_DebugItems::push_end(uint32_t flags) {
  _items->push_end(flags);
}

inline void
KMX_DebugItems::push_group_enter(LPGROUP group) {
  push_item(KM_KBP_DEBUG_GROUP_ENTER, 0, group);
}

inline void
KMX_DebugItems::push_group_exit(LPGROUP group, uint32_t flags) {
  push_item(KM_KBP_DEBUG_GROUP_EXIT, flags, group);
}

inline void
KMX_DebugItems::push_nomatch_enter(LPGROUP group, int first_action) {
  push_item(KM_KBP_DEBUG_NOMATCH_ENTER, 0, group, nullptr, nullptr, nullptr, first_action);
}

inline void
KMX_DebugItems::push_nomatch_exit(LPGROUP group) {
  push_item(KM_KBP_DEBUG_NOMATCH_EXIT, 0, group);
}

inline void
KMX_DebugItems::push_match_enter(LPGROUP group, int first_action) {
  push_item(KM_KBP_DEBUG_MATCH_ENTER, 0, group, nullptr, nullptr, nullptr, first_action);
}

inline void
KMX_DebugItems::push_match_exit(LPGROUP group) {
  push_item(KM_KBP_DEBUG_MATCH_EXIT, 0, group);
}

inline void
KMX_DebugItems::push_rule_enter(
  LPGROUP group,
  LPKEY key,
  PKMX_WCHAR context,
  PKMX_WORD index_stack,
  int first_action
) {
  push_item(KM_KBP_DEBUG_RULE_ENTER, 0, group, key, context, index_stack, first_action);
}

inline void
KMX_DebugItems::push_rule_exit(
  LPGROUP group,
  LPKEY key,
  PKMX_WCHAR context,
  PKMX_WORD index_stack
) {
  push_item(KM_KBP_DEBUG_RULE_EXIT, 0, group, key, context, index_stack);
}

} // namespace kmx
} // namespace kbp
} // namespace km