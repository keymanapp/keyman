/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - KMX Debugger
 *
 * Functions for preparing kmx debug events
 */

#include <kmx/kmx_processevent.h>

using namespace km::kbp;
using namespace kmx;

void KMX_DebugItems::push_item(uint8_t type, uint32_t flags, LPGROUP group, LPKEY key, PKMX_WCHAR context) {
  _items->assert_push_entry();
  km_kbp_state_debug_item item = {type, flags};
  item.kmx_info.rule = key;
  u16ncpy(item.kmx_info.context, context, DEBUG_MAX_CONTEXT - 1);
  item.kmx_info.context[DEBUG_MAX_CONTEXT-1] = 0;
  item.kmx_info.group = group;
  _items->emplace_back(item);
}
