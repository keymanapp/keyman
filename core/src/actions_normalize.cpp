/*
  Copyright:    © 2024 SIL International.
  Description:  Implementation of the action output normalization.
  Create Date:  16 Jan 2024
  Authors:      Marc Durdin (MCD)
  History:      16 Jan 2024 - MCD - Initial implementation from #9999
*/
#include <cassert>
#include <algorithm>
#include <sstream>
#include <memory>

#include "action.hpp"
#include "state.hpp"
#include "option.hpp"

bool km::core::actions_normalize(
  km_core_context const *cached_context,
  km_core_context const *app_context,
  km_core_actions *actions
) {
  assert(actions != nullptr);
  assert(cached_context != nullptr);
  assert(app_context != nullptr);
  if(actions == nullptr || cached_context == nullptr || app_context == nullptr) {
    return false;
  }

  // Normalize output to NFC
  //TODO
  // actions->code_points_to_delete++;

  return true;
}
