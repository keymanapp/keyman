/*
  Copyright:    © 2023 SIL International.
  Description:  Internal actions methods for Keyman Core
  Create Date:  23 Oct 2023
  Authors:      Marc Durdin (MCD)
  History:      23 Oct 2023 - MCD - Initial implementation
*/

#pragma once

#include "keyman_core.h"
#include <state.hpp>

namespace km {
namespace core
{
  km_core_actions *action_item_list_to_actions_object(
    km_core_action_item const *action_items
  );

  bool actions_normalize(
    /* in */      km_core_context const *cached_context,
    /* in, out */ km_core_context *app_context,
    /* in, out */ km_core_actions *actions
  );

  bool actions_update_app_context_nfu(
    /* in */      km_core_context const *cached_context,
    /* in, out */ km_core_context *app_context
  );

} // namespace core
} // namespace km
